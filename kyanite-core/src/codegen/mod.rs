use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate,
};

use crate::{
    ast::{self, node, Node, Type},
    token::{Span, Token, TokenKind},
};
use builtins::Builtins;

mod builtins;

macro_rules! num_instrs  {
    {$self:ident, $bin:ident, $($kind:ident => $int_instr:ident $float_instr:ident),*} => {
        match $bin.op.kind {
            $(
                TokenKind::$kind => {
                    let left = $self.compile(&$bin.left)?;
                    let right = $self.compile(&$bin.right)?;
                    match (left, right) {
                        (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) => {
                            return Ok($self.builder.$int_instr(left, right, "tmp").into())
                        }
                        (BasicValueEnum::FloatValue(left), BasicValueEnum::FloatValue(right)) => {
                            return Ok($self.builder.$float_instr(left, right, "tmp").into())
                        }
                        ty => unreachable!("cannot perform numeric operation on {ty:?}"),
                    }
                }
            )*,
            _ => {
                // fallback
            }
        }
    }
}

macro_rules! bool_instrs {
    {$self:ident, $bin:ident, $conversion:ident, $build_fn:ident, $ty:ident, $predicate:ident, $($kind:ident => $member:ident),*} => {
        if $bin.left.ty() == Type::$ty {
            match $bin.op.kind {
                $(
                    TokenKind::$kind => {
                        let left = $self.compile(&$bin.left)?.$conversion();
                        let right = $self.compile(&$bin.right)?.$conversion();
                        return Ok($self.builder.$build_fn($predicate::$member, left, right, "tmp").into())
                    }
                )*,
                _ => {
                    // fallback
                }
            };
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum IrError {
    #[error("Undefined variable {0}")]
    Undefined(String),

    #[error("Malformed function")]
    MalformedFunction,

    #[error("Undefined function {0}")]
    UndefinedFunction(String),
}

pub struct Ir<'a, 'ctx> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    #[allow(dead_code)]
    fpm: &'a PassManager<FunctionValue<'ctx>>,

    variables: HashMap<String, (PointerValue<'ctx>, Type)>,
    function: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Ir<'a, 'ctx> {
    pub fn from_ast(ast: &ast::Ast) -> Result<String, IrError> {
        let context = Context::create();
        let module = context.create_module("main");
        let builder = context.create_builder();
        let fpm: PassManager<FunctionValue<'_>> = PassManager::create(&module);

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();

        fpm.initialize();

        let mut ir = Ir {
            context: &context,
            module: &module,
            builder: &builder,
            fpm: &fpm,

            variables: HashMap::new(),
            function: None,
        };

        Builtins::new().build(&mut ir);

        for node in &ast.file.nodes {
            match node {
                Node::FuncDecl(func) => ir.function(func),
                Node::ConstantDecl(_) => todo!(),
                _ => unreachable!(
                    "parser is guaranteed to produce a `FuncDecl` or `ConstantDecl` at toplevel"
                ),
            }?;
        }

        Ok(ir.module.print_to_string().to_string())
    }

    fn prototype(&mut self, func: &node::FuncDecl) -> Result<FunctionValue<'ctx>, IrError> {
        let args: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|p| Type::from(&p.ty).as_llvm_basic_type(self).into())
            .collect();
        let types = args.as_slice();

        let fn_ty = if Type::from(func.ty.as_ref()) == Type::Void {
            self.context.void_type().fn_type(types, false)
        } else {
            Type::from(func.ty.as_ref())
                .as_llvm_basic_type(self)
                .fn_type(types, false)
        };

        let val = self
            .module
            .add_function(&String::from(&func.name), fn_ty, None);

        for (i, arg) in val.get_param_iter().enumerate() {
            match arg {
                BasicValueEnum::IntValue(_) => arg
                    .into_int_value()
                    .set_name(&String::from(&func.params[i].name)),
                BasicValueEnum::FloatValue(_) => arg
                    .into_float_value()
                    .set_name(&String::from(&func.params[i].name)),
                BasicValueEnum::PointerValue(_) => arg
                    .into_pointer_value()
                    .set_name(&String::from(&func.params[i].name)),
                _ => unimplemented!("formal parameter type `{arg}` is not implemented"),
            };
        }

        Ok(val)
    }

    fn block(&mut self, block: &[Node]) -> Result<BasicValueEnum<'ctx>, IrError> {
        for node in block {
            self.compile(node)?;
        }

        Ok(self.context.i64_type().const_int(0, false).into())
    }

    fn compile(&mut self, node: &Node) -> Result<BasicValueEnum<'ctx>, IrError> {
        match node {
            Node::Str(s, _) => {
                let bytes = s[1..s.len() - 1].as_bytes().to_vec();
                let global = self
                    .builder
                    .build_global_string_ptr(&"0".repeat(bytes.len()), "tmp");
                global.set_initializer(&self.context.const_string(&bytes, true));
                Ok(global.as_pointer_value().into())
            }
            Node::Float(f, _) => Ok(self.context.f64_type().const_float(*f).into()),
            Node::Call(call) => self.call(call),
            Node::Ident(ident) => self.ident(ident),
            Node::Int(n, _) => Ok(self
                .context
                .i64_type()
                .const_int((*n).try_into().unwrap(), false)
                .into()),
            Node::Binary(binary) => self.binary(binary),
            Node::Return(r) => {
                let val = self.compile(&r.expr)?;
                self.builder.build_return(Some(&val));
                Ok(val)
            }
            _ => todo!("compilation not implemented for {:?}", node),
        }
    }

    fn binary(&mut self, binary: &node::Binary) -> Result<BasicValueEnum<'ctx>, IrError> {
        num_instrs! { self, binary,
            Plus => build_int_add build_float_add,
            Minus => build_int_sub build_float_sub,
            Star => build_int_mul build_float_mul,
            Slash => build_int_signed_div build_float_div
        }

        bool_instrs! { self, binary, into_int_value, build_int_compare, Int, IntPredicate,
            EqualEqual => EQ,
            BangEqual => NE,
            GreaterEqual => SGE,
            LessEqual => SLE,
            Greater => SGT,
            Less => SLT
        }

        bool_instrs! { self, binary, into_float_value, build_float_compare, Float, FloatPredicate,
            EqualEqual => OEQ,
            BangEqual => ONE,
            GreaterEqual => OGE,
            LessEqual => OLE,
            Greater => OGT,
            Less => OLT
        }

        // finally fail if still not implemented (should be type error)
        unimplemented!("binary operation not implemented for {:?}", binary.op.kind)
    }

    fn call(&mut self, call: &node::Call) -> Result<BasicValueEnum<'ctx>, IrError> {
        let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(call.args.len());
        for arg in &call.args {
            args.push(self.compile(arg)?.into());
        }
        let name = match *call.left {
            Node::Ident(ref ident) => String::from(&ident.name),
            _ => todo!(),
        };
        match self.get_function(&name, &args) {
            Some(func) => {
                match self
                    .builder
                    .build_call(func, args.as_slice(), "tmp")
                    .try_as_basic_value()
                    .left()
                {
                    Some(val) => Ok(match val {
                        BasicValueEnum::IntValue(_) => val.into_int_value().into(),
                        BasicValueEnum::FloatValue(_) => val.into_float_value().into(),
                        BasicValueEnum::PointerValue(_) => val.into_pointer_value().into(),
                        _ => unimplemented!("return type `{val}` is not implemented"),
                    }),
                    None => Ok(self.context.i64_type().const_int(0, false).into()),
                }
            }
            None => Err(IrError::UndefinedFunction(name.to_string())),
        }
    }

    fn load(
        &self,
        var: &(PointerValue<'ctx>, Type),
        ty: BasicTypeEnum<'ctx>,
        ident: &node::Ident,
    ) -> BasicValueEnum<'ctx> {
        self.builder.build_load(ty, var.0, &ident.name.to_string())
    }

    fn ident(&mut self, ident: &node::Ident) -> Result<BasicValueEnum<'ctx>, IrError> {
        match self.variables.get(&String::from(&ident.name)) {
            Some(var) => Ok(self.load(var, var.1.as_llvm_basic_type(self), ident)),
            None => Err(IrError::Undefined(String::from(&ident.name))),
        }
    }

    #[inline]
    fn get_function(
        &self,
        name: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> Option<FunctionValue<'ctx>> {
        match name {
            "println" => match args.first().unwrap() {
                BasicMetadataValueEnum::PointerValue(_) => self.module.get_function("println_str"),
                BasicMetadataValueEnum::IntValue(i) => {
                    if i.get_type().get_bit_width() == 1 {
                        self.module.get_function("println_bool")
                    } else {
                        self.module.get_function("println_int")
                    }
                }
                BasicMetadataValueEnum::FloatValue(_) => self.module.get_function("println_float"),
                node => unreachable!("impossible to call println with {node:?}"),
            },
            "max" => match args.first().unwrap() {
                BasicMetadataValueEnum::IntValue(_) => self.module.get_function("max_int"),
                BasicMetadataValueEnum::FloatValue(_) => self.module.get_function("max_float"),
                node => unreachable!("impossible to call max with {}", node),
            },
            "min" => match args.first().unwrap() {
                BasicMetadataValueEnum::IntValue(_) => self.module.get_function("min_int"),
                BasicMetadataValueEnum::FloatValue(_) => self.module.get_function("min_float"),
                node => unreachable!("impossible to call min with {}", node),
            },
            _ => self.module.get_function(name),
        }
    }

    fn alloca(&self, name: &str, arg: &BasicValueEnum) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.function.unwrap().get_first_basic_block().unwrap();
        match entry.get_first_instruction() {
            Some(instr) => builder.position_before(&instr),
            None => builder.position_at_end(entry),
        }
        match arg {
            BasicValueEnum::IntValue(_) => builder.build_alloca(self.context.i64_type(), name),
            BasicValueEnum::FloatValue(_) => builder.build_alloca(self.context.f64_type(), name),
            BasicValueEnum::PointerValue(_) => builder.build_alloca(
                self.context.i8_type().ptr_type(AddressSpace::default()),
                name,
            ),
            _ => unimplemented!("alloca is not implemented for {}", arg),
        }
    }

    pub fn function(&mut self, func: &node::FuncDecl) -> Result<FunctionValue<'ctx>, IrError> {
        let proto = self.prototype(func)?;
        self.function = Some(proto);

        let entry = self.context.append_basic_block(proto, "entry");
        self.builder.position_at_end(entry);
        self.variables.reserve(func.params.len());

        for (i, arg) in proto.get_param_iter().enumerate() {
            let name = String::from(&func.params[i].name);
            let allocation = self.alloca(&name, &arg);
            self.builder.build_store(allocation, arg);
            self.variables.insert(
                String::from(&func.params[i].name),
                (allocation, Type::from(&func.params[i].ty)),
            );
        }

        self.block(&func.body)?;
        let void = Type::from(&func.ty.clone().unwrap_or(Token {
            kind: TokenKind::Type,
            lexeme: Some("void".into()),
            span: Span::new(0, 0, 0),
        })) == Type::Void;
        if void {
            self.builder.build_return(None);
        }

        if let Some(function) = self.function {
            if function.verify(true) {
                self.fpm.run_on(&function);

                return Ok(function);
            } else {
                unsafe { function.delete() };
            }
        }

        Err(IrError::MalformedFunction)
    }
}
