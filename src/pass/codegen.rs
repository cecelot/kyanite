use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};

use crate::{
    ast::{node, File, Node, Type},
    builtins::Builtins,
    token::{Span, Token, TokenKind},
};

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
    pub fn build(file: &File) -> Result<(), IrError> {
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

        for node in &file.nodes {
            match node {
                Node::FuncDecl(func) => ir.function(func),
                _ => unimplemented!(),
            }?;
        }

        ir.module
            .print_to_file(std::path::Path::new("out.ll"))
            .unwrap();

        Ok(())
    }

    fn prototype(&mut self, func: &node::FuncDecl) -> Result<FunctionValue<'ctx>, IrError> {
        let args: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|p| Type::from(&p.ty).llvm(self).into())
            .collect();
        let types = args.as_slice();

        let fn_ty = if Type::from(func.ty.as_ref()) == Type::Void {
            self.context.void_type().fn_type(types, false)
        } else {
            Type::from(func.ty.as_ref())
                .llvm(self)
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
                _ => unimplemented!(),
            };
        }

        Ok(val)
    }

    fn block(&mut self, block: &[Node]) -> Result<BasicValueEnum<'ctx>, IrError> {
        for node in block {
            self.expression(node)?;
        }

        Ok(self.context.i64_type().const_int(0, false).into())
    }

    fn expression(&mut self, node: &Node) -> Result<BasicValueEnum<'ctx>, IrError> {
        match node {
            Node::Str(s) => {
                let bytes = s[1..s.len() - 1].as_bytes().to_vec();
                let global = self
                    .builder
                    .build_global_string_ptr(&"0".repeat(bytes.len()), "tmp");
                global.set_initializer(&self.context.const_string(&bytes, true));
                Ok(global.as_pointer_value().into())
            }
            Node::Float(f) => Ok(self.context.f64_type().const_float(*f).into()),
            Node::Call(call) => self.call(call),
            Node::Ident(ident) => self.ident(ident),
            Node::Int(n) => Ok(self
                .context
                .i64_type()
                .const_int((*n).try_into().unwrap(), false)
                .into()),
            Node::Return(r) => {
                let val = self.expression(&r.expr)?;
                self.builder.build_return(Some(&val));
                Ok(val)
            }
            _ => unimplemented!("codegen for {:?}", node),
        }
    }

    fn call(&mut self, call: &node::Call) -> Result<BasicValueEnum<'ctx>, IrError> {
        let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(call.args.len());
        for arg in &call.args {
            args.push(self.expression(arg)?.into());
        }
        let name = match *call.left {
            Node::Ident(ref ident) => String::from(&ident.name),
            _ => todo!(),
        };
        let name = if name == "println" {
            match args.first().unwrap() {
                BasicMetadataValueEnum::PointerValue(_) => "println_str",
                BasicMetadataValueEnum::IntValue(_) => "println_int",
                BasicMetadataValueEnum::FloatValue(_) => "println_float",
                node => unimplemented!("println is not implemented for {}", node),
            }
        } else {
            &name
        };
        match self.get_function(name) {
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
                        _ => unimplemented!(),
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
            Some(var) => Ok(self.load(var, var.1.llvm(self), ident)),
            None => Err(IrError::Undefined(String::from(&ident.name))),
        }
    }

    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
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
            _ => unimplemented!(),
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
            span: Span::new(0, 0),
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
