use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate,
};

use crate::{
    ast::{self, node, Node, Type},
    token::TokenKind,
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
                        (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => {
                            return Ok($self.builder.$int_instr(left, right, "tmp").into())
                        }
                        (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => {
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
    #[error("Malformed function call")]
    MalformedCall,
    #[error("Malformed return statement")]
    MalformedReturn,
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

        // Inject builtin function declarations
        Builtins::new().build(&mut ir);

        // entrypoint - compile all toplevel nodes
        for node in &ast.file.nodes {
            ir.toplevel(node)?;
        }

        Ok(ir.module.print_to_string().to_string())
    }

    fn toplevel(&mut self, node: &Node) -> Result<AnyValueEnum<'ctx>, IrError> {
        match node {
            Node::FuncDecl(func) => self.function(func).map(|v| v.into()),
            Node::ConstantDecl(_) => todo!(),
            _ => unreachable!(
                "parser is guaranteed to produce a `FuncDecl` or `ConstantDecl` at toplevel"
            ),
        }
    }

    fn compile(&mut self, node: &Node) -> Result<AnyValueEnum<'ctx>, IrError> {
        match node {
            Node::Str(s, _) => self.str(s),
            Node::Float(f, _) => Ok(self.context.f64_type().const_float(*f).into()),
            Node::Call(call) => self.call(call).map(|v| v.into()),
            Node::Ident(ident) => self.ident(ident).map(|v| v.into()),
            Node::Int(n, _) => self.int(n).map(|v| v.into()),
            Node::Binary(binary) => self.binary(binary).map(|v| v.into()),
            Node::Return(r) => self.ret(r),
            _ => todo!("compilation not implemented for {:?}", node),
        }
    }

    /// Compiles a function prototype into a `FunctionValue`
    fn prototype(&mut self, func: &node::FuncDecl) -> Result<FunctionValue<'ctx>, IrError> {
        // Collect the function argument types and convert them to LLVM types
        let args: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|p| Type::from(&p.ty).as_llvm_basic_type(self).into())
            .collect();
        let types = args.as_slice();

        // `fn_type` creates a function type with the specified `types` (argument types)
        let fn_ty = if Type::from(func.ty.as_ref()) == Type::Void {
            self.context.void_type().fn_type(types, false)
        } else {
            Type::from(func.ty.as_ref())
                .as_llvm_basic_type(self)
                .fn_type(types, false)
        };

        // Adds the function to the module as a complete function value
        let val = self
            .module
            .add_function(&String::from(&func.name), fn_ty, None);

        // Iterate through the function arguments and assign names to them
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

    /// Compiles a function body into a `FunctionValue`
    pub fn function(&mut self, func: &node::FuncDecl) -> Result<FunctionValue<'ctx>, IrError> {
        // Compile our function prototype and set as current function
        let proto = self.prototype(func)?;
        self.function = Some(proto);

        // Create a block using the function prototype to compile instructions into
        let entry = self.context.append_basic_block(proto, "entry");
        // Position the builder at the end of the `entry` block
        self.builder.position_at_end(entry);
        // Reserve space for the function arguments
        self.variables.reserve(func.params.len());

        // Iterate through the function prototype, create allocations for each argument and add it to variables map
        for (i, arg) in proto.get_param_iter().enumerate() {
            let name = String::from(&func.params[i].name);
            let allocation = self.alloca(&name, &arg);
            self.builder.build_store(allocation, arg);
            self.variables.insert(
                String::from(&func.params[i].name),
                (allocation, Type::from(&func.params[i].ty)),
            );
        }

        // Compile the body of the function
        self.block(&func.body)?;

        // Add a return statement on behalf of the user if the function returns void
        if Type::from(func.ty.as_ref()) == Type::Void {
            self.builder.build_return(None);
        }

        if let Some(function) = self.function {
            if function.verify(true) {
                // Optimize the function
                self.fpm.run_on(&function);

                return Ok(function);
            } else {
                unsafe { function.delete() };
            }
        }

        // Failed to produce a valid function
        Err(IrError::MalformedFunction)
    }

    fn block(&mut self, block: &[Node]) -> Result<(), IrError> {
        for node in block {
            match self.compile(node) {
                Ok(_) => {}
                Err(e) => return Err(e),
            }
        }

        Ok(())
    }

    /// Injects a string literal
    fn str(&mut self, s: &String) -> Result<AnyValueEnum<'ctx>, IrError> {
        // Figure out the actual bytes of the string excluding the opening and closing quotes
        let bytes = s[1..s.len() - 1].as_bytes().to_vec();
        // Create a global string pointer with the appropriate length of zeroed out bytes
        let global = self
            .builder
            .build_global_string_ptr(&"0".repeat(bytes.len()), "tmp");
        // Set the initializer of the global string pointer to the actual string
        // (this is kinda hacky, but otherwise character escapes are incorrect)
        global.set_initializer(&self.context.const_string(&bytes, true));
        Ok(global.as_pointer_value().into())
    }

    fn int(&self, n: &i64) -> Result<BasicValueEnum<'ctx>, IrError> {
        Ok(self
            .context
            .i64_type()
            .const_int((*n).try_into().unwrap(), false)
            .into())
    }

    fn ret(&mut self, r: &node::Return) -> Result<AnyValueEnum<'ctx>, IrError> {
        let any = self.compile(&r.expr)?;
        let val: BasicValueEnum<'_> = any.try_into().map_err(|_| IrError::MalformedReturn)?;
        self.builder.build_return(Some(&val));
        Ok(any)
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
        // Collect the arguments and convert them to LLVM types
        let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(call.args.len());
        for arg in &call.args {
            args.push(
                self.compile(arg)?
                    .try_into()
                    .map_err(|_| IrError::MalformedCall)?,
            );
        }
        // Retreive the name of the function
        // (this needs to be updated when member access is implemented)
        let name = match *call.left {
            Node::Ident(ref ident) => String::from(&ident.name),
            _ => todo!(),
        };
        match self.get_function(&name, &args) {
            Some(func) => Ok(self
                .builder
                .build_call(func, args.as_slice(), "tmp")
                .try_as_basic_value()
                .left()
                .map_or_else(
                    // Dummy value if the function returns void
                    || self.context.i64_type().const_int(0, false).into(),
                    |val| val,
                )),
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
            // HACK: handle builtin functions
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
}
