mod builtins;

use crate::{
    ast::{
        node::{self, Ident, RecordDecl},
        Decl, Expr, Stmt, Type,
    },
    backend::llvm::builtins::Builtins,
    pass::{AccessMap, Symbol, SymbolTable},
    token::{Kind, Span, Token},
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType},
    values::{AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate,
};
use std::{collections::HashMap, rc::Rc};

macro_rules! num_instrs  {
    {$self:ident, $bin:ident, $($kind:ident => $int_instr:ident $float_instr:ident),*} => {
        match $bin.op.kind {
            $(
                Kind::$kind => {
                    let left = $self.expr(&$bin.left)?;
                    let right = $self.expr(&$bin.right)?;
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
                    Kind::$kind => {
                        let left = $self.expr(&$bin.left)?.$conversion();
                        let right = $self.expr(&$bin.right)?.$conversion();
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
    #[error("Malformed {0}")]
    Malformed(&'static str),
}

// TODO: look at having this be less of a mess
pub struct Ir<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    fpm: &'a PassManager<FunctionValue<'ctx>>,
    variables: HashMap<String, (PointerValue<'ctx>, Type)>,
    records: HashMap<String, (StructType<'ctx>, Rc<RecordDecl>)>,
    function: Option<FunctionValue<'ctx>>,
    symbols: SymbolTable,
    accesses: AccessMap,
}

impl<'a, 'ctx> Ir<'a, 'ctx> {
    pub fn build(
        program: &mut Vec<Decl>,
        symbols: SymbolTable,
        accesses: AccessMap,
    ) -> Result<String, IrError> {
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
            records: HashMap::new(),
            function: None,

            symbols,
            accesses,
        };

        // Inject builtin function declarations
        Builtins::inject(&mut ir)?;

        // entrypoint - compile all toplevel nodes
        for node in program {
            ir.decl(node)?;
        }

        Ok(ir.module.print_to_string().to_string())
    }

    fn decl(&mut self, decl: &mut Decl) -> Result<AnyValueEnum<'ctx>, IrError> {
        match decl {
            Decl::Function(fun) => self.function(fun).map(Into::into),
            Decl::Constant(_) => todo!(),
            Decl::Implementation(_) => todo!(),
            Decl::Record(rec) => Ok(self.record(rec).into()),
        }
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result<AnyValueEnum<'ctx>, IrError> {
        match stmt {
            Stmt::Assign(assign) => self.assign(assign).map(Into::into),
            Stmt::Expr(expr) => self.expr(expr),
            Stmt::Return(r) => self.ret(r),
            Stmt::Var(var) => self.var(var),
            Stmt::If(_) => todo!(),
            Stmt::While(_) => todo!(),
            Stmt::For(_) => todo!(),
        }
    }

    fn expr(&mut self, expr: &Expr) -> Result<AnyValueEnum<'ctx>, IrError> {
        match expr {
            Expr::Str(s) => Ok(self.str(s.value)),
            Expr::Access(a) => Ok(self.access(a).into()),
            Expr::Bool(b) => Ok(self
                .context
                .bool_type()
                .const_int(u64::from(b.value), false)
                .into()),
            Expr::Float(f) => Ok(self.context.f64_type().const_float(f.value).into()),
            Expr::Call(call) => self.call(call).map(Into::into),
            Expr::Ident(ident) => self.ident(ident).map(Into::into),
            Expr::Int(n) => Ok(self.int(n.value).into()),
            Expr::Binary(binary) => self.binary(binary).map(Into::into),
            Expr::Unary(unary) => self.unary(unary).map(Into::into),
            Expr::Init(init) => self.init(init).map(Into::into),
            Expr::Range(_) => todo!(),
        }
    }

    fn build_struct(&mut self, record: &node::RecordDecl) -> StructType<'ctx> {
        self.context.struct_type(
            record
                .fields
                .iter()
                .map(|f| match Type::from(&f.ty) {
                    Type::UserDefined(name) => {
                        let symbol = self.symbols.get(&name).unwrap().clone();
                        match symbol {
                            Symbol::Record(rec) => self.build_struct(&rec).into(),
                            _ => unreachable!(),
                        }
                    }
                    ty => ty.to_basic_type_enum(self),
                })
                .collect::<Vec<BasicTypeEnum>>()
                .as_slice(),
            false,
        )
    }

    fn record(&mut self, record: &Rc<node::RecordDecl>) -> BasicValueEnum<'ctx> {
        let rec = self.build_struct(record);
        self.records
            .insert(record.name.to_string(), (rec, Rc::clone(record)));
        self.context.i64_type().const_int(0, false).into()
    }

    fn init(&mut self, init: &node::Init) -> Result<BasicValueEnum<'ctx>, IrError> {
        let (rec, _) = *self.records.get(&init.name.to_string()).unwrap();
        let mut values = vec![];
        for init in &init.initializers {
            values.push(
                self.expr(&init.expr)?
                    .try_into()
                    .map_err(|()| IrError::Malformed("init expression"))?,
            );
        }
        Ok(rec.const_named_struct(values.as_slice()).into())
    }

    fn indices(&mut self, access: &node::Access) -> (Vec<(u32, Type)>, Type) {
        let access = self.accesses.remove(&access.id).unwrap();
        let indices = access
            .symbols
            .iter()
            .zip(access.indices)
            .map(|(symbol, index)| (u32::try_from(index).unwrap(), symbol.ty()))
            .collect();
        (indices, access.ty)
    }

    fn gep(&mut self, access: &node::Access) -> (PointerValue<'ctx>, BasicTypeEnum<'ctx>) {
        let (ptr, ty) = match access.chain.first().unwrap() {
            Expr::Ident(ident) => self.variables.get(&ident.name.to_string()).unwrap().clone(),
            Expr::Call(_) => todo!(),
            _ => unimplemented!(),
        };
        let (indices, last) = self.indices(access);
        let ty = self
            .records
            .get(&ty.to_string())
            .map(|(ty, _)| ty)
            .copied()
            .unwrap();
        let gep = indices.iter().skip(1).fold(
            self.builder
                .build_struct_gep(ty, ptr, indices[0].0, "tmp")
                .unwrap(),
            |gep, (index, ty)| {
                self.builder
                    .build_struct_gep(ty.to_basic_type_enum(self), gep, *index, "tmp")
                    .unwrap()
            },
        );
        (gep, last.to_basic_type_enum(self))
    }

    fn access(&mut self, access: &node::Access) -> BasicValueEnum<'ctx> {
        let (gep, field_ty) = self.gep(access);
        self.builder.build_load(field_ty, gep, "tmp")
    }

    /// Compiles a function prototype into a `FunctionValue`
    fn prototype(&mut self, func: &node::FuncDecl) -> FunctionValue<'ctx> {
        // Collect the function argument types and convert them to LLVM types
        let args: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|p| p.ty.to_basic_type_enum(self).into())
            .collect();
        let types = args.as_slice();

        // `fn_type` creates a function type with the specified `types` (argument types)
        let fn_ty = if func.ty == Type::Void {
            self.context.void_type().fn_type(types, false)
        } else {
            func.ty.to_basic_type_enum(self).fn_type(types, false)
        };

        // Adds the function to the module as a complete function value
        let val = self
            .module
            .add_function(&func.name.to_string(), fn_ty, None);

        // Iterate through the function arguments and assign names to them
        for (i, arg) in val.get_param_iter().enumerate() {
            let name = func.params[i].name.to_string();
            match arg {
                BasicValueEnum::IntValue(_) => arg.into_int_value().set_name(&name),
                BasicValueEnum::FloatValue(_) => arg.into_float_value().set_name(&name),
                BasicValueEnum::PointerValue(_) => arg.into_pointer_value().set_name(&name),
                BasicValueEnum::StructValue(_) => arg.into_struct_value().set_name(&name),
                _ => unimplemented!("formal parameter type `{arg}` is not implemented"),
            };
        }

        val
    }

    /// Wraps the main function
    pub fn main(&mut self, func: &Rc<node::FuncDecl>) -> Result<FunctionValue<'ctx>, IrError> {
        // Rename the main function to avoid conflicts with the wrapper
        let mut modified = node::FuncDecl::new(
            func.name.clone(),
            func.params.clone(),
            func.ty.clone(),
            func.body.clone(),
            func.external,
        );
        modified.name.lexeme = Some("_main");
        self.function(&Rc::new(modified))?;

        // TODO: collect CLI arguments
        let types: &[BasicMetadataTypeEnum] = &[];

        let fn_ty = self.context.i64_type().fn_type(types, false);
        let val = self.module.add_function("main", fn_ty, None);

        let entry = self.context.append_basic_block(val, "entry");
        self.builder.position_at_end(entry);

        self.call(&main())?;
        // TODO: handle non-zero exit codes
        self.builder
            .build_return(Some(&self.context.i64_type().const_int(0, false)));

        Ok(val)
    }

    /// Compiles a function body into a `FunctionValue`
    pub fn function(&mut self, func: &Rc<node::FuncDecl>) -> Result<FunctionValue<'ctx>, IrError> {
        // Special case main function
        if func.name == "main" {
            return self.main(func);
        }
        // Compile our function prototype and set as current function
        let proto = self.prototype(func);
        if func.external {
            return Ok(proto);
        }
        self.function = Some(proto);

        // Create a block using the function prototype to compile instructions into
        let entry = self.context.append_basic_block(proto, "entry");
        // Position the builder at the end of the `entry` block
        self.builder.position_at_end(entry);
        // Reserve space for the function arguments
        self.variables.reserve(func.params.len());

        // Iterate through the function prototype, create allocations for each argument and add it to variables map
        for (i, arg) in proto.get_param_iter().enumerate() {
            let name = func.params[i].name.to_string();
            let allocation = self.alloca(&name, &arg);
            self.builder.build_store(allocation, arg);
            self.variables
                .insert(name, (allocation, (&func.params[i].ty).into()));
        }

        // Compile the body of the function
        self.block(&func.body)?;

        // Add a return statement on behalf of the user if the function returns void
        if func.ty == Type::Void {
            self.builder.build_return(None);
        }

        // Once we've compiled the function, we can discard the variables map
        // since none of them should remain valid
        self.variables.clear();

        if let Some(function) = self.function {
            if function.verify(true) {
                // Optimize the function
                self.fpm.run_on(&function);

                return Ok(function);
            }

            unsafe { function.delete() };
        }

        // Failed to produce a valid function
        Err(IrError::MalformedFunction)
    }

    fn block(&mut self, block: &[Stmt]) -> Result<(), IrError> {
        for node in block {
            match self.stmt(node) {
                Ok(_) => {}
                Err(e) => return Err(e),
            }
        }

        Ok(())
    }

    /// Injects a string literal
    fn str(&mut self, s: &str) -> AnyValueEnum<'ctx> {
        // Figure out the actual bytes of the string excluding the opening and closing quotes
        let bytes = s[1..s.len() - 1].as_bytes().to_vec();
        // Create a global string pointer with the appropriate length of zeroed out bytes
        let global = self
            .builder
            .build_global_string_ptr(&"0".repeat(bytes.len()), "tmp");
        // Set the initializer of the global string pointer to the actual string
        // (this is kinda hacky, but otherwise character escapes are incorrect)
        global.set_initializer(&self.context.const_string(&bytes, true));
        global.as_pointer_value().into()
    }

    fn int(&self, n: i64) -> BasicValueEnum<'ctx> {
        self.context
            .i64_type()
            .const_int(n.try_into().unwrap(), false)
            .into()
    }

    fn ret(&mut self, r: &node::Return) -> Result<AnyValueEnum<'ctx>, IrError> {
        let any = self.expr(&r.expr)?;
        let val: BasicValueEnum<'_> = any
            .try_into()
            .map_err(|()| IrError::Malformed("expression in return statement"))?;
        self.builder.build_return(Some(&val));
        Ok(any)
    }

    fn var(&mut self, var: &node::VarDecl) -> Result<AnyValueEnum<'ctx>, IrError> {
        let ty = Type::from(&var.ty);
        let name = var.name.to_string();
        let value = self
            .expr(&var.expr)?
            .try_into()
            .map_err(|()| IrError::Malformed("variable declaration"))?;
        let alloca = self.alloca(&name, &value);
        self.builder.build_store(alloca, value);
        self.variables.insert(name, (alloca, ty));
        Ok(value.into())
    }

    fn assign(&mut self, assign: &node::Assign) -> Result<BasicValueEnum<'ctx>, IrError> {
        // Retreive the pointer to the variable in question
        let ptr = match &assign.target {
            Expr::Ident(ref ident) => {
                let name = ident.name.to_string();
                match self.variables.get(&name) {
                    Some((ptr, _)) => *ptr,
                    None => return Err(IrError::Undefined(name)),
                }
            }
            Expr::Access(access) => self.gep(access).0,
            _ => unimplemented!(),
        };
        // Compile the right-hand-side of assignment to an expression
        let value: BasicValueEnum<'_> = self
            .expr(&assign.expr)?
            .try_into()
            .map_err(|()| IrError::Malformed("right-hand side of assignment"))?;
        // Store the updated value in the variable
        self.builder.build_store(ptr, value);
        Ok(self.context.i64_type().const_zero().into())
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

    fn unary(&mut self, unary: &node::Unary) -> Result<BasicValueEnum<'ctx>, IrError> {
        let expr = self.expr(&unary.expr)?;
        Ok(match unary.op.kind {
            Kind::Minus => match expr {
                AnyValueEnum::IntValue(i) => i.const_neg().into(),
                AnyValueEnum::FloatValue(f) => f.const_neg().into(),
                _ => unimplemented!("cannot perform `-` on {expr:?}"),
            },
            Kind::Bang => match expr {
                AnyValueEnum::IntValue(i) => i.const_not().into(),
                _ => unimplemented!("cannot perform `!` on {expr:?}"),
            },
            _ => unimplemented!("unary operation not implemented for {:?}", unary.op.kind),
        })
    }

    fn call(&mut self, call: &node::Call) -> Result<BasicValueEnum<'ctx>, IrError> {
        // Collect the arguments and convert them to LLVM types
        let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(call.args.len());
        for arg in &call.args {
            args.push(
                self.expr(arg)?
                    .try_into()
                    .map_err(|()| IrError::Malformed("expression to call expr"))?,
            );
        }
        // Retreive the name of the function
        // (this needs to be updated when member access is implemented)
        let name = match *call.left {
            Expr::Ident(ref ident) => ident.name.to_string(),
            Expr::Access(_) => todo!(),
            _ => unimplemented!(),
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
                    std::convert::identity,
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
        let name = ident.name.to_string();
        match self.variables.get(&name) {
            Some(var) => Ok(self.load(var, var.1.to_basic_type_enum(self), ident)),
            None => Err(IrError::Undefined(name)),
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

    fn alloca(&self, name: &str, arg: &BasicValueEnum<'ctx>) -> PointerValue<'ctx> {
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
            BasicValueEnum::StructValue(_) => {
                builder.build_alloca(arg.get_type().into_struct_type(), name)
            }
            _ => unimplemented!("alloca is not implemented for {}", arg),
        }
    }
}

trait ToBasicTypeEnum {
    fn to_basic_type_enum<'ctx>(&self, ir: &Ir<'_, 'ctx>) -> BasicTypeEnum<'ctx>;
}

impl ToBasicTypeEnum for Type {
    fn to_basic_type_enum<'ctx>(&self, ir: &Ir<'_, 'ctx>) -> BasicTypeEnum<'ctx> {
        match self {
            Type::Int => ir.context.i64_type().into(),
            Type::Float => ir.context.f64_type().into(),
            Type::Str => ir
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            Type::Bool => ir.context.bool_type().into(),
            // TODO: this may be something other than a record in the future
            Type::UserDefined(name) => ir
                .records
                .get(name)
                .expect("called before all records built")
                .0
                .into(),
            Type::Void => unimplemented!("void does not implement `BasicTypeEnum`"),
        }
    }
}

impl ToBasicTypeEnum for Token {
    fn to_basic_type_enum<'ctx>(&self, ir: &Ir<'_, 'ctx>) -> BasicTypeEnum<'ctx> {
        Type::from(self).to_basic_type_enum(ir)
    }
}

impl ToBasicTypeEnum for Option<Token> {
    fn to_basic_type_enum<'ctx>(&self, ir: &Ir<'_, 'ctx>) -> BasicTypeEnum<'ctx> {
        Type::from(self.as_ref()).to_basic_type_enum(ir)
    }
}

fn main() -> node::Call {
    node::Call::new(
        Box::new(Ident::wrapped(Token {
            kind: Kind::Identifier,
            lexeme: Some("_main"),
            span: Span::new(0, 0, 0),
        })),
        vec![],
        (
            Token {
                kind: Kind::LeftParen,
                lexeme: None,
                span: Span::new(0, 0, 0),
            },
            Token {
                kind: Kind::RightParen,
                lexeme: None,
                span: Span::new(0, 0, 0),
            },
        ),
        vec![],
    )
}
