use inkwell::{types::BasicMetadataTypeEnum, values::BasicValueEnum, AddressSpace};

use super::{Builtin, Ir};

pub struct Println {}

impl Builtin for Println {
    fn build(&self, ir: &mut Ir<'_, '_>) {
        println(ir, ir.context.i64_type().into(), "println_int");
        println(ir, ir.context.bool_type().into(), "println_bool");
        println(ir, ir.context.f64_type().into(), "println_float");
        println(
            ir,
            ir.context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            "println_str",
        );
    }
}

fn println<'ctx>(ir: &mut Ir<'_, 'ctx>, ty: BasicMetadataTypeEnum<'ctx>, name: &str) {
    let types: &[BasicMetadataTypeEnum] = &[ty];

    let fn_ty = ir.context.void_type().fn_type(types, false);
    let val = ir.module.add_function(name, fn_ty, None);

    for (_, arg) in val.get_param_iter().enumerate() {
        match arg {
            BasicValueEnum::IntValue(_) => arg.into_int_value().set_name("i"),
            BasicValueEnum::FloatValue(_) => arg.into_float_value().set_name("f"),
            BasicValueEnum::PointerValue(_) => arg.into_pointer_value().set_name("s"),
            _ => unreachable!("println builtin has hardcoded formal parameters"),
        };
    }
}
