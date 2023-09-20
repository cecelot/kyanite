use inkwell::{
    types::{BasicMetadataTypeEnum, BasicType},
    values::BasicValueEnum,
};

use super::Builtin;
use crate::codegen::Ir;

pub struct Math {}

impl Builtin for Math {
    fn build(&self, ir: &mut Ir<'_, '_>) {
        pair(
            ir,
            &[ir.context.i64_type().into(), ir.context.i64_type().into()],
            ir.context.i64_type(),
            "max_int",
        );
        pair(
            ir,
            &[ir.context.i64_type().into(), ir.context.i64_type().into()],
            ir.context.i64_type(),
            "min_int",
        );
        pair(
            ir,
            &[ir.context.f64_type().into(), ir.context.f64_type().into()],
            ir.context.f64_type(),
            "max_float",
        );
        pair(
            ir,
            &[ir.context.f64_type().into(), ir.context.f64_type().into()],
            ir.context.f64_type(),
            "min_float",
        );
    }
}

fn pair<'ctx>(
    ir: &mut Ir<'_, 'ctx>,
    types: &[BasicMetadataTypeEnum<'ctx>],
    ret: impl BasicType<'ctx>,
    name: &str,
) {
    let fn_ty = ret.fn_type(types, false);
    let val = ir.module.add_function(name, fn_ty, None);

    for (_, arg) in val.get_param_iter().enumerate() {
        match arg {
            BasicValueEnum::IntValue(_) => arg.into_int_value().set_name("i"),
            BasicValueEnum::FloatValue(_) => arg.into_float_value().set_name("f"),
            _ => unimplemented!(),
        };
    }
}
