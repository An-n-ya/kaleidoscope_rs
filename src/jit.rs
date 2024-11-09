#![allow(unused)]
use std::error::Error;

use llvm_sys::{
    orc2::{LLVMOrcExecutionSessionRef, LLVMOrcJITDylibRef, LLVMOrcObjectLayerRef},
    target::LLVMTargetDataRef,
};

pub struct JIT {
    es: LLVMOrcExecutionSessionRef,
    dl: LLVMTargetDataRef,
    object_layer: LLVMOrcObjectLayerRef,
    main_jd: LLVMOrcJITDylibRef,
}

impl JIT {
    pub fn new() -> Result<Self, Box<dyn Error>> {
        todo!()
    }
}

impl Drop for JIT {
    fn drop(&mut self) {}
}
