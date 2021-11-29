extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod bstruct_ast;
pub mod bstruct_json;
pub mod bstruct_link;

use crate::bstruct_ast::{parse_bstruct_file, ASTRootStatement, ParseError};
pub use crate::bstruct_json::compile_to_json;
use crate::bstruct_link::{BStructLinker, LinkError};
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

#[repr(C)]
pub struct FileInfo {
  name: *const c_char,
  content: *const c_char,
}

#[repr(C)]
pub struct CompileResult {
  err: bool,
  value: *mut c_char,
}

#[no_mangle]
pub extern "C" fn compile_files_to_json(s: *const FileInfo, count: usize) -> CompileResult {
  let mut root_statements: Vec<ASTRootStatement> = vec![];

  for i in 0..count {
    let name = unsafe { CStr::from_ptr((*s.offset(i as isize)).name) };
    let content = unsafe { CStr::from_ptr((*s.offset(i as isize)).content) };

    match parse_bstruct_file(content.to_str().unwrap()) {
      Ok(it) => root_statements.extend(it),
      Err(err) => match err {
        ParseError::PestError(it) => return build_error_result(
          format!("Error parsing {}: {}", name.to_str().unwrap(), it)
        ),
      },
    };
  }

  let mut linker = BStructLinker::new();

  match linker.link(&root_statements) {
    Ok(_) => {}
    Err(err) => match err {
      LinkError::UnknownType(it) => return build_error_result(
        format!("Unknown type {}", it.as_str())
      ),
      LinkError::EnumDoesNotExtendIntegerPrimitive(it) =>return build_error_result(
        format!("Enum must extend integer primitive (found {})", it.as_str())
      ),
      LinkError::CicrularReference(it) =>return build_error_result(
        format!("Reference loop found: {}",
                it.iter()
                  .map(|v| v.value.as_str())
                  .collect::<Vec<&str>>()
                  .join(" -> "))
      ),
      LinkError::TODO(msg, id) =>return build_error_result(
        format!("TODO: {} {}", msg, id.as_str())
      ),
      LinkError::StructsCanOnlyExtendStructs { s, parent } =>return build_error_result(
        format!("Structs can only extend other structs: {}: {}",
                s.value, parent.value)
      ),
      LinkError::AttemptToSpecializeNonTemplatedType(it) =>return build_error_result(
        format!( "Attempt to specialize type which does not have a template: {}",
                 it.value)
      ),
    },
  }

  let compiled = compile_to_json(linker.get_structs(), linker.get_enums());
  let c_str = CString::new(compiled).unwrap();

  CompileResult {
    err: false,
    value: c_str.into_raw(),
  }
}

fn build_error_result(msg: String) -> CompileResult {
  let c_str = CString::new(msg).unwrap();
  CompileResult {
    err: true,
    value: c_str.into_raw(),
  }
}


#[no_mangle]
pub extern "C" fn release_result(r: CompileResult) {
  unsafe {
    if r.value.is_null() {
      return;
    }
    CString::from_raw(r.value)
  };
}