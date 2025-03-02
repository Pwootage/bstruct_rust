extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod bstruct_ast;
pub mod bstruct_json;
pub mod bstruct_link;

use crate::bstruct_ast::{parse_bstruct_file, ASTRootStatement, ParseError};
pub use crate::bstruct_json::compile_to_json;
use crate::bstruct_link::{BEnum, BStruct, BStructLinker, LinkError};

use std::fs;
use walkdir::WalkDir;

#[derive(Debug)]
pub enum CompileError {
  ReadError(std::io::Error),
  ParseError(ParseError),
  LinkError(LinkError),
}

#[derive(Debug)]
pub struct CompileResult {
  pub structs: Vec<BStruct>,
  pub enums: Vec<BEnum>,
}

pub fn build_directory(path: &str) -> Result<CompileResult, CompileError> {
  let mut root_statements: Vec<ASTRootStatement> = vec![];
  for entry in WalkDir::new(path).into_iter().filter_map(Result::ok) {
    let path = entry.path();
    if path.is_file() && path.extension().map_or(false, |ext| ext == "bs") {
      let content = match fs::read_to_string(&path) {
        Ok(it) => it,
        Err(err) => return Err(CompileError::ReadError(err)),
      };

      match parse_bstruct_file(content.as_str()) {
        Ok(it) => root_statements.extend(it),
        Err(err) => return Err(CompileError::ParseError(err)),
      };
    }
  }

  let mut linker = BStructLinker::new();

  match linker.link(&root_statements) {
    Ok(_) => {}
    Err(err) => return Err(CompileError::LinkError(err)),
  }

  Ok(CompileResult {
    structs: linker.get_structs(),
    enums: linker.get_enums(),
  })
}

// #[no_mangle]
// pub extern "C" fn compile_glob_to_json(glob_str: *const c_char) -> CompileResult {
//   let mut root_statements: Vec<ASTRootStatement> = vec![];
//
//   let glob_cstr = unsafe { CStr::from_ptr(glob_str) };
//   let files = match glob(glob_cstr.to_str().unwrap()) {
//     Ok(it) => it,
//     Err(err) => return build_error_result(
//       format!("Error finding files: {}", err)
//     ),
//   };
//
//   for file in files {
//     let path = match file {
//       Ok(it) => it,
//       Err(err) => return build_error_result(
//         format!("Error finding files: {}", err)
//       )
//     };
//     let content = match fs::read_to_string(&path) {
//       Ok(it) => it,
//       Err(err) => return build_error_result(
//         format!("Error reading files: {}", err)
//       )
//     };
//
//     match parse_bstruct_file(content.as_str()) {
//       Ok(it) => root_statements.extend(it),
//       Err(err) => match err {
//         ParseError::PestError(it) => return build_error_result(
//           format!("Error parsing {}: {}", &path.to_str().unwrap(), it)
//         ),
//       },
//     };
//   }
//
//   let mut linker = BStructLinker::new();
//
//   match linker.link(&root_statements) {
//     Ok(_) => {}
//     Err(err) => match err {
//       LinkError::UnknownType(it) => return build_error_result(
//         format!("Unknown type {}", it.as_str())
//       ),
//       LinkError::EnumDoesNotExtendIntegerPrimitive(it) =>return build_error_result(
//         format!("Enum must extend integer primitive (found {})", it.as_str())
//       ),
//       LinkError::CicrularReference(it) =>return build_error_result(
//         format!("Reference loop found: {}",
//                 it.iter()
//                   .map(|v| v.value.as_str())
//                   .collect::<Vec<&str>>()
//                   .join(" -> "))
//       ),
//       LinkError::TODO(msg, id) =>return build_error_result(
//         format!("TODO: {} {}", msg, id.as_str())
//       ),
//       LinkError::StructsCanOnlyExtendStructs { s, parent } =>return build_error_result(
//         format!("Structs can only extend other structs: {}: {}",
//                 s.value, parent.value)
//       ),
//       LinkError::AttemptToSpecializeNonTemplatedType(it) =>return build_error_result(
//         format!( "Attempt to specialize type which does not have a template: {}",
//                  it.value)
//       ),
//     },
//   }
//
//   let compiled = compile_to_json(linker.get_structs(), linker.get_enums());
//   let c_str = CString::new(compiled).unwrap();
//
//   CompileResult {
//     err: false,
//     value: c_str.into_raw(),
//   }
// }
