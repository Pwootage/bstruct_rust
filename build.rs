extern crate cbindgen;

use std::env;
use std::path::PathBuf;

fn main() {
  let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

  let output_file = target_dir()
    .join("bstruct.hpp")
    .display()
    .to_string();

  cbindgen::Builder::new()
    .with_crate(crate_dir)
    .generate()
    .expect("Unable to generate bindings")
    .write_to_file(output_file);
}

fn target_dir() -> PathBuf {
  if let Ok(target) = env::var("CARGO_TARGET_DIR") {
    PathBuf::from(target)
  } else {
    PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("target")
  }
}