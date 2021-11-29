use serde::{Deserialize, Serialize};
use crate::bstruct_link::{BEnum, BEnumValue, BStruct, BStructMember};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CompiledBStructJson {
  pub structs: Vec<CompiledStruct>,
  pub enumes: Vec<CompiledEnum>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CompiledStruct {
  pub name: String,
  pub size: i64,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub vtable: Option<i64>,
  pub extends: Vec<String>,
  pub members: Vec<CompiledMember>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompiledMember {
  pub name: String,
  pub typ: String,
  pub offset: i64,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub bit: Option<i64>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub bit_length: Option<i64>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub array_length: Option<i64>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub pointer: Option<bool>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CompiledEnum {
  pub name: String,
  pub size: i64,
  pub values: Vec<CompiledEnumValue>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CompiledEnumValue {
  pub name: String,
  pub value: i64,
}

pub fn compile_to_json(structs: Vec<BStruct>, enums: Vec<BEnum>) -> String {
  let res = CompiledBStructJson {
    structs: structs.iter()
      .map(|v|compile_bstruct_to_json(v))
      .collect(),
    enumes: enums.iter()
      .map(|v|compile_benum_to_json(v))
      .collect()
  };

  return serde_json::to_string(&res).unwrap();
}

fn compile_bstruct_to_json(s: &BStruct) -> CompiledStruct {
  CompiledStruct {
    name: s.name.value.clone(),
    size: s.size.unwrap().value(),
    vtable: s.vtable.map(|v|v.value()),
    extends: s.ext.iter().map(|v|v.value.clone()).collect(),
    members: s.members.iter().map(|v|compile_member_to_json(v)).collect(),
  }
}

fn compile_member_to_json(m: &BStructMember) -> CompiledMember {
  CompiledMember {
    name: m.name.value.clone(),
    typ: m.type_name.value.clone(),
    offset: m.offset.value(),
    bit: m.bit.map(|v|v.value()),
    bit_length: m.bit_length.map(|v|v.value()),
    array_length: m.array_length.map(|v|v.value()),
    pointer: if m.pointer { Some(true) } else { None }
  }
}

fn compile_benum_to_json(e: &BEnum) -> CompiledEnum {
  CompiledEnum {
    name: e.name.value.clone(),
    size: e.ext.size,
    values: e.values.iter().map(|v|compile_enum_value_to_json(v)).collect()
  }
}

fn compile_enum_value_to_json(v: &BEnumValue) -> CompiledEnumValue {
  CompiledEnumValue {
    name: v.name.value.clone(),
    value: v.value.value()
  }
}