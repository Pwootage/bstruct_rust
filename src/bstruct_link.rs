use crate::bstruct_ast::{
  ASTEnum, ASTEnumValue, ASTIdentifier, ASTInt, ASTRootStatement, ASTStruct,
};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Clone, Debug)]
pub enum LinkError {
  UnknownType(ASTIdentifier),
  EnumDoesNotExtendIntegerPrimitive(ASTIdentifier),
}

pub type LinkResult<'i, T> = Result<T, LinkError>;

#[derive(Clone, Debug)]
enum BType {
  Enum(BEnum),
  Struct(BStruct),
  Primitive(BPrimitive),
}

impl BType {
  fn name(&self) -> &str {
    match self {
      BType::Enum(it) => it.name.as_str(),
      BType::Struct(it) => it.name.as_str(),
      BType::Primitive(it) => it.name,
    }
  }
}

#[derive(Clone, Debug)]
pub struct BEnum {
  pub name: ASTIdentifier,
  pub ext: BPrimitive,
  pub values: Vec<BEnumValue>,
}

#[derive(Clone, Debug)]
pub struct BEnumValue {
  pub name: ASTIdentifier,
  pub value: ASTInt,
}

#[derive(Clone, Debug)]
pub struct BStruct {
  pub name: ASTIdentifier,
}

#[derive(Clone, Debug)]
pub struct BPrimitive {
  pub size: usize,
  pub name: &'static str,
  pub is_int: bool,
}

const PBOOL: BPrimitive = BPrimitive {
  name: "bool",
  size: 1,
  is_int: false,
};
const PU8: BPrimitive = BPrimitive {
  name: "u8",
  size: 1,
  is_int: true,
};
const PU16: BPrimitive = BPrimitive {
  name: "u16",
  size: 2,
  is_int: true,
};
const PU32: BPrimitive = BPrimitive {
  name: "u32",
  size: 4,
  is_int: true,
};
const PU64: BPrimitive = BPrimitive {
  name: "u64",
  size: 8,
  is_int: true,
};
const PI8: BPrimitive = BPrimitive {
  name: "i8",
  size: 1,
  is_int: true,
};
const PI16: BPrimitive = BPrimitive {
  name: "i16",
  size: 2,
  is_int: true,
};
const PI32: BPrimitive = BPrimitive {
  name: "i32",
  size: 4,
  is_int: true,
};
const PI64: BPrimitive = BPrimitive {
  name: "i64",
  size: 8,
  is_int: true,
};
const PF32: BPrimitive = BPrimitive {
  name: "f32",
  size: 4,
  is_int: false,
};
const PF64: BPrimitive = BPrimitive {
  name: "f64",
  size: 8,
  is_int: false,
};
const PRIMITIVES: [BPrimitive; 11] = [
  PBOOL, PU8, PU16, PU32, PU64, PI8, PI16, PI32, PI64, PF32, PF64,
];

struct TypeLookup {
  table: HashMap<String, RefCell<BType>>,
}

impl TypeLookup {
  fn new() -> Self {
    TypeLookup {
      table: HashMap::new(),
    }
  }

  pub fn register(&mut self, v: RefCell<BType>) {
    let name = v.borrow().name().to_string();
    self.table.insert(name, v);
  }

  pub fn lookup(&self, id: &ASTIdentifier) -> Option<&RefCell<BType>> {
    self.table.get(id.as_str())
  }

  pub fn lookup_str(&self, id: &str) -> Option<&RefCell<BType>> {
    self.table.get(id)
  }
}

pub struct BStructLinker {
  structs: Vec<RefCell<BStruct>>,
  enums: Vec<RefCell<BEnum>>,
  lookup: TypeLookup,
}

impl BStructLinker {
  pub fn new() -> Self {
    let mut res = BStructLinker {
      structs: vec![],
      enums: vec![],
      lookup: TypeLookup::new(),
    };
    for p in PRIMITIVES {
      let prim_cell = RefCell::new(BType::Primitive(p));
      res.lookup.register(prim_cell);
    }
    res
  }

  pub fn get_enums(&self) -> &Vec<RefCell<BEnum>> {
    &self.enums
  }

  pub fn get_structs(&self) -> &Vec<RefCell<BStruct>> {
    &self.structs
  }

  pub fn link(&mut self, statements: &Vec<ASTRootStatement>) -> LinkResult<()> {
    // pass 1: register structs, link enums
    for statement in statements {
      match statement {
        ASTRootStatement::Enum(it) => self.link_enum(it)?,
        ASTRootStatement::Struct(it) => {}
      }
    }
    Ok(())
  }

  fn link_enum(&mut self, e: &ASTEnum) -> LinkResult<()> {
    let ext_type = if let Some(id) = e.ext.borrow() {
      if let Some(lookup) = self.lookup.lookup(id) {
        lookup
      } else {
        return Err(LinkError::EnumDoesNotExtendIntegerPrimitive(id.clone()));
      }
    } else {
      self.lookup.lookup_str("u32").unwrap()
    }
    .borrow();
    let ext_prim = if let BType::Primitive(prim) = ext_type.deref() {
      if !prim.is_int {
        return Err(LinkError::EnumDoesNotExtendIntegerPrimitive(
          ASTIdentifier::new(ext_type.name().to_string(), 0, 0),
        ));
      } else {
        prim
      }
    } else {
      return Err(LinkError::EnumDoesNotExtendIntegerPrimitive(
        ASTIdentifier::new(ext_type.name().to_string(), 0, 0),
      ));
    };

    let mut res = BEnum {
      name: e.name.clone(),
      ext: ext_prim.clone(),
      values: vec![],
    };

    let mut last_val: i64 = 0;
    for enum_value in &e.values {
      let value = if let Some(v) = &enum_value.value {
        v.clone()
      } else {
        let v = last_val;
        last_val += 1;
        ASTInt::Decimal(v)
      };
      let name = enum_value.name.clone();
      res.values.push(BEnumValue { name, value })
    }

    self.enums.push(RefCell::new(res));

    Ok(())
  }

  fn register_struct(&self, s: &ASTStruct) {
    println!("TODO: register struct");
  }
}
