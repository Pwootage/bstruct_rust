use crate::bstruct_ast::{ASTEnum, ASTIdentifier, ASTInt, ASTRootStatement, ASTStruct, ASTTemplateValues, ASTType};
use std::borrow::Borrow;
use std::cell::{RefCell};
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

const POINTER_SIZE: i64 = 4;

#[derive(Clone, Debug)]
pub enum LinkError {
  UnknownType(ASTIdentifier),
  EnumDoesNotExtendIntegerPrimitive(ASTIdentifier),
  CicrularReference(Vec<ASTIdentifier>),
  TODO(&'static str, ASTIdentifier),
  StructsCanOnlyExtendStructs {
    s: ASTIdentifier,
    parent: ASTIdentifier,
  },
  AttemptToSpecializeNonTemplatedType(ASTIdentifier),
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
pub struct BPrimitive {
  pub size: i64,
  pub name: &'static str,
  pub is_int: bool,
}

#[derive(Clone, Debug)]
pub struct BStruct {
  pub original: ASTStruct,
  pub name: ASTIdentifier,
  pub specialized: bool,
  pub link_started: bool,
  pub link_complete: bool,
  pub size: Option<ASTInt>,
  pub vtable: Option<ASTInt>,
  pub ext: Vec<ASTIdentifier>,
  pub members: Vec<BStructMember>,
}

impl BStruct {
  fn templated(&self) -> bool {
    if let Some(v) = &self.original.template {
      v.templates.len() > 0
    } else {
      false
    }
  }
}

#[derive(Clone, Debug)]
pub struct BStructMember {
  pub type_name: ASTIdentifier,
  pub name: ASTIdentifier,
  pub offset: ASTInt,
  pub bit: Option<ASTInt>,
  pub bit_length: Option<ASTInt>,
  pub pointer: bool,
  pub array_length: Option<ASTInt>,
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

  pub fn get_table(&self) -> &HashMap<String, RefCell<BType>> {
    &self.table
  }

  pub fn register(&mut self, v: RefCell<BType>) {
    let name = v.borrow().name().to_string();
    self.table.insert(name, v);
  }

  pub fn lookup(&self, id: &ASTIdentifier) -> Option<&RefCell<BType>> {
    self.lookup_str(id.as_str())
  }

  pub fn lookup_str(&self, id: &str) -> Option<&RefCell<BType>> {
    self.table.get(id)
  }
}

pub struct BStructLinker {
  lookup: TypeLookup,
}

impl BStructLinker {
  pub fn new() -> Self {
    let mut res = BStructLinker {
      lookup: TypeLookup::new(),
    };
    for p in PRIMITIVES {
      let prim_cell = RefCell::new(BType::Primitive(p));
      res.lookup.register(prim_cell);
    }
    res
  }

  pub fn get_enums(&self) -> Vec<BEnum> {
    self
      .lookup
      .get_table()
      .values()
      .map(|v| v.borrow().deref().clone())
      .filter_map(|v| match v {
        BType::Enum(it) => Some(it),
        BType::Struct(_) => None,
        BType::Primitive(_) => None,
      })
      .collect()
  }

  pub fn get_structs(&self) -> Vec<BStruct> {
    self
      .lookup
      .get_table()
      .values()
      .map(|v| v.borrow().deref().clone())
      .filter_map(|v| match v {
        BType::Enum(_) => None,
        BType::Struct(it) => Some(it),
        BType::Primitive(_) => None,
      })
      .collect()
  }

  pub fn link(&mut self, statements: &Vec<ASTRootStatement>) -> LinkResult<()> {
    // pass 1: register structs, link enums
    for statement in statements {
      match statement {
        ASTRootStatement::Enum(it) => self.link_enum(it)?,
        ASTRootStatement::Struct(it) => self.register_struct(it)?,
      }
    }

    // pass 2: register specialized structs
    loop {
      let mut specialized = false;
      for s in self.get_structs().clone() {
        specialized |= self.generate_specialized_types(&s.original)?
      }
      if !specialized {
        break;
      }
    };

    // pass 3: link structs
    for (_, cell) in self.lookup.table.iter() {
      match cell.borrow_mut().deref_mut() {
        BType::Enum(_) => {}
        BType::Struct(it) => self.link_struct(it, &vec![])?,
        BType::Primitive(_) => {}
      }
    }

    // pass 4: remove non-specialized template structs
    self
      .lookup
      .table
      .retain(|_, v| match v.deref().borrow().deref() {
        BType::Enum(_) => true,
        BType::Struct(it) => !it.templated(),
        BType::Primitive(_) => true,
      });

    Ok(())
  }

  fn link_enum(&mut self, e: &ASTEnum) -> LinkResult<()> {
    let ext_type = if let Some(id) = e.ext.borrow() {
      if let Some(lookup) = self.lookup.lookup(id) {
        let v = lookup.borrow().clone();
        v
      } else {
        return Err(LinkError::EnumDoesNotExtendIntegerPrimitive(id.clone()));
      }
    } else {
      BType::Primitive(PU32)
    };
    let ext_prim = if let BType::Primitive(prim) = &ext_type {
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

    // parse values
    let mut last_val = ASTInt::Decimal(0);
    for enum_value in &e.values {
      let value = if let Some(v) = &enum_value.value {
        v.clone()
      } else {
        last_val + ASTInt::Decimal(1)
      };
      last_val = value;
      let name = enum_value.name.clone();
      res.values.push(BEnumValue { name, value })
    }

    // register
    self.lookup.register(RefCell::new(BType::Enum(res)));

    Ok(())
  }

  fn register_struct(&mut self, s: &ASTStruct) -> LinkResult<()> {
    let res = BStruct {
      original: s.clone(),
      name: s.name.clone(),
      specialized: false,
      link_started: false,
      link_complete: false,
      size: s.size.clone(),
      vtable: s.vtable.clone(),
      ext: s.extends.clone().map(|v| v.extends).unwrap_or(vec![]),
      members: vec![],
    };

    self.lookup.register(RefCell::new(BType::Struct(res)));

    Ok(())
  }

  fn link_struct(&self, s: &mut BStruct, link_stack: &Vec<ASTIdentifier>) -> LinkResult<()> {
    if s.link_complete {
      return Ok(()); // Already did this one
    }
    if s.templated() {
      return Ok(()); // Link tempaltes only at specialization
    }

    let mut link_stack = link_stack.clone();
    link_stack.push(s.name.clone());
    if s.link_started {
      return Err(LinkError::CicrularReference(link_stack));
    }
    s.link_started = true;

    // Link our parent(s)
    {
      if let Some(ext) = &s.original.extends {
        if ext.extends.len() > 1 {
          return Err(LinkError::TODO(
            "Multiple inheretance is not yet implemented",
            s.name.clone(),
          ));
        }
        for parent in &ext.extends {
          if let Some(parent_cell) = self.lookup.lookup(parent) {
            let parent_type = parent_cell.try_borrow_mut();
            if parent_type.is_err() {
              let err = parent_type.unwrap_err();
              link_stack.push(parent.clone());
              println!("Lock err: {} {:?}", &err, &err);
              return Err(LinkError::CicrularReference(link_stack.clone()));
            }
            match parent_type.unwrap().deref_mut() {
              BType::Enum(it) => {
                return Err(LinkError::StructsCanOnlyExtendStructs {
                  s: s.name.clone(),
                  parent: it.name.clone(),
                });
              }
              BType::Struct(it) => self.link_struct(it, &link_stack)?,
              BType::Primitive(it) => {
                return Err(LinkError::StructsCanOnlyExtendStructs {
                  s: s.name.clone(),
                  parent: ASTIdentifier::new(it.name.to_string(), 0, 0),
                });
              }
            }
          } else {
            return Err(LinkError::UnknownType(parent.clone()));
          }
        }
      }
    }

    let mut last_member: Option<BStructMember> = None;
    // look up last mamber in parent
    if let Some(x) = s.ext.last() {
      match self.lookup.lookup(x).unwrap().borrow().deref() {
        BType::Enum(_) => panic!("Enum parent! This should have been caught earlier!"),
        BType::Struct(it) => last_member = it.members.last().map(|v| v.clone()),
        BType::Primitive(_) => panic!("Primitive parent! This should have been caught earlier!"),
      }
    }

    // Figure out member sizes/offset
    {
      for member in &s.original.members {
        // find the type
        let type_name = if member.type_name.template.is_some() {
          let mut new_type = member.type_name.clone();
          new_type.name = self.get_specialized_type(
            &member.type_name.name,
            member.type_name.template.as_ref().unwrap(),
          );
          new_type.template = None;
          new_type
        } else {
          member.type_name.clone()
        };

        if !member.type_name.pointer {
          let typ = self.lookup.lookup(&type_name.name);
          if typ.is_none() {
            return Err(LinkError::UnknownType(type_name.name.clone()));
          }
          match typ.unwrap().borrow_mut().deref_mut() {
            BType::Enum(_) => {} // already linked
            BType::Struct(it) => self.link_struct(it, &link_stack)?,
            BType::Primitive(_) => {} // already linked
          }
        }

        let pointer = member.type_name.pointer;
        let array_length = member.type_name.array_size;
        let bit = member.bit;
        let bit_length = member.bit_length;

        let offset = member
          .offset
          .or(
            last_member
              .as_ref()
              .map(|v| v.offset + self.get_member_size(v)),
          )
          .unwrap_or(ASTInt::Decimal(0));

        let member = BStructMember {
          type_name: type_name.name,
          name: member.name.clone(),
          offset,
          bit,
          bit_length,
          pointer,
          array_length,
        };
        last_member = Some(member.clone());
        s.members.push(member);
      }
    }

    // Figure out our size
    {
      let size = if let Some(orig) = s.original.size {
        orig
      } else if let Some(last) = s.members.last() {
        last.offset + self.get_member_size(last)
      } else {
        //size is parent size, if it exists
        if let Some(parent_name) = s.ext.last() {
          match self.lookup.lookup(parent_name).unwrap().borrow().deref() {
            BType::Enum(_) => panic!("Enum parent! This should have been caught earlier!"),
            BType::Struct(it) => it.size.unwrap(),
            BType::Primitive(_) => {
              panic!("Primitive parent! This should have been caught earlier!")
            }
          }
        } else {
          ASTInt::Decimal(0)
        }
      };
      s.size = Some(size);
    }

    s.link_complete = true;

    Ok(())
  }

  fn get_member_size(&self, member: &BStructMember) -> ASTInt {
    if member.pointer {
      return ASTInt::Decimal(POINTER_SIZE);
    }

    let mut size = match self
      .lookup
      .lookup(&member.type_name)
      .unwrap()
      .borrow()
      .clone()
    {
      BType::Enum(it) => ASTInt::Decimal(it.ext.size),
      BType::Struct(it) => it.size.unwrap(),
      BType::Primitive(it) => ASTInt::Decimal(it.size),
    };

    if let Some(len) = member.array_length {
      size = size * len;
    }

    return size;
  }
  fn generate_specialized_types(&mut self, s: &ASTStruct) -> LinkResult<bool> {
    if s.template.is_some() {
      return Ok(false); // don't specialize non-specialized structs
    }

    let mut specialized_any = false;
    for member in &s.members {
      if member.type_name.template.is_none() {
        continue;
      }
      specialized_any |= self.specialize_struct(
        &member.type_name.name,
        member.type_name.template.as_ref().unwrap(),
      )?;
    }

    return Ok(specialized_any);
  }
  fn specialize_struct(
    &mut self,
    type_name: &ASTIdentifier,
    template: &ASTTemplateValues,
  ) -> LinkResult<bool> {
    let specialized_type = self.get_specialized_type(type_name, template);

    if self.lookup.lookup(&specialized_type).is_some() {
      return Ok(false); // already generated this psecialization
    }

    // find the type
    let typ = self.lookup.lookup(type_name);
    if typ.is_none() {
      return Err(LinkError::UnknownType(type_name.clone()));
    }
    let typ = typ.unwrap().borrow();
    let s = match typ.deref() {
      BType::Enum(_) => {
        return Err(LinkError::AttemptToSpecializeNonTemplatedType(
          type_name.clone(),
        ));
      }
      BType::Struct(it) => it.clone(),
      BType::Primitive(_) => {
        return Err(LinkError::AttemptToSpecializeNonTemplatedType(
          type_name.clone(),
        ));
      }
    };
    drop(typ);

    let mut mappings: HashMap<String, ASTIdentifier> = HashMap::new();
    for (i, target) in template.type_names.iter().enumerate() {
      let original = &s.original.template.as_ref().unwrap().templates[i];

      let name = if let Some(target_template) = target.template.as_ref() {
        self.specialize_struct(&target.name, target_template)?;
        self.get_specialized_type(&target.name, target_template)
      } else {
        target.name.clone()
      };
      mappings.insert(original.value.clone(), name);
    }

    let mut specialized_ast = s.original.clone();
    specialized_ast.name = specialized_type.clone();
    specialized_ast.template = None;
    specialized_ast.members = specialized_ast
      .members
      .iter()
      .map(|v| {
        let mut res = v.clone();
        res.type_name = self.substitute_types(&v.type_name, &mappings);
        res
      })
      .collect();

    let res = BStruct {
      original: specialized_ast,
      name: specialized_type.clone(),
      specialized: true,
      link_started: false,
      link_complete: false,
      size: None,
      vtable: None,
      ext: vec![],
      members: vec![],
    };

    self.lookup.register(RefCell::new(BType::Struct(res)));

    Ok(true)
  }

  fn get_specialized_type(
    &self,
    type_name: &ASTIdentifier,
    template: &ASTTemplateValues,
  ) -> ASTIdentifier {
    let member_types = template
      .type_names
      .iter()
      .map(|v| {
        if let Some(template) = &v.template {
          self.get_specialized_type(&v.name, template).value
        } else {
          v.name.value.clone()
        }
      })
      .collect::<Vec<String>>()
      .join(", ");
    let value = format!("{}<{}>", type_name.value, member_types);

    return ASTIdentifier {
      value,
      start: type_name.start,
      end: type_name.end,
    };
  }

  fn substitute_types(&self, v: &ASTType, mappings: &HashMap<String, ASTIdentifier>) -> ASTType {
    let mut res = v.clone();
    res.name = mappings
      .get(&v.name.value)
      .unwrap_or(&v.name)
      .clone();
    if let Some(tpl) = &res.template {
      let new_types = tpl.type_names.iter()
        .map(|v| self.substitute_types(v, mappings))
        .collect::<Vec<ASTType>>();
      res.template = Some(ASTTemplateValues {
        type_names: new_types
      });
    }
    res
  }
}
