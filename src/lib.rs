use std::marker::PhantomData;
use {
    cast::{f64, i64, u64},
    serde::{
        de::{self, Deserializer as _, IntoDeserializer as _, VariantAccess as _},
        forward_to_deserialize_any,
        ser::{
            self, Error as _, SerializeStruct as _, SerializeStructVariant as _,
            SerializeTuple as _, SerializeTupleStruct as _, SerializeTupleVariant as _,
        },
        serde_if_integer128,
    },
    std::borrow::Cow,
    wyz::pipe::Pipe as _,
};

pub mod assistant;
use assistant::{EnumAssistant, Seed, VariantKind};

/// Represents a Serde data model value, losslessly.  
/// See <https://serde.rs/data-model.html> for more information.
///
/// # Limitations
///
/// - `i128` is serialized as `i64`, and if that doesn't fit then an error is thrown.
/// - `u128` is serialized as `u64`, and if that doesn't fit then an error is thrown.
/// - There is no way for Deserializers to hint what kind of enum variant they have available, so an assistant must be provided to support deserialising them at all.
///
/// # Leaks
///
/// Some memory is leaked when:
///
/// - a UnitStruct is serialized (for its name).
/// - a UnitVariant is serialized (for its name and variant).
/// - a NewtypeStruct is serialized (for its name).
/// - a NewtypeVariant is serialized (for its name and variant).
/// - a TupleStruct is serialized (for its name).
/// - a TupleVariant is serialized (for its name and variant).
/// - a Struct is serialized (for its name and each of its field keys).
pub enum Object<'a> {
    Bool(bool),

    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),

    F32(f32),
    F64(f64),

    Char(char),

    String(Cow<'a, str>),

    ByteArray(Cow<'a, [u8]>),

    Option(Option<Box<Object<'a>>>),

    Unit,

    UnitStruct {
        name: Cow<'a, str>,
    },

    UnitVariant {
        name: Cow<'a, str>,
        variant: Box<Object<'a>>,
    },

    NewtypeStruct {
        name: Cow<'a, str>,
        value: Box<Object<'a>>,
    },

    NewtypeVariant {
        name: Cow<'a, str>,
        variant: Box<Object<'a>>,
        value: Box<Object<'a>>,
    },

    Seq(Vec<Object<'a>>),

    Tuple(Vec<Object<'a>>),

    TupleStruct {
        name: Cow<'a, str>,
        fields: Vec<Object<'a>>,
    },

    TupleVariant {
        name: Cow<'a, str>,
        variant: Box<Object<'a>>,
        fields: Box<Object<'a>>,
    },

    /// This variant does not care whether keys are duplicated.  
    /// Formats might mind it.
    Map(Vec<(Object<'a>, Object<'a>)>),

    /// This variant may store duplicate fields.
    /// Formats may have a problem with it.
    Struct {
        name: Cow<'a, str>,
        fields: Vec<(Cow<'a, str>, Object<'a>)>,
    },

    /// This variant may store duplicate fields.
    /// Formats may have a problem with it.
    StructVariant {
        name: Cow<'a, str>,
        variant: Box<Object<'a>>,
        fields: Box<Object<'a>>,
    },
}

impl<'a> Object<'a> {
    pub fn into_owned(self) -> Object<'static> {
        match self {
            Object::Bool(bool) => Object::Bool(bool),

            Object::I8(i8) => Object::I8(i8),
            Object::I16(i16) => Object::I16(i16),
            Object::I32(i32) => Object::I32(i32),
            Object::I64(i64) => Object::I64(i64),
            Object::I128(i128) => Object::I128(i128),

            Object::U8(u8) => Object::U8(u8),
            Object::U16(u16) => Object::U16(u16),
            Object::U32(u32) => Object::U32(u32),
            Object::U64(u64) => Object::U64(u64),
            Object::U128(u128) => Object::U128(u128),

            Object::F32(f32) => Object::F32(f32),
            Object::F64(f64) => Object::F64(f64),

            Object::Char(char) => Object::Char(char),

            Object::String(string) => Object::String(string.into_owned().into()),

            Object::ByteArray(bytes) => Object::ByteArray(bytes.into_owned().into()),

            Object::Option(option) => Object::Option(option.map(|b| b.into_owned().into())),

            Object::Unit => Object::Unit,

            Object::UnitStruct { name } => Object::UnitStruct {
                name: name.into_owned().into(),
            },

            Object::UnitVariant { name, variant } => Object::UnitVariant {
                name: name.into_owned().into(),
                variant: variant.into_owned().into(),
            },

            Object::NewtypeStruct { name, value } => Object::NewtypeStruct {
                name: name.into_owned().into(),
                value: value.into_owned().into(),
            },
            Object::NewtypeVariant {
                name,
                variant,
                value,
            } => Object::NewtypeVariant {
                name: name.into_owned().into(),
                variant: variant.into_owned().into(),
                value: value.into_owned().into(),
            },

            Object::Seq(elements) => {
                Object::Seq(elements.into_iter().map(Object::into_owned).collect())
            }

            Object::Tuple(fields) => {
                Object::Seq(fields.into_iter().map(Object::into_owned).collect())
            }

            Object::TupleStruct { name, fields } => Object::TupleStruct {
                name: name.into_owned().into(),
                fields: fields.into_iter().map(Object::into_owned).collect(),
            },

            Object::TupleVariant {
                name,
                variant,
                fields,
            } => Object::TupleVariant {
                name: name.into_owned().into(),
                variant: variant.into_owned().into(),
                fields: fields.into_owned().into(),
            },

            Object::Map(fields) => Object::Map(
                fields
                    .into_iter()
                    .map(|(k, v)| (k.into_owned(), v.into_owned()))
                    .collect(),
            ),

            Object::Struct { name, fields } => Object::Struct {
                name: name.into_owned().into(),
                fields: fields
                    .into_iter()
                    .map(|(k, v)| (k.into_owned().into(), v.into_owned()))
                    .collect(),
            },

            Object::StructVariant {
                name,
                variant,
                fields,
            } => Object::StructVariant {
                name: name.into_owned().into(),
                variant: variant.into_owned().into(),
                fields: fields.into_owned().into(),
            },
        }
    }
}

impl<'a> ser::Serialize for Object<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        match self {
            Object::Bool(bool) => serializer.serialize_bool(*bool),

            Object::I8(i8) => serializer.serialize_i8(*i8),
            Object::I16(i16) => serializer.serialize_i16(*i16),
            Object::I32(i32) => serializer.serialize_i32(*i32),
            Object::I64(i64) => serializer.serialize_i64(*i64),
            Object::I128(i128) => {
                serializer.serialize_i64(i64(*i128).map_err(|error| S::Error::custom(&error))?)
            }

            Object::U8(u8) => serializer.serialize_u8(*u8),
            Object::U16(u16) => serializer.serialize_u16(*u16),
            Object::U32(u32) => serializer.serialize_u32(*u32),
            Object::U64(u64) => serializer.serialize_u64(*u64),
            Object::U128(u128) => {
                serializer.serialize_u64(u64(*u128).map_err(|error| S::Error::custom(&error))?)
            }

            Object::F32(f32) => serializer.serialize_f32(*f32),
            Object::F64(f64) => serializer.serialize_f64(*f64),

            Object::Char(char) => serializer.serialize_char(*char),

            Object::String(cow) => serializer.serialize_str(cow),

            Object::ByteArray(cow) => serializer.serialize_bytes(cow),

            Object::Option(option) => match option {
                Some(object) => serializer.serialize_some(object),
                None => serializer.serialize_none(),
            },

            Object::Unit => serializer.serialize_unit(),

            Object::UnitStruct { name } => serializer.serialize_unit_struct(leak_str(name)),

            Object::UnitVariant { name, variant } => serializer.serialize_unit_variant(
                leak_str(name),
                make_variant_index(variant),
                make_variant_name(variant)?,
            ),

            Object::NewtypeStruct { name, value } => {
                serializer.serialize_newtype_struct(leak_str(name), value)
            }

            Object::NewtypeVariant {
                name,
                variant,
                value,
            } => serializer.serialize_newtype_variant(
                leak_str(name),
                make_variant_index(variant),
                make_variant_name(variant)?,
                value,
            ),

            Object::Seq(vec) => serializer.collect_seq(vec.iter()),

            Object::Tuple(vec) => {
                let mut serializer = serializer.serialize_tuple(vec.len())?;
                for element in vec {
                    serializer.serialize_element(element)?
                }
                serializer.end()
            }

            Object::TupleStruct { name, fields } => {
                let mut serializer =
                    serializer.serialize_tuple_struct(leak_str(name), fields.len())?;
                for field in fields {
                    serializer.serialize_field(field)?
                }
                serializer.end()
            }

            Object::TupleVariant {
                name,
                variant,
                fields,
            } => {
                let mut serializer = serializer.serialize_tuple_variant(
                    leak_str(name),
                    make_variant_index(variant),
                    make_variant_name(variant)?,
                    match Box::as_ref(fields) {
                        Object::String(cow) => cow.as_ref().len(),
                        Object::ByteArray(cow) => cow.as_ref().len(),
                        Object::Seq(vec) | Object::Tuple(vec) => vec.len(),
                        Object::Map(vec) => vec.len(),
                        _ => return Err(ser::Error::custom("Tried to serialise a tuple variant, but couldn't figure out the field count."))
                    },
                )?;

                match Box::as_ref(fields) {
                    Object::String(cow) => {
                        for value in cow.chars() {
                            serializer.serialize_field(&value)?
                        }
                    }
                    Object::ByteArray(cow) => {
                        for value in cow.iter() {
                            serializer.serialize_field(value)?
                        }
                    }
                    Object::Seq(vec) | Object::Tuple(vec) => {
                        for value in vec.iter() {
                            serializer.serialize_field(value)?
                        }
                    }
                    Object::Map(vec) => {
                        for value in vec.iter() {
                            serializer.serialize_field(value)?
                        }
                    }
                    _ => unreachable!(),
                }
                serializer.end()
            }

            Object::Map(pairs) => serializer.collect_map(pairs.iter().map(|(k, v)| (k, v))),

            Object::Struct { name, fields } => {
                let mut serializer = serializer.serialize_struct(leak_str(name), fields.len())?;
                for (key, value) in fields {
                    serializer.serialize_field(leak_str(key), value)?
                }
                serializer.end()
            }

            Object::StructVariant {
                name,
                variant,
                fields,
            } => {
                let mut serializer = serializer.serialize_struct_variant(
                    leak_str(name),
                    make_variant_index(variant),
                    make_variant_name(variant)?,
                    match Box::as_ref(fields) {
                        Object::String(cow) => cow.as_ref().len(),
                        Object::ByteArray(cow) => cow.as_ref().len(),
                        Object::Seq(vec) | Object::Tuple(vec) => vec.len(),
                        Object::Map(vec) => vec.len(),
                        _ => return Err(ser::Error::custom("Tried to serialise a struct variant, but couldn't figure out the field count."))
                    },
                )?;

                match Box::as_ref(fields) {
                    Object::String(cow) => {
                        for (key, value) in cow.chars().enumerate() {
                            serializer
                                .serialize_field(leak_str(key.to_string().as_ref()), &value)?
                        }
                    }
                    Object::ByteArray(cow) => {
                        for (key, value) in cow.iter().enumerate() {
                            serializer.serialize_field(leak_str(key.to_string().as_ref()), value)?
                        }
                    }
                    Object::Seq(vec) | Object::Tuple(vec) => {
                        for (key, value) in vec.iter().enumerate() {
                            serializer.serialize_field(leak_str(key.to_string().as_ref()), value)?
                        }
                    }
                    Object::Map(vec) => {
                        for (key, value) in vec.iter() {
                            serializer.serialize_field(make_field_key(key)?, value)?
                        }
                    }
                    _ => unreachable!(),
                }
                serializer.end()
            }
        }
    }
}

fn make_variant_index<'a>(variant: impl AsRef<Object<'a>>) -> u32 {
    match variant.as_ref() {
        Object::U8(u8) => *u8 as u32,
        Object::U16(u16) => *u16 as u32,
        Object::U32(u32) => *u32,
        _ => u32::MAX,
    }
}

fn make_variant_name<'a, E: ser::Error>(
    variant: impl AsRef<Object<'a>>,
) -> Result<&'static str, E> {
    leak_str(
        match variant.as_ref() {
            Object::Bool(bool) => bool.to_string(),
            Object::I8(i8) => i8.to_string(),
            Object::I16(i16) => i16.to_string(),
            Object::I32(i32) => i32.to_string(),
            Object::I64(i64) => i64.to_string(),
            Object::I128(i128) => i128.to_string(),
            Object::U8(u8) => u8.to_string(),
            Object::U16(u16) => u16.to_string(),
            Object::U32(u32) => u32.to_string(),
            Object::U64(u64) => u64.to_string(),
            Object::U128(u128) => u128.to_string(),
            Object::F32(f32) => f32.to_string(),
            Object::F64(f64) => f64.to_string(),
            Object::Char(char) => char.to_string(),
            Object::String(cow) => cow.to_string(),
            Object::ByteArray(cow) => String::from_utf8_lossy(cow).to_string(),
            _ => {
                return Err(ser::Error::custom(
                    "Tried to serialise a variant, but couldn't make a name.",
                ))
            }
        }
        .as_ref(),
    )
    .pipe(Ok)
}

fn make_field_key<E: ser::Error>(variant: &Object) -> Result<&'static str, E> {
    leak_str(
        match variant {
            Object::Bool(bool) => bool.to_string(),
            Object::I8(i8) => i8.to_string(),
            Object::I16(i16) => i16.to_string(),
            Object::I32(i32) => i32.to_string(),
            Object::I64(i64) => i64.to_string(),
            Object::I128(i128) => i128.to_string(),
            Object::U8(u8) => u8.to_string(),
            Object::U16(u16) => u16.to_string(),
            Object::U32(u32) => u32.to_string(),
            Object::U64(u64) => u64.to_string(),
            Object::U128(u128) => u128.to_string(),
            Object::F32(f32) => f32.to_string(),
            Object::F64(f64) => f64.to_string(),
            Object::Char(char) => char.to_string(),
            Object::String(cow) => cow.to_string(),
            Object::ByteArray(cow) => String::from_utf8_lossy(cow).to_string(),
            _ => {
                return Err(ser::Error::custom(
                    "Tried to serialise a struct variant, but couldn't make a field key.",
                ))
            }
        }
        .as_ref(),
    )
    .pipe(Ok)
}

//TODO: Some leaks may be avoidable if deref specialisation works with lifetimes.
//TODO: Evaluate interning as an option to mitigate memory usage somewhat.
fn leak_str(str: &str) -> &'static str {
    Box::leak(Box::new(str.to_string()))
}

impl<'de> de::Deserialize<'de> for Object<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_any(Visitor(NoEnumAssistant))
    }
}

macro_rules! visit {
    ($($visit_:ident$(+ $self:ident)?($($ty:ty$( | $($expr:expr$(, $question_mark:tt)*);+$(;)?)?)?) => $variant:ident$(($const:expr))? $(/ ::$Error:ident where T: $t_path:path)?),*$(,)?) => {
        $(
            fn $visit_<T>(self$(, v: $ty)?) -> Result<Self::Value, T$(::$Error)?>
            where
                T: $($t_path, T::Error: )?de::Error,
            {
                $(let $self = self;)?
                Ok(Object::$variant
                    // Alternatives:
                    $(({let _: $ty; v$($(.pipe($expr)$($question_mark)*)+)?.into()}))?
                    $(($const))?
                )
            }
        )*
    };
}

#[derive(Clone)]
struct Visitor<Assistant: EnumAssistant + Clone>(Assistant);
impl<'de, Assistant: EnumAssistant + Clone> de::Visitor<'de> for Visitor<Assistant> {
    type Value = Object<'de>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "just about anything")
    }

    visit! {
        visit_bool(bool) => Bool,

        visit_i8(i8) => I8,
        visit_i16(i16) => I16,
        visit_i32(i32) => I32,
        visit_i64(i64) => I64,
        visit_i128(i128) => I128,

        visit_u8(u8) => U8,
        visit_u16(u16) => U16,
        visit_u32(u32) => U32,
        visit_u64(u64) => U64,
        visit_u128(u128) => U128,

        visit_f32(f32) => F32,
        visit_f64(f64) => F64,

        visit_char(char) => Char,

        visit_str(&str | str::to_owned) => String,
        visit_borrowed_str(&'de str) => String,
        visit_string(String) => String,

        visit_bytes(&[u8] | <[u8]>::to_owned) => ByteArray,
        visit_borrowed_bytes(&'de [u8]) => ByteArray,
        visit_byte_buf(Vec<u8>) => ByteArray,

        visit_none() => Option(None),
        visit_some + self_(T | |d| d.deserialize_any(self_), ?; Box::new) => Option / ::Error where T: de::Deserializer<'de>,

        visit_unit() => Unit,
    }

    fn visit_newtype_struct<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        Ok(Object::NewtypeStruct {
            name: "UNKNOWN".into(),
            value: deserializer.deserialize_any(self)?.into(),
        })
    }

    fn visit_seq<A>(self, access: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        struct SeqAccessIterator<'de, Assistant: EnumAssistant + Clone, Access: de::SeqAccess<'de>> {
            assistant: Assistant,
            access: Access,
            marker: PhantomData<&'de ()>,
        }
        impl<'de, Assistant: EnumAssistant + Clone, Access: de::SeqAccess<'de>> Iterator
            for SeqAccessIterator<'de, Assistant, Access>
        {
            type Item = Result<Object<'de>, Access::Error>;
            fn next(&mut self) -> Option<Self::Item> {
                self.access
                    .next_element_seed(Seed(self.assistant.clone()))
                    .transpose()
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                (self.access.size_hint().unwrap_or(0), None)
            }
        }

        Ok(Object::Seq(
            SeqAccessIterator {
                assistant: self.0,
                access,
                marker: PhantomData,
            }
            .collect::<Result<_, _>>()?,
        ))
    }

    fn visit_map<A>(self, access: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        struct MapAccessIterator<'de, Assistant: EnumAssistant + Clone, Access> {
            assistant: Assistant,
            access: Access,
            marker: PhantomData<&'de ()>,
        };
        impl<'de, Assistant: EnumAssistant + Clone, Access: de::MapAccess<'de>> Iterator
            for MapAccessIterator<'de, Assistant, Access>
        {
            type Item = Result<(Object<'de>, Object<'de>), Access::Error>;
            fn next(&mut self) -> Option<Self::Item> {
                self.access
                    .next_entry_seed(Seed(self.assistant.clone()), Seed(self.assistant.clone()))
                    .transpose()
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                (self.access.size_hint().unwrap_or(0), None)
            }
        }

        Ok(Object::Map(
            MapAccessIterator {
                assistant: self.0,
                access,
                marker: PhantomData,
            }
            .collect::<Result<_, _>>()?,
        ))
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: de::EnumAccess<'de>,
    {
        // (Likely!) Goes through deserialize_identifier => deserialize_any, but we'll probably get a string, number (u64) or bytes.
        let (variant, variant_access): (Object<'de>, _) =
            data.variant_seed(Seed(self.0.clone()))?;

        let name = Cow::Borrowed("UNKNOWN_ENUM");
        let variant = Box::new(variant);
        // So this is hacky...
        // Essentially, we have no way of knowing what kind of variant is coming up, so we need a little more info.
        // (We can't do this by trial and error even in a self-describing format since the A::Variant is consumed each time.)
        match self.0.variant_hint(&variant)? {
            VariantKind::Unit => variant_access
                .unit_variant()
                .map(|()| Object::UnitVariant { name, variant }),
            VariantKind::Newtype => {
                variant_access
                    .newtype_variant_seed(Seed(self.0))
                    .map(|value| Object::NewtypeVariant {
                        name,
                        variant,
                        value: Box::new(value),
                    })
            }
            VariantKind::Tuple(len) => {
                variant_access
                    .tuple_variant(len, self)
                    .map(|fields| Object::TupleVariant {
                        name,
                        variant,
                        fields: Box::new(fields),
                    })
            }
            VariantKind::Struct(field_names) => {
                let field_names = field_names
                    .iter()
                    .map(|name| name.as_ref().pipe(leak_str))
                    .collect::<Vec<_>>()
                    .pipe(leak_vec);
                variant_access
                    .struct_variant(field_names, self)
                    .map(|fields| Object::StructVariant {
                        name,
                        variant,
                        fields: Box::new(fields),
                    })
            }
        }
    }
}

fn leak_vec<T>(vec: Vec<T>) -> &'static [T] {
    vec.pipe(Box::new).pipe(Box::leak)
}

impl<'de> de::IntoDeserializer<'de> for Object<'de> {
    type Deserializer = Self;
    fn into_deserializer(self) -> Self::Deserializer {
        self
    }
}

impl<'de> de::Deserializer<'de> for Object<'de> {
    type Error = de::value::Error;
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self {
            Object::Bool(bool) => visitor.visit_bool(bool),

            Object::I8(i8) => visitor.visit_i8(i8),
            Object::I16(i16) => visitor.visit_i16(i16),
            Object::I32(i32) => visitor.visit_i32(i32),
            Object::I64(i64) => visitor.visit_i64(i64),
            Object::I128(i128) => visitor.visit_i128(i128),

            Object::U8(u8) => visitor.visit_u8(u8),
            Object::U16(u16) => visitor.visit_u16(u16),
            Object::U32(u32) => visitor.visit_u32(u32),
            Object::U64(u64) => visitor.visit_u64(u64),
            Object::U128(u128) => visitor.visit_u128(u128),

            Object::F32(f32) => visitor.visit_f32(f32),
            Object::F64(f64) => visitor.visit_f64(f64),

            Object::Char(char) => visitor.visit_char(char),
            Object::String(cow) => match cow {
                Cow::Borrowed(str) => visitor.visit_borrowed_str(str),
                Cow::Owned(string) => visitor.visit_string(string),
            },

            Object::ByteArray(cow) => match cow {
                Cow::Borrowed(slice) => visitor.visit_borrowed_bytes(slice),
                Cow::Owned(vec) => visitor.visit_byte_buf(vec),
            },

            Object::Option(option) => match option {
                Some(b) => visitor.visit_some(*b),
                None => visitor.visit_none(),
            },

            Object::Unit => visitor.visit_unit(),

            Object::UnitStruct { name } => visitor.visit_unit(),
            self_ @ Object::UnitVariant { .. } => visitor.visit_enum(self_),
            Object::NewtypeStruct { name: _, value } => visitor.visit_newtype_struct(*value),
            self_ @ Object::NewtypeVariant { .. } => visitor.visit_enum(self_),
            Object::Seq(elements) => visitor.visit_seq(elements.into_deserializer()),
            Object::Tuple(fields) => visitor.visit_seq(fields.into_deserializer()),
            Object::TupleStruct { name, fields } => todo!(),
            self_ @ Object::TupleVariant { .. } => visitor.visit_enum(self_),
            Object::Map(map) => todo!(),
            Object::Struct { name, fields } => todo!(),
            self_ @ Object::StructVariant { .. } => visitor.visit_enum(self_),
        }
    }

    forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }

    serde_if_integer128!(forward_to_deserialize_any! {
        i128 u128
    });
}

impl<'de> Object<'de> {
    fn unexp(&self) -> de::Unexpected {
        match self {
            Object::Bool(bool) => de::Unexpected::Bool(*bool),

            Object::I8(i8) => de::Unexpected::Signed(i64(*i8)),
            Object::I16(i16) => de::Unexpected::Signed(i64(*i16)),
            Object::I32(i32) => de::Unexpected::Signed(i64(*i32)),
            Object::I64(i64) => de::Unexpected::Signed(*i64),
            Object::I128(i128) => i64(*i128)
                .map(de::Unexpected::Signed)
                .unwrap_or_else(|_| de::Unexpected::Other("i128")),

            Object::U8(u8) => de::Unexpected::Unsigned(u64(*u8)),
            Object::U16(u16) => de::Unexpected::Unsigned(u64(*u16)),
            Object::U32(u32) => de::Unexpected::Unsigned(u64(*u32)),
            Object::U64(u64) => de::Unexpected::Unsigned(*u64),
            Object::U128(u128) => u64(*u128)
                .map(de::Unexpected::Unsigned)
                .unwrap_or_else(|_| de::Unexpected::Other("u128")),

            Object::F32(f32) => de::Unexpected::Float(f64(*f32)),
            Object::F64(f64) => de::Unexpected::Float(*f64),
            Object::Char(char) => de::Unexpected::Char(*char),
            Object::String(cow) => de::Unexpected::Str(cow),
            Object::ByteArray(cow) => de::Unexpected::Bytes(cow),
            Object::Option(_) => de::Unexpected::Option,
            Object::Unit => de::Unexpected::Unit,
            Object::UnitStruct { name } => de::Unexpected::Other("unit struct"),
            Object::UnitVariant { .. } => de::Unexpected::UnitVariant,
            Object::NewtypeStruct { name, value } => de::Unexpected::NewtypeStruct,
            Object::NewtypeVariant { .. } => de::Unexpected::NewtypeVariant,
            Object::Seq(_) => de::Unexpected::Seq,
            Object::Tuple(_) => de::Unexpected::Other("tuple"),
            Object::TupleStruct { name, fields } => de::Unexpected::Other("tuple struct"),
            Object::TupleVariant { .. } => de::Unexpected::TupleVariant,
            Object::Map(_) => de::Unexpected::Map,
            Object::Struct { name, fields } => de::Unexpected::Other("struct"),
            Object::StructVariant { .. } => de::Unexpected::StructVariant,
        }
    }
}

impl<'de> de::EnumAccess<'de> for Object<'de> {
    type Error = <Self as de::Deserializer<'de>>::Error;
    type Variant = VariantAccess<'de>;
    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        match self {
            Object::UnitVariant { variant, .. } => {
                (seed.deserialize(*variant)?, VariantAccess::Unit)
            }
            Object::NewtypeVariant { variant, value, .. } => {
                (seed.deserialize(*variant)?, VariantAccess::Newtype(*value))
            }
            Object::TupleVariant {
                variant, fields, ..
            } => (seed.deserialize(*variant)?, VariantAccess::Tuple(*fields)),
            Object::StructVariant {
                variant, fields, ..
            } => (seed.deserialize(*variant)?, VariantAccess::Struct(*fields)),
            _ => {
                return Err(de::Error::invalid_type(
                    de::Unexpected::Other("non-variant Object"),
                    &"enum variant",
                ))
            }
        }
        .pipe(Ok)
    }
}

pub enum VariantAccess<'a> {
    Unit,
    Newtype(Object<'a>),
    Tuple(Object<'a>),
    Struct(Object<'a>),
}
impl<'de> de::VariantAccess<'de> for VariantAccess<'de> {
    type Error = <Object<'de> as de::EnumAccess<'de>>::Error;
    fn unit_variant(self) -> Result<(), Self::Error> {
        match self {
            VariantAccess::Unit => Ok(()),
            _ => Err(de::Error::invalid_type(self.unexp(), &"unit variant")),
        }
    }
    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        match self {
            VariantAccess::Newtype(value) => seed.deserialize(value),
            _ => Err(de::Error::invalid_type(self.unexp(), &"newtype variant")),
        }
    }
    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self {
            //TODO: Check length?
            VariantAccess::Tuple(fields) => fields.deserialize_any(visitor),
            _ => Err(de::Error::invalid_type(self.unexp(), &"tuple variant")),
        }
    }
    fn struct_variant<V>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self {
            //TODO: Check fields?
            VariantAccess::Struct(fields) => fields.deserialize_any(visitor),
            _ => Err(de::Error::invalid_type(self.unexp(), &"struct variant")),
        }
    }
}
impl<'de> VariantAccess<'de> {
    fn unexp(&self) -> de::Unexpected {
        match self {
            VariantAccess::Unit => de::Unexpected::UnitVariant,
            VariantAccess::Newtype(_) => de::Unexpected::NewtypeVariant,
            VariantAccess::Tuple(_) => de::Unexpected::TupleVariant,
            VariantAccess::Struct(_) => de::Unexpected::StructVariant,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct NoEnumAssistant;
impl EnumAssistant for NoEnumAssistant {
    fn variant_hint<E: de::Error>(&self, _variant: &Object) -> Result<VariantKind, E> {
        Err(de::Error::custom(
            "Encountered enum variant with no EnumAssistant specified.",
        ))
    }
}
