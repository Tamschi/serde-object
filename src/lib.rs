use std::marker::PhantomData;
use {
    cast::{i64, u64},
    serde::{
        de,
        ser::{
            self, Error as _, SerializeStruct as _, SerializeStructVariant as _,
            SerializeTuple as _, SerializeTupleStruct as _, SerializeTupleVariant as _,
        },
    },
    std::borrow::Cow,
    wyz::pipe::Pipe as _,
};

/// Represents a Serde data model value, losslessly.  
/// See <https://serde.rs/data-model.html> for more information.
///
/// # Limitations
///
/// - `i128` is serialized as `i64`, and if that doesn't fit then an error is thrown.
/// - `u128` is serialized as `u64`, and if that doesn't fit then an error is thrown.
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
        variant_index: u32,
        variant: Cow<'a, str>,
    },

    NewtypeStruct {
        name: Cow<'a, str>,
        value: Box<Object<'a>>,
    },

    NewtypeVariant {
        name: Cow<'a, str>,
        variant_index: u32,
        variant: Cow<'a, str>,
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
        variant_index: u32,
        variant: Cow<'a, str>,
        fields: Vec<Object<'a>>,
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
        variant_index: u32,
        variant: Cow<'a, str>,
        fields: Vec<(Cow<'a, str>, Object<'a>)>,
    },
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

            Object::UnitVariant {
                name,
                variant_index,
                variant,
            } => {
                serializer.serialize_unit_variant(leak_str(name), *variant_index, leak_str(variant))
            }

            Object::NewtypeStruct { name, value } => {
                serializer.serialize_newtype_struct(leak_str(name), value)
            }

            Object::NewtypeVariant {
                name,
                variant_index,
                variant,
                value,
            } => serializer.serialize_newtype_variant(
                leak_str(name),
                *variant_index,
                leak_str(variant),
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
                variant_index,
                variant,
                fields,
            } => {
                let mut serializer = serializer.serialize_tuple_variant(
                    leak_str(name),
                    *variant_index,
                    leak_str(variant),
                    fields.len(),
                )?;
                for field in fields {
                    serializer.serialize_field(field)?
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
                variant_index,
                variant,
                fields,
            } => {
                let mut serializer = serializer.serialize_struct_variant(
                    leak_str(name),
                    *variant_index,
                    leak_str(variant),
                    fields.len(),
                )?;
                for (key, value) in fields {
                    serializer.serialize_field(leak_str(key), value)?
                }
                serializer.end()
            }
        }
    }
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
        deserializer.deserialize_any(Visitor)
    }
}

macro_rules! visit {
    ($($visit_:ident($($ty:ty$( | $($expr:expr$(, $question_mark:tt)*);+$(;)?)?)?) => $variant:ident$(($const:expr))? $(/ ::$Error:ident where T: $t_path:path)?),*$(,)?) => {
        $(
            fn $visit_<T>(self$(, v: $ty)?) -> Result<Self::Value, T$(::$Error)?>
            where
                T: $($t_path, T::Error: )?de::Error,
            {
                Ok(Object::$variant
                    // Alternatives:
                    $(({let _: $ty; v$($(.pipe($expr)$($question_mark)*)+)?.into()}))?
                    $(($const))?
                )
            }
        )*
    };
}

struct Visitor;
impl<'de> de::Visitor<'de> for Visitor {
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
        visit_some(T | |d| d.deserialize_any(Self), ?; Box::new) => Option / ::Error where T: de::Deserializer<'de>,

        visit_unit() => Unit,
    }

    fn visit_newtype_struct<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        //TODO: Test this. It's probably correct?
        deserializer.deserialize_any(self)
    }

    fn visit_seq<A>(self, access: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        struct SeqAccessIterator<'de, Access, Element>(Access, PhantomData<&'de Element>);
        impl<'de, Access: de::SeqAccess<'de>, Element: de::Deserialize<'de>> Iterator
            for SeqAccessIterator<'de, Access, Element>
        {
            type Item = Result<Element, Access::Error>;
            fn next(&mut self) -> Option<Self::Item> {
                self.0.next_element().transpose()
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                (self.0.size_hint().unwrap_or(0), None)
            }
        }

        Ok(Object::Seq(
            SeqAccessIterator(access, PhantomData).collect::<Result<_, _>>()?,
        ))
    }

    fn visit_map<A>(self, access: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        struct MapAccessIterator<'de, Access, Key, Value>(Access, PhantomData<&'de (Key, Value)>);
        impl<
                'de,
                Access: de::MapAccess<'de>,
                Key: de::Deserialize<'de>,
                Value: de::Deserialize<'de>,
            > Iterator for MapAccessIterator<'de, Access, Key, Value>
        {
            type Item = Result<(Key, Value), Access::Error>;
            fn next(&mut self) -> Option<Self::Item> {
                self.0.next_entry().transpose()
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                (self.0.size_hint().unwrap_or(0), None)
            }
        }

        Ok(Object::Map(
            MapAccessIterator(access, PhantomData).collect::<Result<_, _>>()?,
        ))
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: de::EnumAccess<'de>,
    {
        let _ = data;
        Err(de::Error::invalid_type(de::Unexpected::Enum, &self))
    }
}
