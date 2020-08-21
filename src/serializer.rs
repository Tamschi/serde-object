//! Serializing directly into an [`Object<'static>`].

use crate::Object;
use serde::{
	ser::{self, Serialize as _, SerializeMap as _},
	serde_if_integer128,
};
use std::{borrow::Cow, fmt::Debug};
use wyz::Pipe as _;

#[derive(Debug)]
pub enum Never {}
impl std::fmt::Display for Never {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		Debug::fmt(self, f)
	}
}
impl std::error::Error for Never {}
impl ser::Error for Never {
	fn custom<T>(_msg: T) -> Self
	where
		T: std::fmt::Display,
	{
		unimplemented!()
	}
}

pub struct Serializer;

macro_rules! serialize {
    ($($serialize_:ident($($ty:ty$( | $($expr:expr$(, $question_mark:tt)*);+$(;)?)?)?) => $variant:ident$(($const:expr))?),*$(,)?) => {
        $(
            fn $serialize_(self$(, v: $ty)?) -> Result<Self::Ok, Self::Error>
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

impl ser::Serializer for Serializer {
	type Ok = Object<'static>;
	type Error = Never;
	type SerializeSeq = SerializeSeq;
	type SerializeTuple = SerializeTuple;
	type SerializeTupleStruct = SerializeTupleStruct;
	type SerializeTupleVariant = SerializeTupleVariant;
	type SerializeMap = SerializeMap;
	type SerializeStruct = SerializeStruct;
	type SerializeStructVariant = SerializeStructVariant;

	serialize! {
		serialize_bool(bool) => Bool,

		serialize_i8(i8) => I8,
		serialize_i16(i16) => I16,
		serialize_i32(i32) => I32,
		serialize_i64(i64) => I64,

		serialize_u8(u8) => U8,
		serialize_u16(u16) => U16,
		serialize_u32(u32) => U32,
		serialize_u64(u64) => U64,
	}

	serde_if_integer128!(serialize! {
		serialize_i128(i128) => I128,
		serialize_u128(u128) => U128,
	});

	serialize! {
		serialize_f32(f32) => F32,
		serialize_f64(f64) => F64,

		serialize_char(char) => Char,
		serialize_str(&str | str::to_string) => String,

		serialize_bytes(&[u8] | <[u8]>::to_owned) => ByteArray,
		serialize_none() => Option(None),
	}

	fn serialize_some<T: ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error>
	where
		T: serde::Serialize,
	{
		Ok(Object::Option(Some(Box::new(value.serialize(Self)?))))
	}

	serialize! {
		serialize_unit() => Unit,
	}

	fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
		Ok(Object::UnitStruct { name: name.into() })
	}

	fn serialize_unit_variant(
		self,
		name: &'static str,
		variant_index: u32,
		variant: &'static str,
	) -> Result<Self::Ok, Self::Error> {
		Ok(Object::UnitVariant {
			name: name.into(),
			variant: Object::DualVariantKey {
				index: variant_index,
				name: variant.into(),
			}
			.into(),
		})
	}

	fn serialize_newtype_struct<T: ?Sized>(
		self,
		name: &'static str,
		value: &T,
	) -> Result<Self::Ok, Self::Error>
	where
		T: serde::Serialize,
	{
		Ok(Object::NewtypeStruct {
			name: name.into(),
			value: value.serialize(Self)?.into(),
		})
	}

	fn serialize_newtype_variant<T: ?Sized>(
		self,
		name: &'static str,
		variant_index: u32,
		variant: &'static str,
		value: &T,
	) -> Result<Self::Ok, Self::Error>
	where
		T: serde::Serialize,
	{
		Ok(Object::NewtypeVariant {
			name: name.into(),
			variant: Object::DualVariantKey {
				index: variant_index,
				name: variant.into(),
			}
			.into(),
			value: value.serialize(Self)?.into(),
		})
	}

	fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
		Ok(SerializeSeq::new(len))
	}

	fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
		Ok(SerializeTuple::new(len))
	}

	fn serialize_tuple_struct(
		self,
		name: &'static str,
		len: usize,
	) -> Result<Self::SerializeTupleStruct, Self::Error> {
		Ok(SerializeTupleStruct::new(name, len))
	}

	fn serialize_tuple_variant(
		self,
		name: &'static str,
		variant_index: u32,
		variant: &'static str,
		len: usize,
	) -> Result<Self::SerializeTupleVariant, Self::Error> {
		Ok(SerializeTupleVariant::new(
			name,
			Object::DualVariantKey {
				index: variant_index,
				name: variant.into(),
			},
			len,
		))
	}

	fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
		Ok(SerializeMap::new(len))
	}

	fn serialize_struct(
		self,
		name: &'static str,
		len: usize,
	) -> Result<Self::SerializeStruct, Self::Error> {
		Ok(SerializeStruct::new(name, len))
	}

	fn serialize_struct_variant(
		self,
		name: &'static str,
		variant_index: u32,
		variant: &'static str,
		len: usize,
	) -> Result<Self::SerializeStructVariant, Self::Error> {
		Ok(SerializeStructVariant::new(
			name,
			Object::DualVariantKey {
				index: variant_index,
				name: variant.into(),
			},
			len,
		))
	}

	fn collect_seq<I>(self, iter: I) -> Result<Self::Ok, Self::Error>
	where
		I: IntoIterator,
		I::Item: serde::Serialize,
	{
		Ok(Object::Seq(
			iter.into_iter()
				.map(|element| element.serialize(Self))
				.collect::<Result<_, _>>()?,
		))
	}

	fn collect_map<K, V, I>(self, iter: I) -> Result<Self::Ok, Self::Error>
	where
		K: serde::Serialize,
		V: serde::Serialize,
		I: IntoIterator<Item = (K, V)>,
	{
		let iter = iter.into_iter();
		let mut serializer = self.serialize_map(Some(iter.size_hint().0))?;
		for (key, value) in iter {
			serializer.serialize_entry(&key, &value)?;
		}
		serializer.end()
	}

	fn collect_str<T: ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error>
	where
		T: std::fmt::Display,
	{
		self.serialize_str(&value.to_string())
	}

	fn is_human_readable(&self) -> bool {
		true
	}
}

pub struct SerializeSeq(Vec<Object<'static>>);
impl SerializeSeq {
	fn new(len: Option<usize>) -> Self {
		Self(Vec::with_capacity(len.unwrap_or_default()))
	}
}
impl ser::SerializeSeq for SerializeSeq {
	type Ok = Object<'static>;
	type Error = Never;
	fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: serde::Serialize,
	{
		self.0.push(value.serialize(Serializer)?);
		Ok(())
	}
	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(Object::Seq(self.0))
	}
}

pub struct SerializeTuple(Vec<Object<'static>>);
impl SerializeTuple {
	fn new(len: usize) -> Self {
		Self(Vec::with_capacity(len))
	}
}
impl ser::SerializeTuple for SerializeTuple {
	type Ok = Object<'static>;
	type Error = Never;
	fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: serde::Serialize,
	{
		self.0.push(value.serialize(Serializer)?);
		Ok(())
	}
	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(Object::Tuple(self.0))
	}
}

pub struct SerializeMap {
	map: Vec<(Object<'static>, Object<'static>)>,
	key: Option<Object<'static>>,
}
impl SerializeMap {
	fn new(len: Option<usize>) -> Self {
		Self {
			map: Vec::with_capacity(len.unwrap_or_default()),
			key: None,
		}
	}
}
impl ser::SerializeMap for SerializeMap {
	type Ok = Object<'static>;
	type Error = Never;

	fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<(), Self::Error>
	where
		T: serde::Serialize,
	{
		assert!(self.key.replace(key.serialize(Serializer)?).is_none());
		Ok(())
	}

	fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: serde::Serialize,
	{
		self.map
			.push((self.key.take().unwrap(), value.serialize(Serializer)?));
		Ok(())
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		assert!(self.key.is_none());
		Ok(Object::Map(self.map))
	}

	fn serialize_entry<K: ?Sized, V: ?Sized>(
		&mut self,
		key: &K,
		value: &V,
	) -> Result<(), Self::Error>
	where
		K: serde::Serialize,
		V: serde::Serialize,
	{
		assert!(self.key.is_none());
		self.map
			.push((key.serialize(Serializer)?, value.serialize(Serializer)?));
		Ok(())
	}
}

pub struct SerializeTupleStruct {
	name: &'static str,
	fields: Vec<Object<'static>>,
}
impl SerializeTupleStruct {
	fn new(name: &'static str, len: usize) -> Self {
		Self {
			name,
			fields: Vec::with_capacity(len),
		}
	}
}
impl ser::SerializeTupleStruct for SerializeTupleStruct {
	type Ok = Object<'static>;
	type Error = Never;
	fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: serde::Serialize,
	{
		self.fields.push(value.serialize(Serializer)?);
		Ok(())
	}
	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(Object::TupleStruct {
			name: self.name.into(),
			fields: self.fields,
		})
	}
}

pub struct SerializeTupleVariant {
	name: &'static str,
	variant: Object<'static>,
	fields: Vec<Object<'static>>,
}
impl SerializeTupleVariant {
	fn new(name: &'static str, variant: Object<'static>, len: usize) -> Self {
		Self {
			name,
			variant,
			fields: Vec::with_capacity(len),
		}
	}
}
impl ser::SerializeTupleVariant for SerializeTupleVariant {
	type Ok = Object<'static>;
	type Error = Never;
	fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: serde::Serialize,
	{
		self.fields.push(value.serialize(Serializer)?);
		Ok(())
	}
	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(Object::TupleVariant {
			name: self.name.into(),
			variant: self.variant.into(),
			fields: Object::Seq(self.fields).into(),
		})
	}
}

pub struct SerializeStruct {
	name: &'static str,
	fields: Vec<(Cow<'static, str>, Option<Object<'static>>)>,
}
impl SerializeStruct {
	fn new(name: &'static str, len: usize) -> Self {
		Self {
			name,
			fields: Vec::with_capacity(len),
		}
	}
}
impl ser::SerializeStruct for SerializeStruct {
	type Ok = Object<'static>;
	type Error = Never;

	fn serialize_field<T: ?Sized>(
		&mut self,
		key: &'static str,
		value: &T,
	) -> Result<(), Self::Error>
	where
		T: serde::Serialize,
	{
		self.fields
			.push((key.into(), value.serialize(Serializer)?.into()));
		Ok(())
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(Object::Struct {
			name: self.name.into(),
			fields: self.fields,
		})
	}

	fn skip_field(&mut self, key: &'static str) -> Result<(), Self::Error> {
		self.fields.push((key.into(), None));
		Ok(())
	}
}

pub struct SerializeStructVariant {
	name: &'static str,
	variant: Object<'static>,
	fields: Vec<(Object<'static>, Option<Object<'static>>)>,
}
impl SerializeStructVariant {
	fn new(name: &'static str, variant: Object<'static>, len: usize) -> Self {
		Self {
			name,
			variant,
			fields: Vec::with_capacity(len),
		}
	}
}
impl ser::SerializeStructVariant for SerializeStructVariant {
	type Ok = Object<'static>;
	type Error = Never;

	fn serialize_field<T: ?Sized>(
		&mut self,
		key: &'static str,
		value: &T,
	) -> Result<(), Self::Error>
	where
		T: serde::Serialize,
	{
		self.fields.push((
			key.serialize(Serializer)?,
			value.serialize(Serializer)?.into(),
		));
		Ok(())
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(Object::StructVariant {
			name: self.name.into(),
			variant: self.variant.into(),
			fields: Object::FieldMap(self.fields).into(),
		})
	}

	fn skip_field(&mut self, key: &'static str) -> Result<(), Self::Error> {
		self.fields.push((key.serialize(Serializer)?, None));
		Ok(())
	}
}
