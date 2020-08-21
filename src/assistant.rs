//! Side channels for providing more information than the Serde API lets through in-band.

use crate::{Object, Visitor};
use lazy_static::lazy_static;
use serde::de::{self, DeserializeSeed as _};
use std::{any::Any, borrow::Cow, marker::PhantomData, sync::Mutex};
use wyz::Pipe as _;

#[cfg(feature = "assistant-extra")]
pub mod extra {
	use super::VariantKind;
	use linkme::distributed_slice;

	/// Called between EnumAccess::variant_seed and one of VariantAccess's methods.
	#[distributed_slice]
	pub static ENUM_VARIANT_ASSISTS: [fn() -> Option<VariantKind>] = [..];

	pub fn enum_variant_hint() -> Option<VariantKind> {
		// If it were possible to get type-GUIDs for non-'static types, this would be doable a lot more nicely.
		// Without that, we can't inspect the EnumAccess or VariantAccess involved safely, though.
		// Unfortunately, the RFC for non-'static TypeIds was rejected (<https://github.com/rust-lang/rust/issues/41875>),
		// so this is the best I can do.

		for enum_variant_assist in ENUM_VARIANT_ASSISTS.iter() {
			if let result @ Some(_) = enum_variant_assist() {
				return result;
			}
		}
		None
	}
}

pub enum VariantKind {
	Unit,
	Newtype,
	Tuple(usize),
	Struct(Cow<'static, [Cow<'static, str>]>),
}

pub trait EnumAssistant {
	fn variant_hint<E: de::Error>(&self, variant: &Object) -> Result<VariantKind, E>;
}

impl<T: EnumAssistant> EnumAssistant for &T {
	#[inline(always)]
	fn variant_hint<E: de::Error>(&self, variant: &Object) -> Result<VariantKind, E> {
		<T as EnumAssistant>::variant_hint(self, variant)
	}
}

pub struct Seed<Assistant: EnumAssistant + Clone>(pub Assistant);
impl<'de, Assistant: EnumAssistant + Clone> de::DeserializeSeed<'de> for Seed<Assistant> {
	type Value = Object<'de>;

	fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
	where
		D: de::Deserializer<'de>,
	{
		deserializer.deserialize_any(Visitor(self.0))
	}
}

pub struct Assisted<'a, Assistant: EnumAssistant + 'static>(Object<'a>, PhantomData<Assistant>);
lazy_static! {
	static ref ASSIST_ASSISTANT: Mutex<Option<&'static (dyn Any + Sync)>> = Mutex::default();
}
pub fn assist<'a, Assistant: EnumAssistant + Sync + 'static>(
	assistant: Assistant, //TODO: Lower the lifetime requirement for this parameter type.
	deserialize: impl FnOnce() -> Assisted<'a, Assistant>,
) -> Object<'a> {
	*ASSIST_ASSISTANT.lock().unwrap() =
		Some(unsafe { extend_ref(&assistant) as &'static (dyn Any + Sync) });
	let result = deserialize();
	ASSIST_ASSISTANT.lock().unwrap().take(); // Make sure the assistant ref doesn't stick around.
	result.0
}

impl<'de, Assistant: EnumAssistant> de::Deserialize<'de> for Assisted<'de, Assistant> {
	fn deserialize<D>(
		deserializer: D,
	) -> std::result::Result<Self, <D as serde::de::Deserializer<'de>>::Error>
	where
		D: de::Deserializer<'de>,
	{
		Ok(Self(
			Seed(
				ASSIST_ASSISTANT
					.lock()
					.unwrap()
					.take()
					.expect("Could not retrieve stored EnumAssistant.")
					.pipe(|a| Any::downcast_ref::<Assistant>(a))
					.expect("Received wrong type of assistant."),
			)
			.deserialize(deserializer)?,
			PhantomData,
		))
	}
}

unsafe fn extend_ref<T>(reference: &'_ T) -> &'static T {
	std::mem::transmute(reference)
}
