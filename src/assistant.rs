use {
    crate::{Object, Visitor},
    lazy_static::lazy_static,
    serde::de::{self, DeserializeSeed as _},
    std::{any::Any, borrow::Cow, marker::PhantomData, sync::Mutex},
    wyz::Pipe as _,
};

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
