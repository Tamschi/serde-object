use {crate::Object, serde::de, std::borrow::Cow};

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
