#[macro_export]
macro_rules! newtype {
    ($name:ident:$wrapped:ty) => {
        #[derive(Debug, Default, Clone)]
        pub struct $name($wrapped);

        impl ::std::ops::Deref for $name {
            type Target = $wrapped;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl ::std::ops::DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    };
}
