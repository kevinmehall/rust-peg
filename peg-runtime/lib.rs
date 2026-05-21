#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(feature = "unstable", feature(error_in_core))]

/// A re-export of the [`hashbrown`] crate used for caching.
pub use hashbrown;

use std::fmt::Display;

pub mod error;
mod slice;
pub mod str;

/// The result type used internally in the parser.
///
/// You'll only need this if implementing the `Parse*` traits for a custom input
/// type, or using the `#{}` syntax to embed a custom Rust snippet within the parser.
///
/// The public API of a parser adapts errors to `std::result::Result` instead of using this type.
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum RuleResult<T> {
    /// Success, with final location
    Matched(usize, T),

    /// Failure (furthest failure location is not yet known)
    Failed,
}

/// A trait for parameter types that need to be usable with `#[cache]`.
pub trait Cacheable {
    /// The type that will be stored in the cache.
    ///
    /// # Contract
    ///
    /// The implementor **must** hash like `Self`, if it is hashable.
    type Cached: Eq;

    /// The borrowed key type. This will usually be `Self`, but for cacheable
    /// references, this will be the dereferenced type.
    type Key: hashbrown::Equivalent<Self::Cached> + ?Sized;

    /// Returns a reference to `Self` in a form that can be compared to
    /// `Self::Cached`.
    ///
    /// This is used to allow caching of references to cacheable types
    /// (e.g. `&bool`).
    fn key(&self) -> &Self::Key;

    /// Converts `self` to the cached value.
    fn to_cached(&self) -> Self::Cached;
}

#[cfg(feature = "std")]
impl Cacheable for str {
    type Cached = std::string::String;
    type Key = Self;

    #[inline]
    fn key(&self) -> &Self::Key {
        self
    }

    #[inline]
    fn to_cached(&self) -> Self::Cached {
        self.into()
    }
}

#[cfg(feature = "std")]
impl<T> Cacheable for &[T]
where
    T: Cacheable + Eq,
    Vec<<T as Cacheable>::Cached>: core::borrow::Borrow<[T]>,
{
    type Cached = std::vec::Vec<<T as Cacheable>::Cached>;
    type Key = [T];

    #[inline]
    fn key(&self) -> &Self::Key {
        self
    }

    #[inline]
    fn to_cached(&self) -> Self::Cached {
        self.iter().map(Cacheable::to_cached).collect()
    }
}

macro_rules! impl_primitive {
    ($($ty:ty),* $(,)?) => {
        $(impl Cacheable for $ty {
            type Cached = $ty;
            type Key = $ty;

            #[inline]
            fn key(&self) -> &Self::Key {
                self
            }

            #[inline]
            fn to_cached(&self) -> Self::Cached {
                *self
            }
        })*
    }
}

impl_primitive!(bool, u8, i8, u16, i16, u32, i32, u64, i64, u128, i128);

macro_rules! impl_tuple {
    ($($name:ident),* $(,)?) => {
        impl<$($name,)*> Cacheable for ($($name,)*)
        where
            $($name: Cacheable,)*
            Self: hashbrown::Equivalent<($(<$name as Cacheable>::Cached,)*)>
        {
            type Cached = ($(<$name as Cacheable>::Cached,)*);
            type Key = Self;

            #[inline]
            fn key(&self) -> &Self::Key {
                self
            }

            #[allow(clippy::unused_unit)]
            #[allow(non_snake_case)]
            fn to_cached(&self) -> Self::Cached {
                let ($($name,)*) = self;
                ($(Cacheable::to_cached($name),)*)
            }
        }
    }
}

macro_rules! smaller_tuples_too {
    ($m:ident, $ty:ident) => {
        $m! {}
        $m! {$ty}
    };

    ($m:ident, $ty:ident, $($tt:ident),*) => {
        smaller_tuples_too! {$m, $($tt),*}
        $m! {$ty, $($tt),*}
    };
}

smaller_tuples_too!(impl_tuple, L, K, J, I, H, G, F, E, D, C, B, A);

impl<T, const N: usize> Cacheable for [T; N]
where
    Self: hashbrown::Equivalent<[<T as Cacheable>::Cached; N]>,
    T: Cacheable,
{
    type Cached = [<T as Cacheable>::Cached; N];
    type Key = Self;

    #[inline]
    fn key(&self) -> &Self::Key {
        self
    }

    #[inline]
    fn to_cached(&self) -> Self::Cached {
        self.each_ref().map(Cacheable::to_cached)
    }
}

impl<T> Cacheable for &T
where
    T: Cacheable + ?Sized,
{
    type Cached = <T as Cacheable>::Cached;
    type Key = <T as Cacheable>::Key;

    #[inline]
    fn key(&self) -> &Self::Key {
        (*self).key()
    }

    #[inline]
    fn to_cached(&self) -> Self::Cached {
        (**self).to_cached()
    }
}

/// A type that can be used as input to a parser.
#[allow(clippy::needless_lifetimes)]
pub trait Parse {
    type PositionRepr: Display;
    fn start<'input>(&'input self) -> usize;
    fn is_eof<'input>(&'input self, p: usize) -> bool;
    fn position_repr<'input>(&'input self, p: usize) -> Self::PositionRepr;
}

/// A parser input type supporting the `[...]` syntax.
pub trait ParseElem<'input>: Parse {
    /// Type of a single atomic element of the input, for example a character or token
    type Element: Copy;

    /// Get the element at `pos`, or `Failed` if past end of input.
    fn parse_elem(&'input self, pos: usize) -> RuleResult<Self::Element>;
}

/// A parser input type supporting the `"literal"` syntax.
pub trait ParseLiteral: Parse {
    /// Attempt to match the `literal` string at `pos`, returning whether it
    /// matched or failed.
    fn parse_string_literal(&self, pos: usize, literal: &str) -> RuleResult<()>;
}

/// A parser input type supporting the `$()` syntax.
pub trait ParseSlice<'input>: Parse {
    /// Type of a slice of the input.
    type Slice;

    /// Get a slice of input.
    fn parse_slice(&'input self, p1: usize, p2: usize) -> Self::Slice;
}

#[cfg(not(feature = "std"))]
extern crate alloc;
#[cfg(not(feature = "std"))]
extern crate core as std;

// needed for type inference on the `#{|input, pos| ..}` closure, since there
// are different type inference rules on closures in function args.
#[doc(hidden)]
pub fn call_custom_closure<I, T>(
    f: impl FnOnce(I, usize) -> RuleResult<T>,
    input: I,
    pos: usize,
) -> RuleResult<T> {
    f(input, pos)
}

// this is used to insert a required lifetime in the cacheable types for cache
// keys where the type expression `<&Foo as ::peg::Cacheable>::Cached` will
// trigger a compiler error. This is used as a cleaner alternative to filtering
// the token tree, and as a way to avoid pulling in all of syn just to do this
// one thing. Technically this maybe should try to also replace anonymous
// generic lifetimes in types, but that requires an unwarranted level of effort
#[doc(hidden)]
#[macro_export]
macro_rules! chomp_ref {
    (& $lt:lifetime $($tt:tt)*) => {
        &$lt $crate::chomp_ref!($($tt)*)
    };
    (& $($tt:tt)*) => {
        &'rule $crate::chomp_ref!($($tt)*)
    };
    ($ty:ty) => {
        $ty
    };
}
