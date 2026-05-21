use core::sync::atomic::{AtomicU8, Ordering};

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Wrapper<'a>(&'a str);

impl peg::hashbrown::Equivalent<String> for Wrapper<'_> {
    fn equivalent(&self, key: &String) -> bool {
        self.0 == *key
    }
}

impl peg::Cacheable for Wrapper<'_> {
    type Cached = String;
    type Key = str;

    fn key(&self) -> &Self::Key {
        self.0
    }

    fn to_cached(&self) -> Self::Cached {
        self.0.to_cached()
    }
}

static CACHE_MISS: AtomicU8 = AtomicU8::new(0);

peg::parser!(grammar foo<'wrap>() for str {
    pub rule main()
    = yepnope(true)
      yepnope(false)
    / yepnope(true)
      yepnope(true)
      yepnope(false)

    #[cache]
    rule yepnope(yep: bool)
    = ({ CACHE_MISS.fetch_add(1, Ordering::SeqCst); })
    ( &assert(yep, "yep") "yep"
    / !assert(yep, "yep") "nope")

    pub rule main_ref()
    = yepnope_ref(&true)
      yepnope_ref(&false)
    / yepnope_ref(&true)
      yepnope_ref(&true)
      yepnope_ref(&false)

    #[cache]
    rule yepnope_ref(yep: &bool)
    = ({ CACHE_MISS.fetch_add(1, Ordering::SeqCst); })
    ( &assert(*yep, "yep") "yep"
    / !assert(*yep, "yep") "nope")

    pub rule main_ref_lifetime()
    = yepnope_ref(&true)
      yepnope_ref(&false)
    / yepnope_ref(&true)
      yepnope_ref(&true)
      yepnope_ref(&false)

    #[cache]
    rule yepnope_ref_lifetime(yep: &'input bool)
    = ({ CACHE_MISS.fetch_add(1, Ordering::SeqCst); })
    ( &assert(*yep, "yep") "yep"
    / !assert(*yep, "yep") "nope")

    pub rule main_ref_non_primitive()
    = yepnope_ref_non_primitive("yep")
      yepnope_ref_non_primitive("nope")
    / yepnope_ref_non_primitive("yep")
      yepnope_ref_non_primitive("yep")
      yepnope_ref_non_primitive("nope")

    #[cache]
    rule yepnope_ref_non_primitive(yep: &str)
    = ({ CACHE_MISS.fetch_add(1, Ordering::SeqCst); })
    ( &assert(yep == "yep", "yep") "yep"
    / !assert(yep == "yep", "yep") "nope")

    pub rule main_ref_tuple()
    = yepnope_ref_tuple((true, true))
      yepnope_ref_tuple((false, false))
    / yepnope_ref_tuple((true, true))
      yepnope_ref_tuple((true, true))
      yepnope_ref_tuple((false, false))

    #[cache]
    rule yepnope_ref_tuple(yep: (bool, bool))
    = ({ CACHE_MISS.fetch_add(1, Ordering::SeqCst); })
    ( &assert(yep == (true, true), "yep") "yep"
    / !assert(yep == (true, true), "yep") "nope")

    pub rule main_ref_array()
    = yepnope_ref_array([true])
      yepnope_ref_array([false])
    / yepnope_ref_array([true])
      yepnope_ref_array([true])
      yepnope_ref_array([false])

    #[cache]
    rule yepnope_ref_array(yep: [bool; 1])
    = ({ CACHE_MISS.fetch_add(1, Ordering::SeqCst); })
    ( &assert(yep == [true], "yep") "yep"
    / !assert(yep == [true], "yep") "nope")

    pub rule main_ref_slice()
    = yepnope_ref_slice(&[true])
      yepnope_ref_slice(&[false])
    / yepnope_ref_slice(&[true])
      yepnope_ref_slice(&[true])
      yepnope_ref_slice(&[false])

    #[cache]
    rule yepnope_ref_slice(yep: &[bool])
    = ({ CACHE_MISS.fetch_add(1, Ordering::SeqCst); })
    ( &assert(yep == [true], "yep") "yep"
    / !assert(yep == [true], "yep") "nope")

    pub rule main_ref_generic()
    = yepnope_ref_generic(Wrapper("yep"))
      yepnope_ref_generic(Wrapper("nope"))
    / yepnope_ref_generic(Wrapper("yep"))
      yepnope_ref_generic(Wrapper("yep"))
      yepnope_ref_generic(Wrapper("nope"))

    #[cache]
    rule yepnope_ref_generic(yep: Wrapper<'wrap>)
    = ({ CACHE_MISS.fetch_add(1, Ordering::SeqCst); })
    ( &assert(yep.0 == "yep", "yep") "yep"
    / !assert(yep.0 == "yep", "yep") "nope")

    rule assert(v: bool, msg: &'static str)
    = {? if v { Ok(()) } else { Err(msg) } }
});

#[test]
fn main() {
    // Without caching, the number of misses for each group is (2, 4, 5, 4)
    foo::main("yepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main("nopeyep").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main("yepyepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 4);
    foo::main("nopeyepnope").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);

    foo::main_ref("yepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref("nopeyep").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref("yepyepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 4);
    foo::main_ref("nopeyepnope").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);

    foo::main_ref_lifetime("yepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref_lifetime("nopeyep").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref_lifetime("yepyepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 4);
    foo::main_ref_lifetime("nopeyepnope").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);

    foo::main_ref_non_primitive("yepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref_non_primitive("nopeyep").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref_non_primitive("yepyepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 4);
    foo::main_ref_non_primitive("nopeyepnope").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);

    foo::main_ref_tuple("yepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref_tuple("nopeyep").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref_tuple("yepyepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 4);
    foo::main_ref_tuple("nopeyepnope").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);

    foo::main_ref_array("yepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref_array("nopeyep").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref_array("yepyepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 4);
    foo::main_ref_array("nopeyepnope").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);

    foo::main_ref_slice("yepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref_slice("nopeyep").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref_slice("yepyepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 4);
    foo::main_ref_slice("nopeyepnope").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);

    foo::main_ref_generic("yepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref_generic("nopeyep").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
    foo::main_ref_generic("yepyepnope").unwrap();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 4);
    foo::main_ref_generic("nopeyepnope").unwrap_err();
    assert_eq!(CACHE_MISS.fetch_and(0, Ordering::SeqCst), 2);
}
