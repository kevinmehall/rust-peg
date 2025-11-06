peg::parser!(grammar foo() for str {
    pub rule main()
    = yepnope(true)
      yepnope(false)
    / yepnope(true)
      yepnope(true)
      yepnope(false)

    #[cache]
    rule yepnope(yep: bool)
    = &assert(yep, "yep") "yep"
    / !assert(yep, "yep") "nope"

    pub rule main_ref()
    = yepnope_ref(&true)
      yepnope_ref(&false)
    / yepnope_ref(&true)
      yepnope_ref(&true)
      yepnope_ref(&false)

    #[cache]
    rule yepnope_ref(yep: &bool)
    = &assert(*yep, "yep") "yep"
    / !assert(*yep, "yep") "nope"

    pub rule main_ref_lifetime()
    = yepnope_ref(&true)
      yepnope_ref(&false)
    / yepnope_ref(&true)
      yepnope_ref(&true)
      yepnope_ref(&false)

    #[cache]
    rule yepnope_ref_lifetime(yep: &'input bool)
    = &assert(*yep, "yep") "yep"
    / !assert(*yep, "yep") "nope"

    pub rule main_ref_to_owned()
    = yepnope_ref_to_owned("yep")
      yepnope_ref_to_owned("nope")
    / yepnope_ref_to_owned("yep")
      yepnope_ref_to_owned("yep")
      yepnope_ref_to_owned("nope")

    #[cache]
    rule yepnope_ref_to_owned(yep: &str)
    = &assert(yep == "yep", "yep") "yep"
    / !assert(yep == "yep", "yep") "nope"

    rule assert(v: bool, msg: &'static str)
    = {? if v { Ok(()) } else { Err(msg) } }
});

#[test]
fn main() {
    foo::main("yepnope").unwrap();
    foo::main("nopeyep").unwrap_err();
    foo::main("yepyepnope").unwrap();
    foo::main("nopeyepnope").unwrap_err();

    foo::main_ref("yepnope").unwrap();
    foo::main_ref("nopeyep").unwrap_err();
    foo::main_ref("yepyepnope").unwrap();
    foo::main_ref("nopeyepnope").unwrap_err();

    foo::main_ref_lifetime("yepnope").unwrap();
    foo::main_ref_lifetime("nopeyep").unwrap_err();
    foo::main_ref_lifetime("yepyepnope").unwrap();
    foo::main_ref_lifetime("nopeyepnope").unwrap_err();

    foo::main_ref_to_owned("yepnope").unwrap();
    foo::main_ref_to_owned("nopeyep").unwrap_err();
    foo::main_ref_to_owned("yepyepnope").unwrap();
    foo::main_ref_to_owned("nopeyepnope").unwrap_err();
}
