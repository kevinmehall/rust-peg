peg! runtime_grammar (r#"
#![arguments(state_tracker : &mut Vec<()>)]

pub evaluated 
    = "pre-eval" #{ state_tracker.push(()) } "post-eval"

pub failed_eval
    = "failed-eval" #{ state_tracker.push(()) } "good-eval"

pub matched_eval
    = "runtime" #={ if state_tracker.len() > 0 { slice_eq(__input, __state, __pos, "some") } else { slice_eq(__input, __state, __pos, "none") } }
"#);

#[test]
fn test_runtime_with_success() {
    let mut state = vec![];
    let evaluation = runtime_grammar::evaluated("pre-evalpost-eval", &mut state);

    assert_eq!(state.len(), 1);
}

#[test]
fn test_runtime_with_failure() {
    let mut state = vec![];
    // Despite not matching fully, the stateful effect should still run
    let evaluation = runtime_grammar::failed_eval("failed-evalbad-eval", &mut state);

    assert_eq!(state.len(), 1);
}

#[test]
fn test_runtime_match() {
    let mut state = vec![];
    let matched_none = runtime_grammar::matched_eval("runtimenone", &mut state);

    assert_eq!(matched_none, Ok(()));

    state.push(());

    let matched_some = runtime_grammar::matched_eval("runtimesome", &mut state);

    assert_eq!(matched_some, Ok(()));

    let failed_none = runtime_grammar::matched_eval("runtimenone", &mut state);

    assert!(failed_none.is_err());

    state.pop();

    let failed_some = runtime_grammar::matched_eval("runtimesome", &mut state);

    assert!(failed_some.is_err());
}
