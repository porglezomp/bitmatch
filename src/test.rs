use super::*;
use boolean_expression::CubeVar::*;

#[test]
fn test_cube() {
    assert_eq!(
        to_cube("101?_aabb").vars().cloned().collect::<Vec<_>>(),
        vec![DontCare, DontCare, DontCare, DontCare, DontCare, True, False, True],
    );
}

#[test]
fn test_masks() {
    assert_eq!(&pat_mask("101?_aabb"), "11100000");
}

#[test]
fn test_values() {
    assert_eq!(&pat_value("101?_aabb"), "10100000");
}
