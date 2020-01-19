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

#[test]
fn test_mask_segments() {
    assert_eq!(mask_segments("0000"), vec![]);
    assert_eq!(mask_segments("0001"), vec![(0, 1)]);
    assert_eq!(mask_segments("0011"), vec![(0, 2)]);
    assert_eq!(mask_segments("0110"), vec![(1, 2)]);
    assert_eq!(mask_segments("1101"), vec![(0, 1), (2, 2)]);
    assert_eq!(mask_segments("1111"), vec![(0, 4)]);
}
