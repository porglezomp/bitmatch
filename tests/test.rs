use bitmatch::bitmatch;

#[bitmatch]
fn swap_nibbles(n: u8) -> u8 {
    #[bitmatch]
    let "xxxx_yyyy" = n;
    bitpack!("yyyy_xxxx")
}

#[bitmatch]
fn interleave(x: u8, y: u8) -> u16 {
    bitpack!("xyxyxyxy_xyxyxyxy")
}

#[bitmatch]
pub fn decode(inst: u8) -> u8 {
    let zero = 0;
    #[bitmatch]
    match inst {
        "0000_0000" => 0,
        "0000_0001" => 1,
        "0000_????" => 2,
        "00oo_aaoo" if a == o => o ^ a,
        "0???_????" => 9,
        "1000_aabb" if a == b => a ^ b,
        "10??_????" => 3,
        "11??_aabb" if a == b => 3,
        "11??_aabb" if a != zero => b,
        "11??_aabb" => {
            let _ = a;
            b
        }
    }
}

#[bitmatch]
pub fn match_with_wildcard(i: u8) -> u8 {
    #[bitmatch]
    match i {
        "0000" => 0,
        "???1" => 1,
        _ => 2
    }
}

#[bitmatch]
fn long_pattern(x: u64) -> u64 {
    #[bitmatch]
    let "rxxxxxxx_xxxxxxxx_xxxxxxxx_cccccccc_dddddddd_dddddddd_dddddddd_dddddddd" = x;
    #[bitmatch]
    let "aaaaaaaa_aaaaaaaa_aaaaaaaa_aaaaaaaa
         aaaaaaaa_bbbbbbbb_aaaaaaaa_aaaaaaaa
         aaaaaaaa_aaaaaaaa_aaaaaaaa_aaaaaaaa
         aaaaaaaa_aaaaaaaa_aaaaaaaa_aaaaaaaa" = 0u128;
    (a as u64) ^ (b as u64) ^ r ^ x ^ c ^ d
}

#[test]
fn test_swap_nibbles() {
    assert_eq!(swap_nibbles(0xfa), 0xaf);
}

#[test]
fn test_interleave() {
    assert_eq!(interleave(0xFF, 0x00), 0xAAAA);
}

#[test]
fn test_match_with_wildcard() {
    assert_eq!(match_with_wildcard(0b0000), 0);
    assert_eq!(match_with_wildcard(0b1001), 1);
    assert_eq!(match_with_wildcard(0b1110), 2);
}

#[test]
fn test_long_pattern() {
    long_pattern(0);
}
