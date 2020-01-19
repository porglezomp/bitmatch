use bitmatch::bitmatch;

#[bitmatch]
fn swap_nibbles(n: u8) -> u8 {
    #[bitmatch]
    let "xxxx_yyyy" = n;
    bitpack!("yyyy_xxxx")
}

#[bitmatch]
fn decode(inst: u8) -> u8 {
    #[bitmatch]
    match inst {
        "0000_0000" => 0,
        "0000_0001" => 1,
        "0000_????" => 2,
        "0ooo_aabb" => o ^ a ^ b,
        "1000_aabb" => a ^ b,
        "10??_????" => 3,
        "11??_????" => 3,
    }
}

#[test]
fn test_swap_nibbles() {
    assert_eq!(swap_nibbles(0xfa), 0xaf);
}
