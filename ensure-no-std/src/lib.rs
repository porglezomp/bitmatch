#![no_std]

use bitmatch::bitmatch;

#[bitmatch]
pub fn bitrev(x: u8) -> u8 {
    #[bitmatch]
    let "abcdefgh" = x;
    bitpack!("hgfedcba")
}

#[bitmatch]
pub fn decode(x: u8) -> u8 {
    #[bitmatch]
    match x {
        "00xy_aaaa" if x == y => a,
        "00??_aaaa" => !a,
        "0100_aabb" => a + b,
        "0101_aabb" => a - b,
        "0110_aabb" => a * b,
        "0111_aabb" => a / b,
        "1000_aabb" => (a < b) as u8,
        "10??_aabb" => (a == b) as u8,
        "11??_aaaa" => 255 - a,
    }
}
