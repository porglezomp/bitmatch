use bitmatch::bitmatch;

#[bitmatch]
fn shuffle(n: u8) -> u8 {
    #[bitmatch]
    let "xxxx_yyyy" = n;
    bitpack!(xyxyxyxy)
}

#[bitmatch]
fn decode(inst: u8) -> u8 {
    #[bitmatch]
    match inst {
        "0000_0000" => 0,
        // "0000_0001" => 1,
        "0000_????" => 2,
        // "0ooo_aabb" => o ^ a ^ b,
        "1000_aabb" => a ^ b,
        "????_????" => 3,
    }
}

fn main() {
    println!("{:b}", shuffle(0xF0));
}
