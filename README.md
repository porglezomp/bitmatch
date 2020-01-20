# Bitmatch

![Crates.io Library Version Number](https://img.shields.io/crates/v/bitmatch)
![Crates.io Library License](https://img.shields.io/crates/l/bitmatch)
![Docs.rs Documentation Version Number](https://docs.rs/bitmatch/badge.svg)

The bitmatch crate provides tools for packing and unpacking integers as
sequences of bits.

## Examples

Using `#[bitmatch]` with a `let` unpacks the bits into separate
single-character variables for each letter you use.

Using `bitpack!()` re-packs the bits from those single-character variables
into a single integer.

```rust
use bitmatch::bitmatch;

#[bitmatch]
fn interleave(n: u8) -> u8 {
    #[bitmatch]
    let "xxxx_yyyy" = n;
    bitpack!("xyxy_xyxy")
}

fn main() {
    assert_eq!(interleave(0xF0), 0xAA);
}
```

You can use `#[bitmatch]` on a `match` as well, and it will ensure that the
literal portions match, and bind the variable portions.

```rust
use bitmatch::bitmatch;
#[bitmatch]
fn decode(inst: u8) -> String {
    #[bitmatch]
    match inst {
        "00oo_aabb" => format!("Op {}, {}, {}", o, a, b),
        "0100_aaii" => format!("Val {}, {}", a, i),
        "01??_????" => format!("Invalid"),
        "10ii_aabb" => format!("Ld {}, {}, {}", a, b, i),
        "11ii_aabb" => format!("St {}, {}, {}", a, b, i),
    }
}
```

