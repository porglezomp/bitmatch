//! The bitmatch crate provides tools for packing and unpacking integers as
//! sequences of bits.
//!
//! These are achieved via the `#[bitmatch]` attribute and the `bitpack!()`
//! macro. Because of the limitations of the current proc-macro system, they
//! both must be used inside a function that has the `#[bitmatch]` attribute
//! on it.
//!
//! Since it generates normal bitmasking, it can be used in `#![no_std]`
//! crates.
//!
//! # Examples
//!
//! Using `#[bitmatch]` with a `let` unpacks the bits into separate
//! single-character variables for each letter you use.
//!
//! Using `bitpack!()` re-packs the bits from those single-character variables
//! into a single integer.
//! ```
//! use bitmatch::bitmatch;
//!
//! #[bitmatch]
//! fn interleave(n: u8) -> u8 {
//!     #[bitmatch]
//!     let "xxxx_yyyy" = n;
//!     bitpack!("xyxy_xyxy")
//! }
//!
//! fn main() {
//!     assert_eq!(interleave(0xF0), 0xAA);
//! }
//! ```
//!
//! You can use `#[bitmatch]` on a `match` as well, and it will ensure that the
//! literal portions match, and bind the variable portions.
//! ```
//! use bitmatch::bitmatch;
//! #[bitmatch]
//! fn decode(inst: u8) -> String {
//!     #[bitmatch]
//!     match inst {
//!         "00oo_aabb" => format!("Op {}, {}, {}", o, a, b),
//!         "0100_aaii" => format!("Val {}, {}", a, i),
//!         "01??_????" => format!("Invalid"),
//!         "10ii_aabb" => format!("Ld {}, {}, {}", a, b, i),
//!         "11ii_aabb" => format!("St {}, {}, {}", a, b, i),
//!     }
//! }
//! ```
//!
//! # Patterns
//!
//! A pattern in bitpack is a string containing a binary literal with some bits
//! replaced with variables and "don't cares." Starting at the most basic end,
//! you have plain literals: `"0001"` is a 4-bit literal number `1`. Just like
//! in Rust's integer literals, you can use underscores as you like, which is
//! particularly helpful when dealing with longer bitstrings. For example,
//! `"00000000_00000000"` is a 16-bit number zero. You can also use spaces for
//! rhythm as well, so that could be equivalently written as
//! `"0000_0000 0000_0000"`.
//!
//! Only working with literals, you don't get any benefit over plain binary
//! integer literals, so that's where don't-cares come in. You can use a `?`
//! to mark a bit as being allowed to be either 0 or 1. The pattern `"0??0"`
//! will match any of `"0000"`, `"0010"`, `"0100"`, and `"0110"`.
//!
//! As the final step, bitmatch also allows you to include variables in your
//! patterns, and they will be extracted as separate variables in your
//! program. Each bit that uses the same letter will be tightly packed into
//! the corresponding output value. As an example, with the pattern `"aabb"`,
//! the variable `a` has its bit `0` correspond to the pattern's bit `2`.
//! If you have a noncontiguous pattern like `"aabbaabb"`, then you get two
//! variables `a` and `b` that have 4 bits each, all compressed down to fit
//! at their least-significant bit.
//!
//! # Bitmatch with let
//!
//! When using `#[bitmatch]` with a `let`, you must use a pattern which can
//! always match, which means no literal bits. The following doesn't compile
//! because it might fail to match in some cases, like if `n = 0xF`.
//!
//! ```compile_fail
//! # use bitmatch::bitmatch;
//! # #[bitmatch]
//! # fn main() {
//! # let n = 42;
//! #[bitmatch]
//! let "00aa" = n;
//! # }
//! ```
//!
//! The following does compile, and binds portions of the integer to the
//! separate variables. Since they're 4 bits, they get one hex digit each:
//!
//! ```
//! # use bitmatch::bitmatch;
//! # #[bitmatch]
//! # fn main() {
//! let n = 0xCD;
//! #[bitmatch]
//! let "aaaabbbb" = n;
//! assert_eq!(a, 0xC);
//! assert_eq!(b, 0xD);
//! # }
//! ```
//!
//! When matching non-contiguous bits in the same variable, the spaces in
//! between the chunks are squished away.
//!
//! ```
//! # use bitmatch::bitmatch;
//! # #[bitmatch]
//! # fn main() {
//! let n = 0b0111_1001;
//! #[bitmatch]
//! let "aabbaabb" = n;
//! assert_eq!(a, 0b0110);
//! assert_eq!(b, 0b1101);
//! # }
//! ```
//!
//! # Bitmatch with match
//!
//! Working with a `match`, you can use any bitmatch pattern. It will check
//! exhaustivity for your pattern and fail if you don't handle all cases.
//!
//! ```compile_fail
//! # use bitmatch::bitmatch;
//! # #[bitmatch]
//! # fn main() {
//! # let x = 42;
//! #[bitmatch]
//! match x {
//!    "1???" => 0,
//!    "00??" => 1,
//! /* "01??" => 1, */ // this case is necessary though :)
//! }
//! # }
//! ```
//!
//! In addition to the main patterns, you can also include pattern guards
//! that include the bit-variables.
//!
//! ```
//! # use bitmatch::bitmatch;
//! # #[bitmatch]
//! # fn main() {
//! # let x = 42;
//! # let _: Option<_> =
//! #[bitmatch]
//! match x {
//!    "aabb" if a == b => None,
//!    "aabb" => Some((a, b)),
//! }
//! # ;
//! # }
//! ```
//!
//! # Bitpack
//!
//! When calling `bitpack!`, you must provide a *fully determined* pattern.
//! That means you can't use any `?` bits, only literal bits and variables.
//!
//! `bitpack!` does the opposite job of all the matching, letting you combine
//! single letter variables back into a packed integer. These were designed
//! with field-packing and mixing in mind. All of the bits of a given variable
//! are included in the same order they were in the variable, starting from
//! the least significant bit.
//!
//! ```
//! # use bitmatch::bitmatch;
//! # #[bitmatch]
//! # fn main() {
//! let a = 0xCD;
//! let b = 0xEF;
//! let y = bitpack!("0011aaaabbbbaaaa");
//! assert_eq!(y, 0x3CFD);
//! # }
//! ```

extern crate proc_macro;
use boolean_expression::{Cube, CubeList, CubeVar};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote,
    spanned::Spanned,
    visit::Visit,
    visit_mut::{self, VisitMut},
    Attribute, Expr, ExprLit, ExprMacro, ExprMatch, Ident, Lit, LitInt, LitStr, Local, Pat, Path,
    Token, Type,
};

#[cfg(test)]
mod test;

fn rewrite_let(i: &mut Local) {
    i.attrs.retain(|attr| !path_eq(&attr.path, "bitmatch"));
    let pat = match pat_str(&i.pat) {
        Some(pat) => pat,
        None => panic!("Requires strings as patterns"),
    };
    if !irrefutable_pat(&pat) {
        panic!("Only irrefutable bit-patterns are allowed, found: {}", pat);
    }
    let var_list = vars(&pat);
    let items = var_list.iter().map(|v| {
        let ident = Ident::new(&format!("{}", v), i.pat.span());
        quote!(#ident)
    });
    i.pat = parse_quote! { ( #(#items),* ) };
    let ident = Ident::new("orig", i.pat.span());
    let (eq, init) = match i.init.clone() {
        Some(i) => i,
        None => panic!("#[bitmatch] let can only be used with an initializer"),
    };
    let elements = var_list.iter().map(|&v| {
        let mask = mask_for(v, &pat);
        extract_with_mask(&mask, &parse_quote! { #ident })
    });

    i.init = Some((
        eq,
        parse_quote! {{
            let #ident = #init;
            ( #( #elements ),* )
        }},
    ));
}

fn rewrite_match(i: &mut ExprMatch) {
    i.attrs.retain(|attr| !path_eq(&attr.path, "bitmatch"));
    let mut cases = Vec::with_capacity(i.arms.len());
    let mut cubelist = CubeList::new();
    for arm in &i.arms {
        match pat_str(&arm.pat) {
            Some(s) => {
                if arm.guard.is_none() {
                    cubelist = cubelist.merge(&CubeList::from_list(&[to_cube(&s)]));
                }
                cases.push(s);
            }
            None => {
                let pat = &arm.pat;
                panic!(
                    "#[bitmatch] match patterns must be string literals, but found {}",
                    quote! { #pat }
                )
            }
        }
    }
    if !true_cubelist(&cubelist) {
        panic!("Some cases not covered. Cases: {:?}", cases);
    }
    for (arm, case) in i.arms.iter_mut().zip(&cases) {
        let vars = vars(case);
        let ident = Ident::new("bits", arm.pat.span());
        let (if_, guard) = pattern_guard(&ident, case);
        arm.pat = parse_quote!(#ident);
        let used = if let Some((_, old_guard)) = &arm.guard {
            let used = used_simple_vars(&old_guard);
            let extra_guard = wrap_with_bindings(&ident, &case, &vars, &*old_guard, &vars);
            arm.guard = Some((if_, parse_quote! { #guard && #extra_guard }));
            used
        } else {
            arm.guard = Some((if_, guard));
            Vec::new()
        };
        if !vars.is_empty() {
            arm.body = Box::new(wrap_with_bindings(&ident, &case, &vars, &arm.body, &used));
        }
    }
    i.arms
        .push(parse_quote! { _ => unreachable!("#[bitmatch] fallback branch") });
}

fn rewrite_macro(i: &mut Expr) {
    let span = i.span();
    let template = if let Expr::Macro(expr) = i {
        match expr.mac.parse_body::<LitStr>() {
            Ok(s) => s.value(),
            _ => panic!("The bitpack!() takes a single string literal"),
        }
    } else {
        unreachable!()
    };
    let vars = vars(&template);
    let clauses = vars.iter().map(|&var| {
        let ident = Ident::new(&format!("{}", var), span);
        let mask = mask_for(var, &template);
        insert_with_mask(&mask, &parse_quote!(#ident))
    });
    *i = or_all(clauses);
    let value = pat_value(&template);
    if value.chars().any(|c| c == '1') {
        let value = make_int_bits(&value);
        *i = parse_quote!(#value | #i);
    }
}

struct BitmatchVisitor;
impl VisitMut for BitmatchVisitor {
    fn visit_local_mut(&mut self, i: &mut Local) {
        if has_bitmatch_attr(&i.attrs) {
            rewrite_let(i);
        }
        visit_mut::visit_local_mut(self, i);
    }

    fn visit_expr_match_mut(&mut self, i: &mut ExprMatch) {
        if has_bitmatch_attr(&i.attrs) {
            rewrite_match(i);
        }
        visit_mut::visit_expr_match_mut(self, i);
    }

    fn visit_expr_macro_mut(&mut self, i: &mut ExprMacro) {
        visit_mut::visit_expr_macro_mut(self, i);
    }

    fn visit_expr_mut(&mut self, i: &mut Expr) {
        if let Expr::Macro(mac) = i {
            if path_eq(&mac.mac.path, "bitpack") {
                rewrite_macro(i);
            }
        }
        visit_mut::visit_expr_mut(self, i);
    }
}

fn has_bitmatch_attr(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| path_eq(&attr.path, "bitmatch"))
}

fn path_eq(path: &Path, name: &str) -> bool {
    match path.get_ident() {
        Some(ident) => ident == &Ident::new(name, ident.span()),
        None => false,
    }
}

fn to_cube(lit: &str) -> Cube {
    let lit = lit.trim();
    let len = lit.chars().filter(|&c| c != '_' && c != ' ').count();
    let mut cube = Cube::true_cube(len);
    let mut i = 0;
    for c in lit.chars().rev() {
        match c {
            '_' | ' ' => continue,
            '0' => cube = cube.with_var(i, CubeVar::False),
            '1' => cube = cube.with_var(i, CubeVar::True),
            _ => (),
        }
        i += 1;
    }
    cube
}

fn true_cubelist(cubelist: &CubeList) -> bool {
    cubelist.cubes().count() == 1
        && cubelist
            .cubes()
            .next()
            .unwrap()
            .vars()
            .all(|v| v == &CubeVar::DontCare)
}

fn pat_str(p: &Pat) -> Option<String> {
    match p {
        Pat::Lit(pl) => match &*pl.expr {
            Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) => Some(s.value()),
            _ => None,
        },
        _ => None,
    }
}

fn pat_mask(p: &str) -> String {
    p.chars()
        .flat_map(|c| match c {
            '0' | '1' => Some('1'),
            '_' | ' ' => None,
            _ => Some('0'),
        })
        .collect()
}

fn pat_value(p: &str) -> String {
    p.chars()
        .flat_map(|c| match c {
            '0' | '1' => Some(c),
            '_' | ' ' => None,
            _ => Some('0'),
        })
        .collect()
}

fn make_int_bits(bits: &str) -> Expr {
    let lit = LitInt::new(&format!("0b{}", bits), Span::call_site());
    parse_quote!(#lit)
}

fn pattern_guard(item: &Ident, case: &str) -> (Token![if], Box<Expr>) {
    let mask = make_int_bits(&pat_mask(case));
    let value = make_int_bits(&pat_value(case));
    (
        Token![if]([item.span()]),
        Box::new(parse_quote!(#item & #mask == #value)),
    )
}

fn vars(p: &str) -> Vec<char> {
    let mut items: Vec<_> = p
        .chars()
        .filter(|&c| c != '0' && c != '1' && c != '_' && c != ' ' && c != '?')
        .collect();
    items.sort();
    items.dedup();
    items
}

fn mask_for(v: char, p: &str) -> String {
    p.chars()
        .flat_map(|c| match c {
            '_' | ' ' => None,
            c if c == v => Some('1'),
            _ => Some('0'),
        })
        .collect()
}

fn irrefutable_pat(p: &str) -> bool {
    p.chars().all(|p| p != '0' && p != '1')
}

fn mask_segments(m: &str) -> Vec<(usize, usize)> {
    let mut result = Vec::new();
    let mut start = 0;
    let mut len = 0;
    for (i, c) in m.chars().rev().enumerate() {
        if len == 0 && c == '1' {
            start = i;
        }
        if c == '1' {
            len += 1;
        }
        if len != 0 && c == '0' {
            result.push((start, len));
            len = 0;
        }
    }
    if len != 0 {
        result.push((start, len));
    }
    result
}

fn int_of_width(m: &str) -> Type {
    if m.len() > 128 {
        panic!("Unsupported: bit pattern {:?} wider than 128 bits", m)
    } else if m.len() > 64 {
        parse_quote!(u128)
    } else if m.len() > 32 {
        parse_quote!(u64)
    } else if m.len() > 16 {
        parse_quote!(u32)
    } else if m.len() > 8 {
        parse_quote!(u16)
    } else {
        parse_quote!(u8)
    }
}

fn extract_with_mask(m: &str, expr: &Expr) -> Expr {
    let mut cumulative = 0;
    let segs = mask_segments(m);
    let clauses = segs.iter().map(|(start, count)| {
        let amt = start - cumulative;
        let mask = LitInt::new(&format!("0x{:X}", ((1 << count) - 1) << start), expr.span());
        cumulative += count;
        quote!((#expr & #mask) >> #amt)
    });
    or_all(clauses)
}

fn insert_with_mask(m: &str, expr: &Expr) -> Expr {
    let ty = int_of_width(m);
    let mut cumulative = 0;
    let segs = mask_segments(m);
    let clauses = segs.iter().map(|(start, count)| {
        let amt = start - cumulative;
        let mask = LitInt::new(
            &format!("0x{:X}", ((1 << count) - 1) << cumulative),
            expr.span(),
        );
        cumulative += count;
        quote!((#expr as #ty & #mask) << #amt)
    });
    or_all(clauses)
}

fn or_all(mut clauses: impl Iterator<Item = impl ToTokens>) -> Expr {
    if let Some(first) = clauses.next() {
        parse_quote!( #first #(| #clauses)* )
    } else {
        parse_quote!(0)
    }
}

fn wrap_with_bindings(
    ident: &Ident,
    case: &str,
    vars: &[char],
    expr: &Expr,
    allow_unused: &[char],
) -> Expr {
    let binds = vars.iter().map(|&var| {
        let bind = Ident::new(&format!("{}", var), expr.span());
        let mask = mask_for(var, case);
        let extracted = extract_with_mask(&mask, &parse_quote! { #ident });
        if allow_unused.iter().find(|&&v| v == var).is_some() {
            quote! { #[allow(unused)] let #bind = #extracted; }
        } else {
            quote! { let #bind = #extracted; }
        }
    });
    parse_quote! {{
        #( #binds )*
        #expr
    }}
}

fn used_simple_vars(e: &Expr) -> Vec<char> {
    let mut visitor = SimpleVarsVisitor(Vec::new());
    visitor.visit_expr(e);
    visitor.0.sort();
    visitor.0.dedup();
    visitor.0
}

struct SimpleVarsVisitor(Vec<char>);
impl<'ast> Visit<'ast> for SimpleVarsVisitor {
    fn visit_ident(&mut self, i: &'ast Ident) {
        let name = format!("{}", quote! { #i });
        if name.chars().count() == 1 {
            self.0.push(name.chars().next().unwrap());
        }
    }
}

/// Marks a function as able to use the bitmatch items.
///
/// See the [module-level docs](index.html) for documentation and usage examples.
#[proc_macro_attribute]
pub fn bitmatch(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as syn::Item);
    BitmatchVisitor.visit_item_mut(&mut input);
    TokenStream::from(quote! { #input })
}
