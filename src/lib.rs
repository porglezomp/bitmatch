use proc_macro::TokenStream;
use quote::quote;
use syn::{spanned::Spanned, parse_macro_input, Expr, ExprLit, Lit, LitInt, Path, Local, Attribute, ExprMatch, ExprMacro, Ident, visit_mut::{self, VisitMut}};

fn rewrite_let(i: &mut Local) {
    i.attrs.retain(|attr| !path_eq(&attr.path, "bitmatch"));
    // println!("Rewrote let: {:#?}", i);
}

fn rewrite_match(i: &mut ExprMatch) {
    i.attrs.retain(|attr| !path_eq(&attr.path, "bitmatch"));
    for arm in &i.arms {
        println!("{:#?}", arm);
    }
    // println!("Rewrote match: {:#?}", i);
}

fn rewrite_macro(i: &mut Expr) {
    *i = Expr::Lit(ExprLit {
        attrs: Vec::new(),
        lit: Lit::Int(LitInt::new("0", i.span())),
    });
    // println!("Rewrote macro: {:#?}", i);
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

#[proc_macro_attribute]
pub fn bitmatch(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as syn::Item);
    BitmatchVisitor.visit_item_mut(&mut input);
    TokenStream::from(quote! { #input })
}
