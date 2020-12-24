extern crate proc_macro;

mod symbol;

use proc_macro::{Span, TokenStream};
use quote::{quote, ToTokens};
use symbol::*;
use syn::{parse_macro_input, Data, DeriveInput, Error, Fields, Meta, MetaNameValue, NestedMeta};

#[proc_macro_derive(ToGMap, attributes(gremlin))]
pub fn derive_to_gmap(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);

    match parse_to_gmap_input(input) {
        Ok(ts) => ts,
        Err(e) => e.to_compile_error().into(),
    }
}

fn parse_to_gmap_input(input: DeriveInput) -> Result<TokenStream, Error> {
    let label = get_label_attr(&input)?;
    // Get the name of our structure
    let ident = input.ident;
    // Get the constraints of our structure
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // Verify that we have a structure
    if let Data::Struct(s) = input.data {
        let name = if let Some(v) = label {
            let v = v.lit;
            quote! {
                #v
            }
        } else {
            quote! {
                stringify!(#ident)
            }
        };
        let mut field_ops = vec![];
        if let Fields::Named(fields) = s.fields {
            for f in fields.named {
                if has_field_attr(&f, SKIP)? {
                    continue;
                }
                if let Some(ref field_ident) = f.ident {
                    let property_string = quote! {
                        concat!(
                            "{}.property('",
                            stringify!(#field_ident),
                            "', _",
                            stringify!(#field_ident),
                        )
                    };
                    if is_optional_type(&f)? {
                        field_ops.push(quote! {
                            if let Some(v) = self.#field_ident.as_ref() {
                                query = format!(#property_string, query);
                                properties.push((
                                    concat!("_", stringify!(#field_ident)),
                                    v
                                ));
                            }
                        });
                    } else {
                        field_ops.push(quote! {
                            query = format!(#property_string, query);
                            properties.push((
                                concat!("_", stringify!(#field_ident)),
                                &self.#field_ident
                            ));
                        });
                    }
                }
            }
        }

        let ts = quote! {
            impl #impl_generics ToGMap for #ident #ty_generics #where_clause {
                fn to_gmap(&self) -> (String, Vec<(&str, &dyn ToGValue)>) {
                    let mut query = "g.addV(_label)".to_string();
                    let mut properties: Vec<(&str, &dyn ToGValue)> = vec![("_label", &#name)];
                    #(#field_ops)*
                    (
                        query,
                        properties
                    )
                }

                fn label() -> &'static str {
                    #name
                }
            }
        };
        Ok(ts.into())
    } else {
        Err(syn::Error::new(Span::call_site().into(), "Not a struct"))
    }
}

fn get_label_attr(item: &syn::DeriveInput) -> Result<Option<MetaNameValue>, Error> {
    for attr in item.attrs.iter() {
        for attr in get_meta_items(attr)? {
            match attr {
                NestedMeta::Meta(Meta::NameValue(m)) if m.path == LABEL => return Ok(Some(m)),
                _ => {
                    return Err(Error::new(
                        Span::call_site().into(),
                        r#"expected #[gremlin(label = "...")]"#,
                    ))
                }
            }
        }
    }
    Ok(None)
}

fn has_field_attr(item: &syn::Field, to_find: Symbol) -> Result<bool, syn::Error> {
    for attr in item.attrs.iter() {
        for attr in get_meta_items(attr)? {
            match attr {
                NestedMeta::Meta(Meta::Path(word)) if word == to_find => return Ok(true),
                _ => {
                    return Err(syn::Error::new(
                        Span::call_site().into(),
                        r#"expected #[gremlin(skip)]"#,
                    ))
                }
            }
        }
    }
    Ok(false)
}

fn get_meta_items(attr: &syn::Attribute) -> Result<Vec<NestedMeta>, Error> {
    if attr.path != GREMLIN {
        return Ok(Vec::new());
    }

    match attr.parse_meta() {
        Ok(Meta::List(meta)) => Ok(meta.nested.into_iter().collect()),
        Ok(other) => Err(syn::Error::new_spanned(
            other.into_token_stream(),
            "expected #[gremlin(...)]",
        )),
        Err(err) => Err(err),
    }
}

fn is_optional_type(item: &syn::Field) -> Result<bool, syn::Error> {
    let r = if let syn::Type::Path(ref tp) = item.ty {
        tp.path
            .segments
            .iter()
            .find(|p| p.ident == OPTION)
            .is_some()
    } else {
        false
    };
    Ok(r)
}
