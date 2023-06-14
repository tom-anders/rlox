use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, DataEnum, DeriveInput, Field, Fields,
};

fn impl_num_bytes(ident: &Ident, data_enum: &DataEnum) -> proc_macro2::TokenStream {
    // This computes the number of bytes each enum variant takes up in memory.
    // The "1+" is to account for the enum discriminant
    let opcode_num_bytes = data_enum.variants.iter().map(|variant| {
        let fields = match variant.fields {
            Fields::Named(ref fields) => fields.named.clone().into_iter(),
            Fields::Unnamed(ref fields) => fields.unnamed.clone().into_iter(),
            Fields::Unit => Punctuated::<Field, Comma>::new().into_iter(),
        };
        let types = fields.map(|f| f.ty);
        let sizes = types.map(|ty| quote! { std::mem::size_of::<#ty>() });
        quote! {
            1 #(+ #sizes)*
        }
    });

    // Match arms for #ident::num_bytes()
    let num_byte_arms =
        opcode_num_bytes.clone().zip(data_enum.variants.clone()).map(|(num_bytes, variant)| {
            let opcode = variant.ident;
            let suffix = match variant.fields {
                Fields::Named(_) => {
                    quote! { { .. } }
                }
                Fields::Unnamed(_) => {
                    quote! { (..) }
                }
                Fields::Unit => {
                    quote! {}
                }
            };
            quote! {
                #ident::#opcode #suffix => #num_bytes,
            }
        });

    // Match arms for OpCode::num_bytes()
    let op_code_num_bytes_arms =
        opcode_num_bytes.clone().zip(data_enum.variants.clone()).map(|(num_bytes, variant)| {
            let ident = variant.ident;
            quote! {
                OpCode::#ident => #num_bytes,
            }
        });

    quote! {
        impl #ident {
            pub fn num_bytes(&self) -> usize {
                match self {
                    #(#num_byte_arms)*
                }
            }
        }

        impl OpCode {
            pub fn num_bytes(&self) -> usize {
                match self {
                    #(#op_code_num_bytes_arms)*
                }
            }
        }
    }
}

fn impl_from_bytes(ident: &Ident, data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let from_bytes_arms = data_enum.variants.iter().map(|variant| match variant.fields {
        syn::Fields::Named(ref fields) => {
            let fields = fields.named.iter().enumerate().map(|(i, f)| {
                let ident = &f.ident;
                let index = i + 1;
                quote! {
                    #ident: bytes[#index],
                }
            });
            let opcode = variant.ident.clone();
            quote! {
                OpCode::#opcode => #ident::#opcode { #(#fields),* }
            }
        }
        syn::Fields::Unnamed(ref fields) => {
            let fields = fields.unnamed.iter().enumerate().map(|(i, _)| {
                let index = i + 1;
                quote! {
                    bytes[#index],
                }
            });
            let opcode = variant.ident.clone();
            quote! {
                OpCode::#opcode => #ident::#opcode(#(#fields),*)
            }
        }
        syn::Fields::Unit => {
            quote! {
                OpCode::#variant => #ident::#variant
            }
        }
    });

    quote! {
        impl #ident {
            pub fn from_bytes(bytes: &[u8]) -> Self {
                let op = OpCode::from(bytes[0]);
                match op {
                    #(#from_bytes_arms),*
                }
            }
        }
    }
}

fn impl_opcode(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let opcodes = data_enum.variants.iter().map(|variant| variant.ident.clone());

    let from_arms = opcodes.clone().enumerate().map(|(i, variant)| {
        let i = i as u8;
        quote! {
            #i => OpCode::#variant,
        }
    });

    let display_arms = opcodes.clone().map(|variant| {
        quote! {
            OpCode::#variant => write!(f, "{}", stringify!(#variant)),
        }
    });

    quote! {
        #[repr(u8)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum OpCode {
            #(#opcodes),*
        }

        impl From<u8> for OpCode {
            fn from(op: u8) -> Self {
                match op {
                    #(
                        #from_arms
                    )*
                    _ => panic!("Invalid opcode: {}", op),
                }
            }
        }

        impl std::fmt::Display for OpCode {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #(
                        #display_arms
                    )*
                }
            }
        }
    }
}

/// implement From<#indent> for OpCode
fn impl_opcode_from_instr(ident: &Ident, data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let from_arms = data_enum.variants.iter().map(|variant| {
        let opcode = variant.ident.clone();
        quote! {
            #ident::#opcode { .. } => OpCode::#opcode,
        }
    });

    quote! {
        impl From<#ident> for OpCode {
            fn from(instr: #ident) -> Self {
                match instr {
                    #(
                        #from_arms
                    )*
                }
            }
        }
    }
}

#[proc_macro_derive(Instruction)]
pub fn derive_instruction(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input as DeriveInput);

    let data_enum = match data {
        syn::Data::Enum(data_enum) => data_enum,
        _ => panic!("Instruction can only be derived for enums"),
    };

    let impl_opcode = impl_opcode(&data_enum);
    let impl_len = impl_num_bytes(&ident, &data_enum);
    let impl_from_bytes = impl_from_bytes(&ident, &data_enum);
    let impl_opcode_from_instr = impl_opcode_from_instr(&ident, &data_enum);

    quote! {
        #impl_len
        #impl_opcode
        #impl_from_bytes
        #impl_opcode_from_instr
    }
    .into()
}
