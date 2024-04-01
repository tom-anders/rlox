use quote::{format_ident, quote};

#[proc_macro]
pub fn generate_tests(_: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let root_dir = format!("{}/..", env!("CARGO_MANIFEST_DIR"));
    let tests = walkdir::WalkDir::new(root_dir.clone())
        .into_iter()
        .flatten()
        .filter(|entry| entry.path().extension() == Some("lox".as_ref()))
        .map(|entry| {
            let test_file_path = entry.path().to_string_lossy();
            let relative_test_path = entry.path().strip_prefix(&root_dir).unwrap().to_string_lossy();
            let test_ident = format_ident!("test_{}", relative_test_path.replace('/', "_").replace(".lox", ""));
            quote! {
                #[test]
                fn #test_ident() {
                    lox_expect(std::fs::read_to_string(#test_file_path).unwrap().as_str());
                }
            }
        });

    quote! {
        #[ctor::ctor]
        fn init() {
            env_logger::init();
        }

        #(#tests)*
    }.into()
}
