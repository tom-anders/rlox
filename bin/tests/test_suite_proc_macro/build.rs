fn main() {
    println!("cargo:rerun-if-changed={}/../inputs", env!("CARGO_MANIFEST_DIR"));
}
