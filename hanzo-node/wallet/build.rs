//! Build script for compiling protobuf definitions

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("cargo:rerun-if-changed=proto/wallet.proto");

    tonic_build::configure()
        .build_server(true)
        .build_client(true)
        .compile_protos(&["proto/wallet.proto"], &["proto"])?;

    Ok(())
}
