// Build script for hanzo-node
// Compiles protobuf definitions into Rust code

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Compile the protobuf file to OUT_DIR (standard location)
    tonic_build::configure()
        .build_server(true)
        .build_client(true)
        .type_attribute(".", "#[derive(serde::Serialize, serde::Deserialize)]")
        .compile_protos(&["proto/hanzo.node.v1.proto"], &["proto"])?;

    // Tell cargo to rerun if proto changes
    println!("cargo:rerun-if-changed=proto/hanzo.node.v1.proto");

    Ok(())
}
