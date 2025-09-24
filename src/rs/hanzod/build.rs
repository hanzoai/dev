fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Only compile proto if the proto directory exists
    if std::path::Path::new("proto").exists() {
        tonic_build::configure()
            .build_server(true)
            .build_client(true)
            .compile_protos(
                &["proto/hanzo_chain.proto"],
                &["proto"],
            )?;
    } else {
        println!("cargo:warning=Proto directory not found, skipping proto compilation");
    }
    Ok(())
}