fn main() {
    let _ = std::fs::create_dir_all("../../web/dist");
    let resources_dir = std::path::Path::new("resources");
    let _ = std::fs::create_dir_all(resources_dir);
    let keep_file = resources_dir.join(".keep");
    if !keep_file.exists() {
        let _ = std::fs::write(&keep_file, b"");
    }
    tauri_build::build()
}
