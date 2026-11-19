fn main() {
    let _ = std::fs::create_dir_all("../../web/dist");
    tauri_build::build()
}
