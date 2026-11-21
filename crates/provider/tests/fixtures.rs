use dev_provider::fixture::{load_all, root};

#[test]
fn every_fixture_is_complete() {
    let all = load_all(&root()).unwrap_or_default();
    assert!(!all.is_empty(), "no fixtures under {}", root().display());
    let unpinned = all.iter().filter(|f| f.events.is_none()).count();
    assert_eq!(unpinned, 4, "manifest.json lists four unpinned event files");
    for f in &all {
        let id = format!("{}/{}", f.name, f.driver);
        let header = f
            .transcript
            .first()
            .and_then(|h| h.get("type"))
            .and_then(|t| t.as_str());
        assert_eq!(header, Some("transcript_start"), "{id}: header");
        assert!(f.transcript.len() > 1, "{id}: transcript has no entries");
        assert_eq!(f.input["driver"], f.driver.as_str(), "{id}: input driver");
        assert_eq!(f.input["fixture"], f.name.as_str(), "{id}: input fixture");
        if let Some(events) = &f.events {
            assert!(!events.is_empty(), "{id}: no events");
            assert!(
                events.iter().all(|e| e.get("type").is_some()),
                "{id}: untyped event"
            );
        }
    }
}
