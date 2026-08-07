//! What a diff touched, whether any of it renders, and where it renders.
//!
//! Everything here is a decision over strings and paths — no clock, no network,
//! no filesystem. That is the point of the split: the effectful half next door
//! has nothing left to decide, and every rule below can be read straight out of
//! a test instead of out of a running browser.

use std::path::Path;
use std::path::PathBuf;

/// Extensions whose content IS the rendered surface.
///
/// Deliberately short. A picture costs a browser launch and, more expensively, a
/// reviewer's attention — so the predicate only fires on extensions that cannot
/// mean anything except markup or style. `.ts` and `.js` are absent on purpose:
/// most of them are logic, and Next's `app/api/route.ts` is a server handler
/// sitting in the middle of the UI tree. A change that genuinely moves pixels
/// nearly always arrives with a partner carrying one of these.
const RENDERED: [&str; 9] = [
    "astro", "css", "html", "jsx", "less", "scss", "svelte", "tsx", "vue",
];

/// Directories whose contents were downloaded or generated. A `.css` under any
/// of them is output, not authorship, and photographing it tells a reviewer
/// nothing about what the agent did.
const DERIVED: [&str; 8] = [
    ".next",
    ".nuxt",
    ".svelte-kit",
    "build",
    "dist",
    "node_modules",
    "out",
    "target",
];

/// Directory names that route by nesting: the URL is the directory path and only
/// the router's own leaf file marks a directory as a route.
const NESTED: [&str; 2] = ["app", "routes"];

/// The directory that routes by filename: `pages/about.tsx` is `/about`.
const FLAT: &str = "pages";

/// Leaf filenames that name their directory rather than a segment of their own —
/// Next's `page.tsx`, SvelteKit's `+page.svelte`.
const LEAF: [&str; 2] = ["page", "+page"];

/// The paths a unified diff writes.
///
/// `+++ b/<path>` is the post-image name: the file as it exists *after* the
/// patch, which is the only version there is anything to photograph. A deletion
/// writes `+++ /dev/null` and names nothing, so it drops out here for free.
///
/// Paths arrive relative to the git root when there is one and absolute when
/// there is not, so callers must relativize before publishing anything.
pub(crate) fn touched(diff: &str) -> Vec<PathBuf> {
    diff.lines()
        .filter_map(|line| line.strip_prefix("+++ "))
        // Some unified-diff writers append a tab and a timestamp to the header.
        .map(|rest| rest.split('\t').next().unwrap_or(rest))
        .filter_map(|rest| rest.strip_prefix("b/"))
        .filter(|rest| !rest.is_empty())
        .map(PathBuf::from)
        .collect()
}

/// Whether this file, changed, is a reason to look at a page.
pub(crate) fn rendered(path: &Path) -> bool {
    if path
        .components()
        .any(|c| DERIVED.contains(&c.as_os_str().to_string_lossy().as_ref()))
    {
        return false;
    }
    match path.extension().and_then(|e| e.to_str()) {
        Some(ext) => RENDERED.contains(&ext.to_ascii_lowercase().as_str()),
        None => false,
    }
}

/// The URL path a source file renders at, for the file-routed frameworks where
/// the answer is written in the path itself — Next (both routers), SvelteKit,
/// Nuxt, Astro, Remix.
///
/// `None` means "not derivable", and the caller opens the site root. That is the
/// deliberate answer for a dynamic segment like `[slug]`: a reviewer can tell a
/// front page from the page they expected, but cannot tell an invented id from a
/// broken route.
pub(crate) fn route(path: &Path) -> Option<String> {
    let parts: Vec<String> = path
        .components()
        .map(|c| c.as_os_str().to_string_lossy().into_owned())
        .collect();
    let file = parts.last()?;
    let stem = Path::new(file).file_stem()?.to_str()?;

    // The router directory this file hangs off. Innermost wins, so a `pages/`
    // nested inside an `app/` routes the file that sits under it.
    let root = parts
        .iter()
        .rposition(|p| NESTED.contains(&p.as_str()) || p == FLAT)?;
    let mut segs: Vec<&str> = parts[root + 1..parts.len() - 1]
        .iter()
        .map(String::as_str)
        .collect();

    if parts[root] == FLAT {
        // `pages/` names a route by the file; `index` names the directory.
        if stem != "index" {
            segs.push(stem);
        }
    } else if !LEAF.contains(&stem) {
        // Under `app/` and `routes/` a directory is a route only because the
        // router's leaf file says so. Everything else living there — a stylesheet,
        // a component, a layout — has no URL of its own.
        return None;
    }

    let mut url = String::new();
    for seg in segs {
        // A Next route group organises files without appearing in the URL.
        if seg.starts_with('(') && seg.ends_with(')') {
            continue;
        }
        if dynamic(seg) {
            return None;
        }
        url.push('/');
        url.push_str(seg);
    }
    Some(if url.is_empty() { "/".to_string() } else { url })
}

/// A path segment standing in for a value the agent does not have: Next's
/// `[slug]`, SvelteKit's `[id]`, Nuxt's `_id`, Remix's `$id`.
fn dynamic(seg: &str) -> bool {
    seg.starts_with('[') || seg.starts_with('$') || seg.starts_with('_') || seg.starts_with(':')
}

/// A short, key-safe name for the object, derived from the route when there is
/// one and from the file otherwise. Lowercase alphanumerics and dashes only, so
/// nothing here can shape the storage key it lands in.
pub(crate) fn slug(route: &str, first: &Path) -> String {
    let raw = if route == "/" {
        first
            .file_stem()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_default()
    } else {
        route.to_string()
    };
    let mut out = String::new();
    for ch in raw.chars() {
        if ch.is_ascii_alphanumeric() {
            out.extend(ch.to_lowercase());
        } else if !out.ends_with('-') {
            out.push('-');
        }
        if out.len() >= 48 {
            break;
        }
    }
    let trimmed = out.trim_matches('-');
    if trimmed.is_empty() {
        "page".to_string()
    } else {
        trimmed.to_string()
    }
}

/// The port the repository serves its UI on, read out of `package.json`: the
/// repository's own dev script first, then the framework's default.
///
/// Reading, never running. The scripts in a `package.json` are arbitrary code
/// belonging to the tree the agent is editing; this function parses one for a
/// number and nothing else.
pub(crate) fn port(manifest: &str) -> Option<u16> {
    let doc: serde_json::Value = serde_json::from_str(manifest).ok()?;
    let script = ["dev", "start", "serve"]
        .iter()
        .find_map(|name| doc["scripts"][name].as_str());
    if let Some(explicit) = script.and_then(declared) {
        return Some(explicit);
    }
    let deps = ["dependencies", "devDependencies"];
    let has = |name: &str| deps.iter().any(|d| !doc[d][name].is_null());
    // Ordered most specific first: SvelteKit and Nuxt both also carry the
    // bundler they are built on, and the framework's port is the one that wins.
    let by_framework = [
        ("@sveltejs/kit", 5173u16),
        ("nuxt", 3000),
        ("astro", 4321),
        ("@angular/cli", 4200),
        ("gatsby", 8000),
        ("next", 3000),
        ("react-scripts", 3000),
        ("@remix-run/dev", 3000),
        ("vite", 5173),
        ("@vitejs/plugin-react", 5173),
    ];
    by_framework
        .iter()
        .find(|(name, _)| has(name))
        .map(|(_, port)| *port)
        .or(script.map(|_| 3000))
}

/// A port named on a dev script's own command line: `--port 4000`, `--port=4000`
/// or `-p 4000`.
fn declared(script: &str) -> Option<u16> {
    let mut words = script.split_whitespace().peekable();
    while let Some(word) = words.next() {
        let value = match word.split_once('=') {
            Some(("--port", v)) => Some(v),
            _ if word == "--port" || word == "-p" => words.peek().copied(),
            _ => None,
        };
        if let Some(parsed) = value.and_then(|v| v.parse::<u16>().ok()) {
            return Some(parsed);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn diff_for(paths: &[&str]) -> String {
        paths
            .iter()
            .map(|p| {
                format!("diff --git a/{p} b/{p}\nindex 000..111\n--- a/{p}\n+++ b/{p}\n@@ -1 +1 @@\n-a\n+b\n")
            })
            .collect()
    }

    fn ui_of(paths: &[&str]) -> Vec<PathBuf> {
        touched(&diff_for(paths))
            .into_iter()
            .filter(|p| rendered(p))
            .collect()
    }

    #[test]
    fn a_ui_file_is_a_reason_to_look() {
        for path in [
            "src/app/page.tsx",
            "src/Button.jsx",
            "components/Card.vue",
            "src/routes/+page.svelte",
            "public/index.html",
            "styles/main.css",
            "styles/main.scss",
            "styles/main.less",
            "src/pages/about.astro",
        ] {
            assert!(rendered(Path::new(path)), "{path} should count as UI");
        }
    }

    #[test]
    fn a_backend_file_is_not() {
        for path in [
            "apps/agents/sessions.go",
            "core/src/shot/mod.rs",
            "scripts/deploy.py",
            "migrations/001.sql",
            "Cargo.toml",
            "LLM.md",
            "src/lib/format.ts",
            "src/app/api/route.ts",
            "Makefile",
        ] {
            assert!(!rendered(Path::new(path)), "{path} should not count as UI");
        }
    }

    #[test]
    fn generated_output_is_not_authorship() {
        for path in [
            "node_modules/react/index.css",
            "dist/app.css",
            ".next/static/chunk.css",
            "build/index.html",
            "out/index.html",
            "target/doc/style.css",
        ] {
            assert!(!rendered(Path::new(path)), "{path} is derived, not UI");
        }
    }

    #[test]
    fn a_backend_only_diff_yields_nothing() {
        assert!(ui_of(&["apps/agents/sessions.go", "core/src/codex.rs"]).is_empty());
    }

    #[test]
    fn a_mixed_diff_yields_the_ui_half() {
        let ui = ui_of(&[
            "apps/agents/sessions.go",
            "web/src/app/settings/page.tsx",
            "core/src/codex.rs",
        ]);
        assert_eq!(ui, vec![PathBuf::from("web/src/app/settings/page.tsx")]);
    }

    #[test]
    fn a_deleted_file_names_nothing() {
        let diff = "diff --git a/src/Old.tsx b/src/Old.tsx\ndeleted file mode 100644\n--- a/src/Old.tsx\n+++ /dev/null\n@@ -1 +0,0 @@\n-gone\n";
        assert!(touched(diff).is_empty());
    }

    #[test]
    fn a_rename_names_the_destination() {
        let diff = "diff --git a/src/A.tsx b/src/B.tsx\nindex 000..111\n--- a/src/A.tsx\n+++ b/src/B.tsx\n@@ -1 +1 @@\n-a\n+b\n";
        assert_eq!(touched(diff), vec![PathBuf::from("src/B.tsx")]);
    }

    #[test]
    fn a_header_timestamp_is_not_part_of_the_path() {
        let diff = "--- a/src/A.tsx\t2026-01-01 00:00:00\n+++ b/src/A.tsx\t2026-01-01 00:00:01\n";
        assert_eq!(touched(diff), vec![PathBuf::from("src/A.tsx")]);
    }

    #[test]
    fn an_absolute_path_survives_intact() {
        let diff = "--- a//home/z/web/src/A.tsx\n+++ b//home/z/web/src/A.tsx\n";
        assert_eq!(touched(diff), vec![PathBuf::from("/home/z/web/src/A.tsx")]);
    }

    #[test]
    fn routes_are_read_off_the_path() {
        let cases = [
            ("src/app/page.tsx", Some("/")),
            ("src/app/settings/page.tsx", Some("/settings")),
            ("app/(marketing)/pricing/page.tsx", Some("/pricing")),
            ("src/routes/blog/+page.svelte", Some("/blog")),
            ("src/pages/about.tsx", Some("/about")),
            ("src/pages/index.tsx", Some("/")),
            ("src/pages/blog/index.vue", Some("/blog")),
            ("src/pages/docs/intro.astro", Some("/docs/intro")),
        ];
        for (path, want) in cases {
            assert_eq!(
                route(Path::new(path)).as_deref(),
                want,
                "route for {path}"
            );
        }
    }

    #[test]
    fn an_underivable_route_is_the_root() {
        for path in [
            // A value the agent does not have.
            "app/blog/[slug]/page.tsx",
            "src/routes/user/[id]/+page.svelte",
            "src/pages/posts/_id.vue",
            "app/$team/page.tsx",
            // Not a route at all: a component, a stylesheet, a layout.
            "src/components/Button.tsx",
            "src/app/globals.css",
            "src/app/layout.tsx",
            "src/routes/styles.css",
        ] {
            assert_eq!(route(Path::new(path)), None, "route for {path}");
        }
    }

    #[test]
    fn slugs_carry_no_punctuation() {
        assert_eq!(slug("/settings/team", Path::new("x.tsx")), "settings-team");
        assert_eq!(slug("/", Path::new("src/Button.tsx")), "button");
        assert_eq!(slug("/", Path::new("src/.hidden")), "hidden");
        assert_eq!(slug("/", Path::new("/")), "page");
        assert!(slug("/a/very/long/route/that/keeps/going/on/and/on/forever/and/ever", Path::new("x.tsx")).len() <= 48);
    }

    #[test]
    fn the_dev_script_names_its_own_port() {
        for (script, want) in [
            (r#"{"scripts":{"dev":"next dev --port 4000"}}"#, 4000u16),
            (r#"{"scripts":{"dev":"next dev --port=4100"}}"#, 4100),
            (r#"{"scripts":{"dev":"vite -p 4200"}}"#, 4200),
        ] {
            assert_eq!(port(script), Some(want), "{script}");
        }
    }

    #[test]
    fn the_framework_names_the_default() {
        for (manifest, want) in [
            (r#"{"scripts":{"dev":"next dev"},"dependencies":{"next":"15"}}"#, 3000u16),
            (r#"{"scripts":{"dev":"vite"},"devDependencies":{"vite":"5"}}"#, 5173),
            (r#"{"scripts":{"dev":"astro dev"},"dependencies":{"astro":"4"}}"#, 4321),
            (r#"{"scripts":{"dev":"ng serve"},"devDependencies":{"@angular/cli":"18"}}"#, 4200),
            (
                r#"{"scripts":{"dev":"vite dev"},"devDependencies":{"@sveltejs/kit":"2","vite":"5"}}"#,
                5173,
            ),
            (r#"{"scripts":{"dev":"nuxt dev"},"dependencies":{"nuxt":"3","vite":"5"}}"#, 3000),
        ] {
            assert_eq!(port(manifest), Some(want), "{manifest}");
        }
    }

    #[test]
    fn a_manifest_that_serves_nothing_names_no_port() {
        assert_eq!(port(r#"{"name":"lib","scripts":{"build":"tsc"}}"#), None);
        assert_eq!(port("not json"), None);
    }
}
