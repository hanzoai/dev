// Test program to verify IIFE detection logic
fn main() {
    let test_cases = vec![
        ("(() => { return 42; })()", true, "arrow function IIFE"),
        ("(function() { return 42; })()", true, "function IIFE"),
        ("(async () => { return 42; })()", true, "async arrow IIFE"),
        ("const x = 42; x", false, "normal code"),
        ("return { test: 'direct' }", false, "direct return"),
    ];

    for (code, expected_iife, description) in test_cases {
        let is_iife = detect_iife(code);
        println!("{}: {} (expected: {})", description, is_iife, expected_iife);
        assert_eq!(is_iife, expected_iife, "Failed for: {}", description);
    }
    println!("All tests passed!");
}

fn detect_iife(code: &str) -> bool {
    let trimmed_code = code.trim();
    if trimmed_code.starts_with("(") || trimmed_code.starts_with("(async") || trimmed_code.starts_with("(function") {
        let mut paren_depth = 0;
        for ch in trimmed_code.chars() {
            match ch {
                '(' => paren_depth += 1,
                ')' => {
                    paren_depth -= 1;
                    if paren_depth == 0 {
                        return true;
                    }
                }
                _ => {}
            }
        }
    }
    false
}