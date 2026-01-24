/// Format a u64 with locale-aware digit separators (e.g. "12345" -> "12,345"
/// for en-US).
///
/// Uses system locale if available, falls back to en-US (comma separator).
pub fn format_with_separators(n: u64) -> String {
    let separator = get_thousands_separator();
    format_with_separator(n, separator)
}

/// Get the thousands separator for the current locale.
/// Falls back to comma for en-US style.
fn get_thousands_separator() -> char {
    // Try to detect locale-appropriate separator
    // For now, we use comma as default (en-US style)
    // This could be extended to check sys_locale for regional preferences
    ','
}

/// Format a number with the given thousands separator.
fn format_with_separator(n: u64, sep: char) -> String {
    let s = n.to_string();
    if s.len() <= 3 {
        return s;
    }

    let chars: Vec<char> = s.chars().collect();
    let mut result = String::with_capacity(s.len() + (s.len() - 1) / 3);
    for (i, &c) in chars.iter().enumerate() {
        if i > 0 && (chars.len() - i) % 3 == 0 {
            result.push(sep);
        }
        result.push(c);
    }
    result
}

fn format_si_suffix_internal(n: u64) -> String {
    if n < 1000 {
        return n.to_string();
    }

    // Format `n / scale` with the requested number of fractional digits.
    let format_scaled = |n: u64, scale: u64, frac_digits: u32| -> String {
        let value = n as f64 / scale as f64;
        let scaled: u64 = (value * 10f64.powi(frac_digits as i32)).round() as u64;
        let integer_part = scaled / 10u64.pow(frac_digits);
        let decimal_part = scaled % 10u64.pow(frac_digits);

        if frac_digits == 0 {
            format_with_separators(integer_part)
        } else {
            format!("{}.{:0>width$}", integer_part, decimal_part, width = frac_digits as usize)
        }
    };

    const UNITS: [(u64, &str); 3] = [(1_000, "K"), (1_000_000, "M"), (1_000_000_000, "G")];
    let f = n as f64;
    for &(scale, suffix) in &UNITS {
        if (100.0 * f / scale as f64).round() < 1000.0 {
            return format!("{}{}", format_scaled(n, scale, 2), suffix);
        } else if (10.0 * f / scale as f64).round() < 1000.0 {
            return format!("{}{}", format_scaled(n, scale, 1), suffix);
        } else if (f / scale as f64).round() < 1000.0 {
            return format!("{}{}", format_scaled(n, scale, 0), suffix);
        }
    }

    // Above 1000G, keep whole‑G precision.
    format!(
        "{}G",
        format_with_separators(((n as f64) / 1e9).round() as u64)
    )
}

/// Format token counts to 3 significant figures, using base-10 SI suffixes.
///
/// Examples (en-US):
///   - 999 -> "999"
///   - 1200 -> "1.20K"
///   - 123456789 -> "123M"
pub fn format_si_suffix(n: u64) -> String {
    format_si_suffix_internal(n)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn thousands_separator() {
        assert_eq!(format_with_separators(0), "0");
        assert_eq!(format_with_separators(1), "1");
        assert_eq!(format_with_separators(12), "12");
        assert_eq!(format_with_separators(123), "123");
        assert_eq!(format_with_separators(1234), "1,234");
        assert_eq!(format_with_separators(12345), "12,345");
        assert_eq!(format_with_separators(123456), "123,456");
        assert_eq!(format_with_separators(1234567), "1,234,567");
    }

    #[test]
    fn kmg() {
        assert_eq!(format_si_suffix(0), "0");
        assert_eq!(format_si_suffix(999), "999");
        assert_eq!(format_si_suffix(1_000), "1.00K");
        assert_eq!(format_si_suffix(1_200), "1.20K");
        assert_eq!(format_si_suffix(10_000), "10.0K");
        assert_eq!(format_si_suffix(100_000), "100K");
        assert_eq!(format_si_suffix(999_500), "1.00M");
        assert_eq!(format_si_suffix(1_000_000), "1.00M");
        assert_eq!(format_si_suffix(1_234_000), "1.23M");
        assert_eq!(format_si_suffix(12_345_678), "12.3M");
        assert_eq!(format_si_suffix(999_950_000), "1.00G");
        assert_eq!(format_si_suffix(1_000_000_000), "1.00G");
        assert_eq!(format_si_suffix(1_234_000_000), "1.23G");
        // Above 1000G we keep whole‑G precision (no higher unit supported here).
        assert_eq!(format_si_suffix(1_234_000_000_000), "1,234G");
    }
}
