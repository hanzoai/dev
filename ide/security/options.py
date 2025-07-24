from ide.security.analyzer import SecurityAnalyzer
from ide.security.invariant.analyzer import InvariantAnalyzer

SecurityAnalyzers: dict[str, type[SecurityAnalyzer]] = {
    'invariant': InvariantAnalyzer,
}
