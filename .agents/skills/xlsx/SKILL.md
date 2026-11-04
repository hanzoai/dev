---
name: xlsx
description: "Excel & Spreadsheet Financial Modeling (`xlsx`). Create, audit, and transform `.xlsx` workbooks with dynamic formulas, multi-sheet financial statements, pivot tables, sensitivity matrices, and zero hardcoded magic numbers."
source: hanzoai/skills
---

# Excel & Financial Modeling (`xlsx`)

Create, inspect, and modify production-grade `.xlsx` spreadsheets, financial models, cohort tables, and operational trackers.

## Core Rules
1. **Formulas Over Hardcoded Values**: Always write live Excel formulas (`SUM`, `INDEX/MATCH`, `XLOOKUP`, `SUMIFS`, `XNPV`, `XIRR`) rather than pre-computed static numbers so downstream cells recalculate automatically.
2. **Assumptions Isolation**: Place all editable inputs and growth/pricing assumptions in a dedicated `Assumptions` or `Inputs` block with distinct styling, referenced cleanly across tabs.
3. **Audit Checks**: Include balance-sheet/reconciliation check rows (`=IF(ABS(A1-B1)<0.01, "OK", "ERROR")`) and never leave `#REF!`, `#DIV/0!`, or `#NAME?` errors in generated sheets.
4. **Formatting & Readability**: Format currencies (`$#,##0`), percentages (`0.0%`), dates (`YYYY-MM-DD`), freeze header panes, and set explicit column widths so no values render as `###`.
