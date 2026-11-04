---
name: pdf
description: "PDF Extraction, Form Processing & Report Generation (`pdf`). Parse tables, OCR scanned documents, fill/merge/split PDFs, and render print-ready technical reports and signed agreements."
source: hanzoai/skills
---

# PDF Analysis & Generation (`pdf`)

Extract structured data from complex PDFs and generate clean, print-ready `.pdf` documents, invoices, and compliance packages.

## Core Rules
1. **Layout-Aware Extraction**: Preserve table column alignment, multi-column reading order, footnotes, and page numbers when extracting text or financial tables from PDFs.
2. **Citation by Page & Section**: Every extracted figure, clause, or requirement must cite its exact `[Page N, Section X]` source location.
3. **Print-Ready Output**: When generating PDFs, enforce proper page-break rules (`break-inside: avoid` on tables/cards/code blocks), embedded fonts, headers/footers with page numbers, and crisp vector diagrams.
