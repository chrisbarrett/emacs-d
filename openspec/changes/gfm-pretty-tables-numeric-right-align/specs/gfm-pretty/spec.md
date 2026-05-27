## ADDED Requirements

### Requirement: Numeric column auto right-alignment

The tables decorator SHALL render a column right-aligned when every
body-row cell in that column is either numeric or empty after
trimming, AND at least one body-row cell in that column is numeric.
A cell is numeric iff its trimmed source text matches the form
`±?d+(.d*)?([eE]±?d+)?` (optional sign, digits, optional decimal
point and digits, optional scientific exponent). Cells with
thousands separators, percent signs, currency sigils, or units are
NOT considered numeric.

Right-alignment SHALL apply uniformly to the header cell, every
body cell, and every visual line of a wrapped cell in that column.
The header cell's own content is NOT considered when deciding the
column's alignment.

Columns that fail the test SHALL render left-aligned, preserving
the existing behaviour.

#### Scenario: All-numeric body column

- **GIVEN** a table with columns `script | role | LOC` and body
  cells in column 3 of `123`, `304`, `95`, `140`, `102`, `39`,
  `59`, `163`, `66`
- **THEN** column 3 renders right-aligned in every body row
- **AND** the header cell `LOC` renders right-aligned in column 3

#### Scenario: Sparse numeric body column

- **GIVEN** a body column with cells `12`, ``, `7`, ``, `99`
  (empty cells trimmed)
- **THEN** that column renders right-aligned

#### Scenario: Mixed numeric and non-numeric column

- **GIVEN** a body column with cells `12`, `abc`, `7`
- **THEN** that column renders left-aligned

#### Scenario: All-empty body column

- **GIVEN** a body column whose body cells are all empty after
  trimming
- **THEN** that column renders left-aligned

#### Scenario: Numeric with sign and decimal

- **GIVEN** a body column with cells `-3.14`, `+0.5`, `1e10`
- **THEN** that column renders right-aligned

#### Scenario: Non-numeric formats stay left-aligned

- **GIVEN** a body column with cells `1,234`, `5,678`
- **THEN** that column renders left-aligned (thousands separator
  is not part of the numeric form)

- **GIVEN** a body column with cells `42%`, `7%`
- **THEN** that column renders left-aligned

#### Scenario: Wrapped numeric cell

- **GIVEN** a right-aligned column whose cell wraps across two
  visual lines under a narrow window
- **THEN** every visual line of that cell pads on the leading
  edge, keeping the trailing digits flush to the column's right
  boundary

### Requirement: Cell segment bounds unchanged by alignment

Right-aligning a column SHALL NOT alter the character bounds of any
cell segment in the rendered row. The display overlay's packed
bounds-vec SHALL produce identical `(BEG . END)` pairs for a given
table whether the column is left- or right-aligned.

Internals-facing. This invariant protects cell-highlight,
navigation, and indirect-edit commit from regressing when alignment
changes.

#### Scenario: Bounds parity across alignments

- **GIVEN** the same parsed table rendered once with a column
  left-aligned and once right-aligned (with identical
  `col-widths`)
- **THEN** the per-cell `(BEG . END)` bounds for that column match
  byte-for-byte across the two renders
