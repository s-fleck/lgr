# Changelog Style Guide

This project follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) 
principles and [Semantic Versioning](https://semver.org/spec/v2.0.0.html), 
adapted to the heading structure R itself expects for `NEWS.md` files.

This is an R package, so the changelog lives in `NEWS.md` at the package root 
(R's naming convention) rather than `CHANGELOG.md`. All rules below apply to `NEWS.md`.

## Why the heading structure differs from vanilla Keep a Changelog

R has its own reader for `NEWS.md` (`utils::news()` / `tools::news()`), and pkgdown's `build_news()` uses the same convention for the rendered "Changelog" site page. Per `?news`:

> the primary heading level giving the version number after a prefix followed by a space, and optionally followed by a space and a parenthesized ISO 8601 format date. Secondary headings are taken to indicate categories.

Concretely:

- The **top-level heading** (`#`) of each block must be the package name + version, e.g. `# lgr 0.13.0 (2026-06-21)`. Bracket-and-dash styles like `## [0.13.0] - 2026-06-21` are **not** recognized as version markers.
- **Second-level headings** (`##`) below it are the change-type categories (Added, Fixed, ...).
- Do **not** put any other top-level (`#`) heading in the file — e.g. a leading `# Changelog` title. R's/pkgdown's parser treats the *first* heading level it encounters as "this is a version marker," so an extra `#` above the real version headings breaks parsing of every version in the file (they get silently demoted to categories).
- There is no bracket-based `[Unreleased]` concept in R's convention. The established R/tidyverse/usethis equivalent is a heading with no version number: `# lgr (development version)`.

None of this is enforced by `R CMD check` (NEWS.md is optional and not a hard check), but a malformed file makes `news(package = "lgr")` return nothing useful and makes pkgdown's changelog page fail to find version headings — both real regressions for a CRAN package with a pkgdown site.

## Format

- Add line breaks at 80 characters for best readability
- Add extra lines between bullet points

Example NEWS.md:

```
# lgr (development version)

## Added

- New features.

- New Features 2.

## Changed

- Changes to existing functionality.

## Deprecated

- Soon-to-be removed features.

## Removed

- Now removed features.

## Fixed

- Bug fixes.

## Security

- Vulnerability fixes.

# lgr 0.13.0 (2026-06-21)

## Added

- **Stats:** Add best sets and record display

```


## Rules

1. **Development section** — always present at the top, headed `# lgr (development version)`, for tracking upcoming changes. This is the R-native replacement for `[Unreleased]`; do not use bracket syntax here, since it has no version number for the parser to key on.

2. **Version header** — top-level (`#`) heading: `# lgr x.y.z (YYYY-MM-DD)`. Package name, version, and the date in parentheses, ISO 8601. This must stay a `#` heading, and it must be the *only* heading level used for versions in the file.

3. **Change types** — second-level (`##`) headings, use only the six types below, in this order. Omit empty sections.
   - `## Added` — new features
   - `## Changed` — changes to existing functionality (mark breaking changes with `**Breaking:**`)
   - `## Deprecated` — soon-to-be removed features
   - `## Removed` — now removed features
   - `## Fixed` — bug fixes
   - `## Security` — vulnerability fixes

4. **Imperative mood** — write entries in present tense: "Fix bug", not "Fixed bug" or "Fixes bug".

5. **Capitalize** — start every entry with an uppercase letter.

6. **Punctuation** — omit trailing period for single-sentence entries. Use periods for multi-sentence entries.

7. **One logical change per bullet** — each `-` describes one atomic change.

8. **Reverse chronological order** — newest version first.

9. **No duplicates** — each version appears exactly once.

10. **Yanked releases** — append `[YANKED]` after the date: `# lgr 0.5.0 (2026-06-01) [YANKED]`.

11. **Updating the development section**: whenever you add a new changelog entry here, review all the other entries under `# lgr (development version)`. Check if the new entry replaces or changes existing entries, and consolidate if necessary.

12. **Ordering within sections** — entries inside an `## Added`/`## Changed`/`## Fixed` block should be sorted by area name (alphabetically), then by importance within the same area.

13. **No extra top-level headings** — never add a `# Changelog`/`# NEWS` title or any other `#` heading besides version headers. If you want an intro sentence ("All notable changes..."), put it as plain text with no heading, before the first version block.

## Prose Style

Entries should be **concise**, **specific**, and **user-facing**. A reader should immediately understand what changed and how it affects them.

### Voice

- **Active voice** — "Add import of Strong CSV files", not "Strong CSV files were added"
- **Subject at the start** — Lead with the component or action, not caveats or context
- **Describe the change, not the work** — "Reduce session editor re-renders by 40%" not "Spent time profiling and optimizing render paths"

### Structure

Each entry follows one of these patterns:

1. **Direct** — `Fix crash on empty workout list`
2. **Scoped** — `**History Editor:** Fix discard workout functionality`
3. **Scoped + justification** — `**DB:** Change entity format from YAML to JSON to improve population speed`

Aim for a single line. If a second line is needed for context, keep it brief and prefer a parenthetical or conjunction over a second sentence.

### What to avoid

| Instead of | Write |
|---|---|
| Refactor muscle groups | **Exercises:** Reorganize muscle group hierarchy |
| Various performance improvements | **Session Editor:** Reduce re-renders in set row |
| Internal improvements | **History Editor:** Stabilize undo/redo state across navigations |
| Minor fixes | (name the actual fix) |

- Every entry should pass the **"so what?" test** — if a user can't tell why the change matters, rewrite it.

### Development-version Features

Treat each unreleased feature as a single evolving changelog entry.

- If a feature already has an `Added` entry under `# lgr (development version)`, update that entry as the feature evolves.
- Do **not** create additional `Changed` or `Fixed` entries for implementation iterations, bug fixes, or refinements made before the feature ships.
- The development section should describe the feature as it will exist in the upcoming release, not document its development history.
- Once the feature has been released, record future improvements and bug fixes as separate changelog entries in later versions.
- Every time you add to the development section of the changelog, analyze if there are entries that are redundant or can be consolidated.

Example:

✅ Good

```
## Added

- **Rest Timer:** Add configurable rest timer with notification support
```

❌ Bad

```
## Added

- **Rest Timer:** Add rest timer

## Fixed

- **Rest Timer:** Fix notification bug

## Changed

- **Rest Timer:** Improve timer UI
```

### Merge Heuristics

When deciding whether to consolidate entries under `# lgr (development version)`, use this decision table:

| Scenario | Merge? | Rationale |
|---|---|---|
| Same area + same feature (one entry is a refinement/bug fix of the other) | ✅ Merge into `Added` | Users see one feature ship, not its development history |
| Same area + same feature but scattered across `Added`/`Fixed`/`Changed` | ✅ Merge into `Added` | Part of the same development cycle |
| Same area + different features (e.g., "Add heatmap" vs. "Add weekly stats") | ❌ Keep separate | Two distinct user-facing capabilities |
| Different areas (e.g., "Session Editor" vs. "Stats") | ❌ Keep separate | Different subsystems — users scan by area |
| Fix for a **previously released** feature (not part of current development cycle) | ❌ Keep as `Fixed` under the new version | Real bug fix, not pre-release iteration |

**Actionable summary:** Merge when the entries target the same user-facing feature in the same release cycle. Keep separate when they target genuinely different features or subsystems.

## Area Prefix Convention

Use an **`Area:`** prefix to group entries by subsystem. Capitalize the area name and the first word after the colon. This is encouraged but optional.

```
- **Stats:** Add best sets and record display
- **Session Editor:** Improve performance and persistence safety
- History: Add trophy icons for records
```

Formatting: bold area name is preferred but not required. Be consistent within a single release.

### Sub-areas

Use a `/` to denote a sub-area when a change targets a specific part of a larger area:

```
- **Stats/Heatmap:** Fix day header not scrolling horizontally with grid
- **Library/Exercises:** Weekly sets graph
```

### Multi-area entries

When a change touches multiple areas equally, either split into separate entries or use a combined prefix with `&`:

```
- **Session & History Editor:** Enable drag-and-drop reorder of exercises
```

Prefer separate entries if the changes are logically independent. Use combined prefixes only when the change is a single atomic update that affects both areas.

## Breaking Changes

Prefix the affected entry with `**Breaking:**` when the change breaks backward compatibility. Place it inside the relevant category section.

```
## Changed

- **Breaking:** Remove deprecated `initial_value` column from exercise metrics
```

## Pre-commit Validation

Before finalizing any changelog edit, verify:

1. **Version header format** — `# lgr x.y.z (YYYY-MM-DD)` as a top-level (`#`) heading — package name, version, and the date in parentheses, not `## [x.y.z] - YYYY-MM-DD`.
2. **No stray top-level headings** — the only `#` headings in the file are version headers (plus the development-version header); no `# Changelog`/`# NEWS` title above them.
3. **Category heading level** — change-type sections (`Added`, `Fixed`, ...) are `##`, one level below the version header.
4. **Area prefix colon** — bold area prefix always uses `**Area:**` with the colon **inside** the bold markers.
5. **First-word capitalization** — the first word after `**Area:**` or at the start of an un-scoped entry is capitalized.
6. **Imperative mood** — "Add", "Fix", "Remove", not "Added", "Fixes", "Removed".
7. **Trailing punctuation** — no trailing period for single-sentence entries; add periods for multi-sentence entries.
8. **Section ordering** — entries within each section are sorted alphabetically by area name.
9. **Consolidated development section** — no duplicate or redundant entries under `# lgr (development version)`.

## Bad Practices

- Don't use raw git commit logs as entries.
- Don't use vague descriptions like "Various improvements" — be specific.
- Don't inconsistently record changes (be thorough for notable changes).
- Don't use ambiguous date formats.
- Don't create duplicate version entries.
- Don't add a `## [x.y.z] - date` bracket/dash version header — use the plain `# lgr x.y.z (date)` heading R's parser expects.
- Don't add a top-level title heading (e.g. `# Changelog`) above the version headings — it breaks `tools::news()`/pkgdown's `build_news()`, which treat the first heading level as the version marker.
