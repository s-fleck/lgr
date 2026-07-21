# Changelog Style Guide

This project follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Format

```
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- New features.

### Changed

- Changes to existing functionality.

### Deprecated

- Soon-to-be removed features.

### Removed

- Now removed features.

### Fixed

- Bug fixes.

### Security

- Vulnerability fixes.

## [0.13.0] - 2026-06-21

### Added

- **Stats:** Add best sets and record display

```

## Rules

1. **`[Unreleased]` section** — always present at the top for tracking upcoming changes.

2. **Version header** — `## [x.y.z] - YYYY-MM-DD`, with brackets. Date in ISO 8601.

3. **Change types** — use only the six types below, in this order. Omit empty sections.
   - `### Added` — new features
   - `### Changed` — changes to existing functionality (mark breaking changes with `**Breaking:**`)
   - `### Deprecated` — soon-to-be removed features
   - `### Removed` — now removed features
   - `### Fixed` — bug fixes
   - `### Security` — vulnerability fixes

4. **Imperative mood** — write entries in present tense: "Fix bug", not "Fixed bug" or "Fixes bug".

5. **Capitalize** — start every entry with an uppercase letter.

6. **Punctuation** — omit trailing period for single-sentence entries. Use periods for multi-sentence entries.

7. **One logical change per bullet** — each `-` describes one atomic change.

8. **Reverse chronological order** — newest version first.

9. **No duplicates** — each version appears exactly once.

10. **Yanked releases** — append `[YANKED]` after the date: `## [0.5.0] - 2026-06-01 [YANKED]`.

11. **Updating unreleased section**: Whenever you add a new changelog entry here, review all the other `[Unreleased]` entries. Check if the new entry replaces or changes existing entries, and consolidate if necessary.

12. **Ordering within sections** — entries inside a `### Added`/`### Changed`/`### Fixed` block should be sorted by area name (alphabetically), then by importance within the same area.

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

### Unreleased Features

Treat each unreleased feature as a single evolving changelog entry.

- If a feature already has an `Added` entry under `[Unreleased]`, update that entry as the feature evolves.
- Do **not** create additional `Changed` or `Fixed` entries for implementation iterations, bug fixes, or refinements made before the feature ships.
- The `[Unreleased]` section should describe the feature as it will exist in the upcoming release, not document its development history.
- Once the feature has been released, record future improvements and bug fixes as separate changelog entries in later versions.
- Every time you add to the unreleased section of the changelog, analyze if there are entries that are redundant or can be consolidated.

Example:

✅ Good

```
### Added

- **Rest Timer:** Add configurable rest timer with notification support
```

❌ Bad

```
### Added

- **Rest Timer:** Add rest timer

### Fixed

- **Rest Timer:** Fix notification bug

### Changed

- **Rest Timer:** Improve timer UI
```

### Merge Heuristics

When deciding whether to consolidate entries under `[Unreleased]`, use this decision table:

| Scenario | Merge? | Rationale |
|---|---|---|
| Same area + same feature (one entry is a refinement/bug fix of the other) | ✅ Merge into `Added` | Users see one feature ship, not its development history |
| Same area + same feature but scattered across `Added`/`Fixed`/`Changed` | ✅ Merge into `Added` | Part of the same development cycle |
| Same area + different features (e.g., "Add heatmap" vs. "Add weekly stats") | ❌ Keep separate | Two distinct user-facing capabilities |
| Different areas (e.g., "Session Editor" vs. "Stats") | ❌ Keep separate | Different subsystems — users scan by area |
| Fix for a **previously released** feature (not part of current unreleased cycle) | ❌ Keep as `Fixed` under the new version | Real bug fix, not pre-release iteration |

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
### Changed

- **Breaking:** Remove deprecated `initial_value` column from exercise metrics
```

## Pre-commit Validation

Before finalizing any changelog edit, verify:

1. **Version header format** — `## [x.y.z] - YYYY-MM-DD` with hyphens around the date, not `## [x.y.z] YYYY-MM-DD`.
2. **Area prefix colon** — bold area prefix always uses `**Area:**` with the colon **inside** the bold markers.
3. **First-word capitalization** — the first word after `**Area:**` or at the start of an un-scoped entry is capitalized.
4. **Imperative mood** — "Add", "Fix", "Remove", not "Added", "Fixes", "Removed".
5. **Trailing punctuation** — no trailing period for single-sentence entries; add periods for multi-sentence entries.
6. **Section ordering** — entries within each section are sorted alphabetically by area name.
7. **Consolidated unreleased** — no duplicate or redundant entries under `[Unreleased]`.

## Bad Practices

- Don't use raw git commit logs as entries.
- Don't use vague descriptions like "Various improvements" — be specific.
- Don't inconsistently record changes (be thorough for notable changes).
- Don't use ambiguous date formats.
- Don't create duplicate version entries.


