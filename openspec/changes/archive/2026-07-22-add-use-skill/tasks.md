## 1. Agent registry

- [x] 1.1 Create an internal (non-exported) registry mapping preset names to `marker` (detection directory) and `path` (install path), in this priority order — `claude` first (also the fallback default), then the rest alphabetically by name (this is a deliberate tie-break rule, not incidental list order — see design.md Decision 3). Double-check the alphabetization against `sort()`/manual letter comparison before committing the list order — a previous draft of this registry got `cline`'s position wrong (placed last instead of second) despite claiming to be alphabetical:
  - `claude` → marker `.claude/`, path `.claude/skills/lgr/SKILL.md`
  - `cline` → marker `.cline/`, path `.cline/skills/lgr/SKILL.md`
  - `codex` → marker `.codex/`, path `.codex/skills/lgr/SKILL.md`
  - `cursor` → marker `.cursor/`, path `.cursor/skills/lgr/SKILL.md`
  - `gemini` → marker `.gemini/`, path `.gemini/skills/lgr/SKILL.md`
  - `github-copilot` → marker `.github/skills/` (NOT bare `.github/`), path `.github/skills/lgr/SKILL.md`
  - `windsurf` → marker `.windsurf/`, path `.windsurf/skills/lgr/SKILL.md`
  - `continue` is deliberately NOT included — its skills-directory convention could not be confirmed against Continue's own documentation (see design.md Context/Open Questions).
- [x] 1.2 Write a helper that, given a project root, returns the names of all presets whose marker directory exists, preserving registry priority order.

## 2. Implement `use_skill()`

- [x] 2.1 Create `R/use_skill.R` with `use_skill(agent = NULL, path = NULL, overwrite = FALSE)`.
- [x] 2.2 Resolve the project root via `rprojroot` (package-or-RStudio-project criterion), falling back to `getwd()`.
- [x] 2.3 Destination resolution order: (a) `path` if supplied, wins outright; (b) else `agent` if supplied, validated against the registry (clear error listing valid names if unrecognized), path resolved from the registry regardless of marker presence; (c) else auto-detect via the task 1.2 helper — use the highest-priority detected preset, or fall back to `"claude"` if none detected.
- [x] 2.4 In the auto-detect branch, `message()` the detected preset name(s) (or "none found") and which one was chosen and why, before doing anything else.
- [x] 2.5 Support `path` pointing at either a directory (append `SKILL.md`) or a full file path, using the disambiguation rule from design.md Decision 5: (a) existing directory on disk → directory; (b) else has a file extension (`nzchar(tools::file_ext(path))`) → literal file path; (c) else → directory to create. Do NOT use a bare `dir.exists()` check alone — it cannot distinguish a not-yet-created directory from a not-yet-created file.
- [x] 2.6 Create missing intermediate destination directories with `dir.create(recursive = TRUE)`.
- [x] 2.7 Implement overwrite protection: skip and message when the destination exists and `overwrite = FALSE`; copy and message when it doesn't exist or `overwrite = TRUE`.
- [x] 2.8 Return `invisible(TRUE)` when a file is written and `invisible(FALSE)` when skipped.
- [x] 2.9 Copy `inst/skills/SKILL.md` via `file.copy()`, sourced with `system.file("skills/SKILL.md", package = "lgr")`.

## 3. Documentation and exports

- [x] 3.1 Add roxygen docs (`@param agent` documenting recognized presets and priority order, `@param path`, `@param overwrite`, `@return`, `@examples`, `@details` explaining (a) the copy-vs-print divergence from `use_logger()`, (b) the auto-detection behavior, and (c) — per design.md Decision 1 — that the same unmodified `SKILL.md` works across all listed tools because they implement the shared "Agent Skills" open standard, which is *why* no per-tool content customization is needed) and `@export`.
- [x] 3.2 Run `devtools::document()` to update `NAMESPACE` and `man/use_skill.Rd`.
- [x] 3.3 Add `use_skill` to `_pkgdown.yml` reference list alongside `use_logger`.

## 4. Tests

- [x] 4.1 Add `tests/testthat/test-use_skill.R` covering: no markers present (falls back to claude + message), single marker detected for each of the 7 presets, multiple markers detected with `claude` present (claude wins), multiple markers detected without `claude` present testing the alphabetical tie-break at its previously-buggy boundary (e.g. `.codex/` + `.cline/` → `cline` wins, since `cline` sorts before `codex`) as well as a simple case (e.g. `.windsurf/` + `.cursor/` → `cursor` wins), explicit `agent` overriding detection, explicit `agent` used even when its marker is absent, unrecognized `agent` errors, `path` overriding both `agent` and detection, custom directory path (no extension, doesn't yet exist → treated as directory), custom file path (has extension, doesn't yet exist → treated as literal file), custom path that is an already-existing directory with no extension in its name, overwrite = FALSE skip behavior, overwrite = TRUE replace behavior, return value (`TRUE`/`FALSE`) in each case, and the `github-copilot` false-positive guard (bare `.github/` without `.github/skills/` does not count as detected).
- [x] 4.2 Use `withr::local_tempdir()` / `withr::local_dir()` (or equivalent) so tests are self-contained and don't touch the real project tree.

## 5. Verification

- [x] 5.1 Run `devtools::check()` (or `devtools::test()` + `R CMD check` as appropriate) and confirm no new warnings/notes. (`devtools::test()`: 821 pass, 3 pre-existing skips, 0 fail. `devtools::check(vignettes = FALSE)`: 0 errors, 0 warnings; the 3 NOTEs present are pre-existing/unrelated to this change — openspec tooling files, a timestamp-verification note, and non-standard top-level files that already existed. Vignette building fails in this environment due to a missing Pandoc install, unrelated to this change.)
- [x] 5.2 Manually verify `use_skill()` in scratch projects: one with no agent dirs, one with `.cursor/` only, and one with both `.claude/` and `.cursor/` — confirm the reported detection message and chosen path match expectations in each case. (All three matched expected messages/paths exactly.)
- [x] 5.3 Diff `DESCRIPTION` after implementation and confirm no new entries were added under `Imports`/`Suggests`/`Depends` (only base R and the pre-existing `rprojroot` Suggests dependency should be used). (Confirmed: only the `Collate` field gained `'use_skill.R'`; no dependency fields changed.)
