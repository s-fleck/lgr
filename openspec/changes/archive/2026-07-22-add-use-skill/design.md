## Context

`lgr` ships `inst/skills/SKILL.md`, an agent-facing skill file describing logging conventions. Once installed, this file lives inside the package library (e.g. `system.file("skills/SKILL.md", package = "lgr")`), which is not a location AI coding agents (Claude Code, etc.) look at by convention — those tools discover skills from paths inside the *user's project*, typically `.claude/skills/<name>/SKILL.md`. There is currently no supported way to get the file from the package into a project.

The package already has one precedent for a `use_*()` onboarding helper, `use_logger()` (`R/use_logger.R`), but it follows a "generate code, print it, let the user paste it" pattern rather than writing files — appropriate for code that needs to be reviewed and slotted into the right file by the user, but not a good fit for a static file that should just be dropped in as-is.

Users work with a variety of AI coding agents. Two research passes were needed to get this right, because the first one was wrong:

1. An initial general-web-sources pass found only Claude Code with a true directory-based "Skill" convention, with Cursor/Windsurf using their own `rules/*.mdc` formats and Codex/Gemini/Copilot appearing to only support a single shared root file (`AGENTS.md`/`GEMINI.md`/`copilot-instructions.md`).
2. A second pass, cross-checking a single third party's (Fission-AI/OpenSpec) install-path matrix, suggested all of those tools had *also* grown a `.<tool>/skills/<name>/SKILL.md` directory — but this was flagged in review as insufficient evidence: OpenSpec's chosen install location for its own generated files is not proof that a given tool actually reads from there.

A third, vendor-first-party verification pass (web search against each tool's own current documentation, since this space moves fast enough that pre-trained knowledge cannot be trusted) resolved this properly. The directory convention is real and is not each vendor's independent invention: Anthropic published an open **Agent Skills** specification (the `SKILL.md` format with YAML frontmatter `name`/`description`) on 2025-12-18, now under Linux Foundation AAIF governance, explicitly designed to be portable, unmodified, across tools. Confirmed first-party for 7 tools:

| Tool | Skills path (first-party confirmed) | Source |
|---|---|---|
| Claude Code | `.claude/skills/<name>/SKILL.md` | code.claude.com/docs/en/skills |
| Cursor | `.cursor/skills/<name>/SKILL.md` | cursor.com/docs/skills — explicitly documents cross-tool portability |
| Windsurf | `.windsurf/skills/<name>/SKILL.md` (+ user-level `~/.codeium/windsurf/skills/`) | docs.windsurf.com/windsurf/cascade/skills — states files "work in Claude Code and Cursor... without modification" |
| Cline | `.cline/skills/<name>/SKILL.md` (also auto-detects `.clinerules/skills/` and `.claude/skills/` as fallbacks) | docs.cline.bot/customization/skills |
| Codex CLI | `.codex/skills/<name>/SKILL.md` (+ user-level `~/.codex/skills/`) | developers.openai.com/codex/skills |
| Gemini CLI | `.gemini/skills/<name>/SKILL.md` (+ user-level `~/.gemini/skills/`; also aliased as `.agents/skills/`) | geminicli.com/docs/cli/skills |
| GitHub Copilot | `.github/skills/<name>/SKILL.md` (also accepts `.claude/skills/` or `.agents/skills/` in the same repo) | docs.github.com/en/copilot/how-tos/copilot-sdk/features/skills |

**Continue.dev could not be confirmed first-party** — no page on Continue's own docs site (`docs.continue.dev`) mentions `SKILL.md` or a skills directory; only Continue's "Rules" and MCP-based tool extension are documented there. The only source for `.continue/skills/` was the same third-party OpenSpec matrix that prompted this re-verification in the first place. **`continue` is therefore dropped from the registry** rather than shipped on unconfirmed evidence — it can be added back once a Continue-owned source confirms the convention (see Open Questions).

This also resolves the earlier "does dropping identical content into another tool's directory even work?" concern: cross-tool portability of an unmodified `SKILL.md` is the explicit, stated design goal of the Agent Skills standard, not an assumption this package is making on its own. `GitHub Copilot`'s marker is treated as `.github/skills/` specifically rather than bare `.github/`, since the latter is near-universal (CI workflows, issue templates) and would be a false-positive-prone detection signal.

## Goals / Non-Goals

**Goals:**
- Provide `use_skill()` to copy `inst/skills/SKILL.md` into a calling project at a sensible, agent-appropriate path.
- Auto-detect which supported agent(s) are already configured in the project and pick a sensible target without requiring the user to know each tool's convention.
- Always tell the user what was detected and what was chosen, before writing anything.
- Protect existing files from accidental overwrite.
- Introduce zero new package dependencies: base R for file I/O, plus `rprojroot`, which is already a `Suggests` dependency used by `use_logger()`.
- Make the destination fully overridable (`agent` preset or raw `path`) so users with tooling outside the detected set, or multiple skill directories, aren't locked out.

**Non-Goals:**
- Supporting arbitrary skill file *content* customization/templating (the file is copied verbatim; no interpolation of package name etc., since the skill file is already generic).
- Installing skills for any package other than `lgr` itself.
- Registering/activating the skill with a specific agent's config beyond dropping the file at the conventional path (e.g. not editing any agent settings.json).
- Writing into single-shared-root-file conventions (e.g. Aider's `CONVENTIONS.md`, or any tool's root `AGENTS.md`-style file) — these would need append/merge logic against a file the user already owns and edits, which is out of scope for this change.
- Including `continue` (Continue.dev) in the registry — its `.continue/skills/` directory could not be confirmed against Continue's own documentation (see Context); revisit once first-party confirmation exists.
- Installing into more than one agent target per call — if several are detected, only the first (by priority order) is used; the user can call `use_skill()` again with an explicit `agent` to install additional targets.
- Tracking which tools were "already configured" via our own state/manifest file (as OpenSpec's `openspec init` does) — `use_skill()` is stateless and re-derives the target every call by scanning for marker directories.

## Decisions

**1. Copy-file, not print-and-paste.**
Unlike `use_logger()`, `use_skill()` performs the file write directly via base R's `file.copy()` — no dependency on `usethis` itself; it is cited below only as a familiar reference point for the pattern. Rationale: the skill file is static markdown meant to be dropped in unmodified — there is nothing for the user to review/merge, so print-and-paste only adds friction. This diverges from `use_logger()`'s style but is the correct pattern for this kind of asset (mirrors `usethis::use_template()`-style helpers, which do write files). Copying the exact same file unmodified into every target is only valid because portability without modification is the explicit design goal of the Agent Skills standard these tools implement (see Context) — the roxygen `@details` for `use_skill()` MUST say this explicitly (which standard, which tools, and that this is why the content isn't tool-specific), so a user reading `?use_skill` understands why one file works everywhere instead of just trusting it silently.

**2. Project root resolution.**
Project root is resolved via `rprojroot::find_root_file(criterion = rprojroot::is_r_package | rprojroot::is_rstudio_project, path = ".")` when available, falling back to `getwd()`, mirroring the root-finding pattern already used by `use_logger()` (which uses `rprojroot::find_package_root_file`). Using a package-root-or-RStudio-project criterion (rather than package-only) allows `use_skill()` to work in non-package RStudio projects too, since the skill's audience (agents editing R code) isn't restricted to package authors. All detection and default paths below are relative to this root.

**3. Agent registry with detection markers and install paths, checked in a fixed priority order.**
A small internal registry (not exported) maps each recognized `agent` preset name to a detection marker (a directory whose existence indicates that tool is used in the project) and an install path template:

| Preset name | Detection marker | Install path |
|---|---|---|
| `"claude"` | `.claude/` | `.claude/skills/lgr/SKILL.md` |
| `"cline"` | `.cline/` | `.cline/skills/lgr/SKILL.md` |
| `"codex"` | `.codex/` | `.codex/skills/lgr/SKILL.md` |
| `"cursor"` | `.cursor/` | `.cursor/skills/lgr/SKILL.md` |
| `"gemini"` | `.gemini/` | `.gemini/skills/lgr/SKILL.md` |
| `"github-copilot"` | `.github/skills/` (not bare `.github/`) | `.github/skills/lgr/SKILL.md` |
| `"windsurf"` | `.windsurf/` | `.windsurf/skills/lgr/SKILL.md` |

All seven presets share the identical `<tool-dir>/skills/lgr/SKILL.md` shape, per the vendor-confirmed convention in Context, which keeps the registry a simple `tool -> marker` lookup rather than needing per-tool path templates. `continue` is intentionally excluded (see Context) pending first-party confirmation.

**Priority order when multiple markers are detected: `"claude"` first, then the rest alphabetically by preset name** (`cline`, `codex`, `cursor`, `gemini`, `github-copilot`, `windsurf`). `"claude"` leads and stays the default fallback because this package's `SKILL.md` is authored against Claude Code's own docs, it's the feature's primary target audience, and it's the one convention supported across effectively all of these tools (several of them explicitly document reading `.claude/skills/` as a fallback location — see Cline's row in the Context table above). Everything after `claude` is alphabetical specifically *because* alphabetical order carries no implied endorsement or popularity ranking between the remaining six vendor-confirmed tools — it's a tie-break that's easy to state, easy to verify, and doesn't require maintaining or defending a subjective "which tool matters more" ordering as new tools are added. (A previous revision of this table listed `cline` last instead of second, in violation of its own stated alphabetical rule — corrected here: `cline` < `codex` because `l` < `o` at the second character.)

**4. Signature and detection algorithm: `use_skill(agent = NULL, path = NULL, overwrite = FALSE)`.**
- If `path` is supplied, it wins outright: no detection, no registry lookup, used as-is (directory-or-file handling as before).
- Else if `agent` is supplied, it must be one of the registry names (validated via `match.arg`-style check with a clear error listing valid values); its install path is used, regardless of whether that tool's marker is actually present (the user is explicitly stating intent).
- Else (both `NULL`, the common case): scan the registry in priority order, collect every preset whose detection marker directory exists under the project root, and:
  - if the set is non-empty, use the first (highest-priority) match and message which marker(s) were found and which one was chosen, e.g. `"Detected agent config for: cursor, claude. Installing lgr skill for 'claude' (highest priority) at .claude/skills/lgr/SKILL.md."`;
  - if the set is empty, fall back to `"claude"` and message that no known agent directories were found, so the Claude Code default is being used, e.g. `"No existing agent config directories found (checked: .claude, .cline, .codex, .cursor, .gemini, .github/skills, .windsurf). Defaulting to Claude Code convention: .claude/skills/lgr/SKILL.md."`.
- This messaging happens unconditionally before the write/overwrite decision, so the user always sees *what* was detected and *why* a target was chosen, even when they never touch `agent`/`path`.

**5. `path` argument to fully override the destination, with an unambiguous file-vs-directory rule.**
`path` may point directly at the destination file, or at a directory (in which case `SKILL.md` is appended). A `dir.exists()` check alone cannot disambiguate this for a path that doesn't exist yet on disk (the common case — `dir.exists("some/new/dir")` and `file.exists("some/new/dir/SKILL.md")` are both `FALSE` before anything is written), so the actual rule, checked in this order, is:
1. If `path` already exists on disk **and** is a directory (`dir.exists(path)` is `TRUE`) → treat as a directory; append `SKILL.md`.
2. Else if `path` has a file extension (`nzchar(tools::file_ext(path))` is `TRUE`, e.g. ends in `.md`) → treat as the literal destination file path.
3. Else (no extension, doesn't exist yet) → treat as a directory to create; append `SKILL.md`.

This makes `use_skill(path = "some/other/dir")` (no extension) unambiguously a directory, and `use_skill(path = "some/other/dir/CUSTOM_NAME.md")` (has an extension) unambiguously a file, regardless of whether either exists yet — closing the gap where a bare `dir.exists()` check could not tell the two cases apart. This remains the escape hatch for agents/locations not in the registry (e.g. a custom directory, or a tool added after this package ships).

**6. Overwrite protection defaults to off, matching `usethis` conventions.**
If the destination file already exists and `overwrite = FALSE` (default), `use_skill()` message()s that the file exists and is left untouched, then returns `invisible(FALSE)`. With `overwrite = TRUE` it proceeds and returns `invisible(TRUE)`. This avoids silently clobbering a user's local edits to the skill file.

**7. Directory creation.**
Missing intermediate directories in the destination path are created via `dir.create(recursive = TRUE)` before copying, so `use_skill()` works on a totally fresh project without requiring the target rules directory to pre-exist.

**8. Return value and messaging.**
`use_skill()` calls `message()` naming the destination file (consistent with `use_logger()`'s informative-messaging style), and returns `invisible(TRUE)` if a file was written (fresh write or overwrite) or `invisible(FALSE)` if it was skipped because the destination already existed and `overwrite = FALSE`. The path itself is always named in the message text, not encoded in the return value, so the return value stays a simple, scriptable boolean (`if (use_skill()) ...`) rather than requiring callers to inspect a path string to detect success.

**9. No new package dependencies.**
All detection/messaging/copy logic uses base R (`file.exists()`, `dir.exists()`, `dir.create()`, `file.copy()`, `message()`, `sprintf()`/`paste0()`) plus the project's existing `rprojroot` `Suggests` dependency (decision 2) — nothing new is added to `DESCRIPTION`. In particular, richer console formatting via `cli` (already a `Suggests` dependency for other parts of the package, but not currently used by `use_logger()`) is deliberately NOT introduced here, to keep `use_skill()`'s dependency footprint identical to `use_logger()`'s.

## Risks / Trade-offs

- **[Risk]** `rprojroot` root-finding could fail or pick an unexpected root in unusual project layouts → **Mitigation**: fall back to `getwd()` with a clear message about where the file was written, so the user isn't left guessing; same fallback pattern as it would apply to `use_logger()`'s existing root-finding usage.
- **[Risk]** Overwriting a user-customized `SKILL.md` → **Mitigation**: `overwrite = FALSE` default plus explicit message when skipping.
- **[Risk]** Divergence in style from `use_logger()` (copy vs. print) could look inconsistent → **Mitigation**: documented rationale in roxygen `@details` so the difference is intentional and discoverable, not accidental.
- **[Risk]** A project marker directory exists but wasn't created by that agent (e.g. leftover `.cursor/` from an abandoned experiment), so detection picks an irrelevant target → **Mitigation**: the mandatory "Detected agent config for: ..." message surfaces exactly what was found, so a wrong guess is immediately visible and correctable via `agent =`/`path =`, rather than silently writing to the wrong place.
- **[Risk]** Registry drift as agent tools change conventions → **Mitigation**: registry is a single internal (non-exported) data structure, cheap to update in one place. However, unlike a purely internal implementation detail, the `agent` preset **names** (`"cursor"`, `"cline"`, etc.) are effectively public API once users start writing `use_skill(agent = "cline")` in their own scripts/READMEs — if a future fix moves a preset's *install path* (e.g. because a vendor changes its convention), that silently orphans any file a user already wrote at the old path with no migration. **Mitigation**: any change to an existing preset's install path is treated as a behavior change requiring a `NEWS.md` entry and, if the vendor's own convention changed, a note pointing users at the old path so they can move the file themselves; preset *names* are only ever added, never renamed or removed, to keep existing calls valid.
- **[Risk]** Excluding Aider and Continue.dev disappoints those users → **Mitigation**: `path` remains a full escape hatch (e.g. `use_skill(path = "CONVENTIONS.md")` for Aider — though this would still overwrite-protect rather than append, so isn't a perfect fit for shared root files); both exclusions are explicitly called out as considered scope decisions (see Non-Goals), open to a future change — for Continue specifically, once its skills-directory convention is confirmed via a Continue-owned source.
- **[Risk]** Even vendor-documented conventions can lag or be reverted — first-party docs confirmed today (see Context) are a snapshot, not a permanent guarantee → **Mitigation**: acceptable risk given all 7 remaining entries are corroborated by the vendor's own current documentation (not a third party's install choices, as the previous revision of this design relied on); if one goes stale, only that registry row needs correcting.

## Migration Plan

Purely additive — no migration needed. New function, new file, new man page, `NEWS.md` entry on archive.

## Open Questions

- Should `continue` be added back once a first-party Continue.dev source confirms a `.continue/skills/` (or equivalent) convention? Left open rather than decided now, since it depends on evidence this change doesn't currently have.
