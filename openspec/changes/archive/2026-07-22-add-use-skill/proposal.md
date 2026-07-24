## Why

`lgr` ships an agent-facing skill file at `inst/skills/SKILL.md` that documents conventions for logging with `lgr`/`LoggerGlue` in downstream packages, but there is no supported way for a user to get that file into their own project. Users must manually locate the file inside the installed package library and copy it by hand, which is error-prone and undiscoverable. `use_logger()` already establishes the convention of a `use_*()` helper for onboarding a new project onto `lgr`; a matching `use_skill()` helper closes the gap for the skill file itself.

## What Changes

- Add a new exported function `use_skill()` that copies the packaged `inst/skills/SKILL.md` file into the calling project.
- Auto-detect which supported coding agent(s) are already in use in the project by checking for their known config directories (`.claude`, `.cline`, `.codex`, `.cursor`, `.gemini`, `.github/skills`, `.windsurf`), and install into the matching convention-specific path (`<tool-dir>/skills/lgr/SKILL.md`), the vendor-confirmed convention for each of these seven tools' shared "Agent Skills" standard. If multiple are detected, use the first match in a fixed priority order (`claude` first, then the rest alphabetically). If none are detected, fall back to the Claude Code convention (`.claude/skills/lgr/SKILL.md`), since it's supported as at least a fallback location by essentially every one of these tools.
- Allow the caller to force a specific target via an `agent` argument (one of the recognized preset names), bypassing detection.
- Allow the caller to fully override the destination via a `path` argument, for agents/locations not in the preset list.
- Always report what was detected/chosen and why (e.g. "Detected agent config for: cursor, claude. Installing lgr skill for 'claude' (highest priority) at .claude/skills/lgr/SKILL.md.") before writing.
- Detect an existing destination file and refuse to overwrite unless the caller passes `overwrite = TRUE`, informing the user either way.
- Create any missing intermediate directories needed for the destination path.
- Print a short confirmation message naming the file that was written (or would be written), consistent with the informative messaging style used elsewhere in the package (e.g. `use_logger()`).

## Capabilities

### New Capabilities
- `use-skill`: A `use_skill()` helper that installs the package's `SKILL.md` file into a user's project, with overwrite protection and directory creation.

### Modified Capabilities
(none)

## Impact

- Affected code: new `R/use_skill.R`, updated `NAMESPACE`/`man/` via roxygen, `_pkgdown.yml` reference list (if it enumerates exported functions).
- Affected docs: package documentation site regenerated to include the new function; `NEWS.md` entry on archive.
- No changes to existing exported functions or behavior; purely additive.
- **No new package dependencies.** File I/O uses base R (`file.copy()`, `dir.create()`, `file.exists()`, `dir.exists()`). Project-root detection reuses `rprojroot`, which is already a `Suggests` dependency (already used by `use_logger()`) — nothing is added to `DESCRIPTION`.
