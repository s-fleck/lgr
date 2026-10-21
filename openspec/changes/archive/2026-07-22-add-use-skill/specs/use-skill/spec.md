## ADDED Requirements

### Requirement: Explicit `path` argument overrides destination resolution
`lgr` SHALL provide an exported function `use_skill()` that copies the package's bundled `inst/skills/SKILL.md` file to a destination inside the calling project. When the caller supplies a `path` argument, it SHALL take precedence over both `agent` and auto-detection (see the "Target agent is auto-detected..." and "Recognized agent presets..." requirements below for the destination-resolution behavior when `path` is not supplied), and SHALL be interpreted unambiguously as either a directory or a literal file path regardless of whether it exists yet on disk.

#### Scenario: Custom destination path is honored
- **WHEN** the user calls `use_skill(path = "some/other/dir")`
- **THEN** `SKILL.md` is copied into `some/other/dir/SKILL.md`, creating `some/other/dir` if it does not exist

#### Scenario: Custom destination file path is honored
- **WHEN** the user calls `use_skill(path = "some/other/dir/CUSTOM_NAME.md")`
- **THEN** the package's `SKILL.md` content is copied to exactly that file path, creating missing intermediate directories as needed

### Requirement: Recognized agent presets map to convention-specific install paths
`use_skill()` SHALL support an `agent` argument naming one of a fixed set of recognized coding agent presets (`"claude"`, `"cline"`, `"codex"`, `"cursor"`, `"gemini"`, `"github-copilot"`, `"windsurf"`), each mapping to that tool's own per-file skills location, and SHALL error on an unrecognized preset name. Recognized preset names are additive across releases of `lgr`: a name, once shipped, SHALL NOT be removed or repointed to a different install path in a later release without a corresponding `NEWS.md` entry documenting the change and the old path.

#### Scenario: Explicit agent preset is honored
- **WHEN** the user calls `use_skill(agent = "cursor")`
- **THEN** the package's `SKILL.md` content is copied to `.cursor/skills/lgr/SKILL.md` relative to the detected project root, regardless of whether a `.cursor/` directory currently exists, creating missing intermediate directories as needed

#### Scenario: github-copilot preset uses its distinct detection marker
- **WHEN** the user calls `use_skill(agent = "github-copilot")`
- **THEN** the package's `SKILL.md` content is copied to `.github/skills/lgr/SKILL.md` relative to the detected project root

#### Scenario: Unrecognized agent preset is rejected
- **WHEN** the user calls `use_skill(agent = "some-unknown-tool")`
- **THEN** `use_skill()` raises an error listing the recognized preset names, and no file is written

#### Scenario: Explicit path takes precedence over agent
- **WHEN** the user calls `use_skill(agent = "cursor", path = "custom/location.md")`
- **THEN** the file is written to `custom/location.md` and the `agent` argument is ignored for path resolution

### Requirement: Target agent is auto-detected from existing project config directories when not specified
When neither `agent` nor `path` is supplied, `use_skill()` SHALL scan the project for existing config directories belonging to recognized agent presets, in priority order `"claude"` first, then the remaining presets alphabetically by name (`"cline"`, `"codex"`, `"cursor"`, `"gemini"`, `"github-copilot"`, `"windsurf"`), and SHALL install into the highest-priority detected match. If none are detected, `use_skill()` SHALL fall back to the `"claude"` preset. In all cases, `use_skill()` SHALL inform the user, before writing, which agent directories were detected (or that none were found) and which target was chosen and why.

#### Scenario: Default destination is used when no agent directories exist
- **WHEN** the user calls `use_skill()` with no arguments in a project with none of `.claude/`, `.cline/`, `.codex/`, `.cursor/`, `.gemini/`, `.github/skills/`, `.windsurf/` present
- **THEN** a message states that no known agent config directories were found and that the Claude Code default is being used, the package's `SKILL.md` is copied to `.claude/skills/lgr/SKILL.md` relative to the detected project root, and any missing intermediate directories are created

#### Scenario: Single agent directory is detected
- **WHEN** the user calls `use_skill()` in a project where only `.cursor/` exists among the recognized markers
- **THEN** a message states that `.cursor/` was detected and that the file is being installed for `"cursor"`, and the file is copied to `.cursor/skills/lgr/SKILL.md`

#### Scenario: Multiple agent directories are detected, claude present
- **WHEN** the user calls `use_skill()` in a project where both `.cursor/` and `.claude/` exist
- **THEN** a message lists all detected agent directories and states that `"claude"` was chosen as the highest-priority match, and the file is copied to `.claude/skills/lgr/SKILL.md`

#### Scenario: Multiple agent directories are detected, claude absent — alphabetical tie-break applies
- **WHEN** the user calls `use_skill()` in a project where both `.windsurf/` and `.cursor/` exist, and `.claude/` does not
- **THEN** a message lists both detected agent directories and states that `"cursor"` was chosen (alphabetically first among the non-`claude` presets), and the file is copied to `.cursor/skills/lgr/SKILL.md`

#### Scenario: Alphabetical tie-break at the cline/codex boundary
- **WHEN** the user calls `use_skill()` in a project where both `.codex/` and `.cline/` exist, and `.claude/` does not
- **THEN** a message lists both detected agent directories and states that `"cline"` was chosen (`"cline"` sorts alphabetically before `"codex"`), and the file is copied to `.cline/skills/lgr/SKILL.md`

#### Scenario: github-copilot's narrower marker avoids false-positive detection
- **WHEN** the user calls `use_skill()` in a project that has a `.github/` directory (e.g. containing only CI workflow files) but no `.github/skills/` subdirectory, and no other recognized marker is present
- **THEN** `"github-copilot"` is NOT counted as detected, the detected set is treated as empty, and `use_skill()` falls back to the `"claude"` preset

### Requirement: Existing destination files are protected from accidental overwrite
`use_skill()` SHALL NOT overwrite an existing destination file unless the caller explicitly opts in via `overwrite = TRUE`. This rule applies uniformly regardless of how the destination was resolved (auto-detected, explicit `agent`, or explicit `path`). In every case, `use_skill()` SHALL communicate the outcome both via a `message()` and via its return value: `invisible(TRUE)` when a file is written (fresh write or overwrite), `invisible(FALSE)` when the write is skipped.

#### Scenario: Destination already exists and overwrite is not requested
- **WHEN** the user calls `use_skill()` and the resolved destination file already exists and `overwrite` is `FALSE` (the default)
- **THEN** no file is written, a message informs the user that the file already exists and was left unchanged, and the function returns `invisible(FALSE)`

#### Scenario: Destination already exists and overwrite is requested
- **WHEN** the user calls `use_skill(overwrite = TRUE)` and the resolved destination file already exists
- **THEN** the existing file is replaced with the package's `SKILL.md` content, a message confirms the file was written, and the function returns `invisible(TRUE)`
