# Install the lgr Skill File into a Project

Copies the `SKILL.md` file bundled with **lgr** (documenting `lgr`/
`LoggerGlue` conventions) into a coding agent's project-level skills
directory, so agent tooling can discover it. Unlike
[`use_logger()`](https://s-fleck.github.io/lgr/reference/use_logger.md),
which only prints code for you to paste, `use_skill()` writes the file
directly: the skill file is static content meant to be dropped in
unmodified, so there is nothing to review or merge first.

## Usage

``` r
use_skill(agent = NULL, path = NULL, overwrite = FALSE)
```

## Arguments

- agent:

  `character` scalar. One of the supported agent presets (`"claude"`,
  `"cline"`, `"codex"`, `"cursor"`, `"gemini"`, `"github-copilot"`,
  `"windsurf"`), naming which agent's convention to install to. If
  `NULL` (the default) and `path` is also `NULL`, the target is
  auto-detected (see Details). Ignored if `path` is supplied.

- path:

  `character` scalar. A custom destination, overriding both `agent` and
  auto-detection. May point at a directory (in which case `SKILL.md` is
  appended) or directly at a destination file. An existing directory is
  always treated as a directory; otherwise a `path` with a file
  extension (e.g. ending in `.md`) is treated as the literal destination
  file, and a `path` without an extension is treated as a directory to
  create.

- overwrite:

  `logical` scalar. If `FALSE` (the default) and the destination file
  already exists, `use_skill()` leaves it untouched and reports that it
  did so. Set to `TRUE` to replace an existing file.

## Value

`invisible(TRUE)` if a file was written (fresh write or overwrite),
`invisible(FALSE)` if the write was skipped because the destination
already existed and `overwrite = FALSE`.

## Details

The same `SKILL.md` file is copied verbatim to every supported agent.
This is deliberate rather than an oversight: Claude Code, Cline, Codex
CLI, Cursor, Gemini CLI, GitHub Copilot, and Windsurf all implement the
open **Agent Skills** standard (a `SKILL.md` file with YAML frontmatter,
installed at `<tool-dir>/skills/<name>/SKILL.md`), which is explicitly
designed so that the same file works unmodified across tools.

If `agent` and `path` are both `NULL` (the default), `use_skill()` looks
for existing config directories belonging to the supported agents, in
this priority order: `"claude"` first, then the rest alphabetically
(`"cline"`, `"codex"`, `"cursor"`, `"gemini"`, `"github-copilot"`,
`"windsurf"`). If exactly one is found, the file is installed for that
agent. If several are found, the highest-priority one is used. If none
are found, `use_skill()` falls back to the `"claude"` convention, since
it is this package's primary target and several other tools already
treat it as one of their own fallback locations. Either way,
`use_skill()` always reports what it found and which target it chose
before writing anything.

## Examples

``` r
if (FALSE) { # \dontrun{
# auto-detect based on existing agent config directories
use_skill()

# force a specific agent
use_skill(agent = "cursor")

# install to a custom location
use_skill(path = "tools/agent-config")
} # }
```
