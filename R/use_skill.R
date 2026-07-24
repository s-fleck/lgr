#' Install the lgr Skill File into a Project
#'
#' Copies the `SKILL.md` file bundled with **lgr** (documenting `lgr`/
#' `LoggerGlue` conventions) into a coding agent's project-level skills
#' directory, so agent tooling can discover it. Unlike [use_logger()], which
#' only prints code for you to paste, `use_skill()` writes the file directly:
#' the skill file is static content meant to be dropped in unmodified, so
#' there is nothing to review or merge first.
#'
#' The same `SKILL.md` file is copied verbatim to every supported agent. This
#' is deliberate rather than an oversight: Claude Code, Cline, Codex CLI,
#' Cursor, Gemini CLI, GitHub Copilot, and Windsurf all implement the open
#' **Agent Skills** standard (a `SKILL.md` file with YAML frontmatter,
#' installed at `<tool-dir>/skills/<name>/SKILL.md`), which is explicitly
#' designed so that the same file works unmodified across tools.
#'
#' If `agent` and `path` are both `NULL` (the default), `use_skill()` looks
#' for existing config directories belonging to the supported agents, in
#' this priority order: `"claude"` first, then the rest alphabetically
#' (`"cline"`, `"codex"`, `"cursor"`, `"gemini"`, `"github-copilot"`,
#' `"windsurf"`). If exactly one is found, the file is installed for that
#' agent. If several are found, the highest-priority one is used. If none
#' are found, `use_skill()` falls back to the `"claude"` convention, since it
#' is this package's primary target and several other tools already treat it
#' as one of their own fallback locations. Either way, `use_skill()` always
#' reports what it found and which target it chose before writing anything.
#'
#' @param agent `character` scalar. One of the supported agent presets
#'   (`"claude"`, `"cline"`, `"codex"`, `"cursor"`, `"gemini"`,
#'   `"github-copilot"`, `"windsurf"`), naming which agent's convention to
#'   install to. If `NULL` (the default) and `path` is also `NULL`, the
#'   target is auto-detected (see Details). Ignored if `path` is supplied.
#' @param path `character` scalar. A custom destination, overriding both
#'   `agent` and auto-detection. May point at a directory (in which case
#'   `SKILL.md` is appended) or directly at a destination file. An existing
#'   directory is always treated as a directory; otherwise a `path` with a
#'   file extension (e.g. ending in `.md`) is treated as the literal
#'   destination file, and a `path` without an extension is treated as a
#'   directory to create.
#' @param overwrite `logical` scalar. If `FALSE` (the default) and the
#'   destination file already exists, `use_skill()` leaves it untouched and
#'   reports that it did so. Set to `TRUE` to replace an existing file.
#'
#' @return `invisible(TRUE)` if a file was written (fresh write or
#'   overwrite), `invisible(FALSE)` if the write was skipped because the
#'   destination already existed and `overwrite = FALSE`.
#' @export
#'
#' @examples
#' \dontrun{
#' # auto-detect based on existing agent config directories
#' use_skill()
#'
#' # force a specific agent
#' use_skill(agent = "cursor")
#'
#' # install to a custom location
#' use_skill(path = "tools/agent-config")
#' }
use_skill <- function(
  agent     = NULL,
  path      = NULL,
  overwrite = FALSE
){
  registry <- skill_registry()

  if (!is.null(path)){
    dest <- resolve_skill_path(path)
  } else {
    root <- skill_root()

    if (!is.null(agent)){
      if (!agent %in% names(registry)){
        stop(
          "`agent` must be one of: ",
          paste(shQuote(names(registry)), collapse = ", "),
          ". Got: ", shQuote(agent),
          call. = FALSE
        )
      }
      chosen <- agent
    } else {
      detected <- detect_agent_presets(registry, root)

      if (length(detected)){
        chosen <- detected[[1]]
        priority_note <- if (length(detected) > 1) "(highest priority) " else ""
        message(
          "Detected agent config for: ", paste(detected, collapse = ", "), ". ",
          "Installing lgr skill for '", chosen, "' ", priority_note,
          "at ", registry[[chosen]]$path, "."
        )
      } else {
        chosen <- "claude"
        message(
          "No existing agent config directories found (checked: ",
          paste(vapply(registry, `[[`, character(1), "marker"), collapse = ", "),
          "). Defaulting to Claude Code convention: ", registry[[chosen]]$path, "."
        )
      }
    }

    dest <- file.path(root, registry[[chosen]]$path)
  }

  if (file.exists(dest) && !isTRUE(overwrite)){
    message(
      "'", dest, "' already exists. Set `overwrite = TRUE` to replace it. ",
      "No file written."
    )
    return(invisible(FALSE))
  }

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  file.copy(
    system.file("skills", "SKILL.md", package = "lgr"),
    dest,
    overwrite = TRUE
  )

  message("Installed lgr skill file to '", dest, "'")
  invisible(TRUE)
}




#' Registry of supported agent presets
#'
#' Internal registry mapping each recognized `agent` preset name to its
#' detection marker (a directory whose existence indicates that agent is
#' configured in the project) and its install path (relative to the project
#' root). List order is the detection priority order: `claude` first, then
#' the rest alphabetically by name. `continue` (Continue.dev) is
#' deliberately not included, as its skills-directory convention could not
#' be confirmed against first-party documentation.
#'
#' @return a named `list` of `list(marker = <character>, path = <character>)`
#' @noRd
skill_registry <- function(){
  list(
    claude = list(
      marker = ".claude",
      path   = file.path(".claude", "skills", "lgr", "SKILL.md")
    ),
    cline = list(
      marker = ".cline",
      path   = file.path(".cline", "skills", "lgr", "SKILL.md")
    ),
    codex = list(
      marker = ".codex",
      path   = file.path(".codex", "skills", "lgr", "SKILL.md")
    ),
    cursor = list(
      marker = ".cursor",
      path   = file.path(".cursor", "skills", "lgr", "SKILL.md")
    ),
    gemini = list(
      marker = ".gemini",
      path   = file.path(".gemini", "skills", "lgr", "SKILL.md")
    ),
    `github-copilot` = list(
      marker = file.path(".github", "skills"),
      path   = file.path(".github", "skills", "lgr", "SKILL.md")
    ),
    windsurf = list(
      marker = ".windsurf",
      path   = file.path(".windsurf", "skills", "lgr", "SKILL.md")
    )
  )
}




#' Detect which supported agents are configured in a project
#'
#' @param registry the result of `skill_registry()`
#' @param root `character` scalar. project root directory
#'
#' @return a `character` vector of preset names whose marker directory
#'   exists under `root`, in registry (priority) order
#' @noRd
detect_agent_presets <- function(
  registry,
  root
){
  detected <- character(0)

  for (nm in names(registry)){
    if (dir.exists(file.path(root, registry[[nm]]$marker))){
      detected <- c(detected, nm)
    }
  }

  detected
}




#' Resolve the project root for `use_skill()`
#'
#' @return `character` scalar. the project root directory, falling back to
#'   the current working directory if no root can be determined.
#' @noRd
skill_root <- function(){
  tryCatch(
    rprojroot::find_root_file(
      criterion = rprojroot::is_r_package | rprojroot::is_rstudio_project,
      path = "."
    ),
    error = function(e) getwd()
  )
}




#' Resolve an explicit user-supplied `path` for `use_skill()`
#'
#' A bare `dir.exists()` check cannot distinguish a not-yet-created
#' directory from a not-yet-created file, since neither exists on disk yet
#' in the common case. Instead: an existing directory is always treated as
#' a directory; otherwise a `path` with a file extension is treated as the
#' literal destination file, and a `path` without one is treated as a
#' directory to create.
#'
#' @param path `character` scalar as passed to `use_skill()`
#'
#' @return `character` scalar. the resolved destination file path
#' @noRd
resolve_skill_path <- function(
  path
){
  if (dir.exists(path)){
    file.path(path, "SKILL.md")
  } else if (nzchar(tools::file_ext(path))){
    path
  } else {
    file.path(path, "SKILL.md")
  }
}
