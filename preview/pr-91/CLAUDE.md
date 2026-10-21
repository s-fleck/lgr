# Agent Memory

## General

- Prefer **KISS** (Keep It Simple, Stupid) solutions.
- Prioritize **user experience** over clever implementations.
- Avoid introducing abstractions unless they significantly reduce
  complexity or duplication.
- Maintain consistency with the existing architecture and coding style.

## Changelog

- Update the NEWS.md file whenever we archive an openspec with
  /opsx-archive. Try to add only one or maximum two entries for a single
  spec. Follow the `r-news-styleguide` skill.

## Version Bump

- Use the `lgr-release` skill when asked to cut a release or bump the
  package version.

## Skills

- Agent tooling lives in `.claude/skills/<name>/SKILL.md` (read by
  Claude Code and opencode alike).
- `r-package-development`, `r-package-style-guide`, `r-package-testing`,
  `r-roxygen` — vendored from the agent-tools repo, byte-identical.
- `r-news-styleguide` — vendored from agent-tools and amended for lgr;
  its `variant` metadata records the upstream version and what changed.
- `lgr-release` — lgr-local: the release procedure.
