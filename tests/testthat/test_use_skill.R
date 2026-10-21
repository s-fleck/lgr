test_that("use_skill: falls back to 'claude' when no agent dirs are detected", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  expect_message(
    res <- use_skill(),
    "No existing agent config directories found.*Defaulting to Claude Code convention"
  )
  expect_true(res)
  expect_true(file.exists(file.path(dir, ".claude/skills/lgr/SKILL.md")))
})


test_that("use_skill: detects a single existing agent directory", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create(".cursor")

  expect_message(
    res <- use_skill(),
    "Detected agent config for: cursor.*Installing lgr skill for 'cursor'"
  )
  expect_true(res)
  expect_true(file.exists(file.path(dir, ".cursor/skills/lgr/SKILL.md")))
})


test_that("use_skill: 'claude' wins priority when detected alongside others", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create(".cursor")
  dir.create(".claude")

  expect_message(
    res <- use_skill(),
    "Detected agent config for: claude, cursor.*Installing lgr skill for 'claude' \\(highest priority\\)"
  )
  expect_true(res)
  expect_true(file.exists(file.path(dir, ".claude/skills/lgr/SKILL.md")))
  expect_false(file.exists(file.path(dir, ".cursor/skills/lgr/SKILL.md")))
})


test_that("use_skill: alphabetical tie-break at the cline/codex boundary (regression)", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create(".codex")
  dir.create(".cline")

  expect_message(
    res <- use_skill(),
    "Installing lgr skill for 'cline' \\(highest priority\\)"
  )
  expect_true(res)
  expect_true(file.exists(file.path(dir, ".cline/skills/lgr/SKILL.md")))
})


test_that("use_skill: alphabetical tie-break, simple case", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create(".windsurf")
  dir.create(".cursor")

  expect_message(
    res <- use_skill(),
    "Installing lgr skill for 'cursor' \\(highest priority\\)"
  )
  expect_true(res)
  expect_true(file.exists(file.path(dir, ".cursor/skills/lgr/SKILL.md")))
})


test_that("use_skill: github-copilot's narrower marker avoids false-positive detection", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create(".github", recursive = TRUE)
  writeLines("name: CI", file.path(".github", "workflow.yml"))

  expect_message(
    res <- use_skill(),
    "No existing agent config directories found"
  )
  expect_true(res)
  expect_true(file.exists(file.path(dir, ".claude/skills/lgr/SKILL.md")))
  expect_false(file.exists(file.path(dir, ".github/skills/lgr/SKILL.md")))
})


test_that("use_skill: github-copilot IS detected when .github/skills/ exists", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create(file.path(".github", "skills"), recursive = TRUE)

  expect_message(
    res <- use_skill(),
    "Installing lgr skill for 'github-copilot'"
  )
  expect_true(res)
  expect_true(file.exists(file.path(dir, ".github/skills/lgr/SKILL.md")))
})


test_that("use_skill: explicit `agent` overrides detection", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create(".claude")
  dir.create(".cursor")

  res <- use_skill(agent = "codex")

  expect_true(res)
  expect_true(file.exists(file.path(dir, ".codex/skills/lgr/SKILL.md")))
  expect_false(file.exists(file.path(dir, ".claude/skills/lgr/SKILL.md")))
})


test_that("use_skill: explicit `agent` is honored even when its marker is absent", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  res <- use_skill(agent = "gemini")

  expect_true(res)
  expect_true(file.exists(file.path(dir, ".gemini/skills/lgr/SKILL.md")))
})


test_that("use_skill: unrecognized `agent` is rejected and writes nothing", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  expect_error(use_skill(agent = "some-unknown-tool"), "must be one of")
  expect_length(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE), 0)
})


test_that("use_skill: explicit `path` overrides both `agent` and detection", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create(".claude")

  res <- use_skill(agent = "cursor", path = "custom/location.md")

  expect_true(res)
  expect_true(file.exists(file.path(dir, "custom/location.md")))
  expect_false(file.exists(file.path(dir, ".cursor/skills/lgr/SKILL.md")))
  expect_false(file.exists(file.path(dir, ".claude/skills/lgr/SKILL.md")))
})


test_that("use_skill: `path` without extension, not yet existing, is treated as a directory", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  res <- use_skill(path = "some/other/dir")

  expect_true(res)
  expect_true(file.exists(file.path(dir, "some/other/dir/SKILL.md")))
})


test_that("use_skill: `path` with extension, not yet existing, is treated as a literal file", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  res <- use_skill(path = "some/other/dir/CUSTOM_NAME.md")

  expect_true(res)
  expect_true(file.exists(file.path(dir, "some/other/dir/CUSTOM_NAME.md")))
})


test_that("use_skill: `path` pointing at an already-existing directory (no extension) is treated as a directory", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create("existing_dir")

  res <- use_skill(path = "existing_dir")

  expect_true(res)
  expect_true(file.exists(file.path(dir, "existing_dir/SKILL.md")))
})


test_that("use_skill: overwrite = FALSE (default) skips an existing destination", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  use_skill(path = "SKILL.md")
  writeLines("custom edits", "SKILL.md")

  expect_message(
    res <- use_skill(path = "SKILL.md"),
    "already exists.*Set `overwrite = TRUE`"
  )
  expect_false(res)
  expect_identical(readLines("SKILL.md"), "custom edits")
})


test_that("use_skill: overwrite = TRUE replaces an existing destination", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  use_skill(path = "SKILL.md")
  writeLines("custom edits", "SKILL.md")

  expect_message(
    res <- use_skill(path = "SKILL.md", overwrite = TRUE),
    "Installed lgr skill file"
  )
  expect_true(res)
  expect_false(identical(readLines("SKILL.md"), "custom edits"))
})
