---
name: lgr-release
description: Use when cutting a release of the lgr package — bumping the DESCRIPTION version, promoting the NEWS.md development block to a dated release, checking archived openspec changes for missing entries, tagging, and preparing the CRAN submission. Triggers on "bump the version", "cut a release", "prep for release", "submit to CRAN".
metadata:
  author: Stefan Fleck
  version: "1.0.0"
---

# lgr release

Cutting a release for the lgr package: version, `NEWS.md`, tag, CRAN. The `NEWS.md` format itself is governed by the `r-news-styleguide` skill — this skill covers the release mechanics.

## When To Use

- "Bump the version" / "cut a release" / "prep for release" / "submit to CRAN".
- Promoting the `NEWS.md` development block to a dated release heading.
- After a batch of changes accumulated under `# lgr (development version)`.

## Preconditions

- The changes to release are merged and archived under `openspec/changes/archive/`.
- The working tree is clean — the release commit carries the release edits (version, `NEWS.md`, `cran-comments.md`) and nothing else.
- The `NEWS.md` development block lists every user-visible change since the last release.

## Steps

1. **Review what shipped** — list the openspec changes archived since the last release (`openspec/changes/archive/`) and compare them with the `NEWS.md` development block. Ask the user about anything user-visible that is missing before continuing.
2. **Consolidate the entries** — per the `r-news-styleguide` skill: one entry per feature, imperative mood, area prefix, no trailing period. Internal refactors, test-only fixes, and CI churn get no entry.
3. **Promote the development block** — change `# lgr (development version)` to `# lgr x.y.z (YYYY-MM-DD)` (today's date) and add a fresh, empty `# lgr (development version)` block at the top. The new block keeps only the change-type sections that have content.
4. **Bump the version** — set `Version:` in `DESCRIPTION` to `x.y.z`, without the `.9000` suffix (that is the development suffix). Minor for user-visible changes, patch for internal- or docs-only changes.
5. **Check** — run the full test suite and `R CMD check --as-cran`; the `.github/workflows/R-CMD-check.yaml` workflow runs the same. Fix what the release breaks; unrelated findings are not this pass.
6. **Commit** — `git commit -m "Bump version to x.y.z"`.
7. **Tag** — `git tag x.y.z` on that commit. Recent releases use a bare version (`0.5.0`, `0.5.2`); older ones used a `v` prefix.
8. **Start the next development cycle** — set `Version:` in `DESCRIPTION` to `x.y.z.9000` and commit (`Bump version to x.y.z.9000`). The `NEWS.md` development block stays empty until the next change.
9. **Submit to CRAN** (only when asked):
   - Refresh `cran-comments.md` — the notes that go with the tarball.
   - Build the tarball (`R CMD build .`) and submit it.
   - Record the submission in `CRAN-SUBMISSION`: `Version:`, `Date:` (UTC), `SHA:` (the git commit SHA of the release commit).
   - Push the release commits and the tag.

## Red Flags — Stop And Reconsider

- `DESCRIPTION` `Version:` and the newest dated heading in `NEWS.md` disagree.
- The development block still lists the same feature twice.
- A release is cut with uncommitted or untested changes in the tree.
- The `.9000` development suffix leaks into a release version.
- A user-visible change from an archived openspec change is missing from `NEWS.md`.
