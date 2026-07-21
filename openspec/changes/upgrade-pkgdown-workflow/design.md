## Context

`lgr`'s pkgdown site is configured via `_pkgdown.yml` (Bootstrap 5, light-switch, a manually curated `reference` index) and deployed by `.github/workflows/pkgdown.yaml`. That workflow builds the site into a local `docs/` folder (git-ignored) with `pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)`, then pushes the folder's contents to the `gh-pages` branch using `JamesIves/github-pages-deploy-action@v4.7.2`. `docs/pkgdown.yml` shows the last real build used `pkgdown 2.0.3.9000` (2022), and GitHub Pages is currently configured (per repo settings, not tracked in-repo) to serve from the `gh-pages` branch.

Verified live against `r-lib/actions` (default branch `v2-branch`, `examples/pkgdown.yaml`, checked 2026-07-21): r-lib's current reference workflow still builds to `docs/` and deploys by pushing to the `gh-pages` branch with `JamesIves/github-pages-deploy-action` — it has **not** moved to GitHub's native Pages Actions integration (`upload-pages-artifact`/`deploy-pages`). What has changed since this repo's workflow was last touched is smaller: `actions/checkout` is now pinned at `@v6` (was `@v4`), `r-lib/actions/setup-r@v2` no longer needs `use-public-rspm: true` in the template, the `pull_request` trigger in the template has no branch filter, and the deploy action is now pinned to a commit SHA (`d92aa235d04922e8f08b40ce78cc5442fcfbfa2f`, tag `v4.8.0`) rather than a floating tag.

## Goals / Non-Goals

**Goals:**
- Bring `.github/workflows/pkgdown.yaml` byte-for-byte in line with the current r-lib/actions reference template (verified live, not from memory).
- Bring `_pkgdown.yml` up to current pkgdown conventions and confirm the reference index is complete and accurate.
- Generate/commit favicons via `pkgdown::build_favicons()`.
- Leave package code, exports, and behavior untouched — this is a docs/CI-only change.

**Non-Goals:**
- Redesigning the site's information architecture (nav structure, article set) beyond fixing staleness/omissions in the reference index.
- Changing where the package's canonical documentation URL points (`https://s-fleck.github.io/lgr` stays the same).
- Migrating to GitHub's native Pages Actions deployment — r-lib's own template doesn't do this yet, so adopting it here would be diverging from upstream best practice, not following it.

## Decisions

### Match the current r-lib/actions template exactly, keep `gh-pages` branch deploy
Update `.github/workflows/pkgdown.yaml` to be identical in substance to the live `examples/pkgdown.yaml` in `r-lib/actions`:
- `actions/checkout@v4` → `actions/checkout@v6`.
- Drop `with: use-public-rspm: true` from the `r-lib/actions/setup-r@v2` step (no longer present in the template; `setup-r-dependencies` handles repo selection).
- Change `pull_request: branches: [main, master]` → bare `pull_request:` (upstream no longer scopes this trigger to specific branches).
- Replace `JamesIves/github-pages-deploy-action@v4.7.2` with the SHA-pinned form `JamesIves/github-pages-deploy-action@d92aa235d04922e8f08b40ce78cc5442fcfbfa2f # v4.8.0`.
- Keep everything else as-is: `permissions: read-all` at workflow level with `contents: write` on the job, the `concurrency` group keyed off non-PR events, `GITHUB_PAT` env var, `pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)`, deploying `docs/` to the `gh-pages` branch with `clean: false`.
- **Alternative considered**: migrate to GitHub's native Pages Actions deployment (`upload-pages-artifact` + `deploy-pages`). Rejected after verifying live that r-lib's own current template does not do this — adopting it would mean diverging from upstream best practice on a guess, not following current recommended practice. If r-lib adopts native deployment in a future template revision, this can be revisited then.

### No GitHub Pages settings changes needed
Since the deployment mechanism (`gh-pages` branch) is unchanged, there is no repo-settings step required. The site continues to be served exactly as it is today.

### PR previews are keyed to open pull requests, not arbitrary branch pushes
A pushed feature branch with no open PR has no stable, bounded identifier and no natural place to surface a preview link, so previews are built only for `pull_request` events (`opened`/`synchronize`/`reopened`), not for `push` to arbitrary non-master branches. In practice this covers the scenario that prompted the question — a page to look at while reviewing work on a branch — since that work is reviewed through a PR.

- Reuse the same `pkgdown` job's already-built `docs/` output (don't rebuild) — add a second `JamesIves/github-pages-deploy-action` step deploying to `target-folder: preview/pr-<number>` with `clean: true` (scoped to that target folder only, per the action's v4 semantics — it does not touch the site root or other PRs' preview folders).
- Guard both the preview deploy and PR comment steps on `github.event.pull_request.head.repo.fork == false`: `GITHUB_TOKEN` is force-read-only on workflows triggered by fork PRs (a GitHub security measure that cannot be overridden from workflow YAML), so a deploy attempt from a fork PR would just fail. Skipping it outright avoids a permanently-red required check for external contributions.
- Post a PR comment with the preview URL via `actions/github-script` (first-party, no new third-party action to trust), upserting a single comment (matched by an HTML marker) rather than posting a new one on every push.
- **Alternative considered**: a separate `netlify`/Vercel-style preview service. Rejected — no such integration exists for this repo today, and reusing the existing `gh-pages` branch avoids new external accounts/secrets.

### Preview cleanup on PR close
Widen the `pull_request` trigger to include `types: [opened, synchronize, reopened, closed]` and add a separate `pkgdown-preview-cleanup` job gated on `github.event.action == 'closed'` that checks out the `gh-pages` branch, removes `preview/pr-<number>/` if present, and pushes a commit removing it. The main `pkgdown` job is gated on `github.event.action != 'closed'` so it doesn't redundantly build/deploy when a PR is merely closed.
- **Alternative considered**: leave stale preview folders on `gh-pages` forever. Rejected — unbounded growth of the branch and stale/misleading previews reachable by URL after the PR is gone.

### `_pkgdown.yml`: keep structure, refresh contents
Keep the existing Bootstrap 5 + `light-switch: true` template config (already current best practice) and the existing hand-curated `reference` sections (Logger, Appenders, Layouts, LogEvent, Filters, Utilities, Abstract Classes, Exceptions) rather than replacing them with an auto-generated index — the curated grouping is a UX intentional choice worth preserving. Concretely:
- Diff the `reference` list against current `NAMESPACE`/`man/` exports to catch anything added or removed since the list was last touched, and fix any drift.
- Add favicons by running `pkgdown::build_favicons()` locally and committing the generated `pkgdown/favicon/` directory (pkgdown auto-detects and links these; no `_pkgdown.yml` change needed beyond having the files present, provided a `man/figures/logo.png`/`logo.svg` exists to source them from — `man/figures/yog-logo.svg` will be used as the source).
- **Alternative considered**: switch to an auto-generated (non-curated) reference index. Rejected — curated grouping is more useful for this package's audience and there's no indication it's stale enough to warrant scrapping.

## Risks / Trade-offs

- [Widening the `pull_request` trigger to drop the `main`/`master` filter means the build job also runs for PRs targeting other branches] → Low risk: it's a validate-only build (deploy still gated on `github.event_name != 'pull_request'`), and matching upstream exactly is preferable to a bespoke deviation.
- [Favicon source SVG may not convert cleanly via `pkgdown::build_favicons()` (requires the `magick` package)] → Verify locally before committing generated assets; if `magick`/rsvg support is unavailable, fall back to skipping favicon generation for this change rather than committing broken assets.
- [Reference index drift fixes could accidentally omit a newly-exported symbol or leave a removed one] → Cross-check against `NAMESPACE` explicitly as a task step, not just a visual diff.

## Migration Plan

1. Update `_pkgdown.yml` and generate favicons; build the site locally (if R/pkgdown are available) to sanity-check.
2. Update `.github/workflows/pkgdown.yaml` per the Decisions section above.
3. Merge to `master`.
4. Confirm the next push triggers a successful build + deploy to the `gh-pages` branch and that `https://s-fleck.github.io/lgr` serves the freshly built site.

Rollback is a straightforward revert of the workflow/config file changes; no branch or repo-settings state is affected.

## Open Questions

- None.
