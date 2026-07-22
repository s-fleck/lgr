## 1. `_pkgdown.yml` refresh

- [x] 1.1 Diff `_pkgdown.yml`'s `reference` sections against `NAMESPACE` exports; add any missing exported objects to an appropriate section and remove any that no longer exist
- [x] 1.2 Review `_pkgdown.yml` against current pkgdown defaults/best practice (template, `development`, `home`, `navbar`) and add any missing but valuable keys; keep existing Bootstrap 5 + light-switch config and curated reference grouping
- [ ] 1.3 Generate favicons locally with `pkgdown::build_favicons()` from `man/figures/yog-logo.svg` (or a suitable PNG derived from it) and commit the generated `pkgdown/favicon/` directory — **blocked: no local R/pkgdown/magick available in this environment**

## 2. Workflow update

- [x] 2.1 Bump `actions/checkout@v4` → `actions/checkout@v6` in `.github/workflows/pkgdown.yaml`
- [x] 2.2 Remove `with: use-public-rspm: true` from the `r-lib/actions/setup-r@v2` step
- [x] 2.3 Change the `pull_request:` trigger from `branches: [main, master]` to a bare `pull_request:` (no branch filter), matching the current upstream template
- [x] 2.4 Re-pin `JamesIves/github-pages-deploy-action` from `@v4.7.2` to the SHA-pinned form `@d92aa235d04922e8f08b40ce78cc5442fcfbfa2f # v4.8.0`
- [x] 2.5 Diff the final workflow file against the live `r-lib/actions` `examples/pkgdown.yaml` template to confirm no unintended drift remains

## 3. Verification

- [ ] 3.1 If R + pkgdown are available locally, run `pkgdown::build_site()` (or `check_pkgdown()`) to confirm `_pkgdown.yml` is valid and the site builds without new warnings — **blocked: no local R available in this environment; needs to be run by a maintainer**
- [x] 3.2 Validate the updated workflow YAML for syntax correctness — manually reviewed structure against upstream template (no `actionlint`/YAML linter available in this environment); no automated linter was run
- [ ] 3.3 After merging, confirm the next push triggers a successful build + deploy to the `gh-pages` branch and that `https://s-fleck.github.io/lgr` serves the freshly built site — post-merge step, not yet done

## 4. PR preview deploys

- [x] 4.1 Widen the `pull_request` trigger to `types: [opened, synchronize, reopened, closed]` and gate the main `pkgdown` job on `github.event.action != 'closed'`
- [x] 4.2 Add a "Deploy PR preview" step (reusing the already-built `docs/` output) that deploys to `target-folder: preview/pr-<number>` with `clean: true`, guarded to same-repo (non-fork) PRs only
- [x] 4.3 Add a step using `actions/github-script` to create-or-update a single PR comment with the preview URL, guarded the same way
- [x] 4.4 Add a `pkgdown-preview-cleanup` job gated on PR-closed that checks out `gh-pages`, removes `preview/pr-<number>/` if present, and pushes the removal commit
- [ ] 4.5 Verify end-to-end on a real PR after merging: preview builds, comment appears/updates, and the folder disappears from `gh-pages` on close — post-merge step, not yet done
