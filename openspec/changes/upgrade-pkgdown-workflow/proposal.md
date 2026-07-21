## Why

The pkgdown setup for `lgr` is stale: the deploy workflow builds to a git-ignored `docs/` folder and pushes it to `gh-pages` with a third-party action (`JamesIves/github-pages-deploy-action`), which was the standard pattern before pkgdown/GitHub Pages added native support for deploying straight from Actions. The last local build (`docs/pkgdown.yml`) was made with `pkgdown 2.0.3.9000` in 2022, so the config and workflow predate several pkgdown releases and current `usethis`/r-lib best practices (native GitHub Pages deployment, `concurrency`, minimal permissions, favicons, and a leaner `_pkgdown.yml`).

## What Changes

- Update `.github/workflows/pkgdown.yaml` to match the current r-lib/actions reference template verbatim (verified live from `r-lib/actions` on 2026-07-21): `actions/checkout@v6`, drop the now-unnecessary `use-public-rspm: true` option on `setup-r`, widen the `pull_request` trigger (no branch filter, matching upstream), and pin `JamesIves/github-pages-deploy-action` to its current SHA (`d92aa235d04922e8f08b40ce78cc5442fcfbfa2f`, tag `v4.8.0`) instead of a floating tag. The deploy mechanism itself (build to `docs/`, push to `gh-pages` branch) stays the same — that is still r-lib's current recommended approach, not a legacy pattern.
- Update `_pkgdown.yml` to current pkgdown conventions: keep Bootstrap 5 + light-switch, add favicons generation, review/complete the `reference` index for completeness (all exported objects covered, no stale/removed entries).
- Regenerate favicons (`pkgdown::build_favicons()`) and commit them to `pkgdown/favicon/`.
- Verify articles/vignette and `NEWS.md`/changelog render correctly under the refreshed config.

## Capabilities

### New Capabilities
- `pkgdown-site`: Governs how the package documentation website is configured (`_pkgdown.yml`) and built/deployed (CI workflow), including reference index completeness, branding assets, and the GitHub Pages deployment mechanism.

### Modified Capabilities
(none — no existing spec covers the pkgdown site)

## Impact

- `.github/workflows/pkgdown.yaml`: action versions and trigger config updated to match the current r-lib template; deployment mechanism (gh-pages branch) unchanged.
- `_pkgdown.yml`: updated with current best-practice keys and a reviewed reference index.
- `pkgdown/favicon/*`: new generated favicon assets.
- No changes to package R code, exports, or runtime behavior. No GitHub repository settings changes needed.
