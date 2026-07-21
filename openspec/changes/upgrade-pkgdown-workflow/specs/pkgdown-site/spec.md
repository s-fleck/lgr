## ADDED Requirements

### Requirement: CI deployment matches current r-lib/actions template
The pkgdown site's build/deploy workflow SHALL follow the current r-lib/actions reference template (`examples/pkgdown.yaml`): building with `pkgdown::build_site_github_pages()` and publishing the `docs/` output by pushing to the `gh-pages` branch via a SHA-pinned `JamesIves/github-pages-deploy-action`, using current action versions (`actions/checkout@v6`, `r-lib/actions/setup-pandoc@v2`, `r-lib/actions/setup-r@v2`, `r-lib/actions/setup-r-dependencies@v2`).

#### Scenario: Push to master builds and deploys the site
- **WHEN** a commit is pushed to `master`
- **THEN** the workflow builds the pkgdown site and pushes the built output to the `gh-pages` branch

#### Scenario: Pull request builds without deploying
- **WHEN** a pull request is opened or updated
- **THEN** the workflow builds the pkgdown site to validate it, but the deploy step does not run

#### Scenario: Manual dispatch or release also builds and deploys
- **WHEN** the workflow is triggered manually (`workflow_dispatch`) or by a published release
- **THEN** the workflow builds and deploys the site the same as a push to `master`

#### Scenario: Deploy action is pinned by commit SHA
- **WHEN** the workflow's deploy step is inspected
- **THEN** `JamesIves/github-pages-deploy-action` is referenced by a full commit SHA (with the version as a trailing comment), not a floating version tag

### Requirement: Reference index reflects current exports
The `_pkgdown.yml` `reference` section SHALL list every currently exported object from the package and SHALL NOT list objects that are no longer exported.

#### Scenario: Reference index checked against NAMESPACE
- **WHEN** the `_pkgdown.yml` reference index is reviewed as part of this change
- **THEN** every name in `NAMESPACE`'s `export()`/`S3method()` entries that should appear in documentation is present in exactly one `reference` section, and no listed name is absent from `NAMESPACE`

### Requirement: Pull requests get a preview deployment
For an open, same-repo (non-fork) pull request, the workflow SHALL build the pkgdown site and publish it to `preview/pr-<number>/` on the `gh-pages` branch, and SHALL post (or update) a comment on the pull request linking to the preview URL. The preview folder SHALL be removed when the pull request is closed.

#### Scenario: Same-repo PR gets a preview and a comment
- **WHEN** a pull request from a branch of the same repository is opened or updated
- **THEN** the workflow builds the site, deploys it to `preview/pr-<number>/` on `gh-pages` without altering the site root or other PRs' preview folders, and creates or updates a single PR comment containing the preview URL

#### Scenario: Fork PR does not attempt a preview deployment
- **WHEN** a pull request from a fork is opened or updated
- **THEN** the workflow does not attempt to deploy a preview or post a preview-link comment (since a fork PR's `GITHUB_TOKEN` cannot have write access)

#### Scenario: Preview is removed on PR close
- **WHEN** a pull request with an existing `preview/pr-<number>/` folder is closed (merged or not)
- **THEN** the workflow removes that folder from the `gh-pages` branch

### Requirement: Site favicons are generated
The pkgdown site SHALL have favicon assets generated from the package logo via `pkgdown::build_favicons()`, committed to the repository so they are available at build time.

#### Scenario: Favicons present after build
- **WHEN** the pkgdown site is built
- **THEN** favicon files generated from the package logo are included in the built site's output and referenced by the generated HTML
