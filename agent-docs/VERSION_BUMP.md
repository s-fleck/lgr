# Version Bump Procedure

Follows [Semantic Versioning](https://semver.org/). Unless explicitly told otherwise, bump the **minor** (middle) version.

Run these steps when cutting a new release.

## Steps

0. **Review open specs** — Check recently completed open specs in `openspec/changes/archive/`. If any describe user-facing changes missing from `CHANGELOG.md`, ask the user whether they should be added.

1. **Update `package.json`** — bump `version` field.

2. **Update `app.json`** — bump `expo.version` field (same value as `package.json`). Verify the two files match after editing.

3. **Update `CHANGELOG.md`**
   - Review the `[Unreleased]` section. Consolidate duplicated entries following the merge heuristics in [`CHANGELOG_STYLE_GUIDE.md`](./CHANGELOG_STYLE_GUIDE.md#merge-heuristics). Confirm all entries conform to the style guide.
   - Change `## [Unreleased]` heading to `## [x.y.z] - YYYY-MM-DD` (per [`CHANGELOG_STYLE_GUIDE.md`](./CHANGELOG_STYLE_GUIDE.md)).
   - Add a fresh empty `## [Unreleased]` section at the top of the file.

4. **Validate** — run lint and typecheck to ensure no breakage from the changelog edit or any coincidental changes.

5. **Commit** — `git commit -m "bump version to x.y.z"`
