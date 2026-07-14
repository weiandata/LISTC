# Licensing Profiles

Select the profile before adding source or publishing a generated repository.
The company-wide authority is the
[Copyright and Licensing Policy](https://github.com/weiandata/.github/blob/main/handbook/chapters/36-copyright-and-licensing-policy.md).

## R package

1. Copy `r-package/LICENSE` to the R package root.
2. Apply `r-package/DESCRIPTION-identity.txt` to `DESCRIPTION`.
3. Copy `r-package/inst/COPYRIGHTS` to `inst/COPYRIGHTS` and replace every
   bracketed package-specific field using verified dependency metadata.
4. Build the source package and verify all three files are present.

R packages use GPL version 2 or later. A repository-local license substitution
requires an approved exception.

## Proprietary project

Copy `proprietary/PROPRIETARY.md` to the repository root. Static websites and
WAEF-style internal frameworks use this profile. Keep individual content
authorship separate from company copyright ownership.

## Other project types

Do not publish until an accountable owner selects an approved profile or
records an exception under the company policy.
