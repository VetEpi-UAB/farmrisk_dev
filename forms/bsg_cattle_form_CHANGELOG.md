# bsg_cattle form CHANGELOG

Semantic Versioning: Major.Minor.Patch

-   Major: Breaking changes with no backward compatibility (e.g., changing the panel structure, adding a new page)

-   Minor: Changes with backward compatibility script (e.g., changing a question key or its options)

-   Patch: Small changes that don't need a backward compatibility script (e.g., changing question wording)

Always include the migration script path and a brief note in the CHANGELOG entry

------------------------------------------------------------------------

## v4.5.0

*2025-12-18*

-   New version format from v4_4 → v4.5.0
-   Insivible fields `version` and `survey_id` now are saved in form answers
-   Migration script: `forms/migrations/bsg_cattle_form_v4_4tov4.5.0.R`

## v4_4

*2025-11-14*

Adapted from pilot to online app

-   `farm_id` and `user_id` removed from this form, now they are automatically collected by FARMR!SK online app
-   Include last biosecurity assessment date and assessor name information
-   Included `farm_comments` for optional farm notes
-   Removed "designer mode", reinforced required responses