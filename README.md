# farmrisk_dev

Development repository for the FARMR!SK algorithm. This repo contains the modular components and developer tools used to build and run the farmrisk algorithm locally. The app-ready repository can be found at: <https://github.com/VetEpi-UAB/farmrisk>.

This algorithm uses the [`mcmodule`](https://nataliaciria.github.io/mcmodule/index.html) R package for modular Monte Carlo risk analysis. Installing the latest version from the GitHub repository is recommended. Before reinstalling packages from GitHub, first remove them using remove.packages() and then restart R.

## Repository layout

-   `R/`: custom functions, most of them are documented using roxygen2 standards.
-   `unified_script/`: .R scripts generated from .qmd modules, used to generate the unified script for each model (e.g. `cattle.R`)
-   `input_files/`:
    -   `admin/`: files provided by the model developers
        -   `key_dictionary.csv`: conversion between machine-readable keys and human-readable literals. Also indicates the key type and, if it's a factor level, the parent factor key. The default literal `lit` is English. Translations to Spanish (`lit_esp`), Catalan (`lit_cat`), and other languages will be provided.
        -   `administrative_levels.csv`: list of regional administrative levels for Andorra, Austria, Belgium, Germany, Spain, Estonia, Finland, France, Greece, Italy, Luxembourg, Poland, and Portugal. Includes [GADM](https://www.notion.so/Code-sandbox-14a8e979bd6980e29fc0e642ae1785b8?pvs=21) hierarchy, names, and GID codes for municipalities (level 3), provinces (level 2), regions (level 1), and countries (level 0), or their equivalent levels. Note that some municipality (level 3) names are missing.
        -   Model-specific files (e.g., cattle). Includes information needed to run the farmrisk algorithm for the selected species using the mcmodule framework:
            -   `[model]_core_exp.R`
            -   `[model]_link_exp.R`
            -   `[model]_data_keys.R`
            -   `[model]_mctable.R`
            -   `data/`
    -   `user/`: only needed to run the model locally. Should contain a directory named after the farm_id, which includes the farm's bsg and mov files.
-   `output_files/`: default location for storing model outputs when run locally. A file with the farm_id will be created.
-   `forms/`: JSON files to create SurveyJS forms, their version change logs, and migrations
-   `cattle.R`: unified script that runs the cattle farm biosecurity risk analysis model
-   `run_farmrisk_local.R`: unified script that runs farmrisk from local user inputs in the `input_files/user/` directory
-   `run_farmrisk.R`: unified script that runs farmrisk from user inputs retrieved from an SQL database (see connect to database)

## Connect to SQL database

\[...\]

## Process flow

### `farmrisk` developemt

\[...\]

### FARMR!SK app

\[...\]

## Contact

For questions or bug reports, please open an issue or contact Natalia Ciria ([Natalia.Ciria\@uab.cat](mailto:Natalia.Ciria@uab.cat))

## License

![License: CC BY-SA 4.0](https://img.shields.io/badge/License-CC_BY--SA_4.0-lightgrey.svg)

## Acknowledgements

`farmrisk` was developed with support from:

-   [BIOSECURE](https://biosecure.eu/), a European Union HORIZON Europe FARM2FORK project
-   [INNOTUB](https://innotub.eu/), funded by the INTERREG POCTEFA 2021–2027 European territorial cooperation program
-   [Ministerio de Agricultura, Pesca y Alimentación de España](https://www.mapa.gob.es/es/)
-   [Universitat Autònoma de Barcelona](https://www.uab.cat/)

*Views and opinions expressed are those of the author(s) only and do not necessarily reflect those of the European Union or REA. Neither the European Union nor the granting authority can be held responsible for them.*