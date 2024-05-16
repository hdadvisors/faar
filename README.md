# Fredericksburg Area Housing Gap Analysis

🚧 README under construction 🚧

## Contents

### 📁 `faar` main directory

Project configuration files:

| Configuration files      | Description                                                                                        |
|--------------------------|----------------------------------------------------------------------------------------------------|
| ├── 📒 `_quarto.yml`     | YAML config file for project                                                                       |
| ├── 🧾 `_common.R`       | R setup script with `knitr` chunk global settings, core packages,<br />and plot rendering settings |
| ├── ⚙️ `faar.Rproj`      | Project options file                                                                               |
| ├── ⚙️ `.gitignore`      | Files and folders omitted in user commits                                                          |
| ├── ⚙️ `.gitattributes`  | Enables Git Large File Storage (LFS)                                                               |
| ├── ⚙️ `.nojekyll`       | Github Pages config file                                                                           |

Quarto documents with study content:

| Quarto files                        | Description                    |
|-------------------------------------|--------------------------------|
| ├── 🔵 `index.qmd`                  | *About this study*             |
| ├── 🔵 `exec-sum.qmd`               | *Executive summary*            |
| ├── 🔵 `background.qmd`             | *Ch. 1: Background*            |
| ├── 🔵 `*.qmd`                      | \*                             |
| ├── 🔵 `*.qmd`                      | \*                             |
| └── 🔵 `*.qmd`                      | \*                             |

### ├── 📁 `data` subdirectory

Individual folders with data files (`.rds`, `.csv`, `.shp`, etc.) for each source:

| Folder                              | Description                    |
|-------------------------------------|--------------------------------|
| │ ├── 📁 `acs`                      | American Community Survey      |
| │ ├── 📁 `bls`                      | Bureau of Labor Statistics     |
| │ └── 📁 `*`                        | \*                             |

### ├── 📁 `r` subdirectory

`.R` scripts with source code for data, mapping, and other functions:

| Folder                              | Description                               |
|-------------------------------------|-------------------------------------------|
| │ ├── 🧾 `build-files.R`            | Helper functions to generate `.qmd` files |
| │ ├── 🧾 `gwrc-puma.R`              | Extract PUMA geographies for study area   |
| │ └── 🧾 `*.R`                      | \*                                        |
