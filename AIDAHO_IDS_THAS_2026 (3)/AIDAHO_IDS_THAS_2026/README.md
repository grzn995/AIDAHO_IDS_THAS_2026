# AIDAHO_IDS_THAS_2026

Take Home Assignment - Introduction to Data Science with R & RStudio
AIDAHO - AI & Data Science Certificate Hohenheim, Summer Term 2026

Team: Team 32
Members: Filip Pasti - mnr 1091212, David Gvenetadze-  mnr 1090976

## Repository structure

```
AIDAHO_IDS_THAS_2026/
├── 00_docs/            assignment sheet and supporting documents
├── 01_data/            generated sample data and intermediate data products
├── 02_code/
│   ├── R/              analysis scripts (one per task, see below)
│   └── functions/       reusable helper functions (api_helpers.R)
├── 03_report/
│   ├── graphs/          all figures used in the report
│   └── report.pdf       final written report
├── shiny/               Shiny app source (Task 7)
├── nginx_html/          static files served by NGINX (Task 6)
├── docker-compose.yaml  container setup for NGINX + Shiny
├── nginx.conf           NGINX configuration (static dashboard + reverse proxy)
└── README.md            this file
```

## How to reproduce the results

All scripts assume the **project root** (this folder) as the R working
directory. Open `AIDAHO_IDS_THAS_2026.Rproj` in RStudio so the working
directory is set automatically, or run `setwd()` to this folder manually.

### Required R packages

```r
install.packages(c(
  "httr", "jsonlite", "dplyr", "ggplot2", "readr",
  "leaflet", "htmlwidgets", "geosphere", "shiny"
))
```

(Add `rnaturalearth`, `sf`, `h3jsr` here if Task 5 is included in your
final submission.)

### Execution order

1. `02_code/R/02_overview.R` — Task 2: API exploration & summary statistics
2. `02_code/R/ais_dynamic_sample_points.R` — Task 3: generates
   `01_data/sample_intervals.csv` and `01_data/sample_stratified.csv`
3. `02_code/R/sample_dashboard.R` — Task 4.1: interactive leaflet dashboard
4. `02_code/R/ais_dynamic_individual_paths.R` — Task 4.2: individual vessel
   tracks and ship-lock detection
5. `02_code/R/sample_html_dashboard.R` — Task 6.1: generates
   `sample_points.html` for static NGINX serving
6. `02_code/R/shiny_data_prep.R` — Task 7.3: pre-aggregates data consumed
   by `shiny/app.R`

No script requires manual edits to run, other than the working directory
(per the THAS guidelines, the only adjustment a grader should need to make
is a path/working-directory variable).

### Reproducibility

All random sampling steps use `set.seed(2026)`. Re-running
`ais_dynamic_sample_points.R` reproduces identical samples, provided the
underlying AIS database has not changed.

## Web service / deployment

- Static dashboard: `https://<your-server>/sample_points.html`
- Shiny application: `https://<your-server>/ais_app`

Both are served via Docker Compose (`docker-compose.yaml`), which starts
an NGINX container (reverse-proxying `/ais_app` to the Shiny container)
and a `rocker/shiny` container running `shiny/app.R`.

To run locally:

```bash
docker compose up --build
```

Then open `http://localhost:8080/sample_points.html` and
`http://localhost:8080/ais_app/`.

## Use of generative AI

See the "Declaration on the Use of AI Tools" section in `03_report/report.pdf`.
