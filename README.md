# Pertussis in Colombia, 2007–2025: code and data

Code, data and outputs for:

> Cardona D, Segura Cardona A, Segura Cardona A, Velez-Pardo P. **Pertussis in Colombia before and after the introduction of maternal pertussis vaccination, a nationwide analysis of surveillance microdata, 2007–2025.** Manuscript in preparation for *Frontiers in Public Health*, Research Topic "Bordetella Pertussis in the 21st Century: Surveillance, Immunity, and Control of a Resurgent Pathogen".

Version 1.0.0, the one used for the article, is archived in Zenodo: https://doi.org/10.5281/zenodo.24182502 (all versions: https://doi.org/10.5281/zenodo.24182501).

The study analyses the 17,021 confirmed pertussis cases notified to Colombia's national public health surveillance system (SIVIGILA, event 800) between 2007 and 2025, using the anonymised case-level microdata published by the Instituto Nacional de Salud (INS). It links them to population projections, census poverty and ethnicity data and municipal boundaries from DANE, municipal health indicators from TerriData (DNP) and WHO/UNICEF estimates of national immunization coverage (WUENIC).

## Contents

| Folder | Contents |
|---|---|
| `01_datos/raw/` | Input files as downloaded on 3 November 2026: SIVIGILA microdata, one file per year (`sivigila_800/`); DANE census multidimensional poverty (`dane_ipm/`), population by ethnicity (`dane_etnia/`) and simplified MGN 2018 municipal boundaries (`dane_mgn/`); TerriData health dimension, zipped (`terridata_salud/`); WUENIC (`wuenic/`); departmental vaccine coverage from datos.gov.co (`minsalud_coberturas/`). `checksums_md5.csv` gives the size and MD5 of every input file. |
| `01_datos/procesados/` | Processed data: the analytic case file (`tosferina_casos_2007_2025.rds` and `.csv`), population denominators (`pob_*.rds`), municipal covariates, the municipal adjacency graph (`grafo_municipios.adj`) and the model outputs used by the figures |
| `02_scripts/` | R scripts numbered in execution order; one Python script builds the reference list. Code comments are in Spanish. |
| `03_resultados/` | Result tables (CSV and `Tablas_descriptivas.xlsx`) and execution logs |
| `04_figuras/` | Figures 1–6 and S1–S6 as PDF and SVG (vector) with PNG previews |

The DANE population projections (eight files, about 195 MB) are not included because of their size. `02_scripts/01_descarga_datos.R` downloads them from DANE and checks them against `checksums_md5.csv`.

## Data sources

| Source | Files | URL |
|---|---|---|
| INS, SIVIGILA microdata, event 800 (confirmed cases) | `sivigila_800/Datos_YYYY_800.xlsx` | `https://portalsivigila.ins.gov.co/_api/Web/GetFileByServerRelativeUrl('/Microdatos/Datos_YYYY_800.xlsx')/$value` |
| DANE, population projections and back-projections based on the 2018 census, post-COVID-19 update (`_VP`) | `dane_proyecciones/` (downloaded by script) | https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/proyecciones-de-poblacion |
| DANE, municipal population by ethnic-racial self-recognition | `dane_etnia/` | same page as above |
| DANE, census multidimensional poverty index 2018, municipal annex | `dane_ipm/` | https://www.dane.gov.co/index.php/estadisticas-por-tema/pobreza-y-condiciones-de-vida/pobreza-y-desigualdad/medida-de-pobreza-multidimensional-de-fuente-censal |
| DANE, Marco Geoestadístico Nacional 2018, municipalities (ArcGIS REST service, 0.002° simplification) | `dane_mgn/` | https://portalgis.dane.gov.co/mparcgis/rest/services/MGN2018/Serv_CapaMunicipiosInt_2018/MapServer/0 |
| DNP, TerriData, health dimension (version of 7 August 2026) | `terridata_salud/` | https://terridata.dnp.gov.co/assets/docs/txt/dimensiones/TerriData_Dim5.txt.zip |
| WHO/UNICEF estimates of national immunization coverage, 2025 revision (UNICEF SDMX API) | `wuenic/` | https://sdmx.data.unicef.org |
| MinSalud, administrative vaccine coverage by department (datos.gov.co, dataset 6i25-2hdt) | `minsalud_coberturas/` | https://www.datos.gov.co/resource/6i25-2hdt |

The exact download URLs are in `02_scripts/01_descarga_datos.R`. INS revises the microdata after publication, especially for the most recent year, so a new download may differ from this snapshot. The script reports any file that no longer matches.

## Reproducing the analysis

Software: R 4.6.1 with data.table 1.18.6.1, readxl 1.5.0.1, MASS 7.3.65, lme4 2.0.6, logistf 1.26.1, INLA 26.8.7 (https://www.r-inla.org), sf 1.1.3, spdep 1.4.2, SpatialEpi 1.2.8, DHARMa 0.5.1, ggplot2 4.0.3, patchwork 1.3.2, scales 1.4.0, svglite 2.2.2, ragg 1.5.2, png 0.1.9, jsonlite 2.0.0, openxlsx 4.2.9, officer 0.7.7 and flextable 0.10.3. INLA models run with 4 threads, and simulation-based intervals use fixed seeds; with several threads, INLA estimates can differ in the last reported decimal between runs. Posterior samples of the age–period–cohort model are drawn with a fixed seed on one thread.

From the repository root:

```bash
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8   # file names and data contain accented characters
Rscript 02_scripts/01_descarga_datos.R        # DANE projections and unzipped TerriData (--verificar only checks the URLs)
Rscript 02_scripts/02_poblacion.R             # population denominators
Rscript 02_scripts/03_sivigila_limpieza.R     # analytic case file
Rscript 02_scripts/04_descriptivo.R           # Table 1 and age-specific rate ratios
Rscript 02_scripts/08_calidad_datos.R         # data quality, duplicate flags, TerriData covariates
Rscript 02_scripts/09_series_tiempo.R         # expected cases, 2020–2024 deficit and 2025 excess
Rscript 02_scripts/10_edad_modelos.R          # age models, ratio of rate ratios, event study, Holm and trend tests, sensitivity analyses
Rscript 02_scripts/11_cohortes_letalidad.R    # pandemic birth cohorts, Bayesian APC (identifiable contrasts), infant death models
Rscript 02_scripts/12_espacial_inequidades.R  # BYM2 models, joint antenatal-care model and sensitivity analyses, clusters, inequality
Rscript 02_scripts/13_figuras_v4.R            # Figures 1–6 and S1–S6
```

Because the processed data are included, scripts 04 to 13 also run without downloading anything. The exception is script 08, which reads TerriData unzipped into `01_datos/raw/terridata_salud/extraido/` (done by `01_descarga_datos.R`, or unzip the file by hand).

Other scripts: `00_tema_figuras.R` (figure theme, sourced by 13); `05_modelos.R` and `06_figuras.R` (preliminary models and figures of the abstract phase); `14_informe_resultados.R` (results report in Spanish); `15_referencias.py`, `16_manuscrito.R` and `17_armar_envio_frontiers.py` (reference list from PubMed, Word manuscript and supplement on the Frontiers templates, and submission package), which need the manuscript source and the submission folder, not included. `07_documentos.R`, which produced the documents of the abstract phase, is left out because it holds the co-authors' contact details.

## Analytic case file

`01_datos/procesados/tosferina_casos_2007_2025.rds` (and `.csv`), one row per notified confirmed case:

| Variables | Description |
|---|---|
| `CONSECUTIVE`, `anio`, `semana` | INS record number, epidemiological year and week of notification |
| `fec_not`, `ini_sin`, `fec_con`, `fec_hos`, `fec_def`, `fecha_nto` | Dates of notification, symptom onset, consultation, hospitalization, death and birth |
| `f_inicio` | Onset date used in the analyses (symptom onset when it is up to 120 days before consultation, otherwise consultation) |
| `edad_dias`, `edad_meses`, `edad_anios` | Age at consultation, from date of birth when consistent with the reported age |
| `grupo_edad`, `grupo_pob`, `lactante`, `cohorte` | Analysis age group (half-open intervals in days; <2 months = <61 days), age group of the population denominators, infant (<1 year), birth year |
| `sexo`, `regimen`, `etnia`, `area`, `migrante`, `gestante`, `estrato` | Sex, health insurance scheme, ethnic self-recognition, urban or rural residence, migrant, pregnant, socioeconomic stratum |
| `confirmacion` | Classification: laboratory (culture or PCR), clinical, or epidemiological link |
| `muerte`, `hospitalizado` | Death; hospitalization (not recorded in 2008–2009) |
| `ret_consulta`, `ret_notif` | Days from onset to consultation and from consultation to notification |
| `dp_res`, `mpio_res`, `dp_nom`, `mpio_nom` | Department and municipality of residence (DIVIPOLA codes and names) |
| `periodo`, `Nombre_upgd` | Analysis period; reporting health facility |
| `flag_posible_duplicado`, `flag_duplicado_probable` | Possible re-notification and probable duplicate (excluded in a sensitivity analysis) |

## Data use

The SIVIGILA microdata are open data published by INS without names or identity numbers, but they include date of birth, sex, ethnicity and municipality of residence. Do not attempt to identify individuals, and cite INS, DANE, DNP, MinSalud and WHO/UNICEF as the original sources.

## Citation and contact

Citation metadata are in `CITATION.cff`; please cite the article once published and the Zenodo archive. Corresponding authors: Doris Cardona (doris.cardona@gmail.com) and Pedro Velez-Pardo (pvelez@ces.edu.co), Medellín, Colombia.

## En español

Este repositorio contiene el código, los datos crudos y procesados, los resultados y las figuras del artículo sobre la tos ferina en Colombia, 2007–2025. Para reproducir los análisis se siguen los pasos de la sección *Reproducing the analysis*. Los documentos de trabajo en Word (resumen, propuesta, informe de resultados, manuscrito y material suplementario), el texto fuente del manuscrito y las versiones anteriores se conservan en la carpeta del proyecto y no se suben al repositorio.

## License

The code is released under the MIT License (`LICENSE`). Processed data, results and figures created by the authors are released under CC BY 4.0 (`LICENSE-DATA.md`). The raw input files in `01_datos/raw/` are copies of public data and remain under the terms of their providers.
