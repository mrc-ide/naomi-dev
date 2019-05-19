
<!-- README.md is generated from README.Rmd. Please edit that file -->

# naomi-dev

Development version of model for joint small-area estimation of HIV
prevalence, ART coverage, and HIV incidence, provisionally named
*Naomi*.

## Data

The relational diagramme below summarises the data elements required for
model inputs.

<p align="center">

<img src="data-raw/data-model.png" width="75%" />

</p>

The directory `data-raw` contains scripts for preparing data inputs from
primary data sources. Following the relational model, data inputs are
recommended to be prepared in the following order:

  - `data-raw/shapefile/`: creates the location hierarchy and area
    boundaries.
  - `data-raw/population/`: creates population by area, sex, and age
    group, and aggregated population (15-49, 15-64, 15+).
  - `data-raw/survey/`: analyses household survey data for area-level
    survey estimates of prevalence, ART coverage, viral load
    suppression, and recent HIV infection.
  - `data-raw/programme/`: creates datasets for for number receiving ART
    and routine antenatal HIV testing (ANC-RT).

*District-level data for MPHIA 2015-16 are **simulated** for
illustrative purposes by randomly allocating survey clusters within each
stratum to districts proportionally to population size. See script
`data-raw/surveys/surveys.R` for simulated district allocation.*

The directory `data` contains processed data inputs:

  - `data/shapefile.rda`: shapefile for area boundaries and hierarchy.
  - `data/population.rda`: two datasets *pop\_agesex* for population
    stratified by area, sex, and 5-year age-group, and *pop\_aggr* with
    area-level aggregate population.
  - `data/survey.rda`: survey estimates for HIV prevalence, ART
    coverage, VLS, and recent infection at all levels of location
    hierarchy and various age/sex stratifications.
  - `data/programme.rda`: aggregated area-level data for ART and ANC-RT
    by quarter.
