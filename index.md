# PETConnector

Provides tools to create cohorts from Perinatal Extension Tables (PET)
of the Observational Medical Outcomes Partnership (OMOP) Common Data
Model (CDM).

## Features

- Attaches any non-OMOP CDM table in a database to a ‘CDM Reference’
  from `CDMConnector`.
- Creates a cohort table from the Pregnancy Extension Table
- Creates a cohort table from the Infant Extension Table or from the
  fact_relationship table, linked to the Pregnancy Extension Table.

## Installation

You can install `PETConnector` with:

``` r

remotes::install_github("darwin-eu/PETConnector")
```
