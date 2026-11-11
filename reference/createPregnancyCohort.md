# createPregnancyCohort

Creates the pregnancy cohort from a specified pregnancy extension table
(PET) in a specified schema

## Usage

``` r
createPregnancyCohort(
  cdm,
  petTable,
  petSchema,
  keepExtensionTable = TRUE,
  pregnancyCohortTableName = "pregnancy_cohort",
  samePregDiffDates = "None",
  cohortDefinitionID = 101,
  minGestationalDuration = 0,
  maxGestationalDuration = 308,
  minAge = 12,
  maxAge = 55,
  startDate = NULL,
  endDate = NULL,
  sex = "Female",
  outcomeIds = NULL,
  .softValidation = FALSE
)
```

## Arguments

- cdm:

  (`cdm_reference`) Created with i.e.
  [`CDMConnector::cdmFromCon`](https://darwin-eu.github.io/CDMConnector/reference/cdmFromCon.html).

- petTable:

  (`character(1)`) Name of the Pregnancy Extension Table.

- petSchema:

  (`character(1)`) Name of the schema where the Pregnancy Extension
  Table resides

- keepExtensionTable:

  (`logical(1)`: `TRUE`) Keep the reference to the pregnancy extension
  table? default = TRUE

- pregnancyCohortTableName:

  (`character(1)`: `"pregnancy_cohort"`) Name to assign to to pregnancy
  cohort table

- samePregDiffDates:

  (`character(1)`: `"None"`) In the case of same subject_id and
  pregnancy_id, but differing start/end dates, which record to keep?
  "None" will drop all records, "Earliest" will keep the record with the
  earliest pregnancy start date, and "latest" will keep the record with
  the latest start date. For selection of "Earliest" or "Latest", if
  there is more than one record with that start date, then the record
  with the greatest gestational duration for that start date will be
  kept.

- cohortDefinitionID:

  (`numeric(1)`: `101`) Cohort definition id to assign to newly created
  cohort

- minGestationalDuration:

  (`numeric(1)`: `0`) Minimum gestational duration (days) to include.

- maxGestationalDuration:

  (`numeric(1)`: `308`) Maximum gestational duration (days) to include.

- minAge:

  (`numeric(1)`: `12`) Minimum age to include.

- maxAge:

  (`numeric(1)`: `55`) Maximum age to include.

- startDate:

  (`Date(1)`: `NULL`) Earliest pregnancy start date to include, e.g.
  as.Date("2001-09-20", "%Y-%m-%d")

- endDate:

  (`Date(1)`: `NULL`) Latest pregnancy end date to include, e.g
  as.Date("10/20/21", "%m/%d/%y")

- sex:

  (`character(2)`: `"Female"`) Sexes to include. One of or both
  `c("Female", "Male")`.

- outcomeIds:

  (`numeric(n)`: `NULL`) Vector of IDs to filter pregnancy outcomes

- .softValidation:

  (`logical(1)`: `FALSE`) Should a softValidation be done? default =
  FALSE

## Value

(`cdm_reference`) Returns the CDM with the added cohort table.

## Note

A pregnancy of multiples will be recorded with one pregnancy record

- Multiple pregnancies of the same pregnancy_id these will be collapsed
  to one record.

- If a multiples pregnancy with different pregnancy_ids for a birthing
  parent is recognized, then this will be collapsed to one pregnancy
  record with the smallest pregnancy_id kept to represent it

A pregnancy which appears as multiple records with differing dates (same
pregnancy_id, different dates) will be dropped

## Examples

``` r
if (interactive()) {
# Example CDM with a pregnancy extension table
path <- system.file("exampleData", package = "PETConnector")

cdm <- TestGenerator::patientsCDM(
 pathJson = path,
 testName = "example_patients",
 cdmVersion = "5.4"
)

# Create a pregnancy cohort on default parameters
PETConnector::createPregnancyCohort(
 cdm = cdm,
 petTable = "pregnancy",
 petSchema = "main"
)
}
```
