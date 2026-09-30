# createChildCohort

Creates the child cohort from a specified child extension table or the
fact_relationship table if no child extension table and schema are
provided

## Usage

``` r
createChildCohort(
  cdm,
  childTable = NULL,
  childSchema = NULL,
  keepExtensionTable = TRUE,
  childCohortTableName = "child_cohort",
  pregnancyCohortTableName = "pregnancy_cohort",
  collapseDupRecords = TRUE,
  cohortDefinitionID = 102,
  childConceptIds = c(40485452, 4285883),
  .softValidation = FALSE
)
```

## Arguments

- cdm:

  (`cdm_reference`) CDM reference object

- childTable:

  (`character(1)`: `NULL`) Name of the Child Extension Table

- childSchema:

  (`character(1)`: `NULL`) Name of the schema where the Child Extension
  Table resides

- keepExtensionTable:

  (`logical(1)`: `TRUE`) Keep the reference to the perinatal extension
  table? default = TRUE

- childCohortTableName:

  (`character(1)`: `"child_cohort"`) Name to assign to to child cohort
  table

- pregnancyCohortTableName:

  (`character(1)`: `"pregnancy_cohort"`) Pregnancy cohort table name

- collapseDupRecords:

  (`logical(1)`: `TRUE`) When TRUE, collapses duplicated records to one
  record per child. In the case that a subject id is duplicated but the
  rest of the record isn't, all records for the subject_id will be
  filtered out

- cohortDefinitionID:

  (`numeric(1)`: `102`) Cohort definition id to assign to newly created
  cohort

- childConceptIds:

  (`numeric(n)`: `c(40485452, 4285883)`) Concepts to use to link the
  child to the parent. I.e. `40485452` = Child of subject

- .softValidation:

  (`logical(1)`: `FALSE`) Should a softValidation be done? default =
  FALSE

## Value

`cdm_reference`

## Note

After collapsing duplicate records (collapseDupRecords = TRUE) or not
(collapseDupRecords = FALSE), records with identical subject_ids will be
completely filtered out. Every record with that subject ID will be
filtered out of child_cohort.

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

# Create a pregnancy cohort
cdm <- PETConnector::createPregnancyCohort(
 cdm = cdm,
 petTable = "pregnancy",
 petSchema = "main",
 pregnancyCohortTableName = "pregnancy_cohort"
)

# Create a child cohort table using a child extension table
cdm <- PETConnector::createChildCohort(
 cdm = cdm,
 childTable = "infant",
 childSchema = "main",
 childCohortTableName = "child_cohort1",
 pregnancyCohortTableName = "pregnancy_cohort"
)

# Create a child cohort table using the fact_relationship table
cdm <- PETConnector::createChildCohort(
 cdm = cdm,
 childCohortTableName = "child_cohort2",
 pregnancyCohortTableName = "pregnancy_cohort"
)
}
```
