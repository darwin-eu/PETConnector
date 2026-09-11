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
