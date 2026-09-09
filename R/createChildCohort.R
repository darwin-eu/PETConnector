getNumberSubjects <- function(tbl) {
  tbl %>%
    dplyr::group_by(.data$subject_id) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::count() %>%
    dplyr::pull(.data$n)
}

getNumberRecords <- function(tbl) {
  tbl %>%
    dplyr::count() %>%
    dplyr::pull(.data$n)
}

createPerinatalCohortFromTbl <- function(cdm, keepExtensionTable, cohortDefinitionID) {
  cdm$child_cohort <- cdm$perinatal_extension_table %>%
    dplyr::left_join(
      cdm[["pregnancy_duplicate_map"]] %>%
        dplyr::select("removed_pregnancy_id", "kept_pregnancy_id"),
      dplyr::join_by(pregnancy_id == removed_pregnancy_id)
      # pregnancy_id is unique! Two people cannot have the same pregnancy_id, this is a safe join
    ) %>%
    dplyr::mutate(
      pregnancy_id = dplyr::coalesce(.data$kept_pregnancy_id, .data$pregnancy_id)
    ) %>%
    dplyr::select(-c("kept_pregnancy_id")) %>%
    dplyr::compute(
      name = "child_cohort",
      temporary = FALSE,
      overwrite = TRUE
    )

  cdm$child_cohort <- cdm$child_cohort %>%
    dplyr::left_join(cdm$pregnancy_cohort, by = dplyr::join_by(pregnancy_id == pregnancy_id)) %>%
    dplyr::mutate(
      cohort_definition_id = cohortDefinitionID,
      parent_id = .data$subject_id # subject_id comes from pregnancy_cohort
    ) %>%
    dplyr::mutate(
      subject_id = .data$person_id # overwrites existing subject_id (from pregnancy_cohort) to person_id (childTable)
    ) %>%
    dplyr::left_join(cdm$observation_period, by = dplyr::join_by(subject_id == person_id)) %>%
    dplyr::mutate(
      cohort_start_date = as.Date(.data$observation_period_start_date),
      cohort_end_date = as.Date(.data$observation_period_end_date)
    ) %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
    omopgenerics::newCohortTable(
      cohortSetRef = data.frame(
        cohort_definition_id = cohortDefinitionID,
        cohort_name = "child"
      ),
      .softValidation = TRUE
    )

  if (isFALSE(keepExtensionTable)) {
    cdm$perinatal_extension_table <- NULL
  }
  return(cdm)
}

createPerinatalCohortFromFactRel <- function(cdm, childConceptIds, collapseDupRecords, cohortDefinitionID) {
  pregnancyCols <- colnames(cdm$pregnancy_cohort)

  cdm$child_cohort <- cdm[["fact_relationship"]] %>%
    dplyr::filter(.data$relationship_concept_id %in% childConceptIds) %>%
    dplyr::rename(
      subject_id = "fact_id_1",
      parent_id = "fact_id_2"
    ) %>%
    dplyr::left_join(cdm$observation_period, by = dplyr::join_by(subject_id == person_id)) %>%
    dplyr::mutate(
      cohort_definition_id = cohortDefinitionID,
      cohort_start_date = as.Date(.data$observation_period_start_date),
      cohort_end_date = as.Date(.data$observation_period_end_date)
    ) %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
    omopgenerics::newCohortTable(
      cohortSetRef = data.frame(
        cohort_definition_id = cohortDefinitionID,
        cohort_name = "child"
      ),
      .softValidation = TRUE # table will be messy, use .softValidation
    )

  attrition <- data.frame(
    reason = "Initial qualifying events",
    number_subjects = getNumberSubjects(cdm$child_cohort),
    number_records = getNumberRecords(cdm$child_cohort)
  )

  cdm$child_cohort <- cdm$child_cohort %>%
    dplyr::left_join(cdm$pregnancy_cohort, by = dplyr::join_by(parent_id == subject_id)) %>%
    dplyr::select(
      "parent_id",
      cohort_definition_id = "cohort_definition_id.x",
      "subject_id",
      cohort_start_date = "cohort_end_date.y", # y, child cohort_start_date = pregnancy cohort_end_date
      cohort_end_date = "cohort_end_date.x",
      dplyr::any_of(pregnancyCols)
    ) %>%
    dplyr::filter(!is.na(.data$pregnancy_id)) %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE)

  attrition <- dplyr::bind_rows(
    attrition,
    data.frame(
      reason = "Filter children for birthing parent in pregnancy cohort",
      number_subjects = getNumberSubjects(cdm$child_cohort),
      number_records = getNumberRecords(cdm$child_cohort)
    )
  )

  cdm$child_cohort <- cdm$child_cohort %>%
    dplyr::left_join(cdm$person, by = dplyr::join_by(subject_id == person_id)) %>%
    dplyr::mutate(year_match = !!CDMConnector::datepart(date = "pregnancy_end_date", interval = "year")) %>%
    dplyr::filter(.data$year_match == .data$year_of_birth) %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE)

  attrition <- dplyr::bind_rows(
    attrition,
    data.frame(
      reason = "Filter children where birth_year == birthing parent's year of pregnancy_end_date",
      # "Filter children correct pregnancy for birthing parent in pregnancy Cohort"
      number_subjects = getNumberSubjects(cdm$child_cohort),
      number_records = getNumberRecords(cdm$child_cohort)
    )
  )


  if (isTRUE(collapseDupRecords)) {
    cdm$child_cohort <- cdm$child_cohort %>%
      dplyr::distinct() %>%
      dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE)

    attrition <- dplyr::bind_rows(
      attrition,
      data.frame(
        reason = "Filter duplicate infants to keep only one record",
        number_subjects = getNumberSubjects(cdm$child_cohort),
        number_records = getNumberRecords(cdm$child_cohort)
      )
    )
  }

  filterDuplicateIds(cdm$child_cohort)

  attrition <- dplyr::bind_rows(
    attrition,
    data.frame(
      reason = "Filter out infants with duplicated subject_id",
      number_subjects = getNumberSubjects(cdm$child_cohort),
      number_records = getNumberRecords(cdm$child_cohort)
    )
  )

  cdm$child_cohort <- cdm$child_cohort %>%
    filterLiveBirth() %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE)

  attrition <- dplyr::bind_rows(
    attrition,
    data.frame(
      reason = "Filter to live births",
      number_subjects = getNumberSubjects(cdm$child_cohort),
      number_records = getNumberRecords(cdm$child_cohort)
    )
  )
  cdm$child_cohort <- cdm$child_cohort %>%
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") %>%
    omopgenerics::newCohortTable(
      cohortSetRef = data.frame(
        cohort_definition_id = cohortDefinitionID,
        cohort_name = "child"
      ),
      .softValidation = FALSE # table should not be messy anymore, use this sanity check
    )

  attrition <- attrition %>%
    dplyr::mutate(cohort_definition_id = cohortDefinitionID) %>%
    dplyr::mutate(reason_id = dplyr::row_number()) %>% # add in post to account for optional filter (i.e collapseDupRecords)
    dplyr::mutate(
      excluded_subjects = dplyr::lag(.data$number_subjects) - .data$number_subjects,
      excluded_records = dplyr::lag(.data$number_records) - .data$number_records,
      excluded_subjects = tidyr::replace_na(.data$excluded_subjects, 0),
      excluded_records = tidyr::replace_na(.data$excluded_records, 0)
    ) %>%
    dplyr::relocate("cohort_definition_id", "number_records", "number_subjects", "reason_id", "reason", "excluded_records", .before = "excluded_subjects")

  cdm$child_cohort %>%
    omopgenerics::newCohortTable(
      cohortSetRef = data.frame(
        cohort_definition_id = cohortDefinitionID,
        cohort_name = "child"
      ),
      cohortAttritionRef = attrition,
      .softValidation = FALSE
    )

  return(cdm)
}

filterDuplicateIds <- function(tbl) {
  multipleIds <- tbl %>% # based on subject_id only, filter OUT instances of duplicate children
    dplyr::group_by(.data$subject_id) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::collect()

  tbl <- tbl %>%
    dplyr::filter(!.data$subject_id %in% multipleIds$subject_id) %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE)
}

filterLiveBirth <- function(tbl) {
  tbl %>%
    dplyr::filter(.data$pregnancy_outcome == 4092289)
}

#' createChildCohort
#'
#' Creates the child cohort from a specified child extension table or the fact_relationship table if no child extension table and schema are provided
#'
#' @param cdm (`cdm_reference`) CDM reference object
#' @param cohortDefinitionID (`numeric(1)`: `102`) Cohort definition id to assign to newly created cohort
#' @param keepExtensionTable (`logical(1)`: `TRUE`) Keep the reference to the perinatal extension table? default = TRUE
#' @param collapseDupRecords (`logical(1)`: `TRUE`) When TRUE, collapses duplicated records to one record per child. In the case that a subject id is duplicated but the rest of the record isn't, all records for the subject_id will be filtered out
#' @param childSchema (`character(1)`: `NULL`) Name of the schema where the Child Extension Table resides
#' @param childTable (`character(1)`: `NULL`) Name of the Child Extension Table
#' @param childConceptIds (`numeric(n)`: `c(40485452, 4285883)`) Concepts to use to link the child to the parent. I.e. `40485452` = Child of subject
#' @param .softValidation (`logical(1)`: `FALSE`) Should a softValidation be done? default = FALSE
#'
#' @note After collapsing duplicate records (collapseDupRecords = TRUE) or not (collapseDupRecords = FALSE), records with identical subject_ids will be completely filtered out. Every record with that subject ID will be filtered out of child_cohort.
#'
#' @returns `cdm_reference`
#' @import dplyr
#' @import CDMConnector
#' @import PatientProfiles
#' @import checkmate
#' @importFrom omopgenerics newCohortTable recordCohortAttrition
#' @export
createChildCohort <- function(
    cdm,
    cohortDefinitionID = 102,
    childConceptIds = c(40485452, 4285883), # child -> parent ("child of subject" non-standard, "child" standard)
    childSchema = NULL,
    childTable = NULL,
    keepExtensionTable = TRUE,
    collapseDupRecords = TRUE,
    .softValidation = FALSE) {

  # Check inputs ----
  assertions <- checkmate::makeAssertCollection()

  checkmate::assertClass(x = cdm, classes = "cdm_reference", add = assertions)
  checkmate::assertNumber(x = cohortDefinitionID, add = assertions)
  checkmate::assertClass(x = childSchema, classes = "character", null.ok = TRUE, add = assertions) # don't need to check against (attr(cdm, "write_schema")
  checkmate::assertClass(x = childTable, classes = "character", null.ok = TRUE, add = assertions) # don't need to check against names(cdm)
  checkmate::assertLogical(x = collapseDupRecords, len = 1, add = assertions)

  if (is.null(childTable) & is.null(childSchema)) { # meaning, we create child_cohort from fact_relationship (parentCohortTable) + childConceptIds
    checkmate::assertNumeric(x = childConceptIds, null.ok = FALSE, add = assertions)
  }

  if (!is.null(childTable) & !is.null(childSchema)) {
    checkmate::assertLogical(x = keepExtensionTable, len = 1, add = assertions)
    checkmate::assertLogical(x = .softValidation, len = 1, add = assertions)
  }

  if ((!is.null(childTable) & is.null(childSchema)) |
    (is.null(childTable) & !is.null(childSchema))) {
    assertions$push(
      "Double check that you have provided both a childTable and childSchema if you want to create the child_cohort from the childTable."
    )
  }

  checkmate::reportAssertions(assertions)

  # Create child_cohort from childTable ----
  if (!is.null(childTable) & !is.null(childSchema)) {
    childColnames <- cdm[[childTable]] %>%
      colnames()

    cdm <- attachExtensionTable(
      cdm = cdm,
      table = childTable,
      schema = childSchema,
      name = "perinatal_extension_table"
    )

    cdm <- createPerinatalCohortFromTbl(
      cdm = cdm,
      keepExtensionTable = keepExtensionTable,
      cohortDefinitionID = cohortDefinitionID
    )

    # Check validity of child extension table
    if (isFALSE(.softValidation)) {

      keptIds <- cdm$pregnancy_cohort %>%
        dplyr::select("pregnancy_id") %>%
        dplyr::pull()

      cdm$child_cohort <- cdm$child_cohort %>%
        dplyr::filter(.data$pregnancy_id %in% keptIds) %>%
        dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
        omopgenerics::recordCohortAttrition(reason = "Filter only children with parent in pregnancy cohort")

      if(isTRUE(collapseDupRecords)) {
        cdm$child_cohort <- cdm$child_cohort %>%
          dplyr::distinct() %>%
          omopgenerics::recordCohortAttrition("Filter duplicate infants to keep only one record")
      }

      cdm$child_cohort <- cdm$child_cohort %>%
        filterDuplicateIds() %>%
        omopgenerics::recordCohortAttrition("Filter out infants with duplicated subject_id") %>%
        filterLiveBirth() %>% # cohort attrition recorded in function
        omopgenerics::recordCohortAttrition(reason = "Filter to live births")
    }

    cdm$child_cohort <- cdm$child_cohort %>%
      dplyr::select(-c("person_id")) %>%
      dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", dplyr::any_of(childColnames)) %>%
      dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE)

  # Create child_cohort from fact_relationship table ----
  } else { # will be is.null(childTable) & is.null(childSchema), we have a checkmate catch for cases when 1/2 args provided

    cdm <- createPerinatalCohortFromFactRel(
      cdm = cdm,
      childConceptIds = childConceptIds,
      collapseDupRecords = collapseDupRecords,
      cohortDefinitionID = cohortDefinitionID
    )

    cdm$child_cohort <- cdm$child_cohort %>%
      dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE)
  }

  return(cdm)
}
