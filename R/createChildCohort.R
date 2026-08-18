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

initPerinatal <- function(cdm, childSchema, childTable) {
  cdm$peri_et <- dplyr::tbl(
    src = attr(cdm, "dbcon") ,
    CDMConnector::inSchema(schema = childSchema, table = childTable)
  ) %>%
    dplyr::compute(name = CDMConnector::inSchema(attr(cdm, "write_schema"), "peri_et"), temporary = FALSE, overwrite = TRUE)
  return(cdm)
}

createPerinatalCohortFromTbl <- function(cdm) {
  cdm$child_cohort <- cdm$peri_et %>%
    dplyr::left_join(
      cdm[["pregnancy_duplicate_map"]] %>%
        dplyr::select(removed_pregnancy_id, kept_pregnancy_id),
      by = c("pregnancy_id" = "removed_pregnancy_id") # pregnancy_id is unique! Two people cannot have the same pregnancy_id, this is a safe join
    ) %>%
    dplyr::mutate(
      pregnancy_id = dplyr::coalesce(kept_pregnancy_id, pregnancy_id)
    ) %>%
    dplyr::select(-kept_pregnancy_id) %>%
    dplyr::compute(
      name = "child_cohort",
      temporary = FALSE,
      overwrite = TRUE
    )

  cdm$child_cohort <-  cdm$child_cohort %>%
    dplyr::left_join(cdm$pregnancy_cohort, by = dplyr::join_by(pregnancy_id == pregnancy_id)) %>%
    dplyr::mutate(
      cohort_definition_id = 102,
      parent_id = subject_id) %>%
    dplyr::mutate(
      subject_id = .data$infant_id
    ) %>%
    dplyr::left_join(cdm$observation_period, by = dplyr::join_by(subject_id == person_id)) %>%
    dplyr::mutate(
      cohort_start_date = as.Date(.data$observation_period_start_date),
      cohort_end_date = as.Date(.data$observation_period_end_date)
    ) %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
    omopgenerics::newCohortTable(
      cohortSetRef = data.frame(
        cohort_definition_id = 102,
        cohort_name = "child"
      ),
      .softValidation = TRUE
    )

  return(cdm)
}

initPerinatalCohort <- function(cdm, outputDir, childConceptIds, parentCohortTable) {
  pregnancyCols <- colnames(cdm$pregnancy_cohort)

  cdm$child_cohort <- cdm[[parentCohortTable]] %>%
    dplyr::filter(.data$relationship_concept_id %in% childConceptIds) %>%
    dplyr::rename(
      subject_id = "fact_id_1",
      parent_id = "fact_id_2"
    )  %>%
    dplyr::left_join(cdm$observation_period, by = dplyr::join_by(subject_id == person_id)) %>%
    dplyr::mutate(
      cohort_definition_id = 102,
      cohort_start_date = as.Date(.data$observation_period_start_date),
      cohort_end_date = as.Date(.data$observation_period_end_date)
    ) %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
    omopgenerics::newCohortTable(
      cohortSetRef = data.frame(
        cohort_definition_id = 102,
        cohort_name = "child"
      ),
      .softValidation = TRUE
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
  # omopgenerics::recordCohortAttrition(reason = "Filter children to to Pregnancy Extension Table")


  cdm$child_cohort <- cdm$child_cohort %>%
    dplyr::left_join(cdm$person, by = dplyr::join_by(subject_id == person_id)) %>%
    dplyr::mutate(year_match = !!CDMConnector::datepart(date = "pregnancy_end_date", interval = "year")) %>%
    dplyr::filter(.data$year_match == .data$year_of_birth) %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE)
  # omopgenerics::recordCohortAttrition(reason = "Filter infants where birth_year == year of pregnancy_end_date")

  # attrition <- dplyr::bind_rows(
  #   attrition,
  #   data.frame(
  #     reason = "Filter infants where birth_year == year of pregnancy_end_date",
  #     number_subjects = getNumberSubjects(cdm$child_cohort),
  #     number_records = getNumberRecords(cdm$child_cohort)
  #   )
  # )
  attrition <- dplyr::bind_rows(
    attrition,
    data.frame(
      reason = "Filter children correct pregnancy for birthing parent in pregnancy Cohort ",
      number_subjects = getNumberSubjects(cdm$child_cohort),
      number_records = getNumberRecords(cdm$child_cohort)
    )
  )

  multipleIds <- cdm$child_cohort %>%
    dplyr::group_by(.data$subject_id) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::collect()

  cdm$child_cohort <- cdm$child_cohort %>%
    dplyr::filter(!.data$subject_id %in% multipleIds$subject_id)
  # omopgenerics::recordCohortAttrition(reason = "Filter out duplicate infants")

  attrition <- dplyr::bind_rows(
    attrition,
    data.frame(
      reason = "Filter out duplicate infants",
      number_subjects = getNumberSubjects(cdm$child_cohort),
      number_records = getNumberRecords(cdm$child_cohort)
    )
  )

  cdm$child_cohort <- cdm$child_cohort %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
    omopgenerics::newCohortTable(
      cohortSetRef = data.frame(
        cohort_definition_id = 102,
        cohort_name = "child"
      )
    ) %>%
    filterLiveBirth()

  attrition <- dplyr::bind_rows(
    attrition,
    data.frame(
      reason = "Filter to live births",
      number_subjects = getNumberSubjects(cdm$child_cohort),
      number_records = getNumberRecords(cdm$child_cohort)
    )
  )

  write.csv(attrition, file = file.path(outputDir, "child_cohort-attrition.csv"), row.names = FALSE)

  return(cdm)
}

# filterNegativeAges <- function(tbl) {
#   cols <- colnames(tbl)
#
#   tbl %>%
#     PatientProfiles::addAge(indexDate = "pregnancy_end_date") %>%
#     dplyr::filter(!.data$age < 0) %>%
#     dplyr::select(dplyr::any_of(cols)) %>%
#     dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
#     omopgenerics::recordCohortAttrition(reason = "Filter out children with negative age")
# }

# filterNoParent <- function(tbl) {
#   tbl %>%
#     dplyr::filter(!is.na(.data$parent_id)) %>%
#     dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
#     omopgenerics::recordCohortAttrition(reason = "Filter out children with no parent")
# }

filterLiveBirth <- function(tbl){
  tbl %>%
    dplyr::filter(.data$pregnancy_outcome == 4092289)
}

# filterMultipleParents <- function(tbl) {
#   # Filter multiple pregnancies
#   temp_n_parents <- tbl %>%
#     dplyr::group_by(.data$subject_id, .data$parent_id) %>%
#     dplyr::summarise(n_parents = dplyr::n()) %>%
#     dplyr::filter(.data$n_parents == 1)
#
#   tbl %>%
#     dplyr::inner_join(temp_n_parents, by = "subject_id") %>%
#     dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
#     omopgenerics::recordCohortAttrition(reason = "Filter children from multiple parents")
# }

# filterPregnancyCohort <- function(tbl) {
#   tbl %>%
#     dplyr::filter(pregnancy_id %in% keptIds) %>%
#     dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
#     omopgenerics::recordCohortAttrition(reason = "Filter only children with parent in pregnancy cohort")
# }

#' createChildCohort
#'
#' @param cdm (`cdm_reference`) CDM reference object
#' @param parentCohortTable (`cohort_table`: `NULL`) Cohort table to link the children to
#' @param childSchema (`character(1)`: `NULL`) Name of the schema where the Child Extension Table resides
#' @param childTable (`character(1)`: `NULL`) Name of the Child Extension Table
#' @param childConceptIds (`numeric(n)`: `c(40485452, 4285883)`) Concepts to use to link the child to the parent. I.e. `40485452` = Child of subject
#' @param outputDir (`path`: `NULL`) Path to output child_cohort-attrition.csv to if generating child_cohort from parentCohortTable
#' @param .softValidation (`logical(1)`: `FALSE`) Should a softValidation be done? default = FALSE
#'
#' @returns `cdm_reference`
#' @import dplyr
#' @import CDMConnector
#' @import omopgenerics
#' @import PatientProfiles
#' @import checkmate
#' @export
createChildCohort <- function(
    cdm,
    parentCohortTable = NULL,
    childSchema = NULL,
    childTable = NULL,
    childConceptIds = c(40485452, 4285883), # child -> parent ("child of subject" non-standard, "child" standard), why not 4326600?
    outputDir = NULL,
    .softValidation = FALSE
) {

  # Check inputs
  assertions <- checkmate::makeAssertCollection()

  checkmate::assertClass(x = cdm, classes = "cdm_reference", add = assertions)
  checkmate::assertClass(x = parentCohortTable, classes = "character",  null.ok = TRUE, add = assertions) # don't need to check against names(cdm)
  checkmate::assertClass(x = childSchema, classes = "character", add = assertions) # don't need to check against (attr(cdm, "write_schema")
  checkmate::assertClass(x = childTable, classes = "character", null.ok = TRUE, add = assertions) # don't need to check against names(cdm)
  checkmate::assertNumeric(x = childConceptIds, null.ok = TRUE, add = assertions)
  if (!is.null(outputDir)) {
    checkmate::assertPathForOutput(x = outputDir, overwrite = TRUE, add = assertions) # will overwrite child_cohort-attrition.csv if one already exists there
  }
  checkmate::assertLogical(x = .softValidation, len = 1, add = assertions) # fine to keep default FALSE even if using parentCohortTable

  if (
    (is.null(childTable) & is.null(parentCohortTable)) |
    (!is.null(childTable) & !is.null(parentCohortTable))
  ) {
    assertions$push(
      "You must provide either a parentCohortTable or a childTable to create a child_cohort table "
    )
  }


  if ((!is.null(parentCohortTable) & is.null(outputDir))
  ) {
    assertions$push(
      "If creating the child_cohort table from the parentCohortTable, an outputDir must be provided"
    )
  }
  checkmate::reportAssertions(assertions)

  # Create child_cohort from childTable
  if (is.null(parentCohortTable)) { # parentCohortTable condition instead of childConceptIds to keep c(40485452, 4285883) as default for childConceptIds instead of NULL    cdm <- initPerinatal(cdm , childSchema, childTable)
    cdm <- initPerinatal(cdm = cdm, childSchema = childSchema, childTable = childTable)
    cdm <- createPerinatalCohortFromTbl(cdm = cdm)

    # Check validity of child extension table
    if (isFALSE(.softValidation)) {
      keptIds <- cdm$pregnancy_cohort %>%
        dplyr::select(pregnancy_id) %>%
        dplyr::pull()

      cdm$child_cohort <- cdm$child_cohort %>%
        dplyr::filter(pregnancy_id %in% keptIds) %>%
        dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
        omopgenerics::recordCohortAttrition(reason = "Filter only children with parent in pregnancy cohort") %>%
        dplyr::distinct() %>%
        omopgenerics::recordCohortAttrition("Removing duplicate children") %>%
        filterLiveBirth() %>% # cohort attrition recorded in function
        omopgenerics::recordCohortAttrition(reason = "Filter to live birth") %>%
        dplyr::select(-person_id) %>%
        dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE)
    }


  # Create child_cohort from parentCohortTable
  } else if (!is.null(parentCohortTable) & !is.null(outputDir)) {

    cdm <- initPerinatalCohort(cdm = cdm, outputDir = outputDir, childConceptIds = childConceptIds, parentCohortTable = parentCohortTable) # parentCohortTable

    cdm$child_cohort <- cdm$child_cohort  %>%
      dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE)
  }

  return(cdm)
}
