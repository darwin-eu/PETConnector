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

initPerinatal <- function(cdm, perinatalSchema, perinatalTable) {
  cdm$peri_et <- dplyr::tbl(
    src = attr(cdm, "dbcon"),
    CDMConnector::inSchema(schema = perinatalSchema, table = perinatalTable)
  ) %>%
    dplyr::compute(name = CDMConnector::inSchema(attr(cdm, "write_schema"), "peri_et"), temporary = FALSE, overwrite = TRUE)
  return(cdm)
}

createPerinatalCohortFromTbl <- function(cdm) {
  cdm$child_cohort <- cdm$peri_et %>%
    dplyr::left_join(
      cdm[["pregnancy_duplicate_map"]] %>%
        dplyr::select(removed_pregnancy_id, kept_pregnancy_id),
      by = c("pregnancy_id" = "removed_pregnancy_id")
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

initPerinatalCohort <- function(cdm, outputDir, relationshipConceptId = c(40485452, 4285883)) {
  pregnancyCols <- colnames(cdm$pregnancy_cohort)

  cdm$child_cohort <- cdm$fact_relationship %>%
    dplyr::filter(.data$relationship_concept_id %in% relationshipConceptId) %>%
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
      cohort_start_date = "cohort_end_date.y",
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

  write.csv(attrition, file = file.path(outputDir, "child_cohort-attrition.csv"))

  return(cdm)
}

filterNegativeAges <- function(tbl) {
  cols <- colnames(tbl)

  tbl %>%
    PatientProfiles::addAge(indexDate = "pregnancy_end_date") %>%
    dplyr::filter(!.data$age < 0) %>%
    dplyr::select(dplyr::any_of(cols)) %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
    omopgenerics::recordCohortAttrition(reason = "Filter out children with negative age")
}

filterNoParent <- function(tbl) {
  tbl %>%
    dplyr::filter(!is.na(.data$parent_id)) %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
    omopgenerics::recordCohortAttrition(reason = "Filter out children with no parent")
}

filterLiveBirth <- function(tbl){
  tbl %>%
    dplyr::filter(.data$pregnancy_outcome == 4092289) %>%
    omopgenerics::recordCohortAttrition(reason = "Filter to live birth")
}

filterMultipleParents <- function(tbl) {
  # Filter multiple pregnancies
  temp_n_parents <- tbl %>%
    dplyr::group_by(.data$subject_id, .data$parent_id) %>%
    dplyr::summarise(n_parents = dplyr::n()) %>%
    dplyr::filter(.data$n_parents == 1)

  tbl %>%
    dplyr::inner_join(temp_n_parents, by = "subject_id") %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
    omopgenerics::recordCohortAttrition(reason = "Filter children from multiple parents")
}

filterPregnancyCohort <- function(tbl) {


  tbl %>%
    dplyr::filter(pregnancy_id %in% keptIds) %>%
    dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
    omopgenerics::recordCohortAttrition(reason = "Filter only children with parent in pregnancy cohort")
}

#' createPerinatalCohort
#'
#' @param cdm (`cdm_reference`)
#' @param perinatalSchema (`character(1)`)
#' @param perinatalTable (`character(1)`)
#' @param relationshipConceptId (`numeric(1)`: `NULL`) When set to `NULL` it
#' will use the specified Perinatal Extension Table. When concepts are
#' provided, it will use the concept_relationship table to link persons with
#' the provided relationship concept to the Pregnancy Extension Table.
#' @param outputDir (`character(1)`)
#'
#' @returns `cdm_reference`
#' @export
createPerinatalCohort <- function(cdm, perinatalSchema, perinatalTable, outputDir, relationshipConceptId = c(40485452, 4285883)) {

  if (is.null(relationshipConceptId)) {
    cdm <- initPerinatal(cdm = cdm, perinatalSchema = perinatalSchema, perinatalTable = perinatalTable)
    cdm <- createPerinatalCohortFromTbl(cdm = cdm)

    keptIds <- cdm$pregnancy_cohort %>%
      select(pregnancy_id) %>%
      pull()

    cdm$child_cohort <- cdm$child_cohort %>%
      dplyr::filter(pregnancy_id %in% keptIds) %>%
      dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE) %>%
      omopgenerics::recordCohortAttrition(reason = "Filter only children with parent in pregnancy cohort") %>%
      dplyr::distinct() %>%
      omopgenerics::recordCohortAttrition("Removing duplicate children") %>%
      filterLiveBirth() %>%
      select(-person_id) %>%
      dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE)



  } else {
    cdm <- initPerinatalCohort(cdm = cdm, outputDir = outputDir, relationshipConceptId = relationshipConceptId)

    cdm$child_cohort <- cdm$child_cohort  %>%
      dplyr::compute(name = "child_cohort", temporary = FALSE, overwrite = TRUE)
  }

  return(cdm)
}
