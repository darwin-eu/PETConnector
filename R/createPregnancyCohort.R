initMotherTable <- function(cdm, petName, petSchema) {
  cdm$pet <- dplyr::tbl(
    attr(cdm, "dbcon"),
    CDMConnector::inSchema(schema = petSchema, table = petName)) %>%
    dplyr::compute(name = CDMConnector::inSchema(attr(cdm, "write_schema"), "pet"), temporary = FALSE, overwrite = TRUE)

  cdm$pet <- cdm$pet %>%
    dplyr::mutate(
      pregnancy_start_date = as.Date(.data$pregnancy_start_date),
      pregnancy_end_date = as.Date(.data$pregnancy_end_date)
    )
  return(cdm)
}

initPregnancyCohort <- function(cdm, keepExtensionTable) {
  cdm$pregnancy_cohort <- cdm$pet %>%
    dplyr::mutate(
      cohort_definition_id = 101,
      cohort_start_date = .data$pregnancy_start_date,
      cohort_end_date = .data$pregnancy_end_date
    ) %>%
    dplyr::rename(subject_id = "person_id") %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE, overwrite = TRUE) %>%
    omopgenerics::newCohortTable(.softValidation = TRUE)

  if (keepExtensionTable == FALSE) {
    cdm <- omopgenerics::dropSourceTable(cdm = cdm, name = "pet")
  }

  return(cdm)
}

inclusionAge <- function(tbl, minAge, maxAge) {
  tbl %>%
    PatientProfiles::addAge() %>%
    dplyr::filter(age >= minAge & age <= maxAge) %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(
      reason = sprintf("Age at pregnancy start date in [%s, %s]", minAge, maxAge)
    )
}

inclusionSex <- function(tbl, sex) {
  tbl %>%
    PatientProfiles::addSex() %>%
    dplyr::filter(.data$sex %in% !!sex) %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = sprintf("Sex: %s", paste(sex, collapse = ", ")))
}

filterInObservationStart <- function(tbl) {
  tbl %>%
    PatientProfiles::addInObservation() %>%
    dplyr::filter(in_observation == 1) %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = "In observation at pregnancy start date")
}

filterInObservationEnd <- function(tbl) {
  tbl %>%
    PatientProfiles::addInObservation(indexDate = "cohort_end_date") %>%
    dplyr::filter(in_observation == 1) %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = "In observation at pregnancy end date")
}

filterInObservation <- function(table) {
  table %>%
    filterInObservationStart() %>%
    filterInObservationEnd()
}

filterStartEndDate <- function(tbl) {
  tbl %>%
    dplyr::filter(.data$pregnancy_start_date < .data$pregnancy_end_date) %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = "Pregnancy end date > pregnancy start_date")
}

filterGestationalLength <- function(tbl, nDays) {
  tbl %>%
    dplyr::filter(!!CDMConnector::datediff("pregnancy_start_date", "pregnancy_end_date") < nDays) %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = sprintf("Gestational length <%s days", nDays)) %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE) %>%
    dplyr::filter(.data$gestational_length_in_day > 0) %>%
    omopgenerics::recordCohortAttrition(reason = "Gestational length days > 0")
}

filterMultiplePregnancies <- function(tbl, outputDir) {
  cdm <- attr(tbl, "cdm_reference")
  # 1 — DB‑safe grouping (no list column)
  grouped_base <- tbl %>%
    dplyr::group_by(
      .data$subject_id,
      .data$pregnancy_start_date,
      .data$pregnancy_end_date
    ) %>%
    dplyr::summarise(
      kept_pregnancy_id = min(.data$pregnancy_id, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::compute()

  # 1b — Collect both the grouped keys and all pregnancy_ids
  grouped_keys <- grouped_base %>% collect()

  all_ids_local <- tbl %>%
    dplyr::select(
      subject_id,
      pregnancy_start_date,
      pregnancy_end_date,
      pregnancy_id
    ) %>%
    collect()

  # 1c — Reconstruct your original list column "all_ids"
  grouped <- grouped_keys %>%
    dplyr::group_by(
      subject_id,
      pregnancy_start_date,
      pregnancy_end_date
    ) %>%
    dplyr::mutate(
      all_ids = list(all_ids_local$pregnancy_id[
        all_ids_local$subject_id == subject_id &
          all_ids_local$pregnancy_start_date == pregnancy_start_date &
          all_ids_local$pregnancy_end_date == pregnancy_end_date
      ])
    ) %>%
    dplyr::ungroup()


  # 2 — Extract removed IDs


  removed_mapping <- grouped %>%
    dplyr::mutate(
      removed_ids = purrr::map2(all_ids, kept_pregnancy_id, ~ setdiff(.x, .y))
    ) %>%
    tidyr::unnest_longer(removed_ids, values_to = "removed_pregnancy_id") %>%
    dplyr::filter(!is.na(removed_pregnancy_id)) %>%
    dplyr::select(!all_ids) %>%
    dplyr::compute()

  write.csv(removed_mapping, file.path(outputDir, "pregnancy_duplicate_map.csv"), row.names = FALSE)
  # 3 — Keep only the canonical pregnancy IDs
  kept_ids <- grouped %>% pull(kept_pregnancy_id)

  tbl %>%
    dplyr::filter(.data$pregnancy_id %in% kept_ids) %>%
    dplyr::distinct(subject_id, pregnancy_id, .keep_all = TRUE) %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE, overwrite = TRUE) %>%
    omopgenerics::recordCohortAttrition(
      reason = "Removed identical pregnancy duplicates"
    )



  tbl %>%
    PatientProfiles::addCohortIntersectCount(
      targetCohortTable = "pregnancy_cohort",
      window = list(c(0, Inf)),
      indexDate = "pregnancy_start_date",
      censorDate = "pregnancy_end_date",
      targetStartDate = "pregnancy_start_date",
      targetEndDate = "pregnancy_end_date",
      nameStyle = "overlap"
    ) %>%
    dplyr::filter(.data$overlap <= 1) %>%
    dplyr::left_join(
      cdm$observation_period %>%
        dplyr::select(
          subject_id = "person_id", "observation_period_start_date", "observation_period_end_date")
    ) %>%
    dplyr::filter(
      .data$cohort_start_date >= .data$observation_period_start_date
      & .data$cohort_start_date <= .data$observation_period_end_date
    ) %>%
    dplyr::filter(
      .data$cohort_end_date >= .data$observation_period_start_date
      & .data$cohort_end_date <= .data$observation_period_end_date
    ) %>%
    dplyr::select(!c("observation_period_start_date", "observation_period_end_date")) %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = "No overlapping pregnancy records")

}

filterStudyPeriod <- function(tbl, startDate, endDate) {
  cdm <- attr(tbl, "cdm_reference")
  snap <- CDMConnector::snapshot(cdm = cdm)
  maxStartDate <- as.Date(snap$latest_observation_period_end_date) - 365

  if (is.null(startDate) & is.null(endDate)) {
    tbl %>%
      omopgenerics::recordCohortAttrition(reason = "No study period restrictions on pregnancy start and/or end date")
  }

  if (!is.null(startDate)) {
    tbl <- tbl %>%
      dplyr::filter(
        .data$pregnancy_start_date >= as.Date(startDate),
        .data$pregnancy_start_date < maxStartDate
      ) %>%
      omopgenerics::recordCohortAttrition(reason = sprintf("Pregnancy start >= %s and < %s (end of database - 1 year)", startDate, maxStartDate))

  }

  if (!is.null(endDate)) {
    tbl <- tbl %>%
      dplyr::filter(
        .data$pregnancy_end_date <= as.Date(endDate)
      ) %>%
      dplyr::compute(name = "pregnancy_cohort", temporary = FALSE) %>%
      omopgenerics::recordCohortAttrition(reason = "Pregnancy end <= %s)")
  }


  tbl %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE)

  # could set tbl <- tbl only for endDate then keep one compute inside !is.null(startDate) and one outside of the if statements!
  # or set tbl <- tbl for both the !is.null() conditions and just keep one compute outside the if

  # Scenarios this covers:
  # 1. Null if statement + compute, works
  # 2. Only non-Null start + compute, works
  # 3. Only non-Null end + compute, works (setting of tbl <- tbl is redundant in this case)
  # 4. non-Null start + non-Null end, works (setting of tbl <- tbl is necessary here to apply then endDate filtering on the already startDate filtered tbl)

}

inclusionCriteria <- function(tbl, minAge, maxAge, sex, startDate, endDate) {
  tbl %>%
    inclusionAge(minAge, maxAge) %>%
    inclusionSex(sex) %>%
    filterStudyPeriod(startDate, endDate)
}


filterPregnancyTable <- function(tbl, maxGestationalDuration, .softValidation = FALSE, outputDir) {
  if (isFALSE(.softValidation)) {
    tbl %>%
      filterInObservation() %>%
      filterStartEndDate() %>%
      filterGestationalLength(nDays = maxGestationalDuration) %>%
      filterMultiplePregnancies(outputDir) %>%
      omopgenerics::newCohortTable()

  } else
    tbl %>%
    omopgenerics::newCohortTable(.softValidation = TRUE) # don't use omopgenerics .softvalidation either!
}

intersectCohorts <- function(tbl1, tbl2) {
  tbl1 %>%
    dplyr::inner_join(tbl2, dplyr::join_by(subject_id == subject_id), suffix = c("", "_y")) %>%
    dplyr::select(-"cohort_definition_id_y", -"cohort_start_date_y", -"cohort_end_date_y")
}


loadPregnancyDuplicateMap <- function(cdm, csv_path) {
  map_df <- readr::read_csv(csv_path, show_col_types = FALSE)
  if (nrow(map_df) == 0) {
    map_df <- tibble::tibble(
      subject_id            = integer(),
      pregnancy_start_date  = as.Date(character()),
      pregnancy_end_date    = as.Date(character()),
      kept_pregnancy_id     = integer(),
      removed_pregnancy_id  = integer()
    )
  }
  cdm <- CDMConnector::insertTable(
    cdm        = cdm,
    name       = "pregnancy_duplicate_map",
    table      =  map_df,
    overwrite  = TRUE,
    temporary = FALSE
  )
  return(cdm)
}

#' createPregnancyCohort
#'
#' Creates the pregnancy cohort from a specified (PET) table in a specified schema
#'
#' @param cdm (`cdm_reference`) Created with i.e. `CDMConnector::cdmFromCon`.
#' @param petTable (`character(1)`) Name of the mother extention table.
#' @param petSchema (`character(1)`) Name of the schema where the mother extension table exists.
#' @param pregnancyCohortTableName (`character(1)`) Name of the mother cohort table.
#' @param cohortDefinitionId (`numeric(1)`) Cohort definitionId.
#' @param maxGestationalDuration (`numeric(1)`: `308`) Maximum gestational duration to include.
#' @param minAge (`numeric(1)`: `12`) Minimum age to include.
#' @param maxAge (`numeric(1)`: `55`) Maximum age to include.
#' @param sex (`character(2)`: `"Female"`) Sexes to include. One of, or both `c("Female", "Male")`.
#' @param startDate (`character(1)`: `NULL`) Earliest pregnancy start date to include, follow "year-month-day" format
#' @param endDate (`character(1)`: `NULL`) Latest pregnancy end date to include, follow "year-month-day" format
#'
#' @returns (`cdm_reference`) Returns the CDM with the added cohort table.
#' @export
createPregnancyCohort <- function(
    cdm,
    petName,
    petSchema,
    keepExtensionTable = TRUE,
    maxGestationalDuration = 308,
    minAge = 12,
    maxAge = 55,
    startDate = NULL,
    endDate = NULL,
    sex = "Female",
    outputDir = NULL,
    .softValidation = FALSE
) {

  cdm <- initMotherTable(
    cdm = cdm,
    petName = petName,
    petSchema = petSchema
  )

  cdm <- initPregnancyCohort(cdm = cdm, keepExtensionTable)

  cdm$pregnancy_cohort <- cdm$pregnancy_cohort %>%
    filterPregnancyTable(maxGestationalDuration, outputDir, .softValidation = isTRUE(.softValidation)) %>% # connection to .softValidation as arg (arg FALSE returns FALSE, arg TRUE returns TRUE)
    inclusionCriteria(minAge, maxAge, sex, startDate, endDate)

  if (isFALSE(.softValidation)) { # only if .softValidation is FALSE will filterMultiplePregnancies() be run and pregnancy_duplicate_map.csv produced
    cdm <- loadPregnancyDuplicateMap(
      cdm,
      file.path(outputDir, "pregnancy_duplicate_map.csv")
    )
  }


  return(cdm)

}
