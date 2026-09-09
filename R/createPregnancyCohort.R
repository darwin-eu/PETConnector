initPregnancyCohort <- function(cdm, keepExtensionTable, cohortDefinitionID) {
  cdm$pregnancy_cohort <- cdm$pregnancy_extension_table %>%
    dplyr::mutate(
      cohort_definition_id = cohortDefinitionID,
      cohort_start_date = .data$pregnancy_start_date,
      cohort_end_date = .data$pregnancy_end_date
    ) %>%
    dplyr::rename(subject_id = "person_id") %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE, overwrite = TRUE) %>%
    omopgenerics::newCohortTable(.softValidation = TRUE)

  if (isFALSE(keepExtensionTable)) {
    cdm$pregnancy_extension_table <- NULL
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
    dplyr::filter(.data$pregnancy_start_date <= .data$pregnancy_end_date) %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = "Pregnancy end date > pregnancy start_date")
}

filterGestationalLength <- function(tbl, nDays_min, nDays_max) {
  tbl %>%
    dplyr::filter(!!CDMConnector::datediff("pregnancy_start_date", "pregnancy_end_date") <= nDays_max) %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = sprintf("Gestational length <= %s days", nDays_max)) %>%
    dplyr::filter(!!CDMConnector::datediff("pregnancy_start_date", "pregnancy_end_date") >= nDays_min) %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE) %>%
    omopgenerics::recordCohortAttrition(reason = sprintf("Gestational length >= %s days ", nDays_min))
}

filterMultiplePregnancies <- function(tbl, outputDir, samePregDiffDates) {
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

  utils::write.csv(removed_mapping, file.path(outputDir, "pregnancy_duplicate_map.csv"), row.names = FALSE)
  # 3 — Keep only the canonical pregnancy IDs
  kept_ids <- grouped %>% pull(kept_pregnancy_id)

  tbl %>%
    dplyr::filter(.data$pregnancy_id %in% kept_ids) %>%
    dplyr::distinct() %>%
    dplyr::compute(name = "pregnancy_cohort", temporary = FALSE, overwrite = TRUE) %>%
    omopgenerics::recordCohortAttrition(
      reason = "Removed identical pregnancy duplicates"
    )

  if (samePregDiffDates == "none") {
    tbl %>%
      dplyr::group_by(.data$subject_id, .data$pregnancy_id) %>%
      dplyr::filter(dplyr::n() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::compute(name = "pregnancy_cohort", temporary = FALSE, overwrite = TRUE) %>%
      omopgenerics::recordCohortAttrition(
        reason = "Removed identical pregnancies with differing dates"
      )
  } else if (samePregDiffDates == "earliest" | samePregDiffDates == "latest") {
    if (samePregDiffDates == "earliest") {
      sliceRecord <- dplyr::slice_min
    } else if (samePregDiffDates == "latest") {
      sliceRecord <- dplyr::slice_max
    }

    tbl %>%
      dplyr::group_by(.data$subject_id, .data$pregnancy_id) %>%
      sliceRecord(.data$pregnancy_start_date, n = 1,  with_ties = TRUE) %>% # keep ties if same start date
      dplyr::slice_max(!!CDMConnector::datediff("pregnancy_start_date", "pregnancy_end_date", interval = "day"), n = 1, with_ties = FALSE) %>% # use gest_length as tiebreaker
      dplyr::ungroup() %>%
      dplyr::compute(name = "pregnancy_cohort", temporary = FALSE, overwrite = TRUE) %>%
      omopgenerics::recordCohortAttrition(reason = sprintf("Keeping only record with %s start date for identical pregnancies with differing dates", samePregDiffDates))
  }

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
          subject_id = "person_id", "observation_period_start_date", "observation_period_end_date"
        )
    ) %>%
    dplyr::filter(
      .data$cohort_start_date >= .data$observation_period_start_date &
        .data$cohort_start_date <= .data$observation_period_end_date
    ) %>%
    dplyr::filter(
      .data$cohort_end_date >= .data$observation_period_start_date &
        .data$cohort_end_date <= .data$observation_period_end_date
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
    # tbl <- tbl %>%

    tbl %>%
      omopgenerics::recordCohortAttrition(reason = "Restrictions on pregnancy start and/or end date: NONE")
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
      omopgenerics::recordCohortAttrition(reason = sprintf("Pregnancy end <= %s", endDate))
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


filterPregnancyTable <- function(tbl, minGestationalDuration, maxGestationalDuration, samePregDiffDates, .softValidation = FALSE, outputDir) {
  if (isFALSE(.softValidation)) {
    tbl %>%
      filterInObservation() %>%
      filterStartEndDate() %>%
      filterGestationalLength(nDays_min = minGestationalDuration, nDays_max = maxGestationalDuration) %>%
      filterMultiplePregnancies(outputDir, samePregDiffDates) %>%
      omopgenerics::newCohortTable()
  } else {
    tbl %>%
      omopgenerics::newCohortTable(.softValidation = TRUE)
  } # don't use omopgenerics .softvalidation either!
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
    cdm = cdm,
    name = "pregnancy_duplicate_map",
    table = map_df,
    overwrite = TRUE,
    temporary = FALSE
  )
  return(cdm)
}

#' createPregnancyCohort
#'
#' Creates the pregnancy cohort from a specified pregnancy extension table (PET) in a specified schema
#'
#' @param cdm (`cdm_reference`) Created with i.e. `CDMConnector::cdmFromCon`.
#' @param petTable (`character(1)`) Name of the Pregnancy Extension Table.
#' @param petSchema (`character(1)`)  Name of the schema where the Pregnancy Extension Table resides
#' @param keepExtensionTable (`logical(1)`: `TRUE`) Should the intermediate table between the petTable and pregnancy_cohort be kept, default = TRUE
#' @param minGestationalDuration (`numeric(1)`: `NULL`) Minimum gestational duration to include.
#' @param maxGestationalDuration (`numeric(1)`: `308`) Maximum gestational duration to include.
#' @param minAge (`numeric(1)`: `12`) Minimum age to include.
#' @param maxAge (`numeric(1)`: `55`) Maximum age to include.
#' @param startDate (`Date(1)`: `NULL`) Earliest pregnancy start date to include, e.g. as.Date("2001-09-20", "%Y-%m-%d")
#' @param endDate (`Date(1)`: `NULL`) Latest pregnancy end date to include, e.g as.Date("10/20/21", "%m/%d/%y")
#' @param samePregDiffDates (`character(1)`: `"none"`) In the case of same subject_id and pregnancy_id, but differing start/end dates, which record to keep? "none" will drop all records, "earliest" will keep the record with the earliest pregnancy start date, and "latest" will keep the record with the latest start date. For selection of "earliest" or "latest", if there is more than one record with that start date, then the record with the greatest gestational duration for that start date will be kept.
#' @param sex (`character(2)`: `"Female"`) Sexes to include. One of or both `c("Female", "Male")`.
#' @param cohortDefinitionID (`numeric(1)`: `101`) Cohort definition id to assign to newly created cohort
#' @param outputDir (`path`) Path to output pregnancy_duplicate_map.csv to
#' @param .softValidation (`logical(1)`: `FALSE`) Should a softValidation be done? default = FALSE

#' @note A pregnancy of multiples will be recorded with one pregnancy record
#' - Multiple pregnancies of the same pregnancy_id these will be collapsed to one record.
#' - If a multiples pregnancy with different pregnancy_ids for a birthing parent is recognized, then this will be collapsed to one pregnancy record with the smallest pregnancy_id kept to represent it
#' @note A pregnancy which appears as multiple records with differing dates (same pregnancy_id, different dates) will be dropped
#' @returns (`cdm_reference`) Returns the CDM with the added cohort table.
#' @import dplyr
#' @importFrom omopgenerics newCohortTable dropSourceTable recordCohortAttrition
#' @import checkmate
#' @import PatientProfiles
#' @import CDMConnector
#' @importFrom tidyr unnest_longer
#' @importFrom stringr str_to_sentence
#' @importFrom utils write.csv
#' @importFrom purrr map2
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @export
createPregnancyCohort <- function(
    cdm,
    petTable,
    petSchema,
    keepExtensionTable = TRUE,
    cohortDefinitionID = 101,
    minGestationalDuration = 0,
    maxGestationalDuration = 308,
    minAge = 12,
    maxAge = 55,
    startDate = NULL,
    endDate = NULL,
    samePregDiffDates = "none",
    sex = "Female",
    outputDir,
    .softValidation = FALSE) {

  # Check inputs ----
  assertions <- checkmate::makeAssertCollection()

  checkmate::assertClass(x = cdm, classes = "cdm_reference", add = assertions)
  checkmate::assertClass(x = petTable, classes = "character", add = assertions) # don't need to check against names(cdm)
  checkmate::assertClass(x = petSchema, classes = "character", add = assertions) # don't need to check against (attr(cdm, "write_schema")
  checkmate::assertLogical(x = keepExtensionTable, len = 1, add = assertions)
  checkmate::assertNumber(x = cohortDefinitionID, add = assertions)
  checkmate::assertNumber(x = minGestationalDuration, finite = TRUE, add = assertions)
  checkmate::assertNumber(x = maxGestationalDuration, finite = TRUE, add = assertions)
  checkmate::assertNumber(x = minAge, upper = maxAge, finite = TRUE, add = assertions) # shouldn't be larger than provided max age
  checkmate::assertNumber(x = maxAge, lower = minAge, finite = TRUE, add = assertions) # shouldn't be smaller than provided min age
  checkmate::assertDate(x = startDate, len = 1, null.ok = TRUE, add = assertions)
  checkmate::assertDate(x = endDate, len = 1, null.ok = TRUE, add = assertions)
  checkmate::assertChoice(x = tolower(samePregDiffDates), choices = c("none", "earliest", "latest"), null.ok = FALSE, add = assertions)
  checkmate::assertSubset(x = stringr::str_to_sentence(sex), choices = c("Female", "Male"), empty.ok = FALSE, add = assertions) # throw error for null unlike assertChoice
  checkmate::assertLogical(x = .softValidation, len = 1, add = assertions)

  if (isFALSE(.softValidation)) {
    checkmate::assertPathForOutput(x = outputDir, overwrite = TRUE, add = assertions) # will overwrite pregnancy_duplicate_map.csv if one already exists there
  }

  checkmate::reportAssertions(assertions)


  # pregnancy_extension_table ----
  cdm <- attachExtensionTable(
    cdm = cdm,
    table = petTable,
    schema = petSchema,
    name = "pregnancy_extension_table"
  )

  cdm$pregnancy_extension_table <- cdm$pregnancy_extension_table %>%
    dplyr::mutate(
      pregnancy_start_date = as.Date(.data$pregnancy_start_date),
      pregnancy_end_date = as.Date(.data$pregnancy_end_date)
    )

  # pregnancy_cohort table ----
  cdm <- initPregnancyCohort(
    cdm = cdm,
    keepExtensionTable = keepExtensionTable,
    cohortDefinitionID = cohortDefinitionID
  )

  cdm$pregnancy_cohort <- cdm$pregnancy_cohort %>%
    filterPregnancyTable(
      minGestationalDuration = minGestationalDuration,
      maxGestationalDuration = maxGestationalDuration,
      samePregDiffDates = samePregDiffDates,
      outputDir = outputDir,
      .softValidation = .softValidation
    ) %>% # alt to isTRUE(.softValidation) (connection to .softValidation as arg, arg FALSE returns FALSE, arg TRUE returns TRUE)
    inclusionCriteria(
      minAge = minAge,
      maxAge = maxAge,
      sex = stringr::str_to_sentence(sex),
      startDate = startDate,
      endDate = endDate
    )

  if (isFALSE(.softValidation)) { # only if .softValidation is FALSE will filterMultiplePregnancies() be run and pregnancy_duplicate_map.csv produced
    cdm <- loadPregnancyDuplicateMap(
      cdm,
      file.path(outputDir, "pregnancy_duplicate_map.csv")
    )
  }

  return(cdm)
}
