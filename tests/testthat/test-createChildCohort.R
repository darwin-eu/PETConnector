cdm <- omopgenerics::insertTable(
  cdm = cdm,
  name = "pregnancy_cohort",
  table = pregnancy_cohort
)

cdm <- omopgenerics::insertTable(
  cdm = cdm,
  name = "pregnancy_duplicate_map",
  table = pregnancy_duplicate_map
)

testthat::test_that("input args are as expected", {
  expect_error(
    createChildCohort(
      cdm = cdm,
      collapseDupRecords = "TRUE", # character instead of logical
      childSchema = TRUE, # logical instead of character
      childTable = "infant",
      childConceptIds = c("40485452", "4285883"), # won't be checked, character instead of numeric
      .softValidation = NULL # NULL instead of logical
    ),
    "3 assertions failed:"
  )

  expect_error(
    createChildCohort(
      cdm = cdm,
      collapseDupRecords = TRUE,
      childSchema = "main", # provided childSchema without childTable
      childTable = NULL, # provided childTable without childSchema
      childConceptIds = c("40485452"), # won't be checked, character instead of numeric
      .softValidation = FALSE
    ),
    "1 assertions failed:"
  )

  expect_error(
    createChildCohort(
      cdm = "hello", # character instead of cdm_reference
      collapseDupRecords = 123, # numeric instead of logical,
      childSchema = NULL,
      childTable = NULL,
      childConceptIds = NULL, # NULL instead of character
      .softValidation = "FALSE" # won't be checked, character instead of logical
    ),
    "3 assertions failed:"
  )

  expect_no_error(
    createChildCohort(
      cdm = cdm,
      collapseDupRecords = FALSE,
      childSchema = "main",
      childTable = "infant",
      childConceptIds = NULL, # not used when creating from childTable
      .softValidation = TRUE
    )
  )

  expect_no_error(
    createChildCohort(
      cdm = cdm,
      collapseDupRecords = TRUE
    )
  )
})

testthat::test_that("Creating child_cohort from childTable + .softValidation = TRUE goes as expected", {
  cdm <- PETConnector::createChildCohort(
    cdm = cdm,
    childTable = "infant",
    childSchema = "main",
    .softValidation = TRUE
  )

  attrition_tbl <- omopgenerics::attrition(cdm[["child_cohort"]])

  child_cohort <- cdm[["child_cohort"]] %>%
    dplyr::collect()

  # Sanity check multiple pregnancy IDs for twins ----
  # createPerinatalCohortFromTbl() looks to kept IDs
  expect_equal(
    nrow(
      child_cohort %>%
        dplyr::filter(pregnancy_id == 71)
    ),
    3
  )

  expect_equal(
    nrow(
      child_cohort %>%
        dplyr::filter(pregnancy_id == 72)
    ), # this pregnancy_id should be dropped
    0
  )

  # With .softValidation = TRUE----
  expect_equal(
    nrow(child_cohort),
    14
  )

  expect_disjoint(
    "person_id",
    colnames(child_cohort)
  )

  # Check attrition ----
  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 1) # 1, Initial qualifying events

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    0
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    0
  )
})

testthat::test_that("Creating child_cohort from childTable + .softValidation = FALSE + collapseDupRecords = TRUE goes as expected", {
  cdm <- PETConnector::createChildCohort(
    cdm = cdm,
    childTable = "infant",
    childSchema = "main",
    collapseDupRecords = TRUE,
    .softValidation = FALSE
  )

  attrition_tbl <- omopgenerics::attrition(cdm[["child_cohort"]])

  child_cohort <- cdm[["child_cohort"]] %>%
    dplyr::collect()

  # Sanity check multiple pregnancy IDs for twins ----
  # createPerinatalCohortFromTbl() looks to kept IDs
  expect_equal(
    nrow(
      child_cohort %>%
        dplyr::filter(pregnancy_id == 71)
    ),
    2
  )

  expect_equal(
    nrow(
      child_cohort %>%
        dplyr::filter(pregnancy_id == 72)
    ), # this pregnancy_id should be dropped
    0
  )

  # With .softValidation = FALSE ----
  expect_equal(
    nrow(child_cohort),
    3
  )

  expect_disjoint(
    "person_id",
    colnames(child_cohort)
  )

  # Parent is not in pregnancy cohort ----
  notInPregCohort <- child_cohort %>%
    dplyr::filter(
      pregnancy_id == 1
      | pregnancy_id == 2
      | pregnancy_id == 3
      | pregnancy_id == 7
      | pregnancy_id == 9
    )

  expect_equal(
    nrow(notInPregCohort),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 2) # 2, Filter only children with parent in pregnancy cohort

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    6 # 2 records for pregnancy 1
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    5
  )

  # Duplicate child ----
  duplicateChildExact <- child_cohort %>%
    dplyr::filter(pregnancy_id == 71 & infant_id == 17) # pregnancy_id == 8 & infant_id == 111 is filtered out downstream as a non-live birth

  expect_equal(
    nrow(duplicateChildExact),
    1
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 3) # 3, Filter duplicate infants to keep only one record

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    2 # preg 8/inf 111 & preg 71/inf 17
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    0 # 1 record per duplicate child persists
  )

  # Duplicate subject_id for child, collapseDupRecords = TRUE so these records are NOT identical ----
  duplicateSubjectId <- child_cohort %>%
    dplyr::filter(subject_id == 13)

  expect_equal(
    nrow(duplicateSubjectId),
    0 # subject_id 13 has a "bad' duplicate, all records of child 13 should be removed
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 4) # 4, Filter out infants with duplicated subject_id

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    2
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    1
  )

  # Not live birth ----
  nonLiveBirth <- child_cohort %>%
    dplyr::filter(pregnancy_id == 8 & infant_id == 111)

  expect_equal(
    nrow(nonLiveBirth),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 5) # 5, Filter to live birth

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    1
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    1
  )
})

testthat::test_that("Creating child_cohort from childTable + .softValidation = FALSE + collapseDupRecords = FALSE goes as expected", {
  cdm <- PETConnector::createChildCohort(
    cdm = cdm,
    childTable = "infant",
    childSchema = "main",
    collapseDupRecords = FALSE,
    .softValidation = FALSE
  )

  attrition_tbl <- omopgenerics::attrition(cdm[["child_cohort"]])

  child_cohort <- cdm[["child_cohort"]] %>%
    dplyr::collect()

  # Sanity check multiple pregnancy IDs for twins ----
  # createPerinatalCohortFromTbl() looks to kept IDs
  expect_equal(
    nrow(
      child_cohort %>%
        dplyr::filter(pregnancy_id == 71)
    ),
    1 # instead of being collapsed, we completely dropped preg 71/subject 17! Only preg 71/subject 18 persists
  )

  expect_equal(
    nrow(
      child_cohort %>%
        dplyr::filter(pregnancy_id == 72)
    ), # this pregnancy_id should be dropped
    0
  )

  # With .softValidation = FALSE ----
  expect_equal(
    nrow(child_cohort),
    2
  )

  expect_disjoint(
    "person_id",
    colnames(child_cohort)
  )

  # Parent is not in pregnancy cohort ----
  notInPregCohort <- child_cohort %>%
    dplyr::filter(
      pregnancy_id == 1
      | pregnancy_id == 2
      | pregnancy_id == 3
      | pregnancy_id == 7
      | pregnancy_id == 9
    )

  expect_equal(
    nrow(notInPregCohort),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 2) # 2, Filter only children with parent in pregnancy cohort

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    6 # 2 records for pregnancy 1
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    5
  )

  # Duplicate subject_id for child, collapseDupRecords = FALSE so some of these records are IDENTICAL ----
  duplicateSubjectId <- child_cohort %>%
    dplyr::filter(
      subject_id == 13
      | subject_id == 17
      | subject_id == 111
    )

  expect_equal(
    nrow(duplicateSubjectId),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 3) # 4, Filter out infants with duplicated subject_id

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    6
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    3
  )
})

testthat::test_that("Creating child_cohort from fact_relationship (parentCohortTable) goes as expected, collapseDupRecords = TRUE", {
  cdm <- PETConnector::createChildCohort(
    cdm = cdm,
    collapseDupRecords = TRUE
  )

  child_cohort <- cdm[["child_cohort"]] %>%
    dplyr::collect()

  attrition_tbl <- omopgenerics::attrition(cdm[["child_cohort"]])

  # Check of rows in final child_cohort ----
  expect_equal(
    nrow(child_cohort),
    1
  )

  # Initial qualifying events attrition check ----
  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 1) # 1, Initial qualifying events

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    0 # number_records == 25; attrition recording occurs after left_join() with observation_period with "bad" duplicate
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    0 # number_subjects == 14; c(1, 2, 3, 4, 5, 6, 7, 8, 11, 12, 13, 14, 15, 25)
  )

  # Parent not in pregnancy_cohort ----
  notInPregCohort <- child_cohort %>%
    dplyr::filter(subject_id %in% c(1, 2, 3, 4, 5, 6, 7, 8, 11, 14))
    # linked to parents c(1, 2, 3, 4, 5, 6, 7, 8, 12, 13, 15) who aren't in pregnancy_cohort

  expect_equal(
    nrow(notInPregCohort),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 2) # 2, Filter children for birthing parent in pregnancy cohort

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    19
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    10
  )

  # Parent's pregnancy end year doesn't match child's birth year ----
  birthYearMismatch <- child_cohort %>%
    dplyr::filter(subject_id == 15) # pregnancy ended 2020/baby born 2022

  expect_equal(
    nrow(birthYearMismatch),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 3) # 3, Filter children where birth_year == birthing parent's year of pregnancy_end_date

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    1
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    1
  )

  # Duplicate child, records are exactly identical ----
  duplicateChildExact <- child_cohort %>%
    dplyr::filter(subject_id == 12)

  expect_equal(
    nrow(duplicateChildExact),
    1 # one record for the duplicate should persist
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 4) # 4, Filter duplicate infants to keep only one record

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    1
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    0
  )

  # Duplicate subject_id for child, collapseDupRecords = TRUE so these records are NOT identical ----
  duplicateSubjectId <- child_cohort %>%
    dplyr::filter(subject_id == 13)

  expect_equal(
    nrow(duplicateSubjectId),
    0 # subject_id 13 has a "bad' duplicate, all records of child 13 should be removed
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 5) # 5, Filter out infants with duplicated subject_id

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    2
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    1
  )

  # Not live birth ----
  nonLiveBirth <- child_cohort %>%
    dplyr::filter(subject_id == 25) # pregnancy_id == 27

  expect_equal(
    nrow(nonLiveBirth),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 6) # 6, Filter to live births

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    1
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    1
  )
})

testthat::test_that("Creating child_cohort from fact_relationship (parentCohortTable) goes as expected, collapseDupRecords = FALSE", {
  cdm <- PETConnector::createChildCohort(
    cdm = cdm,
    collapseDupRecords = FALSE
  )

  child_cohort <- cdm[["child_cohort"]] %>%
    dplyr::collect()

  attrition_tbl <- omopgenerics::attrition(cdm[["child_cohort"]])

  # Check of rows in final child_cohort ----
  expect_equal(
    nrow(child_cohort),
    0
  )

  # Initial qualifying events attrition check ----
  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 1) # 1, Initial qualifying events

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    0 # number_records == 25; attrition recording occurs after left_join() with observation_period with "bad" duplicate
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    0 # number_subjects == 14; c(1, 2, 3, 4, 5, 6, 7, 8, 11, 12, 13, 14, 15, 25)
  )

  # Parent not in pregnancy_cohort ----
  notInPregCohort <- child_cohort %>%
    dplyr::filter(subject_id %in% c(1, 2, 3, 4, 5, 6, 7, 8, 11, 14))
    # linked to parents c(1, 2, 3, 4, 5, 6, 7, 8, 12, 13, 15) who aren't in pregnancy_cohort


  expect_equal(
    nrow(notInPregCohort),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 2) # 2, Filter children for birthing parent in pregnancy cohort

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    19
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    10
  )

  # Parent's pregnancy end year doesn't match child's birth year ----
  birthYearMismatch <- child_cohort %>%
    dplyr::filter(subject_id == 15) # pregnancy ended 2020/baby born 2022

  expect_equal(
    nrow(birthYearMismatch),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 3) # 3, Filter children where birth_year == birthing parent's year of pregnancy_end_date

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    1
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    1
  )

  # Duplicate subject_id for child, collapseDupRecords = FALSE so these records can be duplicated ----
  duplicateSubjectId <- child_cohort %>%
    dplyr::filter(subject_id == 13 | subject_id == 12)

  expect_equal(
    nrow(duplicateSubjectId),
    0 # subject_id 13 has a "bad' duplicate, all records of child 13 should be removed
    # subject_id 12 has an identical duplicate, should also be removed here when collapseDupRecords = FALSE
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 4) # 4, Filter out infants with duplicated subject_id

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    4
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    2
  )

  # Not live birth ----
  nonLiveBirth <- child_cohort %>%
    dplyr::filter(subject_id == 25) # pregnancy_id == 27

  expect_equal(
    nrow(nonLiveBirth),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 5) # 5, Filter to live births

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    1
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    1
  )
})
