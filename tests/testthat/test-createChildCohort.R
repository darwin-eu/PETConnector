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
    PETConnector::createChildCohort(
      cdm = cdm,
      cohortDefinitionID = "102", # character instead of number
      collapseDupRecords = "TRUE", # character instead of logical
      childSchema = TRUE, # logical instead of character
      childTable = "infant",
      keepExtensionTable = "FALSE", # character instead of logical
      childConceptIds = c("40485452", "4285883"), # won't be checked, character instead of numeric
      .softValidation = NULL # NULL instead of logical
    ),
    "5 assertions failed:"
  )

  expect_error(
    PETConnector::createChildCohort(
      cdm = cdm,
      cohortDefinitionID = c(105, 102), # length 2 instead of 1
      collapseDupRecords = TRUE,
      childSchema = "main", # provided childSchema without childTable
      childTable = NULL, # provided childTable without childSchema
      keepExtensionTable = "FALSE", # won't be checked, didn't provide both childTable and childSchema
      childConceptIds = c("40485452"), # won't be checked, character instead of numeric
      .softValidation = FALSE
    ),
    "2 assertions failed:"
  )

  expect_error(
    PETConnector::createChildCohort(
      cdm = "hello", # character instead of cdm_reference
      collapseDupRecords = 123, # numeric instead of logical,
      childSchema = NULL,
      childTable = NULL,
      keepExtensionTable = "TRUE", # won't be checked, didn't provide childTable and childSchema
      childConceptIds = NULL, # NULL instead of character
      .softValidation = "FALSE" # won't be checked, character instead of logical
    ),
    "3 assertions failed:"
  )

  expect_no_error(
    PETConnector::createChildCohort(
      cdm = cdm,
      collapseDupRecords = FALSE,
      childSchema = "main",
      childTable = "infant",
      keepExtensionTable = TRUE,
      childConceptIds = NULL, # not used when creating from childTable
      .softValidation = TRUE
    )
  )

  expect_no_error(
    PETConnector::createChildCohort(
      cdm = cdm,
      collapseDupRecords = TRUE
    )
  )
})

testthat::test_that("createChildCohort() is not reliant on childTable being in cdm_reference, only in database", {
  # childTable won't exist in in cdm_reference, only in db! This is why we attatchExtensionTable() and build cohort from extension
  # Check for sneaky reliance on childTable being in cdm_reference
  cdm$infant <- NULL

  cdm <- PETConnector::createChildCohort(
    cdm = cdm,
    cohortDefinitionID = 101,
    childTable = "infant",
    childSchema = "main",
    keepExtensionTable = FALSE,
    .softValidation = TRUE
  )

  # Check that child_cohort exists ----
  expect_contains(
    names(cdm),
    "child_cohort"
  )
})

testthat::test_that("keepExtensionTable = TRUE keeps perinatal_extension_table reference and that the characteristics of the table align with what is expected", {
  cdm <- PETConnector::createChildCohort(
    cdm = cdm,
    cohortDefinitionID = 101,
    childTable = "infant",
    childSchema = "main",
    keepExtensionTable = TRUE,
    .softValidation = TRUE
  )

  # Check that perinatal_extension_table exists ----
  expect_contains(
    names(cdm),
    "perinatal_extension_table"
  )

  # Collect tables to compare ----
  infant <- cdm[["infant"]] %>%
    dplyr::collect()

  perinatal_extension_table <- cdm[["perinatal_extension_table"]] %>%
    dplyr::collect()

  # Check that we have the same number of rows and columns in 'infant' and 'perinatal_extension_table' ----
  expect_identical(
    nrow(infant),
    nrow(perinatal_extension_table)
  )

  expect_identical(
    ncol(infant),
    ncol(perinatal_extension_table)
  )

  # Check that we have the same number NAs in 'infant' and 'perinatal_extension_table' ----
  expect_identical(
    sum(is.na(infant)),
    sum(is.na(perinatal_extension_table))
  )

  # Check that we have the same colnames 'infant' and 'perinatal_extension_table' ----
  expect_identical(
    colnames(infant),
    colnames(perinatal_extension_table)
  ) # an extra layer check since colnames were not changed in the function and we already checked ncols

})

testthat::test_that("keepExtensionTable = FALSE drops reference to perinatal_extension_table", {
  cdm <- PETConnector::createChildCohort(
    cdm = cdm,
    cohortDefinitionID = 101,
    childTable = "infant",
    childSchema = "main",
    keepExtensionTable = FALSE,
    .softValidation = TRUE
  )

  # Check that perinatal_extension_table does not exist ----
  expect_all_false(stringr::str_detect(names(cdm), "perinatal_extension_table"))
})

testthat::test_that("cohortDefinitionID updates to user choice", {

  # cohort_definition_id with default settings should be 101 ----
  cdm <- PETConnector::createChildCohort(
    cdm = cdm,
    childTable = "infant",
    childSchema = "main",
    keepExtensionTable = TRUE
  )

  child_cohort <- cdm[["child_cohort"]] %>%
    dplyr::collect()

  expect_all_equal(
    child_cohort$cohort_definition_id,
    102
  )

  cdm <- PETConnector::createChildCohort(
    cdm = cdm,
    collapseDupRecords = TRUE
  )

  child_cohort <- cdm[["child_cohort"]] %>%
    dplyr::collect()

  expect_all_equal(
    child_cohort$cohort_definition_id,
    102
  )

  # cohort_definition_id with non-default ----
  cdm <- PETConnector::createChildCohort(
    cdm = cdm,
    childTable = "infant",
    childSchema = "main",
    keepExtensionTable = TRUE,
    cohortDefinitionID = 405
  )
  child_cohort <- cdm[["child_cohort"]] %>%
    dplyr::collect()

  expect_all_equal(
    child_cohort$cohort_definition_id,
    405
  )

  cdm <- PETConnector::createChildCohort(
    cdm = cdm,
    cohortDefinitionID = 405,
    collapseDupRecords = TRUE
  )

  child_cohort <- cdm[["child_cohort"]] %>%
    dplyr::collect()

  expect_all_equal(
    child_cohort$cohort_definition_id,
    405
  )
})

testthat::test_that("Creating child_cohort from childTable + .softValidation = TRUE goes as expected", {
  cdm <- PETConnector::createChildCohort(
    cdm = cdm,
    cohortDefinitionID = 101,
    childTable = "infant",
    childSchema = "main",
    keepExtensionTable = TRUE,
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
    cohortDefinitionID = 101,
    childTable = "infant",
    childSchema = "main",
    keepExtensionTable = TRUE,
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
    cohortDefinitionID = 101,
    childTable = "infant",
    childSchema = "main",
    keepExtensionTable = TRUE,
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
    cohortDefinitionID = 101,
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
    cohortDefinitionID = 101,
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
