testthat::test_that("input args are as expected", {
  expect_error(
    createPregnancyCohort(
      cdm = cdm,
      petTable = "pregnancy",
      petSchema = "main",
      keepExtensionTable = "no", # should be logical TRUE/FALSE
      cohortDefinitionID = "101", # character instead of number
      minGestationalDuration = "20", # character instead of number
      maxGestationalDuration = "308", # character instead of number
      minAge = "12", # character instead of number
      maxAge = c(1, 2), # two numbers provided
      startDate = "2021-08-31", # character instead of Date
      endDate = "2020-06-28", # character instead of Date
      sex = "femle", # typo
      outputDir = "path/to/nowhere", # softValidation is not FALSE, no error!
      .softValidation = c(TRUE, FALSE) # length 2 instead of 1
    ),
    "10 assertions failed:"
  )


  expect_error(
    createPregnancyCohort(
      cdm = "hello", # of class "character" instead of "cdm_reference"
      petTable = "pregnancy",
      petSchema = "main",
      outputDir = testthat::test_path("testthat_testOutput"),
      sex = c("MaLe", "FEmale"),
      .softValidation = NULL # NULL instead of logical
    ),
    "2 assertions failed:"
  )

  expect_error(
    createPregnancyCohort(
      cdm = cdm,
      petTable = "pregnancy",
      petSchema = "main",
      outputDir = "path/to/nowhere", # bad path when .softValidation = FALSE
      sex = "Female",
      .softValidation = FALSE
    ),
    "1 assertions failed:"
  )

  expect_error(
    createPregnancyCohort(
      cdm = cdm,
      petTable = "pregnancy",
      petSchema = "main",
      keepExtensionTable = c(TRUE, FALSE), # length 2 instead of 1
      cohortDefinitionID = c(101, 102), # length 2 instead of 1
      minGestationalDuration = NULL, # NULL instead of number
      maxGestationalDuration = Inf, # infinite
      minAge = 55, # greater than maxAge
      maxAge = 12, # less than minAge
      startDate = as.Date("2021-08-31", "%Y-%m-%d"),
      endDate = NULL,
      sex = NULL, # NULL instead of "Female", "Male" or c("Female", "Male")
      outputDir = testthat::test_path("testthat_testOutput"),
      .softValidation = "FALSE" # character instead of logical
    ),
    "8 assertions failed:"
  )

  expect_error(
    createPregnancyCohort(
      cdm = cdm,
      petTable = "pregnancy",
      petSchema = "main"
      # outputDir = "testthat/testthat_testOutput"
    ),
    'argument "outputDir" is missing, with no default'
  )

  expect_no_error(
    createPregnancyCohort(
      cdm = cdm,
      petTable = "pregnancy",
      petSchema = "main",
      outputDir = outputDir # defined in setup.R
    )
  )
})

testthat::test_that("keepExtensionTable = TRUE keeps pregnancy_extension_table and that the characteristics of the table align with what is expected", {
  cdm <- createPregnancyCohort(
    cdm = cdm,
    petTable = "pregnancy",
    petSchema = "main",
    keepExtensionTable = TRUE,
    outputDir = outputDir
  )

  # Check that pregnancy_extension_table exists ----
  expect_contains(
    names(cdm),
    "pregnancy_extension_table"
  )

  # Check that we have the same number of rows and columns in 'pregnancy' and 'pregnancy_extension_table' ----
  expect_identical(
    nrow(cdm[["pregnancy"]]),
    nrow(cdm[["pregnancy_extension_table"]])
  )

  expect_identical(
    ncol(cdm[["pregnancy"]]),
    ncol(cdm[["pregnancy_extension_table"]])
  )

  # Check that we have the same number NAs in 'pregnancy' and 'pregnancy_extension_table' ----
  expect_identical(
    sum(is.na(cdm[["pregnancy"]] %>%
      dplyr::collect())),
    sum(is.na(cdm[["pregnancy_extension_table"]] %>%
      dplyr::collect()))
  )

  # Check that we have the same colnames 'pregnancy' and 'pregnancy_extension_table' ----
  expect_identical(
    colnames(cdm[["pregnancy"]]),
    colnames(cdm[["pregnancy_extension_table"]])
  ) # an extra layer check since colnames were not changed in the function and we already checked ncols

  # Check that start and end dates are of "Date" class in pregnancy_extension_table ----
  expect_type(
    cdm[["pregnancy"]] %>%
      dplyr::pull(pregnancy_start_date),
    "character"
  ) # sanity check of original

  expect_s3_class(
    cdm[["pregnancy_extension_table"]] %>%
      dplyr::pull(pregnancy_start_date),
    "Date"
  )

  expect_type(
    cdm[["pregnancy"]] %>%
      dplyr::pull(pregnancy_end_date),
    "character"
  )

  expect_s3_class(
    cdm[["pregnancy_extension_table"]] %>%
      dplyr::pull(pregnancy_end_date),
    "Date"
  )
})

testthat::test_that("keepExtensionTable = FALSE drops pregnancy_extension_table", {
  cdm <- createPregnancyCohort(
    cdm = cdm,
    petTable = "pregnancy",
    petSchema = "main",
    keepExtensionTable = FALSE,
    outputDir = outputDir
  )

  # Check that pregnancy_extension_table does not exist ----
  expect_all_false(stringr::str_detect(names(cdm), "pregnancy_extension_table"))
})

testthat::test_that("cohortDefinitionID updates to user choice", {

  # cohort_definition_id with default settings should be 101 ----
  cdm <- createPregnancyCohort(
    cdm = cdm,
    petTable = "pregnancy",
    petSchema = "main",
    outputDir = outputDir
  )

  pregnancy_cohort <- cdm[["pregnancy_cohort"]] %>%
    dplyr::collect()

  expect_all_equal(
    pregnancy_cohort$cohort_definition_id,
    101
  )

  # cohort_definition_id with non-default ----
  cdm <- createPregnancyCohort(
    cdm = cdm,
    petTable = "pregnancy",
    petSchema = "main",
    cohortDefinitionID = 404,
    outputDir = outputDir
  )
    pregnancy_cohort <- cdm[["pregnancy_cohort"]] %>%
    dplyr::collect()

  expect_all_equal(
    pregnancy_cohort$cohort_definition_id,
    404
  )

})

testthat::test_that("Filtering of pregnancy_cohort with defaults occurs as expected & pregnancy_duplicate_map.csv output file is created", {
  cdm <- createPregnancyCohort(
    cdm = cdm,
    petTable = "pregnancy",
    petSchema = "main",
    keepExtensionTable = TRUE, # default
    cohortDefinitionID = 101, # default
    minGestationalDuration = 0, # default
    maxGestationalDuration = 308, # default
    minAge = 12, # default
    maxAge = 55, # default
    startDate = NULL, # default
    endDate = NULL, # default
    sex = "Female", # default
    outputDir = outputDir,
    .softValidation = FALSE # default
  )

  attrition_tbl <- omopgenerics::attrition(cdm[["pregnancy_cohort"]])

  pregnancy_cohort <- cdm[["pregnancy_cohort"]] %>%
    dplyr::collect()

  # With .softValidation = FALSE and all set to default ----
  expect_equal(
    nrow(pregnancy_cohort),
    8
  )

  # Pregnancy starts prior to observation start ----
  outsideObsStart <- pregnancy_cohort %>%
    dplyr::filter(
      (subject_id == 20 & pregnancy_id == 13) # represents 2 records in pregnancy table, no corresponding person in person table nor observation_period
      | (subject_id == 1 & pregnancy_id == 1)
      | (subject_id == 1 & pregnancy_id == 2)
      | (subject_id == 1 & pregnancy_id == 3) # these pregnancies in pregnancy_extension_table start prior to obs start
    )

  expect_equal(
    nrow(outsideObsStart),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 2) # 2, In observation at pregnancy start dat

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    5
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    2
  )


  # Pregnancy ends after observation end ----
  outsideObsStart <- pregnancy_cohort %>%
    # These pregnancies in pregnancy_extension_table end after obs end
    dplyr::filter(subject_id == 16 & pregnancy_id == 9)

  expect_equal(
    nrow(outsideObsStart),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 3) # 3, In observation at pregnancy end date

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

  # Pregnancy start after pregnancy end ----
  pregStartAfterEnd <- pregnancy_cohort %>%
    dplyr::filter(
      (subject_id == 14 & pregnancy_id == 7)
      | (subject_id == 19 & pregnancy_id == 12)
    )

  expect_equal(
    nrow(pregStartAfterEnd),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 4) # 4, Pregnancy end date > pregnancy start_date

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    2
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    1 # subject id 14 other pregnancy still included
  )

  # Multiple birth, multiple records ----
  # Twins, two of the same pregnancy ID
  twins_samePregID <- pregnancy_cohort %>%
    dplyr::filter(subject_id == 11 & pregnancy_id == 6)

  expect_equal(
    nrow(twins_samePregID),
    1 # should collapse to ONE pregnancy for multiples
  )

  # Twins, two different pregnancy IDs
  twins_diffPregID <- pregnancy_cohort %>%
    dplyr::filter(subject_id == 102) # two different pregnancy IDs but same start and end dates (twins) (Note, pregnancy_id is unique! Two birthing parents can't have same pregnancy_id)

  expect_equal(
    nrow(twins_diffPregID),
    1 # should collapse to ONE pregnancy for multiples
  )

  expect_equal(
    twins_diffPregID %>%
      dplyr::pull(pregnancy_id),
    71 # lowest of the pregnancy IDs should be selected to keep
  )


  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 7) # 7, Removed identical pregnancy duplicates

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    2
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    0 # should maintain subjects, just collapse to one pregnancy
  )

  # Check that pregnancy_duplicate_map.csv was created ----
  expect_true(file.exists(file.path(pregDupFile)))
})

testthat::test_that("Filtering on GestationalDuration goes as expected", {
  cdm <- createPregnancyCohort(
    cdm = cdm,
    petTable = "pregnancy",
    petSchema = "main",
    keepExtensionTable = TRUE, # default
    minGestationalDuration = 6,
    maxGestationalDuration = 266,
    minAge = 12, # default
    maxAge = 55, # default
    startDate = NULL, # default
    endDate = NULL, # default
    sex = "Female", # default
    outputDir = outputDir,
    .softValidation = FALSE # default
  )

  attrition_tbl <- omopgenerics::attrition(cdm[["pregnancy_cohort"]])

  pregnancy_cohort <- cdm[["pregnancy_cohort"]] %>%
    dplyr::collect()

  # Sanity that only those pregnancies included with defaults are included here ----
  expect_equal(
    nrow(dplyr::anti_join(pregnancy_cohort, includedWithDefaults)),
    0
  ) # alt,(nrow(semi_join(pregnancy_cohort, includedWithDefaults)), 3)

  # With minGestationalDuration and maxGestationalDuration values, we expect 3 pregnancies in pregnancy_extension table ----
  expect_equal(nrow(pregnancy_cohort), 3)

  # Gestation duration greater than maxGestationalDuration ----
  outsideMaxGestDur <- pregnancy_cohort %>%
    dplyr::filter(
      (subject_id == 17 & pregnancy_id == 10)
      | (subject_id == 11 & pregnancy_id == 6)
      | (subject_id == 100 & pregnancy_id == 100)
      | (subject_id == 102 & pregnancy_id == 71)
      | (subject_id == 102 & pregnancy_id == 72)
    )

  expect_equal(
    nrow(outsideMaxGestDur),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 5) # 5, Gestational length <= 266 days

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    6 # 2 pregnancy record for subject 11 before this
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    4
  )

  # Gestation duration less than minGestationalDuration ----
  outsideMinGestDur <- pregnancy_cohort %>%
    dplyr::filter(subject_id == 9 & pregnancy_id == 4)

  expect_equal(
    nrow(outsideMinGestDur),
    0
  )

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 6) # 6, Gestational length >= 6 days

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    1
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    0 # other pregnancy for subject 9 persists
  )

  # Exact minGestationalDuration and maxGestationalDuration ----
  exactGestDur <- pregnancy_cohort %>%
    dplyr::filter(
      (subject_id == 14 & pregnancy_id == 8)
      | (subject_id == 18 & pregnancy_id == 11)
    )

  expect_equal(
    nrow(exactGestDur), # to sanity check that these specifically are still included
    2
  )
})

testthat::test_that("Filtering on Age goes as expected", {
  cdm <- createPregnancyCohort(
    cdm = cdm,
    petTable = "pregnancy",
    petSchema = "main",
    keepExtensionTable = TRUE, # default
    minGestationalDuration = 0, # default
    maxGestationalDuration = 308, # default
    minAge = 21,
    maxAge = 27,
    startDate = NULL, # default
    endDate = NULL, # default
    sex = "Female", # default
    outputDir = outputDir,
    .softValidation = FALSE # default
  )

  attrition_tbl <- omopgenerics::attrition(cdm[["pregnancy_cohort"]])

  pregnancy_cohort <- cdm[["pregnancy_cohort"]] %>%
    dplyr::collect()

  # Sanity that only those pregnancies included with defaults are included here
  expect_equal(
    nrow(dplyr::anti_join(pregnancy_cohort, includedWithDefaults)),
    0
  )

  # With minAge and maxAge values, we expect 4 pregnancies in pregnancy_extension table ----
  expect_equal(
    nrow(pregnancy_cohort),
    4
  )

  # Age at pregnancy start less than minAge ----
  outsideMinAge <- pregnancy_cohort %>%
    dplyr::filter(
      (subject_id == 11 & pregnancy_id == 6)
      | (subject_id == 100 & pregnancy_id == 100)
      | (subject_id == 102 & pregnancy_id == 71)
    )

  expect_equal(
    nrow(outsideMinAge),
    0
  )

  # Age at pregnancy start greater than maxAge ----
  outsideMaxAge <- pregnancy_cohort %>%
    dplyr::filter(subject_id == 17 & pregnancy_id == 10)

  expect_equal(
    nrow(outsideMaxAge),
    0
  )

  # Exact minAge and maxAge ----
  exactAge <- pregnancy_cohort %>%
    dplyr::filter(
      (subject_id == 18 & pregnancy_id == 11)
      | (subject_id == 14 & pregnancy_id == 8)
    )

  expect_equal(
    nrow(exactAge), # to sanity check that these specifically are still included
    2
  )

  # Check of attrition table ----

  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 9) # 9, Age at pregnancy start date in [21, 27]

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    4 # 1 pregnancy record for subject 11 before this
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    4
  )
})

testthat::test_that("Filtering on startDate goes as expected", {
  cdm <- createPregnancyCohort(
    cdm = cdm,
    petTable = "pregnancy",
    petSchema = "main",
    keepExtensionTable = TRUE, # default
    minGestationalDuration = 0, # default
    maxGestationalDuration = 308, # default
    minAge = 12, # default
    maxAge = 55, # default
    startDate = as.Date("2019-12-31", "%Y-%m-%d"),
    endDate = NULL, # default
    sex = "Female", # default
    outputDir = outputDir,
    .softValidation = FALSE # default
  )

  attrition_tbl <- omopgenerics::attrition(cdm[["pregnancy_cohort"]])

  pregnancy_cohort <- cdm[["pregnancy_cohort"]] %>%
    dplyr::collect()

  # Sanity that only those pregnancies included with defaults are included here
  expect_equal(
    nrow(dplyr::anti_join(pregnancy_cohort, includedWithDefaults)),
    0
  )

  # With startDate value, we expect 7 pregnancies in pregnancy_extension table ----
  expect_equal(
    nrow(pregnancy_cohort),
    7
  )

  # Pregnancy starts before our startDate ----
  outsideStartDate <- pregnancy_cohort %>%
    dplyr::filter(subject_id == 18 & pregnancy_id == 11)

  expect_equal(
    nrow(outsideStartDate),
    0
  )

  onStartDate <- pregnancy_cohort %>%
    dplyr::filter(subject_id == 14 & pregnancy_id == 8)

  expect_equal(
    nrow(onStartDate), # to sanity check that these specifically are still included
    1
  )

  # Check of attrition table ----
  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 11) # 11, Pregnancy start >= 2019-12-31 and < 2024-01-01 (end of database - 1 year

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    1 # 1 pregnancy record for subject 11 before this
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    1
  )
})

testthat::test_that("Filtering on endDate goes as expected", {
  cdm <- createPregnancyCohort(
    cdm = cdm,
    petTable = "pregnancy",
    petSchema = "main",
    keepExtensionTable = TRUE, # default
    minGestationalDuration = 0, # default
    maxGestationalDuration = 308, # default
    minAge = 12, # default
    maxAge = 55, # default
    startDate = NULL, # default
    endDate = as.Date("2022-01-07", "%Y-%m-%d"),
    sex = "Female", # default
    outputDir = outputDir,
    .softValidation = FALSE # default
  )

  attrition_tbl <- omopgenerics::attrition(cdm[["pregnancy_cohort"]])

  pregnancy_cohort <- cdm[["pregnancy_cohort"]] %>%
    dplyr::collect()

  # Sanity that only those pregnancies included with defaults are included here
  expect_equal(
    nrow(dplyr::anti_join(pregnancy_cohort, includedWithDefaults)),
    0
  )

  # With endDate value, we expect 6 pregnancies in pregnancy_extension table ----
  expect_equal(
    nrow(pregnancy_cohort),
    6
  )

  # Pregnancy ends after our endDate ----
  outsideEndDate <- pregnancy_cohort %>%
    dplyr::filter(
      (subject_id == 9 & pregnancy_id == 5)
      | (subject_id == 17 & pregnancy_id == 10)
    )

  expect_equal(
    nrow(outsideEndDate),
    0
  )

  onEndDate <- pregnancy_cohort %>%
    dplyr::filter(
      (subject_id == 11 & pregnancy_id == 6)
      | (subject_id == 100 & pregnancy_id == 100)
      | (subject_id == 102 & pregnancy_id == 71)
    )

  expect_equal(
    nrow(onEndDate), # to sanity check that these specifically are still included
    3
  )

  # Check of attrition table ----
  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 11) # 11, Pregnancy end <= 2022-01-07

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    2
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    1 # other pregnancy for subject 9 persists
  )
})

testthat::test_that("Filtering on startDate AND endDate goes as expected", {
  cdm <- createPregnancyCohort(
    cdm = cdm,
    petTable = "pregnancy",
    petSchema = "main",
    keepExtensionTable = TRUE, # default
    minGestationalDuration = 0, # default
    maxGestationalDuration = 308, # default
    minAge = 12, # default
    maxAge = 55, # default
    startDate = as.Date("2019-12-31", "%Y-%m-%d"),
    endDate = as.Date("2022-01-07", "%Y-%m-%d"),
    sex = "Female", # default
    outputDir = outputDir,
    .softValidation = FALSE # default
  )

  attrition_tbl <- omopgenerics::attrition(cdm[["pregnancy_cohort"]])

  pregnancy_cohort <- cdm[["pregnancy_cohort"]] %>%
    dplyr::collect()

  # Sanity that only those pregnancies included with defaults are included here
  expect_equal(
    nrow(dplyr::anti_join(pregnancy_cohort, includedWithDefaults)),
    0
  )

  # With startDate and endDate values, we expect 5 pregnancies in pregnancy_extension table ----
  expect_equal(
    nrow(pregnancy_cohort),
    5
  )

  # Pregnancy ends after our endDate ----
  outsideStartEnd <- pregnancy_cohort %>%
    dplyr::filter(
      (subject_id == 18 & pregnancy_id == 11)
      | (subject_id == 9 & pregnancy_id == 5)
      | (subject_id == 17 & pregnancy_id == 10)
    )

  expect_equal(
    nrow(outsideStartEnd),
    0
  )

  onStartEnd <- pregnancy_cohort %>%
    dplyr::filter(
      (subject_id == 14 & pregnancy_id == 8)
      | (subject_id == 11 & pregnancy_id == 6)
      | (subject_id == 100 & pregnancy_id == 100)
      | (subject_id == 102 & pregnancy_id == 71)
    )

  expect_equal(
    nrow(onStartEnd), # to sanity check that these specifically are still included
    4
  )

  # Check of attrition table ----
  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 11 | reason_id == 12) # 11, Pregnancy start >= 2019-12-31 and < 2024-01-01 (end of database - 1 year); 12, Pregnancy end <= 2022-01-07

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    c(1, 2)
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    c(1, 1) # other pregnancy for subject 9 persists
  )
})

testthat::test_that("Filtering on sex goes as expected", {
  cdm <- createPregnancyCohort(
    cdm = cdm,
    petTable = "pregnancy",
    petSchema = "main",
    keepExtensionTable = TRUE, # default
    minGestationalDuration = 0, # default
    maxGestationalDuration = 308, # default
    minAge = 12, # default
    maxAge = 55, # default
    startDate = NULL, # default
    endDate = NULL, # default
    sex = "Male", # default
    outputDir = outputDir,
    .softValidation = FALSE # default
  )

  attrition_tbl <- omopgenerics::attrition(cdm[["pregnancy_cohort"]])

  pregnancy_cohort <- cdm[["pregnancy_cohort"]] %>%
    dplyr::collect()

  # With sex = "Male', we expect 0 pregnancies in pregnancy_extension table ----
  expect_equal(
    nrow(pregnancy_cohort),
    0
  )

  # Check of attrition table ----
  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 10) # 10 Sex: Male

  # The 8 pregnancies in pregnancy_cohort on default settings are still there until filtering on sex
  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_records),
    8
  )

  expect_equal(
    attrition_subset %>%
      dplyr::pull(excluded_subjects),
    7
  ) # other subject 9 has two pregnancies
})

testthat::test_that("Filtering when .softValidation = TRUE goes as expected & pregnancy_duplicate_map.csv output file is not created", {
  if (file.exists(pregDupFile)) {
    file.remove(pregDupFile) # remove versions of this file from previous tests
  }

  cdm <- createPregnancyCohort(
    cdm = cdm,
    petTable = "pregnancy",
    petSchema = "main",
    keepExtensionTable = TRUE, # default
    minGestationalDuration = 0, # default
    maxGestationalDuration = 308, # default
    minAge = 12, # default
    maxAge = 55, # default
    startDate = NULL, # default
    endDate = NULL, # default
    sex = "Female", # default
    outputDir = outputDir,
    .softValidation = TRUE
  )

  attrition_tbl <- omopgenerics::attrition(cdm[["pregnancy_cohort"]])

  pregnancy_cohort <- cdm[["pregnancy_cohort"]] %>%
    dplyr::collect()

  # With .softValidation, we expect 16 records in pregnancy_extension table ----
  expect_equal(
    nrow(pregnancy_cohort),
    16
  )

  # Outside of age at pregnancy start range ----
  outsideAge <- pregnancy_cohort %>%
    dplyr::filter(subject_id == 20 & pregnancy_id == 13) # no record in person table, no way to calc age

  expect_equal(
    nrow(outsideAge),
    0
  )

  # Check of attrition table ----
  attrition_subset <- attrition_tbl %>%
    dplyr::filter(reason_id == 2) # Age at pregnancy start date in [12, 55]

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

  # Check that pregnancy_duplicate_map.csv was not created
  expect_false(file.exists(file.path(pregDupFile)))
})
