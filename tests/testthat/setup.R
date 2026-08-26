library(TestGenerator)

# CDM
TestGenerator::readPatients(
  filePath = testthat::test_path("testCases", "Test_data_sansComments.xlsx"),
  testName = "PET",
  outputPath = testthat::test_path("testCases"),
  cdmVersion = "5.4",
  extraTable = TRUE
)

cdm <- TestGenerator::patientsCDM(
  pathJson = testthat::test_path("testCases"),
  testName = "PET",
  cdmVersion = "5.4"
)

# Re-used paths
outputDir <- testthat::test_path("testthat_testOutput")
dir.create(outputDir, recursive = FALSE)
pregDupFile <- file.path(outputDir, "pregnancy_duplicate_map.csv")

# Create a pregnancy cohort table to test createChildCohort()
pregnancy_cohort <- tibble::tibble( # excluded will make it more difficult with twin preg 6
  cohort_definition_id = c(101, 101, 101, 101, 101, 101, 101, 101, 101),
  subject_id = as.integer(c(9, 11, 14, 17, 18, 100, 102, 9, 30)),
  cohort_start_date = as.Date(c("2023-01-01", "2021-04-10", "2019-12-31", "2023-09-27", "2013-09-27", "2021-04-10", "2021-04-10", "2022-01-01", "2021-04-10"), "%Y-%m-%d"),
  cohort_end_date = as.Date(c("2023-08-10", "2022-01-07", "2020-01-06", "2024-06-20", "2014-06-20", "2022-01-07", "2022-01-07", "2022-01-04", "2022-01-07"), "%Y-%m-%d"),
  pregnancy_id = as.integer(c(5, 6, 8, 10, 11, 100, 71, 4, 27)),
  pregnancy_start_date = as.Date(c("2023-01-01", "2021-04-10", "2019-12-31", "2023-09-27", "2013-09-27", "2021-04-10", "2021-04-10", "2022-01-01", "2021-04-10"), "%Y-%m-%d"),
  pregnancy_end_date = as.Date(c("2023-08-10", "2022-01-07", "2020-01-06", "2024-06-20", "2014-06-20", "2022-01-07", "2022-01-07", "2022-01-04", "2022-01-07"), "%Y-%m-%d" ),
  gestational_length_in_day = as.integer(c(280, 270, 154, 280, 280, 270, 270, 90, 270)),
  pregnancy_outcome = as.integer(c(4092289, 4092289, 443213, 443213, 443213, 4092289, 4092289, 4067106, 443213)),
  pregnancy_mode_delivery = as.integer(c(4125611, 4015701, 4125611, 4125611, 4125611, NA, 4015701, 4125611, 4015701)),
  pregnancy_single = as.integer(c(4188539, 4188540, 4188539, 4188539, 4188539, 4188540, 4188540, 4188539, 4188539)),
  pre_pregnancy_smoking = as.integer(c(4188540, 4188539, 4188539, 4188539, 4188539, 4188539, 4188539, 4188540, 4188539)),
  in_observation = as.integer(c(1, 1, 1, 1, 1, 1, 1, 1, 1)),
  overlap = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
  age = as.integer(c(26, 20, 27, 31, 21, 20, 20, 25, 20)),
  sex = c("Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female")
)

pregnancy_duplicate_map <- tibble::tibble(
  subject_id = 102,
  pregnancy_start_date = as.Date("2021-04-10", "%Y-%m-%d"),
  pregnancy_end_date = as.Date("2022-01-07", "%Y-%m-%d"),
  kept_pregnancy_id = 71,
  removed_pregnancy_id = 72
)

# Records included in pregnancy_cohort when createPregnancyCohort() is run with default parameters
includedWithDefaults <- tibble::tibble( # excluded will make it more difficult with twin preg 6
  subject_id = c(11, 9, 9, 14, 17, 18, 100, 102),
  pregnancy_id = c(6, 4, 5, 8, 10, 11, 100, 71),
)
