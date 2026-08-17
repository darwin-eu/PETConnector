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

# Records included in pregnancy_cohort when createPregnancyCohort() is run with default parameters
includedWithDefaults <- dplyr::tibble( # excluded will make it more difficult with twin preg 6
  subject_id = c(11, 9, 9, 14, 17, 18, 100, 102),
  pregnancy_id = c(6, 4, 5, 8, 10, 11, 100, 71),
)

# Re-used paths
outputDir <- testthat::test_path("testthat_testOutput")
dir.create(outputDir, recursive = FALSE)
pregDupFile <- file.path(outputDir, "pregnancy_duplicate_map.csv")

