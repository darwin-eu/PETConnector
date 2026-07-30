library(TestGenerator)

TestGenerator::readPatients(
  filePath = "testthat/testCases/Test_data_sansComments.xlsx",
  testName = "PET",
  outputPath = "testthat/testCases/",
  cdmVersion = "5.4",
  extraTable = TRUE
)

cdm <- TestGenerator::patientsCDM(
  pathJson = "testthat/testCases/",
  testName = "PET",
  cdmVersion = "5.4"
)
