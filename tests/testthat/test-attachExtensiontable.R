testthat::test_that("input args are as expected", {
  expect_error(
    PETConnector::attachExtensionTable(
      cdm = "hello",
      table = 123,
      tableSchema = FALSE
    ),
    "3 assertions failed:"
  )

  expect_no_error(
    PETConnector::attachExtensionTable(
      cdm = cdm,
      table = "pregnancy",
      tableSchema = "main"
    )
  )
})

testthat::test_that("attachExtensionTable creates a lazy ref to the pregnancy extension table in the CDM reference", {
  # The cdm ref loaded from setup should have ref to pregnancy table
  expect_contains(
    names(cdm),
    "pregnancy"
  )

  # Drop the reference to the pregnancy extension table
  cdm[["pregnancy"]] <- NULL

  expect_disjoint(
    names(cdm),
    "pregnancy"
  )

  # Run the function
  cdm <- PETConnector::attachExtensionTable(
    cdm = cdm,
    table = "pregnancy",
    tableSchema = "main"
  )

  expect_contains(
    names(cdm),
    "pregnancy"
  )
})
