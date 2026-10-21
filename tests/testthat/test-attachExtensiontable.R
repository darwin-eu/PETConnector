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

testthat::test_that("pregnancy_extension_table is created and attached to cdm", {
  testthat::skip()

  cdm <- PETConnector::attachExtensionTable(
    cdm = cdm,
    table = "pregnancy",
    tableSchema = "main"
  )

  # Check that pregnancy_extension_table exists ----
  expect_contains(
    names(cdm),
    "pregnancy_extension_table"
  )

  # Check that pregnancy and pregnancy_extension_table are identical ----
  pet <- cdm[["pregnancy_extension_table"]] %>%
    dplyr::collect()

  expect_identical(
    pet,
    cdm[["pregnancy"]] %>%
      dplyr::collect()
  )

  expect_false( # sanity check
    identical(
      pet,
      cdm[["person"]] %>%
        dplyr::collect()
    )
  )
})
