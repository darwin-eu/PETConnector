testthat::test_that("input args are as expected", {
  expect_error(
    attachExtensionTable(
      cdm = "hello",
      table = 123,
      schema = FALSE,
      name = NULL
    ),
    "4 assertions failed:"
  )

  expect_no_error(
    attachExtensionTable(
      cdm = cdm,
      table = "pregnancy",
      schema = "main",
      name = "pregnancy_extension_table"
    )
  )
})

testthat::test_that("pregnancy_extension_table is created and attached to cdm", {
  cdm <- attachExtensionTable(
    cdm = cdm,
    table = "pregnancy",
    schema = "main",
    name = "pregnancy_extension_table"
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
