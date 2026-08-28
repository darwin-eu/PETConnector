#' attachExtensionTable
#'
#' Copies an extra CDM table as an extension table in a specified schema
#'
#' @param cdm (`cdm_reference`) CDM reference object
#' @param table (`character(1)`) Name of the extension table
#' @param schema (`character(1)`) Name of the schema where the extension table resides
#' @param name (`character(1)`) How the copy of the extension table should be named
#'
#' @returns (`cdm_reference`) Returns the CDM with attached extension table
#'
#' @import checkmate
#' @import dplyr
#' @importFrom DBI Id
#'
#'
#' @export
attachExtensionTable <- function(
    cdm,
    table,
    schema,
    name) {

  # Check inputs ----
  assertions <- checkmate::makeAssertCollection()

  checkmate::assertClass(x = cdm, classes = "cdm_reference", add = assertions)
  checkmate::assertClass(x = table, classes = "character", add = assertions)
  checkmate::assertClass(x = schema, classes = "character", add = assertions)
  checkmate::assertClass(x = name, classes = "character", add = assertions)

  checkmate::reportAssertions(assertions)

  # Attach extension table ----
  con <- attr(cdm, "dbcon")

  tableRef <- DBI::Id(
    name = table,
    schema = schema
  )

  cdm[[name]] <- dplyr::tbl(con, tableRef) %>%
    dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE)

  return(cdm)
}

