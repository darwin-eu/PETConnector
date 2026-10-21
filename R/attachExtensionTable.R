#' attachExtensionTable
#'
#' Creates a lazy reference to an extension table in a specified schema
#'
#' @param cdm (`cdm_reference`) CDM reference object
#' @param table (`character(1)`) Name of the extension table
#' @param tableSchema (`character(1)`) Name of the schema where the extension table resides
#'
#' @returns (`cdm_reference`) Returns the CDM reference with attached extension table
#'
#' @export
#' @examples
#' if (interactive()) {
#' # Example CDM with a pregnancy extension table
#' path <- system.file("exampleData", package = "PETConnector")
#'
#'cdm <- TestGenerator::patientsCDM(
#'  pathJson = path,
#'  testName = "example_patients",
#'  cdmVersion = "5.4"
#')
#'
#' # Attach a pregnancy extension table
#'cdm <- attachExtensionTable(
#'  cdm = cdm,
#'  table = "pregnancy",
#'  tableSchema = "main"
#')
#'}
attachExtensionTable <- function(
    cdm,
    table,
    tableSchema) {

  # Check inputs ----
  assertions <- checkmate::makeAssertCollection()

  checkmate::assertClass(x = cdm, classes = "cdm_reference", add = assertions)
  checkmate::assertClass(x = table, classes = "character", add = assertions)
  checkmate::assertClass(x = tableSchema, classes = "character", add = assertions)

  checkmate::reportAssertions(assertions)

  # Attach extension table ----
  con <- attr(cdm, "dbcon")

  tableRef <- DBI::Id(
    table = table,
    schema = tableSchema
  )

  cdm[[table]] <- dplyr::tbl(con, tableRef)

  return(cdm)
}
