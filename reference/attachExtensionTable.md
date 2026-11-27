# attachExtensionTable

Adds an extension table to the CDM reference

## Usage

``` r
attachExtensionTable(cdm, table, tableSchema)
```

## Arguments

- cdm:

  (`cdm_reference`) CDM reference object

- table:

  (`character(1)`) Name of the extension table

- tableSchema:

  (`character(1)`) Name of the schema where the extension table resides

## Value

(`cdm_reference`) Returns the CDM reference with extension table

## Examples

``` r
if (interactive()) {
# Example CDM with a pregnancy extension table
path <- system.file("exampleData", package = "PETConnector")

cdm <- TestGenerator::patientsCDM(
 pathJson = path,
 testName = "example_patients",
 cdmVersion = "5.4"
)

# Create lazy ref
cdm <- attachExtensionTable(
 cdm = cdm,
 table = "pregnancy",
 tableSchema = "main"
)
}
```
