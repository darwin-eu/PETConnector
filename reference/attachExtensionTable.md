# attachExtensionTable

Copies an extra CDM table as an extension table in a specified schema

## Usage

``` r
attachExtensionTable(cdm, table, schema, name)
```

## Arguments

- cdm:

  (`cdm_reference`) CDM reference object

- table:

  (`character(1)`) Name of the extension table

- schema:

  (`character(1)`) Name of the schema where the extension table resides

- name:

  (`character(1)`) How the copy of the extension table should be named

## Value

(`cdm_reference`) Returns the CDM with attached extension table

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

# Attach a pregnancy extension table
cdm <- attachExtensionTable(
 cdm = cdm,
 table = "pregnancy",
 schema = "main",
 name = "pregnancy_extension_table"
)
}
```
