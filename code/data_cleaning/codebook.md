# Legacy Regions Database Codebook

Welcome to the online codebook for the legacy regions research dataset. The data are comprised of three groups of variables: controls, assets, and liabilities. This codebook describes each set of variables in terms of:

* *meaning* — how exactly the variable is to be interpreted
* *source* — where the variable was collected from 
* *geographic extent* — whether the variable is measured at the MSA level or at the level of the MSA's principal city

## Control Variables

| **Variable**    | **Description**                   | **Source**     | **Geography** |
|:---------------:|:---------------------------------:|:------------:|:---------:|
| cbsa_fips       | Unique ID for each MSA            |      --      |    MSA    |
| cbsa            | Name of each MSA                  |      --      |    MSA    |
| sqmi            | Square mileage of each MSA        | Census TIGER |    MSA    |
| population_2005 | Population of each MSA in 2005    | NIH SEER     |    MSA    |
| density         | Number of persons per square-mile |      --      |    MSA    |

* The square mileage was calculated with the `sf` GIS package in R and used Census [TIGER/Line](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html) shapefiles downloaded using the `tigris` package developed by [Kyle Walker](https://github.com/walkerke).
* Population estimates (for non-decennial years) was collected from the [NIH SEER research data site](https://seer.cancer.gov/popdata/download.html).

## Asset Variables

## Liability Variables

