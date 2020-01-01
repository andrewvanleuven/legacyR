# Legacy Regions Database Codebook

Welcome to the online codebook for the legacy regions research dataset. The data are comprised of three groups of variables: controls, assets, and liabilities. This codebook describes each set of variables in terms of:

* *meaning* — how exactly the variable is to be interpreted
* *source* — where the variable was collected from 
* *geographic extent* — whether the variable is measured at the MSA level or at the level of the MSA's principal city

## Control Variables

| Variable             | Description                                     |    Source    | Year | Geography |
|----------------------|-------------------------------------------------|:------------:|:----:|:---------:|
| cbsa_fips            | Unique ID for each MSA                          |      --      |  --  |    MSA    |
| cbsa                 | Name of each MSA                                |      --      |  --  |    MSA    |
| sqmi                 | Square mileage of each MSA                      | Census TIGER | 2010 |    MSA    |
| population_2005      | Population of each MSA in 2005                  |   NIH SEER   | 2005 |    MSA    |
| density              | Number of persons per square-mile               | Census TIGER | 2005 |    MSA    |
| age_total_young      | Number of persons younger than age 18           |   US Census  | 2000 |    MSA    |
| age_total_old        | Number of persons older than age 64             |   US Census  | 2000 |    MSA    |
| age_total            | Number of persons                               |   US Census  | 2000 |    MSA    |
| age_pct_u18          | Percentage of persons younger than age 18       |   US Census  | 2000 |    MSA    |
| age_pct_o64          | Percentage of persons older than age 64         |   US Census  | 2000 |    MSA    |
| nat_total            | Number of persons                               |   US Census  | 2000 |    MSA    |
| nat_foreign_born     | Number of persons  foreign-born                 |   US Census  | 2000 |    MSA    |
| nat_not_foreign_born | Number of persons not foreign-born              |   US Census  | 2000 |    MSA    |
| nat_native           | Number of persons native-born                   |   US Census  | 2000 |    MSA    |
| nat_nativity         | Percentage of persons not foreign-born          |   US Census  | 2000 |    MSA    |
| laus_lfpr            | Civilian labor force participation rate         |      BLS     | 2005 |    MSA    |
| laus_pop             | Population of each MSA in 2005                  |   NIH SEER   | 2005 |    MSA    |
| laus_lf              | Number of persons in the labor force            |      BLS     | 2005 |    MSA    |
| laus_emp             | Number of employed persons in the labor force   |      BLS     | 2005 |    MSA    |
| laus_unemp           | Number of unemployed persons in the labor force |      BLS     | 2005 |    MSA    |

* The square mileage was calculated with the `sf` GIS package in R and used Census [TIGER/Line](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html) shapefiles downloaded using the `tigris` package developed by [Kyle Walker](https://github.com/walkerke).
* Population estimates (for non-decennial years) was collected from the [NIH SEER research data site](https://seer.cancer.gov/popdata/download.html).

## Asset Variables

| Variable            | Description |        Source       | Geography |
|---------------------|-------------|:-------------------:|:---------:|
| access              | --          |        US DOT       |    MSA    |
| city_age            | --          |      US Census      |    City   |
| r_1                 | --          |        IPEDS        |    MSA    |
| r_2                 | --          |        IPEDS        |    MSA    |
| st_cap              | --          |          --         |    MSA    |
| mfg_gmp2007         | --          | Moody's Economy.com |    MSA    |
| mfg_gmp2007_sector  | --          | Moody's Economy.com |    MSA    |
| mfg_emp2007         | --          | Moody's Economy.com |    MSA    |
| mfg_emp2007_sector  | --          | Moody's Economy.com |    MSA    |
| mfg_lq_gmp          | --          | Moody's Economy.com |    MSA    |
| mfg_lq_emp          | --          | Moody's Economy.com |    MSA    |
| charitable_assets   | --          |         NCCS        |    MSA    |
| intermodal_freight  | --          |      US DOT BTS     |    MSA    |
| enplanements        | --          |         FAA         |    MSA    |
| hist_bldgs          | --          |         NPS         |    MSA    |
| hist_strct          | --          |         NPS         |    MSA    |
| hist_sites          | --          |         NPS         |    MSA    |
| hist_objct          | --          |         NPS         |    MSA    |
| hist_dists          | --          |         NPS         |    MSA    |
| hist_registry_total | --          |         NPS         |    MSA    |

* Any descriptions with "--" are still *in progress*

## Liability Variables

| Variable                | Description                                                                                               |      Source     | Year | Geography |
|-------------------------|-----------------------------------------------------------------------------------------------------------|:---------------:|:----:|:---------:|
| city_peak_yr            | Year in which MSA's principal city reached its peak                                                       |      Census     | 2010 |    City   |
| city_peak_pop           | Population of MSA's principal city during its peak                                                        |      Census     | 2010 |    City   |
| city_decades_since_peak | Number of decades between 2010 and population peak of MSA's principal city                                |      Census     | 2010 |    City   |
| city_pop_2010           | Population of MSA's principal city in 2010                                                                |      Census     | 2010 |    City   |
| city_decline_since_peak | Percent-change of decline since population peak of MSA's principal city (until 2010)                      |      Census     | 2010 |    City   |
| pctchg_00_05            | Percent-change of MSA population from 2000 to 2005                                                        | Census/NIH SEER | 2005 |    MSA    |
| vacancy_msa             | Percent of total housing units in MSA that are vacant                                                     |      Census     | 2000 |    MSA    |
| vacancy_city            | Percent of total housing units in MSA's principal city that are vacant                                    |      Census     | 2000 |    City   |
| pct_prewar_city         | Percent of total housing units in MSA's principal city built before 1940                                  |      Census     | 2000 |    City   |
| pct_prewar_msa          | Percent of total housing units in MSA built before 1940                                                   |      Census     | 2000 |    MSA    |
| total_units_2005        | Total number of housing units in MSA                                                                      |       HUD       | 2005 |    MSA    |
| hud_units_2005          | Total number of public housing units in MSA                                                               |       HUD       | 2005 |    MSA    |
| pct_ph                  | Percent of total housing units in MSA deemed by hud as public housing                                     |       HUD       | 2005 |    MSA    |
| med_val_cbsa            | Median value (dollars) for all owner-occupied housing units in MSA                                        |       ACS       | 2005 |    MSA    |
| med_val_city            | Median value (dollars) for all owner-occupied housing units in MSA's principal city                       |      Census     | 2000 |    City   |
| prop_crimes_per100k     | Property crimes in MSA per 100,000 residents                                                              |     FBI UCR     | 2005 |    MSA    |
| prop_crimes_burglary    | Number of burglaries in MSA                                                                               |     FBI UCR     | 2005 |    MSA    |
| prop_crimes_larceny     | Number of larceny crimes in MSA                                                                           |     FBI UCR     | 2005 |    MSA    |
| prop_crimes_moto_theft  | Number of motor vehicle thefts in MSA                                                                     |     FBI UCR     | 2005 |    MSA    |
| pct_low_inf_bw          | Percent of infants born with low birth weight                                                             |      UW PHI     | 2005 |    MSA    |
| pct_obese_adult         | Percent of adults classified as obese                                                                     |      UW PHI     | 2005 |    MSA    |
| pct_uninsured_adult     | Percent of adults without health insurance                                                                |      UW PHI     | 2005 |    MSA    |
| pov_total_in_poverty    | Number of persons whose income in 1999 was below the poverty level                                        |      Census     | 2000 |    MSA    |
| pov_total               | Number of persons                                                                                         |      Census     | 2000 |    MSA    |
| pov_poverty_rate        | Percentage of persons whose income in 1999 was below the poverty level                                    |      Census     | 2000 |    MSA    |
| gini                    | Gini index of income inequality                                                                           |       ACS       | 2005 |    MSA    |
| total_bridges_2005      | Number of bridges in an MSA which are 10 years or older and are more than 20 feet in length               |       FHWA      | 2005 |    MSA    |
| bad_bridges_2005        | Number of bridges in an MSA deemed structurally deficient                                                 |       FHWA      | 2005 |    MSA    |
| pct_deficient           | Percentage of bridges in an MSA deemed structurally deficient                                             |       FHWA      | 2005 |    MSA    |
| npl_sites               | Number of sites in an MSA on the National Priorities List                                                 |       EPA       | 2005 |    MSA    |
| sfund_per_sqmi          | Number of NPL sites per square-mile of an MSA                                                             |       EPA       | 2005 |    MSA    |
| edu_no_hs_diploma       | Percent of the population 25 years and over with less than a high-school diploma                          |      Census     | 2000 |    MSA    |
| edu_bachelors_plus      | Percent of the population 25 years and over with a bachelor's degree or higher                            |      Census     | 2000 |    MSA    |
| edu_total_bach          | Number of persons 25 years and over with a bachelor's degree as the highest level of education attained   |      Census     | 2000 |    MSA    |
| edu_total_mast          | Number of persons 25 years and over with a master's degree as the highest level of education attained     |      Census     | 2000 |    MSA    |
| edu_total_prof          | Number of persons 25 years and over with a professional degree as the highest level of education attained |      Census     | 2000 |    MSA    |
| edu_total_doct          | Number of persons 25 years and over with a doctorate degree as the highest level of education attained    |      Census     | 2000 |    MSA    |
| edu_total_nohs          | Number of persons 25 years and over with less than a high-school diploma                                  |      Census     | 2000 |    MSA    |
| edu_total_degr          | Number of persons 25 years and over                                                                       |      Census     | 2000 |    MSA    |
| edu_total_ov25          | Number of persons 25 years and over with a bachelor's degree or higher                                    |      Census     | 2000 |    MSA    |

* 
