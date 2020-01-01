# Legacy Regions Database Codebook

Welcome to the online codebook for the legacy regions research dataset. The data are comprised of three groups of variables: controls, assets, and liabilities. This codebook describes each set of variables in terms of:

* **meaning** — how exactly the variable is to be interpreted
* **source** — where the variable was collected from 
* **geographic extent** — whether the variable is measured at the MSA level or at the level of the MSA's principal city

Jump to:

* [*Control Variables*](https://github.com/andrewvanleuven/legacyR/blob/master/code/data_cleaning/codebook.md#control-variables)
* [*Asset Variables*](https://github.com/andrewvanleuven/legacyR/blob/master/code/data_cleaning/codebook.md#asset-variables)
* [*Liability Variables*](https://github.com/andrewvanleuven/legacyR/blob/master/code/data_cleaning/codebook.md#liability-variables)

***

## Control Variables

| Variable             | Description                                     |    Source    | Year | Geography |
|----------------------|-------------------------------------------------|:------------:|:----:|:---------:|
| cbsa_fips            | Unique ID for each MSA                          |      --      |  --  |    MSA    |
| cbsa                 | Name of each MSA                                |      --      |  --  |    MSA    |
| sqmi                 | Square mileage of each MSA                      | [Census TIGER](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html) | 2010 |    MSA    |
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
| laus_lfpr            | Civilian labor force participation rate         |      [BLS](https://www.bls.gov/lau/)     | 2005 |    MSA    |
| laus_pop             | Population of each MSA in 2005                  |   [NIH SEER](https://seer.cancer.gov/popdata/)   | 2005 |    MSA    |
| laus_lf              | Number of persons in the labor force            |      BLS     | 2005 |    MSA    |
| laus_emp             | Number of employed persons in the labor force   |      BLS     | 2005 |    MSA    |
| laus_unemp           | Number of unemployed persons in the labor force |      BLS     | 2005 |    MSA    |

### Notes

* The square mileage was calculated with the `sf` GIS package in R and used Census [TIGER/Line](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html) shapefiles downloaded using the `tigris` package developed by [Kyle Walker](https://github.com/walkerke).
* All variables constructed from Census and/or ACS data were downloaded with the help of the `tidycensus` package (also developed by Walker).
* The difference between the "foreign-born" and "non-native" population is not intuitive. See [this FAQ](https://www.census.gov/topics/population/foreign-born/about/faq.html) from the U.S. Census for more about this distinction. For the purpose of our analysis, we calculate a "nativity" variable as *total-population-minus-foreign-born* divided by *total population*.

***

## Asset Variables

| Variable            | Description                                                                                                             |        Source       | Year | Geography |
|---------------------|-------------------------------------------------------------------------------------------------------------------------|:-------------------:|:----:|:---------:|
| access              | An index constructed from US DOT's Transportation and Health Tool data on walkability and access to transit             |        [US DOT](https://www.transportation.gov/transportation-health-tool)       | 2015 |    MSA    |
| city_age            | Number of decades since the decennial census year in which an MSA's principal city first reached a population of 50,000 |      US Census      | 2010 |    City   |
| r_1                 | Number of universities in an MSA classified as "high research activity"                                                 |        [IPEDS](https://nces.ed.gov/ipeds/use-the-data)        | 2005 |    MSA    |
| r_2                 | Number of universities in an MSA classified as "very high research activity"                                            |        IPEDS        | 2005 |    MSA    |
| st_cap              | Indicates the presence of a state capital within the MSA                                                                |          --         |  --  |    MSA    |
| mfg_gmp2007         | The gross metropolitan product (GMP) of the MSA in 2007                                                                 | [Moody's Analytics](https://www.economy.com/) | 2007 |    MSA    |
| mfg_gmp2007_sector  | The gross metropolitan product (GMP) of the MSA's manufacturing sector in 2007                                          | Moody's Analytics | 2007 |    MSA    |
| mfg_emp2007         | Total employment in the MSA in 2007                                                                                     | Moody's Analytics | 2007 |    MSA    |
| mfg_emp2007_sector  | Total employment in the MSA's manufacturing sector in 2007                                                              | Moody's Analytics | 2007 |    MSA    |
| mfg_lq_gmp          | The location quotient of the MSA's manufacturing GMP in 2007                                                            | Moody's Analytics | 2007 |    MSA    |
| mfg_lq_emp          | The location quotient of the MSA's manufacturing employment in 2007                                                     | Moody's Analytics | 2007 |    MSA    |
| charitable_assets   | Total assets of all private tax-exempt charitable organizations in 2005                                                 |         [NCCS](https://nccs-data.urban.org/index.php)        | 2005 |    MSA    |
| intermodal_freight  | Number of all intermodal freight facilities in an MSA                                                                   |      [US DOT BTS](http://osav-usdot.opendata.arcgis.com/datasets/c7ee724adff84056a9ec7ec0a8dc83ca_0)     | 2015 |    MSA    |
| enplanements        | Number of passenger enplanements at all commercial service airports within an MSA                                       |         [FAA](https://www.faa.gov/airports/planning_capacity/passenger_allcargo_stats/passenger/)         | 2005 |    MSA    |
| hist_bldgs          | Number of buildings in an MSA listed on the National Historic Register                                                  |         [NPS](https://www.nps.gov/subjects/nationalregister/data-downloads.htm)         | 2005 |    MSA    |
| hist_strct          | Number of structures in an MSA listed on the National Historic Register                                                 |         NPS         | 2005 |    MSA    |
| hist_sites          | Number of sites in an MSA listed on the National Historic Register                                                      |         NPS         | 2005 |    MSA    |
| hist_objct          | Number of objects in an MSA listed on the National Historic Register                                                    |         NPS         | 2005 |    MSA    |
| hist_dists          | Number of districts in an MSA listed on the National Historic Register                                                  |         NPS         | 2005 |    MSA    |
| hist_registry_total | Total number of items in an MSA listed on the National Historic Register                                                |         NPS         | 2005 |    MSA    |

### Notes

* All variables which were constructed from historic decennial census data (e.g., city age, peak population) relied heavily on the [Historical Populations Data Repository](https://github.com/CreatingData/Historical-Populations) assembled by various data scientists (see more info [here](http://creatingdata.us/datasets/US-cities/)).
* In the "city age" variable, any MSA whose principal city has *yet to reach* a population of 50,000 was assigned an age of zero.
* Source data for manufacturing GMP and employment are not publicly available and remain the property of Moody's analytics. The derived location quotients (LQs) are hosted in this public repository, while the source file is not.
* The "charitable assets" variable was constructed with help from the Urban Institute's invaluable `R` function, found [here](https://nccs.urban.org/code/prep-nccs-core-file-data). Furthermore, we used our judgement to pick from the [NTEE Codes](https://nccs.urban.org/publication/irs-activity-codes) that fit the description of the charitable foundations "asset" described in the legacy cities literature. Our selection criteria can be found in the code of the `nccs.R' script [here](code/data_cleaning/nccs.R).
* The definitions for items listed on the National Historic Registry—buildings, structures, sites, objects, and districts—can be found on the National Park Service's [website](https://mapservices.nps.gov/arcgis/rest/services/cultural_resources/nrhp_locations/MapServer/0). The dataset includes a variable for each category, but we only use *historic buildings* in our analysis.
* ENPLANE AND CHRASTS need to be per-capita

***

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
| total_units_2005        | Total number of housing units in MSA                                                                      |       [HUD]( https://www.huduser.gov/portal/picture/query.html)       | 2005 |    MSA    |
| hud_units_2005          | Total number of public housing units in MSA                                                               |       HUD       | 2005 |    MSA    |
| pct_ph                  | Percent of total housing units in MSA deemed by hud as public housing                                     |       HUD       | 2005 |    MSA    |
| med_val_cbsa            | Median value (dollars) for all owner-occupied housing units in MSA                                        |       ACS       | 2005 |    MSA    |
| med_val_city            | Median value (dollars) for all owner-occupied housing units in MSA's principal city                       |      Census     | 2000 |    City   |
| prop_crimes_per100k     | Property crimes in MSA per 100,000 residents                                                              |     [FBI UCR](https://ucr.fbi.gov/crime-in-the-u.s/2005)     | 2005 |    MSA    |
| prop_crimes_burglary    | Number of burglaries in MSA                                                                               |     FBI UCR     | 2005 |    MSA    |
| prop_crimes_larceny     | Number of larceny crimes in MSA                                                                           |     FBI UCR     | 2005 |    MSA    |
| prop_crimes_moto_theft  | Number of motor vehicle thefts in MSA                                                                     |     FBI UCR     | 2005 |    MSA    |
| pct_low_inf_bw          | Percent of infants born with low birth weight                                                             |      [UW PHI](https://www.countyhealthrankings.org/)     | 2005 |    MSA    |
| pct_obese_adult         | Percent of adults classified as obese                                                                     |      UW PHI     | 2005 |    MSA    |
| pct_uninsured_adult     | Percent of adults without health insurance                                                                |      UW PHI     | 2005 |    MSA    |
| pov_total_in_poverty    | Number of persons whose income in 1999 was below the poverty level                                        |      Census     | 2000 |    MSA    |
| pov_total               | Number of persons                                                                                         |      Census     | 2000 |    MSA    |
| pov_poverty_rate        | Percentage of persons whose income in 1999 was below the poverty level                                    |      Census     | 2000 |    MSA    |
| gini                    | Gini index of income inequality                                                                           |       ACS       | 2005 |    MSA    |
| total_bridges_2005      | Number of bridges in an MSA which are 10 years or older and are more than 20 feet in length               |       [FHWA](https://www.fhwa.dot.gov/bridge/nbi.cfm)      | 2005 |    MSA    |
| bad_bridges_2005        | Number of bridges in an MSA deemed structurally deficient                                                 |       FHWA      | 2005 |    MSA    |
| pct_deficient           | Percentage of bridges in an MSA deemed structurally deficient                                             |       FHWA      | 2005 |    MSA    |
| npl_sites               | Number of sites in an MSA on the National Priorities List                                                 |       [EPA](https://www.epa.gov/superfund/npl-site-status-information)       | 2005 |    MSA    |
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

### Notes

* Any MSA whose principal city was at its population peak as of the 2010 census was assigned a "decades since peak" of zero and a "decline since peak" of zero. Furthermore, the "decline since peak" variable is an absolute value measure of decline; thus, while measured as a positive number, *any nonzero value* is considered to be a liability for the pertaining MSA.
* The "hud_units_2005" variable included all housing units participating in HUD low-income housing programs (as classified by the 'program_label' descriptor in [this](https://www.huduser.gov/portal/picture2000/dictionary.pdf) data dictionary).
* The University of Wisconsin's Population Health Institute collected county-wide data for a number of health measures. The years of data collection for the three variables used in our analysis—infant birthweight, adult obesity, adult uninsured rate—are specified in [this](https://www.countyhealthrankings.org/sites/default/files/2010%20Analytic%20Documentation.pdf) data dictionary.

***

## Transformed Variables

The following is a list of variables that were transformed to account for non-normality.

| Variable         | Original             | Transformation Method    |  Type |
|------------------|----------------------|--------------------------|:-----:|
| enplane_per_cap  | enplanements         | per-capita (2005)        | Asset |
| chartbl_per_cap  | charitable_assets    | per-capita (2005)        | Asset |
| r1_per_cap       | r_1                  | per-capita (2005)        | Asset |
| r2_per_cap       | r_2                  | per-capita (2005)        | Asset |
| r_univ_per_cap   | r_1 and r_2 combined | per-capita (2005)        | Asset |
| freight_sqmi     | intermodal_freight   | per square mile (in MSA) | Asset |
| ln_hist_bldg     | hist_bldgs           | natural log              | Asset |
| ln_hist_registry | hist_registry_total  | natural log              | Asset |