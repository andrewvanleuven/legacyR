# Legacy Regions Database Codebook

Welcome to the online codebook for the legacy regions research dataset. The data are comprised of three groups of variables: controls, assets, and liabilities. This codebook describes each set of variables in terms of:

* *meaning* — how exactly the variable is to be interpreted
* *source* — where the variable was collected from 
* *geographic extent* — whether the variable is measured at the MSA level or at the level of the MSA's principal city

## Control Variables

| Variable             | Description                       |    Source    | Geography |
|----------------------|-----------------------------------|:------------:|:---------:|
| cbsa_fips            | Unique ID for each MSA            |      --      |    MSA    |
| cbsa                 | Name of each MSA                  |      --      |    MSA    |
| sqmi                 | Square mileage of each MSA        | Census TIGER |    MSA    |
| population_2005      | Population of each MSA in 2005    |      --      |    MSA    |
| density              | Number of persons per square-mile |      --      |    MSA    |
| age_total_young      | --                                |   US Census  |    MSA    |
| age_total_old        | --                                |   US Census  |    MSA    |
| age_total            | --                                |   US Census  |    MSA    |
| age_pct_u18          | --                                |   US Census  |    MSA    |
| age_pct_o64          | --                                |   US Census  |    MSA    |
| nat_total            | --                                |   US Census  |    MSA    |
| nat_foreign_born     | --                                |   US Census  |    MSA    |
| nat_not_foreign_born | --                                |   US Census  |    MSA    |
| nat_native           | --                                |   US Census  |    MSA    |
| nat_nativity         | --                                |   US Census  |    MSA    |
| laus_lfpr            | --                                |      BLS     |    MSA    |
| laus_pop             | --                                |      BLS     |    MSA    |
| laus_lf              | --                                |      BLS     |    MSA    |
| laus_emp             | --                                |      BLS     |    MSA    |
| laus_unemp           | --                                |      BLS     |    MSA    |

* The square mileage was calculated with the `sf` GIS package in R and used Census [TIGER/Line](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html) shapefiles downloaded using the `tigris` package developed by [Kyle Walker](https://github.com/walkerke).
* Population estimates (for non-decennial years) was collected from the [NIH SEER research data site](https://seer.cancer.gov/popdata/download.html).
* Any descriptions with "--" are still *in progress*

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

| Variable                | Description |      Source     | Geography |
|-------------------------|-------------|:---------------:|:---------:|
| city_peak_yr            | --          |      Census     |    City   |
| city_peak_pop           | --          |      Census     |    City   |
| city_decades_since_peak | --          |      Census     |    City   |
| city_pop_2010           | --          |      Census     |    City   |
| city_decline_since_peak | --          |      Census     |    City   |
| pctchg_00_05            | --          | Census/NIH SEER |    MSA    |
| vacancy_msa             | --          |      Census     |    MSA    |
| vacancy_city            | --          |      Census     |    City   |
| pct_prewar_city         | --          |      Census     |    City   |
| pct_prewar_msa          | --          |      Census     |    MSA    |
| total_units_2005        | --          |       HUD       |    MSA    |
| hud_units_2005          | --          |       HUD       |    MSA    |
| pct_ph                  | --          |       HUD       |    MSA    |
| med_val_cbsa            | --          |       ACS       |    MSA    |
| med_val_city            | --          |      Census     |    City   |
| prop_crimes_per100k     | --          |     FBI UCR     |    MSA    |
| prop_crimes_burglary    | --          |     FBI UCR     |    MSA    |
| prop_crimes_larceny     | --          |     FBI UCR     |    MSA    |
| prop_crimes_moto_theft  | --          |     FBI UCR     |    MSA    |
| pct_low_inf_bw          | --          |      UW PHI     |    MSA    |
| pct_obese_adult         | --          |      UW PHI     |    MSA    |
| pct_uninsured_adult     | --          |      UW PHI     |    MSA    |
| pov_total_in_poverty    | --          |      Census     |    MSA    |
| pov_total               | --          |      Census     |    MSA    |
| pov_poverty_rate        | --          |      Census     |    MSA    |
| gini                    | --          |       ACS       |    MSA    |
| total_bridges_2005      | --          |       FHWA      |    MSA    |
| bad_bridges_2005        | --          |       FHWA      |    MSA    |
| pct_deficient           | --          |       FHWA      |    MSA    |
| npl_sites               | --          |       EPA       |    MSA    |
| sfund_per_sqmi          | --          |       EPA       |    MSA    |
| edu_no_hs_diploma       | --          |      Census     |    MSA    |
| edu_bachelors_plus      | --          |      Census     |    MSA    |
| edu_total_bach          | --          |      Census     |    MSA    |
| edu_total_mast          | --          |      Census     |    MSA    |
| edu_total_prof          | --          |      Census     |    MSA    |
| edu_total_doct          | --          |      Census     |    MSA    |
| edu_total_nohs          | --          |      Census     |    MSA    |
| edu_total_degr          | --          |      Census     |    MSA    |
| edu_total_ov25          | --          |      Census     |    MSA    |

* Any descriptions with "--" are still *in progress*
