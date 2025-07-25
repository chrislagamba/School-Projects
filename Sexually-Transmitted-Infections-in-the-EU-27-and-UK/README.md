# Introduction

This is a **README** file for the DATA folder. This folder contains a single project that was 75% of my final grade for the course. This was a group project completed with two other classmates and was the first significant project we were assigned in the program. The purpose of the project was to get hands-on exposure to R, RStudio, data cleaning, exploratory data analysis, data visualization, and presentation of relevant findings.

## Assignment Instructions

Goal:
- To deal with basic data preparation and data quality issues.
- To reuse health care secondary data to perform data analysis with R.

Data Source:
- GBD - Global Burden of Disease website (https://www.healthdata.org/gbd/about)
- Publicly available through an online repository
- Global Burden of Disease Study 2021 (GBD 2021) Data Resources - https://ghdx.healthdata.org/gbd-2021

Each group will be in charge of collecting, preparing and analyzing GBD data (Exploratory data analysis - EDA):
- To detect and address (when possible) existing data quality issues
    - “Artificial outliers”
    - Incomplete data (Missings)
    - Unreliable data (e.g., data that are outside biologically plausible values, differ from other external/official sources or changed implausibly over time)
    - Improperly formatted/structured data (e.g., date attribute is not formatted accordingly
- To analyze any context, measure, filters of interest, or preferably a combination of them
    - Context (e.g., Risk, cause, impairment, etiology, injury)
    - Measure i.e., health indicator (e.g., Deaths, DALYs, YLL, incidence, prevalence)
    - Filters (e.g., cause, sex, age, country)

Data should be comprised of over 300.000 rows. Use of the R language along with RStudio software is recommended. Compare findings with other external data sources (e.g., World Health Organization database, governmental sources, etc.).

Deliverables:
- Oral presentation (15 minutes)
- Report generated through RMarkdown + Abstract (in the format of a scientific paper)
- Annexes are allowed

## File Explanation

Necessary data files may be downloaded from the GBD Results Tool (https://vizhub.healthdata.org/gbd-results/)  using the filters specified in the DATA_GW.R code file. The IHME_GBD_2021_CODEBOOK_Y2024M05D16.csv file has been provided in this folder and should be downloaded to the same file folder GBD data files have been extracted. All necessary libraries have been included at the beginning of the code file and should be installed (if necessary) before running the code.
