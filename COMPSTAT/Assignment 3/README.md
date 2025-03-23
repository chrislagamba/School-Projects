# COMPSTAT Assignment 3 #

This is a **README** file for this project.

## ASSIGNMENT INSTRUCTIONS ##

Simulate a population with 100,000 subjects. The resulting data frame should contain the following:

- A column for each risk factor:

  - Yearly risk of death - i.e., transition probability from alive to dead.

  - Yearly risk of disease - i.e., transition probability from healthy to ill.

- 48% are male

- 45% are 50 years old or older

- 27% of the males smoke

- 16% of the females smoke

For this population, it is estimated:

- a 5-year risk of disease of 17.6%

  - 50.3% if a smoker| 5.4% if a non-smoker

  - 22.1% if >/= 50 years old | 13.2% if < 50yo

 - a 10-year risk of any-cause mortality of 11.5%

  - 22.9% if >/= 50 years old | 1% if < 50yo

  - 12.3% if male | 10.6% if female

Risk factors are considered independent. How many of the patients developed the disease after 10 years? How many of the patients died after 10 years?

Submitted code files should include comments to improve readability. The work can be done in groups of 2 or 3 people. You must indicate the group composition in the resolution and only one resolution per group needs to be submitted.

## FILE EXPLANATION ##

Only the R file (COMPSTATassignment3.R) is necessary as the dataframe is created using appropriate functions. This was completed using RStudio with R version 4.3.3. Necessary libraries are listed at the beginning of the file and should be installed (if necessary) before running the code. The file directory to load the data files should be updated based on individual requirements. Code explanations are provided throughout the code file for clarity.
