# COMPSTAT Assignment 2 #

This is a **README** file for this project.

## ASSIGNMENT INSTRUCTIONS ##
Using all the files available in https://physionet.org/content/bidmc/1.0.0/:

    Create an object containing the mean HR and age for each subject.
    Plot the histogram of all the HR means with the respective density curve.
    Apply the expectation-maximization algorithm with 2 and 3 latent classes.
    Plot the densities for each of the latent classes. 
    Plot the mean HR as a function of each subject's age in which each point has a color corresponding to the class to which it belongs.
    Generate 1000 data points at random according to a single distribution fitted to the original data.
    Generate 1000 data points at random according to a mixture of distributions fitted using one of the previous EM computed in 3). 

The submitted code file should include comments to improve readability.

Notes:

- the filenames vary between bidmc_01 to bidmc_53

- the bidmc_XX_Numerics.csv contains four columns with the physiological parameters, such as the heart rate (HR), respiratory rate (RESP), and blood oxygen saturation level (SpO2). These are sampled at 1 Hz.

- the bidmc_XX_Fix.txt contains the values of Fixed variables, such as age and gender.

- the link to download the files are in the following format - "https://physionet.org/files/bidmc/1.0.0/bidmc_csv/bidmc_01_Numerics.csv?download" and "https://physionet.org/files/bidmc/1.0.0/bidmc_csv/bidmc_01_Fix.txt"

- do not also forget to use the functions density and EM already available in R.

Submitted code files should include comments to improve readability. The work can be done in groups of 2 or 3 people. You must indicate the group composition in the resolution and only one resolution per group needs to be submitted.

## FILE EXPLANATION ##
The necessary data files may be downloaded as described in the assignment instructions. The total zip file size is ~218 MB.
Data analysis (Assignment 2.R) was completed using RStudio with R version 4.3.3. Necessary libraries are listed at the beginning of the file and should be installed (if necessary) prior to running the code. The file directory to load the data files should be updated based on individual requirements. Code explanations are provided throughout the code file for clarity.
