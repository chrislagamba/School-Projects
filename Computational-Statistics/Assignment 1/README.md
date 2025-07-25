# COMPSTAT Assignment 1 #

This is a **README** file for this project.

## ASSIGNMENT INSTRUCTIONS ##
Consider the data available in the PhysioNet Challenge https://physionet.org/content/bidmc/1.0.0/

"The original data was acquired from critically-ill patients during hospital care at the Beth Israel Deaconess Medical Centre (Boston, MA, USA). Two annotators manually annotated individual breaths in each recording using the impedance respiratory signal. The 53 recordings within the dataset, each of 8-minute duration, each contain:

    Physiological signals include the PPG, impedance respiratory signal, and electrocardiogram (ECG). These are sampled at 125 Hz.
    Physiological parameters, such as the heart rate (HR), respiratory rate (RESP), and blood oxygen saturation level (SpO2). These are sampled at 1 Hz.
    Fixed parameters, such as age and gender
    Manual annotations of breaths"

Using R, keep counters of equal-width grid-cells (base counters for micro-cluster definitions) of a 2-dimensional continuous data stream using different window models (landmark, sliding, weighted, fading).
The code should work with an evolving stream from a single record applied to HR and RESP signals that can be found in one of the "Numerics.csv" files.

The package 'stream' should be used to access the data as a stream.

Explore packages such as the animation one to register the evolution of your 2-dimensional plot.

Submitted code files should include comments to improve readability. The work can be done in groups of 2 or 3 people. You must indicate the group composition in the resolution and only one resolution per group needs to be submitted.

## FILE EXPLANATION ##
The dataset for patient 14 (bidmc_14_Numerics.csv) was chosen for this project. The original dataset is located here: https://physionet.org/content/bidmc/1.0.0/bidmc_csv/bidmc_14_Numerics.csv
Data analysis (COMPSTATassignment1.R) was completed using RStudio with R version 4.3.3. Necessary libraries are listed at the beginning of the file and should be installed (if necessary) before running the code. The csv file directory should be updated based on individual requirements. Code explanations are provided throughout the code file for clarity.
