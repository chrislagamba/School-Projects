# LEARN Assignment 2 Readme

#### Introduction
This is a readme file for LEARN assignment 2.


#### Assignment Instructions
Consider the the Cervical Cancer (Risk Factors) data set (available from UCI repository) and try to accurately classify Dx.Cancer.

You must compare different approaches and parameters of support vector machines.

Evaluation of derived models should follow a correct methodology, comparing different estimates of generalization error (i.e. holdout, cross-validation, bootstrap, ...)

Submit a report (in PDF, generated from R) with the code and the resulting analysis.


#### Dataset
The dataset is available from the UCI Machine Learning Repository here:
https://archive.ics.uci.edu/dataset/383/cervical+cancer+risk+factors
(Link last checked 15 July 2025).

Major problems with this dataset included missing values and severe class imbalance of the target variable (only 18 positive cervical cancer cases out of a total of 858 patients).


#### Language and IDE
This assignment was completed using R version 4.5.0. Initial data analysis and modelling was completed using VSCodium to get experience with a more robust IDE. The final script was loaded into RStudio and checked to verify the code would run. The RMarkdown file was created using RStudio.


#### Files
The file for this assignment is available in R Markdown format. "Notes" regarding data analysis and my thought processes are commented throughout the R code. A scientific paper format (IMRaD) was not required for this assignment. Only a discussion section was included (as requested by my professor) to discuss and compare the results of model performance.


#### Approaches
I followed a similar approach as I used in LEARN assignment 1 (modelling with decision trees and random forests). However, I opted for a different approach to gain more experience with other "tools." 

This dataset contained several correlated clinical variables. I started with a zero-variance analysis to identify variables with either zero-variance or near-zero-variance for removal. I then completed a brief literature review to further determine the clinical relevancy of the remaining variables. A correlation heatmap and matrix was developed to identify high correlations among the remaining variables. Imputation for missing data was completed using missForest. Rounding was completed due to float values being introduced where values should be integers. A second correlation heatmap and matrix was created to compare the "before" and "after" for data imputation.

I then opted to use a principal components analysis and principal components regression to determine the "best" variable combinations to test in the SVM models. I also used a LASSO model for further testing and to compare to the PCR models.

I opted for ROSE to create a balanced train set rather than the SMOTE-NC approach used in LEARN assignment 1 (mainly to try out another approach). 

Modelling was then completed with radial, linear, and polynomial SVM kernels. Initial models were created using the imbalanced train set with all variables for "baseline" models. Modelling was then completed using the balanced train set (again with all features). I compared untuned and tuned models for each (based on gamma and cost). Interestingly, model performance was worse for the tuned models. I then modelled using the most relevant variables based on coefficients from the LASSO regression. I finally modelled the most relevant component from the principal components regression. 

Expanded explanations for some of these approaches are provided in the Discussion section of the RMD file. 
