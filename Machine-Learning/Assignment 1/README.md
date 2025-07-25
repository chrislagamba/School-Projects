# LEARN Assignment 1 Readme

#### Introduction
This is a readme file for LEARN assignment 1.


#### Assignment Instructions
Consider the the Cervical Cancer (Risk Factors) data set (available from UCI repository) and try to accurately classify Dx.Cancer.

You must compare different approaches and parameters of a) single decision tree and b) random forest.

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
This dataset contained several correlated clinical variables. I started with a zero-variance analysis to identify variables with either zero-variance or near-zero-variance for removal. I then completed a brief literature review to further determine the clinical relevancy of the remaining variables. A correlation heatmap and matrix was developed to identify high correlations among the remaining variables. Imputation for missing data was completed using missForest. Rounding was completed due to float values being introduced where values should be integers. A second correlation heatmap and matrix was created to compare the "before" and "after" for data imputation. SMOTE-NC was applied to the train set due to severe class imbalance of the target variable. Rounding was completed again due to the introduction of float values. 

Modelling was then completed with boot.632 as the method. Initial models were created using the imbalanced train set for each model (decision tree and random forest). Modelling was then completed on the balanced train set. The "Dx" variable was noted to have extremely high importance and models were created with the variable removed to determine model performance without this variable.

Expanded explanations for some of these approaches are provided in the Discussion section of the RMD file. 