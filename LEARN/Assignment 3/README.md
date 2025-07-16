# LEARN Assignment 1 Readme

#### Introduction
This is a readme file for LEARN assignment 3.


#### Assignment Instructions
Consider the the Breast Cancer Coimbra data set (available from UCI repository).

Compare three different ensemble approaches, using the ‘caret’ package:

    Bagging (treebag and rf)
    Boosting (C5.0 and gdm)
    Stacking (at least three models)

Useful packages: bag and caretEnsemble

Evaluation of derived models should follow a correct methodology, comparing different estimates of generalization error (i.e. holdout, cross-validation, bootstrap, ...)

Submit a report (in PDF, generated from R) with the code and the resulting analysis.


#### Dataset
The dataset is available from the UCI Machine Learning Repository here:
https://archive.ics.uci.edu/dataset/451/breast+cancer+coimbra
(Link last checked 15 July 2025).

The dataset was essentially clean with no need for data transformation apart from converting the target variable ("Classification") into factor with levels "Healthy" and "Patient" based on the description on the UCI Machine Learning website for this dataset.


#### Language and IDE
This assignment was completed using R version 4.5.0. Initial data analysis and modelling was completed using VSCodium to get experience with a more robust IDE. The final script was loaded into RStudio and checked to verify the code would run. The RMarkdown file was created using RStudio.


#### Files
The file for this assignment is available in R Markdown format. "Notes" regarding data analysis and my thought processes are commented throughout the R code. A scientific paper format (IMRaD) was not required for this assignment. Only a discussion section was included (as requested by my professor) to discuss and compare the results of model performance.


#### Approaches
Since the dataset was clean, I was able to start modelling immediately after transforming the target variable into a factor. The stacked model (ensemble) was built using C5.0, random forest, and a generalized linear model (GLM) with a GLM meta-model. All models were trained with default hyperparameters to compare base performance.

Expanded explanations for the approaches and model performance are provided in the Discussion section of the RMD file. 
