# LEARN Assignment 1 Readme

#### Introduction
This is a readme file for LEARN assignment 4.


#### Assignment Instructions


Consider the the Cervical Cancer (Risk Factors) data set (available from UCI repository) and exploit the data using association rules (for binary variables) and clustering (for continuous variables).
Snippets of code that might help using association rules:

    library(arules)
    transactions <- as(dataset, "transactions")
    rules <- apriori(transactions, parameter = list(supp = s, conf = c, target = "rules")
    inspect(rules)

Snippets of code that might help using clustering:

    clust <- kmeans(dataset, centers = k)
    plot(dataset$x, dataset$y, col = clust$cluster)
    for(i in 1:k) plot(clust$centers[i,], type="l")
    diss <- dist(dataset)
    hc <- hclust(diss, method = "complete")
    plot(hc)

Submit a report (in PDF, generated from R) with the code and the resulting analysis.



#### Dataset
The dataset is available from the UCI Machine Learning Repository here:
https://archive.ics.uci.edu/dataset/383/cervical+cancer+risk+factors
(Link last checked 15 July 2025).

Major problems with this dataset included missing values and severe class imbalance of the target variable (only 18 positive cervical cancer cases out of a total of 858 patients).


#### Language and IDE
This assignment was completed using R version 4.5.0. Initial data analysis and modelling was completed using VSCodium to get experience with a more robust IDE. The final script was loaded into RStudio and checked to verify the code would run. The RMarkdown file was created using RStudio.


#### Files
The file for this assignment is available in R Markdown format. "Notes" regarding data analysis and my thought processes are commented throughout the R code. A scientific paper format (IMRaD) was not required for this assignment.
