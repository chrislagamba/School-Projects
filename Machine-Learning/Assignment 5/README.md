# LEARN Assignment 5 Readme

#### Introduction
This is a readme file for LEARN assignment 5.


#### Assignment Instructions

Using the stream package in R, simulate a learning environment where:

     data is produced in a stream from N(0,2)

     positive class is determined by data being lower than 2

     classification is performed by the default classifier (always positive)

     after 1000 data points, stream changes to N(2,2)

     compute and monitor the prequential error using zero-one loss and four different window models

Incorporate a change detection mechanism into the learning environment monitor.

For example, trigger an alarm if the prequential error rises above a statistical boundary such as

    warning: err + stdev > min (error) + 2 * min (stdev)

    drift: err + stdev > min (error) + 3 * min (stdev)


Submit a report (in PDF, generated from R) with the code and the resulting analysis.


#### Language and IDE
This assignment was completed using R version 4.5.0 and RStudio. The R Markdown file was created using RStudio.


#### Files
The file for this assignment is available in R Markdown format. "Notes" regarding data analysis and my thought processes are commented throughout the R code. A scientific paper format (IMRaD) was not required for this assignment. However, I opted for a scientific paper format for the presentation of my results.
