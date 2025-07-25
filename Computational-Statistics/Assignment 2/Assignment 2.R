## COMPSTAT Assignment 2 group project ##
## Diana Rios and Chris LaGamba ##

library(purrr)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(mclust)

# Create file lists
files_HR <- list.files(path = "D:/Estudio/Estadística Computacional/Assignment 2/bidmc-ppg-and-respiration-dataset-1.0.0/bidmc_csv", pattern = "\\.csv$", full.names = TRUE)
files_Age <- list.files(path = "D:/Estudio/Estadística Computacional/Assignment 2/bidmc-ppg-and-respiration-dataset-1.0.0/bidmc_csv", pattern = "\\.txt$", full.names = TRUE)

# Task 1 - Object
# Function to calculate the average heart rate 
calculate_HR_mean <- function(file_path) {
  data <- read_csv(file_path) 
  HR_mean <- mean(data$HR, na.rm = TRUE) 
  return(HR_mean)
}
HR_mean <- map_dbl(files_HR, calculate_HR_mean)

HR_mean_data_clean <- na.omit(HR_mean)
print(HR_mean_data_clean)

# Function to extract Age

extract_Age <- function(files) {
  lines <- readLines(files)
  
  line_Age <- str_subset(lines, "Age:")
  
  Age <- str_extract(line_Age, "\\d+") %>% as.numeric()
  
  tibble(
    bidmc = tools::file_path_sans_ext(basename(files)),  Age = Age)
}
Ages <- map_df(files_Age, extract_Age)
print(Ages)

# Creating an object containing the mean HR and age for each subject

object1 <- tibble(
  bidmc = 1:length(HR_mean_data_clean),  
  Average_heart_rate = HR_mean_data_clean,
  Age = Ages
)

object1$bidmc <- object1$Age$bidmc  
object1$Age <- object1$Age$Age      
head(object1)
summary(object1)

# Task 2 - Graphic

ggplot(object1, aes(x = Average_heart_rate)) +
  geom_histogram(aes(y = after_stat(density)),  # Actualización aquí
                 binwidth = 5,          
                 color = "black",       
                 fill = "lightblue") + 
  geom_density(color = "blue",          
               size = 1) +              
  labs(title = "Histogram of Heart Rate Averages, BIDMC PPG and Respiration Dataset",
       x = "Average Heart Rate",
       y = "Density") +
  theme_minimal()

# Task 3 - Expectation-Maximization Algorithm

frequencies <- object1$Average_heart_rate
set.seed(523) 

em_2classes <- Mclust(frequencies, G = 2)  
summary(em_2classes)  

em_3classes <- Mclust(frequencies, G = 3)  
summary(em_3classes) 

# Task 4 - Plots

plot(em_2classes, what = "density")  
plot(em_3classes, what = "density")  

# Task 5 Plot of HR as a function of each subject's age

# Add the class to the data frame

object1$class <- em_2classes$classification  
object1$class <- em_3classes$classification  
object1$class <- as.factor(object1$class)


ggplot(object1, aes(x = Age, y = Average_heart_rate, color = class)) +
  geom_point(size = 3, alpha = 0.8) +  
  labs(title = "Average HR as a function of Age",
       x = "Age",
       y = "Average Heart Rate",
       color = "Class") +
  theme_minimal()


# Task 6 Generate 1000 data points - single distribution

# Fitting a distribution to the original data


mean_HR <- mean(object1$Average_heart_rate, na.rm = TRUE)
sd_HR <- sd(object1$Average_heart_rate, na.rm = TRUE)
mean_Age <- mean(object1$Age, na.rm = TRUE)
sd_Age <- sd(object1$Age, na.rm = TRUE)

# Generating 1000 random data points

set.seed(523)  
HR_generated <- rnorm(1000, mean = mean_HR, sd = sd_HR)
Age_generated <- rnorm(1000, mean = mean_Age, sd = sd_Age)

data_generated <- data.frame(
  Age = Age_generated,
  Average_heart_rate = HR_generated
)

summary(data_generated)

# Plot

ggplot(data_generated, aes(x = Age, y = Average_heart_rate)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Mean Heart Rate vs Mean Age",
       x = "Age",
       y = "Mean Heart Rate") +
  theme_minimal()


# Task 7 Generate 1000 data points - mixture of distributions

# Extracting the parameters from the EM model
means <- em_3classes$parameters$mean  
variance_common <- em_3classes$parameters$variance$sigmasq  
proportions <- em_3classes$parameters$pro 

# Generating 1000 random data points
set.seed(523) 
n <- 1000
classes <- sample(1:3, size = n, replace = TRUE, prob = proportions)
data_generated2 <- sapply(1:n, function(i) {
  rnorm(1, mean = means[classes[i]], sd = sqrt(variance_common))  
})
data_generated2 <- data.frame(
  Average_heart_rate = data_generated2,
  Class = as.factor(classes))
summary(data_generated2)

# Plots
ggplot(data_generated2, aes(x = Average_heart_rate, fill = Class)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("1" = "blue", "2" = "green", "3" = "violet")) +  
  labs(title = "Mixture of distributions (3 classes)",
       x = "Average Heart Rate",
       y = "Frequency",
       fill = "Class") +  
  theme_minimal()


ggplot(data_generated2, aes(x = Average_heart_rate, color = Class)) +
  geom_density(size = 1) +
  scale_color_manual(values = c("1" = "blue", "2" = "green", "3" = "violet")) +  
  labs(title = "Mixture of distributions (3 classes)",
       x = "Frecuencia cardíaca media",
       y = "Density",
       color = "Class") +  
  theme_minimal()