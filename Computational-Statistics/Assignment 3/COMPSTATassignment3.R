## COMPSTAT Assignment 3 ##
## Diana Rios and Chris LaGamba ##


##### INITIALIZE THE ENVIRONMENT AND CREATE THE DATAFRAME FOR THE SIMULATED POPULATION #####
# load required libraries
library(dplyr)

# establishing a set seed for reproducibility
set.seed(448570) # 498669

# the number of subjects in the simulated population
n <- 100000

# simulating the gender composition of the simulated population; 48% should be male
sex <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52))

# simulating the age composition of the simulated population; 45% should be >/= 50 years
age <- ifelse(runif(n) < 0.45, sample(50:100, n, replace = TRUE), sample(18:49, n, replace = TRUE))

# simulating the smoking status of the simulated population; 27% males and 16% females are smokers
smoking_status <- ifelse(sex == "Male" & runif(n) < 0.27, "Smoker", ifelse(sex == "Female" & runif(n) < 0.16, "Smoker", "Non-Smoker"))

# create a dataframe using the established variables above
dataframe <- data.frame(sex, age, smoking_status)
View(dataframe)

## using dplyr to count subjects by age, gender, and smoking status to ensure conditions are met
# counting subjects by age
age_50_or_older <- dataframe %>%
  filter(age >= 50) %>%
  summarise(count = n()) %>%
  pull(count)

cat("Number of subjects aged 50 or older:", age_50_or_older, "\n") # should be >/= 45000

# counting subjects by gender
gender_counts <- dataframe %>%
  group_by(sex) %>%
  summarise(count = n())

print(gender_counts) # should be ~52000 for females and ~48000 for males

# Calculate proportions of smokers by gender
smoking_proportions_by_gender <- dataframe %>%
  group_by(sex, smoking_status) %>%
  summarise(count = n(), .groups = 'drop') %>%  # Count by group
  group_by(sex) %>%  # Regroup by gender to calculate proportions
  mutate(proportion = count / sum(count))  # Calculate proportion within each gender

print(smoking_proportions_by_gender) # "Smoker" should be ~0.16 for females and ~0.27 for males


##### ESTABLISHING RISK FACTORS #####

# Calculating yearly disease risk
dataframe$yearly_disease_risk <- ifelse(
  dataframe$smoking_status == "Smoker",
  1 - (1 - 0.503)^(1/5),
  1 - (1 - 0.054)^(1/5)
)
View(dataframe)

# Adjusting disease-risk based on age
dataframe$yearly_disease_risk <- ifelse(
  dataframe$age >= 50,
  dataframe$yearly_disease_risk * (1 - (1 - 0.221)^(1/5)) / (1 - (1 - 0.176)^(1/5)),
  dataframe$yearly_disease_risk * (1 - (1 - 0.132)^(1/5)) / (1 - (1 - 0.176)^(1/5))
)
View(dataframe)

# Calculating yearly mortality risk
dataframe$yearly_mortality_risk <- ifelse(
  dataframe$age >= 50,
  1 - (1 - 0.229)^(1/10),
  1 - (1 - 0.01)^(1/10)
)
View(dataframe)

# Adjusting mortality risk based on age
dataframe$yearly_mortality_risk <- ifelse(
  dataframe$sex == "Male",
  dataframe$yearly_mortality_risk * (1 - (1 - 0.123)^(1/10)) / (1 - (1 - 0.115)^(1/10)),
  dataframe$yearly_mortality_risk * (1 - (1 - 0.106)^(1/10)) / (1 - (1 - 0.115)^(1/10))
)
View(dataframe)


##### SIMULATING DEATH AND DISEASE DEVELOPMENT OVER 10 YEARS #####

# Creating columns for disease and death
dataframe$disease_status <- "Healthy"
dataframe$death_status <- "Alive"
View(dataframe)

# The 10-year simulation
for (year in 1:10) {
  # Simulating disease
  dataframe$disease_status <- ifelse(
    dataframe$disease_status == "Healthy" & runif(n) < dataframe$yearly_disease_risk,
    "Ill",
    dataframe$disease_status
  )
  
  # Simulating death
  dataframe$death_status <- ifelse(
    dataframe$death_status == "Alive" & runif(n) < dataframe$yearly_mortality_risk,
    "Dead",
    dataframe$death_status
  )
}


##### SIMULATION SUMMARY OF ILLNESS AND DEATH #####

# Number of subjects who developed disease
disease_count <- sum(dataframe$disease_status == "Ill")
cat("Number of subjects who developed disease after 10 years:", disease_count, "\n") # 23649 ill

# Number of subjects who died
death_count <- sum(dataframe$death_status == "Dead")
cat("Number of subjects who died after 10 years:", death_count, "\n") # 10658 dead
