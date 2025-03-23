
#### RStudio Setup ####
# Install needed packages for this script (has to be run only once) 

# install.packages("readxl")
# install.packages("tidyr")
# install.packages("summarytools")
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("directlabels")
# install.packages("scales")
# install.packages("rmarkdown")
# install.packages("ggrepel")
# install.packages("ggforce")
# install.packages("maps")
# install.packages("rnaturalearthdata")
# install.packages("rnaturalearth")
# install.packages("sf")
# install.packages("gridExtra)



# Load libraries
library(readxl)
library(tidyr)
library(summarytools)
library(ggplot2)
library(tidyverse)
library(directlabels)
library(scales)
library(rmarkdown)
library(ggrepel)
library(ggforce)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(gridExtra)


#### Preparing the downloaded files from GBD for use ####

# GBD download filters
## Cause of Death
## Death, DALY, Prevalence, Incidence
## Number, Percent, Rate
## HIV/AIDS, Syphilis, Chlamydial Infection, Gonococcal Infection, Trichomoniasis, Genital Herpes, Other sexually transmitter diseases
## [EU-28]
## All ages, 0-14, 15-49, 50-74, 75+
## Male, Female, Both
## [year 2000-2021]


# Due to download restraints in large files from the GBD, we had to download multiple smaller files separately and join them together
# This section prepares the main DF that will be used for the main analysis

# Set working directory to where all the files downloaded from the GBD Database are
setwd("DATA/Coding Project/Second attempt")


# Merge all the downloaded GBD files into a single data frame "STI"
GBD_RAW <- 
  do.call(rbind,
          lapply(list.files("DATA/Coding Project/Second attempt/Unzipped Files/"), read.csv))


# Some of the files downloaded includes one extra variable that was not supposed to be included. 
# Error cleanup: Remove lines with "393" (STIs without HIV) on column "cause"

# create a new DF without the lines that have the wrong code
STI_temp <- filter(GBD_RAW, cause !="393") 

# confirm presence/absence of the 393 code in old/new DF
table(GBD_RAW$cause) 
table(STI_temp$cause) 

# Export STI into CSV, into main Working Directory
write.csv(STI_temp, "DATA/Coding Project/Second attempt/GBD_STI.csv", row.names = FALSE)

# Download error has been corrected: from now, the main data file used will be "./WorkDir/GBD_STI.csv"



#### Mounting and Preparing the Main DF ####

# clear everything in the environment
rm(list = ls())

# Set working directory to main directory
setwd("DATA/Coding Project/Second attempt")

# load required DFs
GBD_STI <- read.csv("GBD_STI.csv")
GBD_KEY <- read.csv("IHME_GBD_2021_CODEBOOK_Y2024M05D16.csv")

# make a copy of the main DF
STI <- GBD_STI

# Swap IDs for their Full Names in STI
STI$measure  <- GBD_KEY$measure_name  [match(STI$measure,  GBD_KEY$measure_id)]
STI$location <- GBD_KEY$location_name [match(STI$location, GBD_KEY$location_id)]
STI$sex      <- GBD_KEY$sex_label     [match(STI$sex,      GBD_KEY$sex_id)]
STI$age      <- GBD_KEY$age_group_name[match(STI$age,      GBD_KEY$age_group_id)]
STI$cause    <- GBD_KEY$cause_name    [match(STI$cause,    GBD_KEY$cause_id)]
STI$metric   <- GBD_KEY$metric_name   [match(STI$metric,   GBD_KEY$metric_id)]

# Change data type
STI$measure  <- as.factor(STI$measure)
STI$location <- as.factor(STI$location)
STI$sex      <- as.factor(STI$sex)
STI$age      <- as.factor(STI$age)
STI$cause    <- as.factor(STI$cause)
STI$metric   <- as.factor(STI$metric)

# confirm changes
str(STI)

# verify all data present after changing existing values. Failed matches return "NA". Both tables should return the same number of NAs
sapply(GBD_STI, function(x) sum(is.na(x)))
sapply(STI, function(x) sum(is.na(x)))



#### Descriptive Statistics ####


## Initial Description of STI DF
# base::summary(STI)
# summarytools::descr(STI)
dfSummary(STI)               # we chose this one for its more detailed info
diagnose_web_report(STI)     # very fancy html output


## Finding outliers
# First diagnosis
diagnose_outlier(STI)

# But 143k outliers in 702k were too much, so we investigated further
X_POR_DEA_HIV_NUM_ALA <- filter(STI, measure == "Deaths" & location =="Portugal" & cause =="HIV/AIDS" & metric =="Number" & age =="All Ages")
diagnose_outlier(X_POR_DEA_HIV_NUM_ALA)

X_POR_PRE_HIV_NUM_ALA <- filter(STI, measure == "Prevalence" & location =="Portugal" & cause =="HIV/AIDS" & metric =="Number" & age =="All Ages")
diagnose_outlier(X_POR_PRE_HIV_NUM_ALA)

# We concluded that the outliers were not true outliers, so we kept them in


## Missing values
# Diagnosis
sapply(STI, function(x) sum(is.na(x)))

# There are no NA values in the data.


## Unreliable data
# Rates are described as "events per 100k population". There should not be any rates >100000
base::summary(STI %>% filter(metric == "Rate" & val > 100000))

# Percentages must be ≤1
base::summary(STI %>% filter(metric == "Percent" & val > 1))

# There should not be any 0s in the data.
base::summary(STI %>% filter(val == "0"))

# make a DF with only the "0"
STI_0 <- STI %>% filter(val == "0")

# Check percentage of "0" 
STI_0_sum = summarytools::descr(STI_0)
STI_sum   = summarytools::descr(STI)
STI_0_sum[14,]/STI_sum[14,]

# first diagnosis of "0"
dfSummary(STI_0)
table(STI_0$measure, STI_0$location)
table(STI_0$sex,     STI_0$age)
table(STI_0$cause,   STI_0$metric)
dfSummary(STI_0 %>% filter(cause == "Chlamydial infection"))
# there is no data on Male + Deaths + Chlamydial infection

# second diagnosis of "0"
STI_0a <- STI_0 %>% filter(cause != "Chlamydial infection")
dfSummary(STI_0a)
table(STI_0a$measure, STI_0a$location)
table(STI_0a$sex,     STI_0a$age)
table(STI_0a$cause,   STI_0a$metric)
dfSummary(STI_0 %>% filter(measure == "Incidence"))
dfSummary(STI_0 %>% filter(measure == "Incidence" & cause == "Syphilis")) 
# there is no data on incidence of Syphilis on ages 75+ 

dfSummary(STI_0 %>% filter(measure == "Incidence" & cause == "Other sexually transmitted infections")) 
# there is no data on incidence of "Other sexually transmitted infections"

# third diagnosis of "0"
STI_0b <- STI_0a %>% filter(measure != "Incidence")
dfSummary(STI_0b)
# there is no data on prevalence of "Other sexually transmitted infections" on 75+



## Improperly formatted data -- we assume that there is none, as it was downloaded directly from GBD



#### ggplot fun time ####

# Maps graph
## Death rate Map of Europe

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]

STI_D_00 <- STI %>%
  filter(measure == "Deaths" & sex == "Both" & age == "All Ages" & metric == "Rate" & year == 2000) %>%
  group_by(location) %>%
  summarise(total= sum(val))
colnames(STI_D_00)[1] <- "name"

EuroSTI_D_00 <- merge(Europe,STI_D_00,by.y="name", all = TRUE)

p1 <- ggplot(EuroSTI_D_00) +
  geom_sf(aes(fill=total))+
  coord_sf(xlim = c(-26,50), ylim = c(34,72), expand = FALSE)+
  scale_fill_gradient(high = "red", low = "blue", limits = c(0,10), name = "Death rate/100k")+
  labs(title= "Death rate of STIs in 2000")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())


STI_D_21 <- STI %>%
  filter(measure == "Deaths" & sex == "Both" & age == "All Ages" & metric == "Rate" & year == 2021) %>%
  group_by(location) %>%
  summarise(total= sum(val))
colnames(STI_D_21)[1] <- "name"

EuroSTI_D_21 <- merge(Europe,STI_D_21,by.y="name", all = TRUE)

p2 <- ggplot(EuroSTI_D_21) +
  geom_sf(aes(fill=total))+
  coord_sf(xlim = c(-26,50), ylim = c(34,72), expand = FALSE)+
  scale_fill_gradient(high = "red", low = "blue", limits = c(0,10), name = "Death rate/100k")+
  labs(title= "Death rate of STIs in 2021")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

grid.arrange(p1,p2,nrow=1)






## Prevalence Map of Europe

world <- ne_countries(scale = "medium", returnclass = "sf") #load worldmap data
Europe <- world[which(world$continent == "Europe"),] #isolate worldmap data to only europe

STI_P_00 <- STI %>% #DF with sums of all the prevalence rates for all the STIs by location for the year 2000
  filter(measure == "Prevalence" & sex == "Both" & age == "All Ages" & metric == "Rate" & year == "2000") %>%
  group_by(location) %>%
  summarise(total= sum(val))
colnames(STI_P_00)[1] <- "name"

EuroSTI_P_00 <- merge(Europe,STI_P_00,by.y="name", all = TRUE)

p3 <- ggplot(EuroSTI_P_00) +
  geom_sf(aes(fill=total))+
  coord_sf(xlim = c(-26,50), ylim = c(34,72), expand = FALSE)+
  scale_fill_gradient(high = "red", low = "green", limits = c(10000,21000), name = "Prevalence/100k")+
  labs(title= "Prevalence of STIs in 2000")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), #remove long labels
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) #remove lat labels


STI_P_21 <- STI %>%
  filter(measure == "Prevalence" & sex == "Both" & age == "All Ages" & metric == "Rate" & year == "2021") %>%
  group_by(location) %>%
  summarise(total= sum(val))
colnames(STI_P_21)[1] <- "name"

EuroSTI_P_21 <- merge(Europe,STI_P_21,by.y="name", all = TRUE)

p4 <- ggplot(EuroSTI_P_21) +
  geom_sf(aes(fill=total))+
  coord_sf(xlim = c(-26,50), ylim = c(34,72), expand = FALSE)+
  scale_fill_gradient(high = "red", low = "green", limits = c(10000,21000), name = "Prevalence/100k")+
  labs(title= "Prevalence of STIs in 2021")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

grid.arrange(p3,p4,nrow=1)




## Incidence Map of Europe

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]

STI_I_00 <- STI %>%
  filter(measure == "Incidence" & sex == "Both" & age == "All Ages" & metric == "Rate" & year == "2000") %>%
  group_by(location) %>%
  summarise(total= sum(val))
colnames(STI_I_00)[1] <- "name"

EuroSTI_I_00 <- merge(Europe,STI_I_00,by.y="name", all = TRUE)

p5 <- ggplot(EuroSTI_I_00) +
  geom_sf(aes(fill=total))+
  coord_sf(xlim = c(-26,50), ylim = c(34,72), expand = FALSE)+
  scale_fill_gradient(high = "orange", low = "royalblue", limits = c(3000,10000), name = "Incidence/100k")+
  labs(title= "Incidence of STIs in 2000")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())


STI_I_21 <- STI %>%
  filter(measure == "Incidence" & sex == "Both" & age == "All Ages" & metric == "Rate" & year == "2021") %>%
  group_by(location) %>%
  summarise(total= sum(val))
colnames(STI_I_21)[1] <- "name"

EuroSTI_I_21 <- merge(Europe,STI_I_21,by.y="name", all = TRUE)

p6 <- ggplot(EuroSTI_I_21) +
  geom_sf(aes(fill=total))+
  coord_sf(xlim = c(-26,50), ylim = c(34,72), expand = FALSE)+
  scale_fill_gradient(high = "orange", low = "royalblue", limits = c(3000,10000), name = "Incidence/100k")+
  labs(title= "Incidence of STIs in 2021")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

grid.arrange(p5,p6,nrow=1)


# combine all maps into a single plot
grid.arrange(p1,p2,p3,p4,p5,p6)





## GRAPHICS
# Death Rate evolution through the years by cause (x= year, y=death rate, color=cause)
## MOST deadly: HIV + Syphilis

#c HIV only
ggplot(STI %>% 
         filter(measure == "Deaths" &
                  year >= 2000 & year <= 2021 &
                  metric == "Number" &
                  sex == "Both"), 
       aes(x = year, y = val, fill = cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Trend of Deaths in EU-28 by Cause (2000-2021)", x = "Year", y = "Number of Deaths") +
  scale_fill_viridis_d(option = "C") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#c without HIV
ggplot(STI %>% 
         filter(measure == "Deaths" &
                  year >= 2000 & year <= 2021 &
                  metric == "Number" &
                  sex == "Both" &
                  cause != "HIV/AIDS"), 
       aes(x = factor(year), y = val, fill = cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Number of Non-HIV/AIDS STI-Related Deaths by Year and Cause (2000-2021)", x = "Year", y = "Number of Deaths") +
  scale_fill_viridis_d(option = "D") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1))

# Trend Death Rate all STIs, per country, per year
ggplot(STI %>%
         filter(measure == "Deaths" & sex == "Both" & metric == "Rate") %>%
         group_by(location, year) %>%
         summarise(totloc = sum(val)))+
  aes(x= year , y= totloc, colour= location)+
  geom_line(size = 1)+
  labs(x= "Year", y= "Deaths/100k pop", title= "Rate of Deaths, by year, by country")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_dl(aes(label= location), method = list("first.points"))

# Trend Death Rate all countries, per STI, per year -- INSET ZOOM
ggplot(STI %>%
         filter(measure == "Deaths" & sex == "Both" & metric == "Rate") %>%
         group_by(cause, year) %>%
         summarise(tcause = sum(val)))+
  aes(x= year , y= tcause, colour= cause)+
  geom_line(size = 2)+
  labs(x= "Year", y= "Deaths/100k pop", title= "Rate of Deaths, by year, by STI")+
  theme(legend.position = "bottom")+
  geom_dl(aes(label= cause), method = list("last.points"))+
  facet_zoom(ylim = c(0, 12), zoom.size = 1)






# DALYs Rate evolution through the years by cause (x= year, y=death rate, color=cause)
## MOST DALYs: HIV + Syphilis
  # + separated HIV graph

#c DALY Rate per year, HIV only
daly_rates_by_year <- STI %>%
  filter(measure == "DALYs (Disability-Adjusted Life Years)" &
           year >= 2000 & year <= 2021 & 
           metric == "Rate" & 
           sex == "Both" &
           cause == "HIV/AIDS") %>%
  group_by(year, cause) %>%
  summarise(daly_rate = sum(val, na.rm = TRUE), .groups = "drop")

ggplot(daly_rates_by_year, aes(x = factor(year), y = daly_rate, color = cause, group = cause)) +
  geom_line(linewidth = .2) + 
  geom_point(size = 1) +
  labs(title = "HIV/AIDS DALY Rates Across All EU-28 Countries (2000-2021)", 
       x = "Year", 
       y = "DALYs per 100,000 Population") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1)) +
  scale_color_viridis_d(option = "D")

#c DALY Rate per year, non-HIV only
daly_rates_by_year <- STI %>%
  filter(measure == "DALYs (Disability-Adjusted Life Years)" &
           year >= 2000 & year <= 2021 & 
           metric == "Rate" & 
           sex == "Both" &
           cause != "HIV/AIDS") %>%
  group_by(year, cause) %>%
  summarise(daly_rate = sum(val, na.rm = TRUE), .groups = "drop")

ggplot(daly_rates_by_year, aes(x = factor(year), y = daly_rate, color = cause, group = cause)) +
  geom_line(linewidth = 0.2) + 
  geom_point(size = 1) +
  labs(title = "STI DALY Rates Across All EU-28 Countries (2000-2021)", 
       x = "Year", 
       y = "DALYs per 100,000 Population") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1)) +
  scale_color_viridis_d(option = "D")

# Trend DALYs all STIs, per country, per year
ggplot(STI %>%
         filter(measure == "DALYs (Disability-Adjusted Life Years)" & sex == "Both" & metric == "Rate") %>%
         group_by(location, year) %>%
         summarise(totloc = sum(val)))+
  aes(x= year , y= totloc, colour= location)+
  geom_line(size = 1)+
  labs(x= "Year", y= "DALYs/100k pop", title= "Rate of DALYs, by year, by country")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_dl(aes(label= location), method = list("first.points"))







# Prevalence Rate evolution through the years by cause (x= year, y=death rate, color=cause)
## MOST prevalent: Herpes, Chlamyida, (Trichomonas)

ggplot(STI %>% 
         filter(measure == "Prevalence" & metric == "Rate" & sex == "Both" & age == "All Ages"),
       aes(x=year, y=val, fill= cause)) +
  geom_area() +
  # scale_color_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Set1") +
  labs(x= "Year", y= "Prevalence/100000 pop", title= "Prevalence") +
  facet_wrap(vars(location), nrow = 4)+
  theme_minimal() +
  theme(legend.position = "bottom")

##

ggplot(STI %>% 
         filter(measure == "Prevalence" &
                  year >= 2000 & year <= 2021 &
                  metric == "Rate" &
                  sex == "Both" &
                  age == "All Ages"), 
       aes(x = factor(year), y = val)) +
  geom_line(size = 1) +
  labs(title = "Prevalence of STIs in EU-28 by Cause (2000-2021)", x = "Year", y = "Prevalence/100000 pop") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1))


# Trend Prevalence Rate all STIs, per country, per year
ggplot(STI %>%
         filter(measure == "Prevalence" & 
                  sex == "Both" & 
                  metric == "Rate" & 
                  age =="All Ages" &
                  location != "Estonia" & location != "Latvia" & location != "Lithuania" &
                  location != "Bulgaria" & location != "Germany" & location != "Sweden" &
                  location != "Netherlands" & location != "Croatia") %>%
         group_by(location, year) %>%
         summarise(totloc = sum(val)))+
  aes(x= year , y= totloc, colour = location)+
  geom_line(size = 1)+
  labs(x= "Year", y= "Prevalence/100k pop", title= "Prevalence, by year, by country")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_dl(aes(label= location), method = list("first.points"))
  scale_y_continuous(labels = label_number(accuracy = 100))





# Incidence Rate evolution through the years by cause (x= year, y=death rate, color=cause)
## HIGHEST incidence: Trichomonas, Chlamyida, Gonococcal

#c
ggplot(STI %>% 
         filter(measure == "Incidence" &
                  year >= 2000 & year <= 2021 &
                  metric == "Number" &
                  sex == "Both"), 
       aes(x = location, y = val, fill = cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "EU-28 Incidence by Country and Cause (2000-2021)", x = "Year", y = "Incidence") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  scale_fill_viridis_d(option = "A") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#c only HIV
ggplot(STI %>% 
         filter(measure == "Incidence" &
                  metric == "Number" &
                  sex == "Both" &
                  cause == "HIV/AIDS"), 
       aes(x = factor(year), y = val, fill = cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "EU-28 Incidence of HIV/AIDS by Year (2000-2021)", x = "Year", y = "Number of New Cases in the Population") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  scale_fill_viridis_d(option = "D") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1))

#c only CT NG HSV
ggplot(STI %>% 
         filter(measure == "Incidence" &
                  year >= 2000 & year <= 2021 &
                  metric == "Number" &
                  sex == "Both" &
                  cause != "HIV/AIDS" & cause != "Other STIs" & cause != "Trichomoniasis" & cause != "Syphilis"), 
       aes(x = factor(year), y = val, fill = cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "EU-28 Incidence of STIs by Year (2000-2021)", x = "Year", y = "Number of New Cases in the Population") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  scale_fill_viridis_d(option = "D") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1))

#c TV
ggplot(STI %>% 
         filter(measure == "Incidence" &
                  year >= 2000 & year <= 2021 &
                  metric == "Number" &
                  sex == "Both" &
                  cause == "Trichomoniasis"), 
       aes(x = factor(year), y = val, fill = cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "EU-28 Incidence of Trichomoniasis by Year (2000-2021)", x = "Year", y = "Number of New Cases in the Population") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  scale_fill_viridis_d(option = "D") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 80, hjust = 1))


# Trend Incidence Rate all STIs, per country, per year
ggplot(STI %>%
         filter(measure == "Incidence" & sex == "Both" & age == "All Ages" & metric == "Rate") %>%
         group_by(location, year) %>%
         summarise(totloc = sum(val)))+
  aes(x= year , y= totloc, colour= location)+
  geom_line(size = 1)+
  labs(x= "Year", y= "Incidence/100k pop", title= "Incidence, by year, by country")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_dl(aes(label= location), method = list("first.points"))







# Death Rate caused by HIV in both sexes (by age, country, year)
ggplot(STI %>% 
         filter(measure == "Deaths" & cause == "HIV/AIDS" & metric == "Rate" & sex == "Both" & age!= "All Ages"),
       aes(x=year, y=val, fill= age)) +
  geom_area() +
  # scale_color_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Set1") +
  labs(x= "Year", y= "No. of Deaths/100000 pop", title= "Death Rate, caused by HIV/AIDS, in both sexes (by age, by country, by year)") +
  facet_wrap(vars(location), nrow = 4, scales = "free")+
  theme_minimal() +
  theme(legend.position = "bottom")


# Death Rate caused by Syphilis in both sexes (by age, country, year)
ggplot(STI %>% 
         filter(measure == "Deaths" & cause == "Syphilis" & metric == "Rate" & sex == "Both" & age!= "All Ages"),
       aes(x=year, y=val, fill= age)) +
  geom_area() +
  # scale_color_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Set1") +
  labs(x= "Year", y= "No. of Deaths/100000 pop", title= "Death Rate, caused by Syphilis, in both sexes (by age, by country, by year)") +
  facet_wrap(vars(location), nrow = 4, scales = "free")+
  theme_minimal() +
  theme(legend.position = "bottom")


##
# DALYs of HIV in both sexes (by age, country, year)
ggplot(STI %>% 
         filter(measure == "DALYs (Disability-Adjusted Life Years)" & cause == "HIV/AIDS" & metric == "Rate" & sex == "Both" & age!= "All Ages"),
       aes(x=year, y=val, fill= age)) +
  geom_area() +
  # scale_color_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Set1") +
  labs(x= "Year", y= "DALYs/100000 pop", title= "DALYs, caused by HIV/AIDS, in both sexes (by age, by country, by year)") +
  facet_wrap(vars(location), nrow = 4, scales = "free")+
  theme_minimal() +
  theme(legend.position = "bottom")


# DALYs of Syphilis in both sexes (by age, country, year)
ggplot(STI %>% 
         filter(measure == "DALYs (Disability-Adjusted Life Years)" & cause == "Syphilis" & metric == "Rate" & sex == "Both" & age!= "All Ages"),
       aes(x=year, y=val, fill= age)) +
  geom_area() +
  # scale_color_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Set1") +
  labs(x= "Year", y= "DALYs/100000 pop", title= "DALYs, caused by Syphilis, in both sexes (by age, by country, by year)") +
  facet_wrap(vars(location), nrow = 4, scales = "free")+
  theme_minimal() +
  theme(legend.position = "bottom")


##
# Prevalence of Genital Herpes in both sexes (by age, country, year)
ggplot(STI %>%
         filter(measure == "Prevalence" & cause == "Genital herpes" & metric == "Rate" & sex == "Both" & age!= "All Ages"),
       aes(x=year, y=val, fill= age)) +
  geom_area() +
  # scale_color_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Set1") +
  labs(x= "Year", y= "Prevalence/100000 pop", title= "Prevalence of Genital herpes, in both sexes (by age, by country, by year)") +
  facet_wrap(vars(location), nrow = 4, scales = "free")+
  theme_minimal() +
  theme(legend.position = "bottom")

# Prevalence of Chlamydial Infection in both sexes (by age, country, age)
ggplot(STI %>%
        filter(measure == "Prevalence" & cause == "Chlamydial infection" & metric == "Rate" & sex == "Both" & age!= "All Ages"),
      aes(x=year, y=val, fill= age)) +
  geom_area() +
  # scale_color_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Set1") +
  labs(x= "Year", y= "Prevalence/100000 pop", title= "Prevalence of Chlamydial infection, in both sexes (by age, by country, by year)") +
  facet_wrap(vars(location), nrow = 4, scales = "free")+
  theme_minimal() +
  theme(legend.position = "bottom")


# Prevalence of Chlamydial Infection in all ages (by sex, country, year)
ggplot(STI %>%
         filter(measure == "Prevalence" & cause == "Chlamydial infection" & metric == "Rate" & sex != "Both" & age== "All Ages"),
       aes(x=year, y=val, fill= sex)) +
  geom_area() +
  # scale_color_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Set1") +
  labs(x= "Year", y= "Prevalence/100000 pop", title= "Prevalence of Chlamydial infection, all ages (by sex, by country, by year)") +
  facet_wrap(vars(location), nrow = 4, scales = "free")+
  theme_minimal() +
  theme(legend.position = "bottom")

# DALYs of HIV in all ages (by sex, country, age)
ggplot(STI %>%
        filter(measure == "DALYs (Disability-Adjusted Life Years)" & cause == "HIV/AIDS" & metric == "Rate" & sex != "Both" & age == "All Ages"),
      aes(x=year, y=val, fill= sex)) +
  geom_area() +
  # scale_color_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Set1") +
  labs(x= "Year", y= "DALYs/100000 pop", title= "DALYs of HIV in both sexes (by age, by country, by year)") +
  facet_wrap(vars(location), nrow = 4, scales = "free")+
  theme_minimal() +
  theme(legend.position = "bottom")



### Testing unreliable data
# Comparing the 0s. Syphilis has no 0s
ggplot(STI %>% 
         filter(measure == "Deaths" & cause =="Syphilis" & metric =="Number" & sex !="Both" & age =="All Ages"),
       aes(x=year, y=val, fill = sex)) + 
  geom_area()+
  #scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  labs(x="Year", y="No. of Deaths/year", title="Deaths caused by Syphilis, in all ages (by sex, by country, by year)")+
  facet_wrap(vars(location), nrow = 4)

# Comparing the 0s. CT has 0s
ggplot(STI %>% 
         filter(measure == "Deaths" & cause =="Chlamydial infection" & metric =="Number" & sex !="Both" & age =="All Ages"),
       aes(x=year, y=val, fill = sex)) + 
  geom_area()+
  #scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  labs(x="Year", y="No. of Deaths/year", title="Deaths caused by Chlamydial infection, in all ages (by sex, by country, by year)")+
  facet_wrap(vars(location), nrow = 4)








STI2 <- STI %>%
  filter(measure == "DALYs (Disability-Adjusted Life Years)"
         & cause == "HIV/AIDS"
         & metric == "Rate"
         & sex != "Both" 
         & age == "All Ages") %>%
  group_by(year)

  
  






#### TESTING AREA ####


STI2 <- STI

STI3 <- STI2 %>%
  filter(measure == "Deaths" & sex == "Both" & metric == "Number") %>%
  group_by(location, year) %>%
  summarise(totloc = sum(val))
head(STI3)

ggplot(STI3)+
  aes(x= year , y= totloc, colour= location)+
  geom_line(size = 1)+
  labs(x= "Year", y= "No. of Deaths", title= "Total of Deaths caused by STIs, by year, by country")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_dl(aes(label= location), method = list("first.points"))


ggplot(STI %>%
         filter(measure == "DALYs (Disability-Adjusted Life Years)" & sex == "Both" & metric == "Rate") %>%
         group_by(location, year) %>%
         summarise(totloc = sum(val)))+
  aes(x= year , y= totloc, colour= location)+
  geom_line(size = 1)+
  labs(x= "Year", y= "Deaths/100k pop", title= "Rate of Deaths by STIs, by year, by country")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_dl(aes(label= location), method = list("first.points"))



ST4 <- STI %>%
  filter(measure == "Prevalence" & sex == "Both" & metric == "Rate") %>%
  group_by(location, year) %>%
  summarise(totloc = sum(val))
head(ST4)


ggplot(STI %>% 
         filter(measure == "Deaths" & cause == "HIV/AIDS" & metric == "Rate" & sex == "Both" & age!= "All Ages"),
       aes(x=year, y=val, fill= age)) +
  geom_area() +
  # scale_color_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Set1") +
  labs(x= "Year", y= "No. of Deaths/100000 pop", title= "Death Rate, caused by HIV/AIDS, in both sexes (by age, by country, by year)") +
  facet_wrap(vars(location), nrow = 4, scales = "free")+
  theme_minimal() +
  theme(legend.position = "bottom")










##

head(STI)

table(STI$measure,STI$cause)

Q1 <- quantile(STI$val, .25)
Q3 <- quantile(STI$val, .75)
IQR <- IQR(STI$val)
outliers <- subset(STI, STI$val<(Q1 - 1.5*IQR) | STI$val>(Q3 + 1.5*IQR))


outliers_data <- STI %>%
  filter(measure %in% c("Deaths", "DALYs", "Prevalence", "Incidence")) %>%
  group_by(measure) %>%
  mutate(
    Q1 = quantile(val, 0.25, na.rm = TRUE),
    Q3 = quantile(val, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR
  ) %>%
  filter(val < lower_bound | val > upper_bound) %>%
  select(location, sex, cause, measure, val, upper, lower)
View(outliers_data)


eda_paged_report(STI, target = "val", output_format = "pdf", output_file = "STI.pdf")

eda_report(STI, target = val, output_format = "pdf", output_file = "STI.pdf")

diagnose_web_report(STI)



STI %>% 
  # group_by(measure) %>%
  summarise(
    n = n(),
    na = sum(is.na(val)), # Calcula o número de valores omissos
    min = min(val),
    q1 = quantile(val, 0.25, na.rm = TRUE),
    median = median(val, na.rm = TRUE),
    q3 = quantile(val, 0.75, na.rm = TRUE),
    max = max(val, na.rm = TRUE),
    mean = mean(val, na.rm = TRUE),
    sd = sd(val, na.rm = TRUE),
  )

STI_0 <- STI %>%
  filter(val=="0" & upper =="0" & lower =="0")

STI_0_M <- STI_0 %>%
  filter(sex=="Male")

hist(STI$val)

## ## ## ## ## ## ## ## ##

X_DEA_POR_BSX_AAG_RTE <- filter(STI, measure == "Deaths" & location =="Portugal" & sex == "Both" & age == "All Ages" & metric == "Rate")
X_DEA_BSX_AAG_RTE     <- filter(STI, measure == "Deaths" & sex == "Both" & age == "All Ages" & metric == "Rate")

ggplot(X_DEA_POR_BSX_AAG_RTE %>%
       group_by(location))+
  aes(x= year, y= val, color= cause)+
  geom_line()

ggplot(X_DEA_BSX_AAG_RTE)+
  aes(x= year, y= val, color= cause)+
  geom_line()

library(data.table)
setkey(setDT(aiddata), country,year)[, list(aid_amount=sum(aid_amount)), by=list(country, year)]





#### DRAFTS // OTHER OR OLDER CODE ####

# Export STI into CSV
write.csv(STI3, "STI.csv", row.names = FALSE)

# Load data
GBD_original <- read.csv("./STI2.csv")
STI <- GBD_original

# Chris Code, bruteforce ID change to name (works)
STI <- STI %>%
  mutate(location = recode(location, '75' = 'Austria', '76' = 'Belgium', '45' = 'Bulgaria', '46' =
                             'Croatia', '77' = 'Cyprus', '47' = 'Czechia', '78' = 'Denmark',
                           '58' = 'Estonia', '79' = 'Finland', '80' = 'France', '81' = 'Germany',
                           '82' = 'Greece', '48' = 'Hungary', '84' = 'Ireland', '86' = 'Italy',
                           '59' = 'Latvia', '60' = 'Lithuania', '87' = 'Luxembourg', '88' = 'Malta',
                           '89' = 'Netherlands', '51' = 'Poland', '91' = 'Portugal', '52' = 'Romania',
                           '54' = 'Slovakia', '55' = 'Slovenia', '92' = 'Spain', '93' = 'Sweden',
                           '95' = 'United Kingdom'),
         measure = recode(measure, '1' = 'Deaths', '2' = 'DALYs (Disability-Adjusted Life Years)', '5' = 'Prevalence', '6' = 'Incidence'),
         sex = recode(sex, '1' = 'Males', '2' = 'Females', '3' = 'Both'),
         cause = recode(cause, '298' = 'HIV/AIDS', '394' = 'Syphilis', '395' = 'Chlamydial infection', '396' = 'Gonococcal infection',
                               '397' = 'Trichomoniasis', '398' = 'Genital Herpes', '399' = 'Other sexually transmitted infections'),
         age = recode(age, '22' = 'All Ages', '39' = '0-14 years', '24' = '15-49 years', '41' = '50-74 years','234' = '75+ years'),
         metric = recode(metric, '1' = 'Number', '2' = 'Percent', '3' = 'Rate'))

# ID Swap alternative (works)
STI$measure  <- GBD_KEY$measure_name  [match(unlist(STI$measure),  GBD_KEY$measure_id)]
STI$location <- GBD_KEY$location_name [match(unlist(STI$location), GBD_KEY$location_id)]
STI$sex      <- GBD_KEY$sex_label     [match(unlist(STI$sex),      GBD_KEY$sex_id)]
STI$age      <- GBD_KEY$age_group_name[match(unlist(STI$age),      GBD_KEY$age_group_id)]
STI$cause    <- GBD_KEY$cause_name    [match(unlist(STI$cause),    GBD_KEY$cause_id)]
STI$metric   <- GBD_KEY$metric_name   [match(unlist(STI$metric),   GBD_KEY$metric_id)]


#
STI3$cause <- as.factor(STI3$cause)
STI3$measure <- as.factor(STI3$measure)
STI3$location <- as.factor(STI3$location)
STI3$sex <- as.factor(STI3$sex)
STI3$age <- as.factor(STI3$age)
STI3$metric <- as.factor(STI3$metric)


# possible options for summarizing the ZEROs
base::summary(STI%>% filter(val=="0"))
summarytools::descr(STI %>% filter(val=="0"))
dfSummary(STI %>% filter(val=="0")) 


# multiple filters for Portugal (testing purposes)
X_POR_DEA_HIV         <- filter(STI, measure == "Deaths" & location =="Portugal" & cause =="HIV/AIDS")
X_POR_DEA_HIV_NUM     <- filter(STI, measure == "Deaths" & location =="Portugal" & cause =="HIV/AIDS" & metric =="Number")
X_POR_DEA_HIV_NUM_BTH <- filter(STI, measure == "Deaths" & location =="Portugal" & cause =="HIV/AIDS" & metric =="Number" & sex =="Both")
X_POR_DEA_HIV_NUM_ALA <- filter(STI, measure == "Deaths" & location =="Portugal" & cause =="HIV/AIDS" & metric =="Number" & age =="All Ages")


# building graphs 101

ggplot(STI %>%
         filter(measure == "Incidence" & metric == "Percent" & sex == "Both" & age == "All Ages"),
       aes(x=year, y=val, color = location)) + 
  geom_line()

# Line chart, number of deaths in portugal, caused by HIV (both sexes), by age (works)
ggplot(X_POR_DEA_HIV_NUM_BTH,
       aes(x=year, y=val, color = age)) + 
  geom_line()

# area chart, number of deaths in portugal, caused by HIV (both sexes), by age (works)
ggplot(X_POR_DEA_HIV_NUM_BTH %>%
         filter(age!="All Ages"),
       aes(x=year, y=val, fill = age)) + 
  geom_area()

# Line chart, number of deaths in portugal, caused by HIV (both sexes), by age (works)
ggplot(STI %>% 
         filter(measure == "Deaths" & location =="Portugal" & cause =="HIV/AIDS" & metric =="Number" & sex =="Both"),
       aes(x=year, y=val, color = age)) + 
  geom_line()

#
ggplot(STI %>% 
         filter(measure == "Deaths" & cause =="HIV/AIDS" & metric =="Number" & sex =="Both"),
       aes(x=year, y=val, color = age)) + 
  geom_line()+
  facet_wrap(vars(location), scales = "free")

#
ggplot(STI %>% 
         filter(measure == "Deaths" & cause =="HIV/AIDS" & metric =="Number" & sex =="Both" & age!="All Ages"),
       aes(x=year, y=val, fill = age)) + 
  geom_area()+
  #scale_color_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Set1")+
  labs(x="Year", y="No. of Deaths/year", title="Deaths caused by HIV/AIDS, in both sexes, (by age, by country, by year)")+
  facet_wrap(vars(location), nrow = 4, scales = "free")

#
ggplot(STI %>% 
         filter(measure == "Deaths" & cause =="Syphilis" & metric =="Number" & sex !="Both" & age =="All Ages"),
       aes(x=year, y=val, fill = sex)) + 
  geom_area()+
  #scale_color_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Set1")+
  labs(x="Year", y="No. of Deaths/year", title="Deaths caused by HIV/AIDS, in both sexes, (by age, by country, by year)")+
  facet_wrap(vars(location), nrow = 4, scales = "free")

#
ggplot(STI %>% 
         filter(measure == "Deaths" & cause =="Chlamydial infection" & metric =="Number" & sex !="Both" & age =="All Ages"),
       aes(x=year, y=val, fill = sex)) + 
  geom_area()+
  #scale_color_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Set1")+
  labs(x="Year", y="No. of Deaths/year", title="Deaths caused by HIV/AIDS, in both sexes, (by age, by country, by year)")+
  facet_wrap(vars(location), nrow = 4, scales = "free")






### ISSUES
# 393 STI w/o HIV     <- delete
# 397 Trichomoniasis  <- 
# 398 Genital Herpes  <-
# 399 Other STIs      <-

