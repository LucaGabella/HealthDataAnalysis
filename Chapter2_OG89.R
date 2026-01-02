
#Packages that help to display and manipulate the data ]

#if you have not used these packages before you will need to run the code commented out below
#install.packages('tidyverse')
#install.packages('ggpubr')
library(tidyverse)
library(ggpubr)

#the code below sets the working directory to where the file is located,
#on your own pc the file path will be different
setwd("~/OneDrive - University of Kent/Big Data group proj")

df <- read.csv("IntOrg_NCD_variables_2024_02_02.csv") #ensure that the data file is in the same folder as this script


#### Exploratory Data Analysis ####
sum(is.na(df)) # there are 6000 instances of missing values in the entire data frame 
sum(is.na(filter(df, Year == 2016))) #3600 instances of missing values for rows from 2016
df <- filter(df, Year != 2016) #One year is not as significant as there are 40 other years
na_counts <- df %>% summarise_all(~ sum(is.na(.))) #Per column which values have NA, all from Diabetes
####

#For my analysis I only focused on gdp and years of education, this code removes all unnecessary columns
#The years of education and gdp value is the same across male and female so one is removed to prevent bias in results
#It also renames the frequently used columns for ease of use
my_info <- df[-c(5:15, 19, 20)] %>% 
  filter(Sex == 'Female') %>% 
  rename(all_of(c(edu='Years_of_education',gdp='GDP_USD')))

#### Below are a list of functions that I created to use with my data analysis ####

#This function assigns a value to a row depending on what time frame the observation is from
# 1975 - 1985 = 1st, 1986-1995 = 2nd, 1996-2005 = 3rd, 2006-2015 = 4th
assign_time_period <- function(column){
  if(column <= 1985){return('1st')}
  if(column <= 1995){return('2nd')}
  if(column <= 2005){return('3rd')}
  else(return('4th'))
}

#this function assigns a value to a row based on where the value from the given column falls across the entire range of that column
#It creates a tag for knowing if that observation is from the 1st, 2nd, 3rd, or 4th percentile
assign_quartile <- function(column, summary){
  if(column <= summary[2]){ return('1st')}
  if(column > summary[2] & column <= summary[3]) { return('2nd')}
  if(column > summary[3] & column <= summary[5]) { return('3rd')}
  else(return('4th'))
}

#calculates the percent change from two observations
percent_change<-function(late, early){
  return(((late-early)/abs(early))*100)
}
#### end of function list ####


# A plot showing the overall correlation between Years of education and GDP
# This analysis takes every country across all years
round_edu <- my_info %>% 
  filter(edu <= 14) %>% #There are 11 instances (two countries) where the years of education > 14, not a significant amount in a sample from 8400
  mutate(edu = round(edu, 0)) %>% #Rounded to the nearest integer for simplification of graphs
  group_by(edu) %>% 
  summarise(across(gdp, mean)) %>% #calculates the average GDP for every grouping of Years of education
  rename_with(.cols = 1, ~"edu")

ggplot(round_edu, aes(edu, gdp)) + 
  geom_col() +
  labs( x = 'Years of Education', 
        y = 'GDP (USD)',
        title = 'Average GDP per rounded Years of Education')


# A plot that shows the spread of GDPS across the rounded Years of education
# A boxplot is generated for each Quartile, which are generated from the overall data (not the rounded data set)
# To find the specific ranges, run the code commented out below
### summary(my_info$edu) ###
round_edu['quartile'] <- sapply(round_edu$edu, 
                                assign_quartile, # uses the assign_quartile function on each row
                                summary = summary(my_info$edu)) #here the quartile is calculated on every instance from the dataset
ggplot(round_edu, aes(quartile, gdp)) + 
  geom_boxplot() + 
  labs( x = 'Quartiles within Years of Education', 
        y = 'GDP (USD)',
        title = 'Boxplot of GDP vs Years of education')

# A scatter plot showing the relationship between years of education and gdp
# For the sake of viewing, each region was averaged together every 10 years to create 84 points
my_info['time_frame'] <- sapply(my_info$Year, 
                                assign_time_period) # Using the assign_time_period function on each row
region_10yr_average <- my_info %>% 
  group_by(Region, time_frame) %>% 
  summarise(across(c(gdp, edu), mean)) #Finds the average of each Region every 10 years

ggplot(region_10yr_average, aes(edu, gdp)) + 
  geom_point() + 
  geom_point(data = filter(region_10yr_average, # Used to identify the bigger outliers
                           gdp > 20000 & edu < 7.5), 
             aes(color = Region)) + 
  labs(x = 'Years of Education', 
       y = 'GDP (USD)',
       title = 'Regional 10 year average of Years of Education vs GDP')



# On a per country basis, showing the percent change in education and percent change in gdp
# This analysis shows that there might not be a direct cause and effect relationship

#These two data frames take every instance from the years 1975 and 2015 respectively
early <- filter(my_info, Year == 1975)
late <- filter(my_info, Year == 2015)

differences <- data.frame(unique(my_info$Country), 
                          early$Superregion, # The first two lines are to access the country and superregion values
                          early$edu, 
                          late$edu, 
                          early$gdp, 
                          late$gdp) %>%
  rename(Superregion = 'early.Superregion')
differences['dif_edu'] <- differences$late.edu - differences$early.edu # computes the difference between 1975 education and 2015
differences['dif_gdp'] <- differences$late.gdp - differences$early.gdp # ditto but for gdp
differences['edu_percent'] <- percent_change(differences$late.edu, differences$early.edu) # computes the percent change in education
differences['gdp_percent']<- percent_change(differences$late.gdp, differences$early.gdp) # ditto for gdp

region_dif <- differences %>% 
  group_by(Superregion) %>%
  summarise(across(c(edu_percent, gdp_percent), mean)) # Calculates the average percent change for both edu and gdp grouped by superregion

#The code below is a way to remove the x axis from plots where it can become cluttered
remove_axis <- theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())

region_edu <- ggplot(region_dif, aes(Superregion, edu_percent)) + # A bar plot of each Superregion's % change in education
  geom_bar(stat = 'identity', 
           aes(fill = Superregion)) + 
  labs(y = 'Change in Years of Education') + 
  remove_axis

region_gdp <- ggplot(region_dif, aes(Superregion, gdp_percent)) + # a bar plot of each Superregions % change in gdp
  geom_bar(stat = 'identity', 
           aes(fill = Superregion)) + 
  labs(y = 'Change in GDP') + 
  remove_axis 

ggarrange(region_edu, region_gdp, 
          common.legend = TRUE, legend = 'right')

# A simple scatter plot showing edu vs gdp percent change
ggplot(differences, aes(edu_percent, gdp_percent)) + geom_point() + 
  geom_point(data = filter(differences, edu_percent > 500 | gdp_percent > 1000),
             aes(color = Superregion)) +  #Identifying some outliers
  labs(x = 'Change in Years of Education (%)',
       y = 'Change in GDP (%)',
       title = 'Percent change in Years of Education vs GDP ')


summary_edu_percent <- summary(differences$edu_percent) #Getting the quartiles of percent change in edu
differences['edu_cent_quat'] <- sapply(differences$edu_percent, 
                                       assign_quartile, 
                                       summary = summary_edu_percent)


#Box plot per quartile of percent change in years of education
ggplot(differences, aes(edu_cent_quat,gdp_percent)) + geom_boxplot() + 
  geom_text(data = filter(differences, gdp_percent > 1000), 
            aes(label = unique.my_info.Country.), nudge_y = 100) + 
  labs(x = 'Quartiles within percent change of Years of Education',
       y = 'Change in GDP (%)')



