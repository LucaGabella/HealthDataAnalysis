library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
#reading the csv data file
data <- read.csv(file = "IntOrg_NCD_variables_2024_02_02.csv")
#filtering out the data
data <- subset(data, Year !=2016)
---------------------------------------

#BMI trends in Adults and Children

---------------------------------------
#Line graph for BMI in children via sexes (all data not using the mean for each year)
ggplot(data = data, mapping = aes(x = Mean_BMI_children, color = Sex)) +
  geom_freqpoly(binwidth = 0.1)
#This is the boxplot graph for children BMI comparing boys and girls
ggplot(data=data, mapping = aes(x=Mean_BMI_children, color=Sex))+ geom_boxplot()
#Line graph for BMI in Adults via sexes
ggplot(data = data, mapping = aes(x = Mean_BMI_adults, color = Sex)) +
  geom_freqpoly(binwidth = 0.1)
ggplot(data = data, mapping = aes(x=Mean_BMI_adults))+geom_boxplot(binwidth = 0.25)
---------------------------------------

#Comparing the Sexes

---------------------------------------
#separating male and female
BMIm <- subset(data, Sex != "Female")
BMIf <- subset(data, Sex != "Male")
#creating a variable of the combined sets
Mean_BMI <- rbind(BMIm,BMIf)
#averaging the data for every year and attributing them to a variable
BMIafe <- aggregate(Mean_BMI_adults ~ Year, data=BMIf, FUN = mean)
BMIama <- aggregate(Mean_BMI_adults ~ Year, data=BMIm, FUN = mean)
BMIcfe <- aggregate(Mean_BMI_children ~ Year, data=BMIf, FUN = mean)
BMIcma <- aggregate(Mean_BMI_children ~ Year, data=BMIm, FUN = mean)
#creating classifiers for men, women, boys and girls so that I can clearly separate them in the graph
BMIafe$Group <- "Men"
BMIama$Group <- "Women"
BMIcfe$Group <- "Boys"
BMIcma$Group <- "Girls"
#binding the variables together to present the mean BMI for adults and children
Mean_BMIa <- rbind(BMIama,BMIafe)
Mean_BMIc <- rbind(BMIcma,BMIcfe)
#individual scatter plots
ggplot(Mean_BMIa, mapping= aes(x=Year, y = Mean_BMI_adults)) + geom_point() + labs(title = "BMI trend")
ggplot(Mean_BMIc, mapping= aes(x=Year, y = Mean_BMI_children, color=Group)) + geom_point() + labs(title = "BMI trend")
# For the comparison between the ages
# Reshape data to have a common BMI column with an indicator for Age Group
data.com <- data %>%
  select(Year, Sex, Mean_BMI_children, Mean_BMI_adults,Country,GDP_USD) %>%
  pivot_longer(cols = c(Mean_BMI_children, Mean_BMI_adults),
               names_to = "Age_Group",
               values_to = "Mean_BMI") %>%
  mutate(Age_Group = ifelse(Age_Group == "Mean_BMI_children", "Children", "Adults"))

# Aggregate mean BMI by Year, Sex, and Age Group
data2 <- data.com %>%
  group_by(Year, Sex, Age_Group,) %>%
  summarise(Mean_BMI = mean(Mean_BMI, na.rm=TRUE), .groups = "drop")

# Plot the BMI trends for both Adults and Children, separated by Sex
ggplot(data2, aes(x=Year, y=Mean_BMI, color=Sex, linetype=Age_Group, group=interaction(Sex, Age_Group))) +
  geom_line(size=1.2) +
  geom_point(size=2) +
  labs(title="BMI Trends by Sex and Age Group",
       x="Year", y="Mean BMI") +
  theme_minimal() +
  scale_color_manual(values=c("blue", "red")) +  # Male = Blue, Female = Red
  scale_linetype_manual(values=c("solid", "dashed"))  # Children = Solid, Adults = Dashed
---------------------------------------

#Sampling the data

---------------------------------------

data <- read.csv(file = "IntOrg_NCD_variables_2024_02_02.csv")
#filter the GDP data and isolate it
data_gdp<-data %>% filter(!is.na(GDP_USD))
#turn the data into quantiles
quantiles <- quantile(data$GDP_USD, probs = c(0.33,0.66),na.rm = TRUE)
#group each country into one of the 3 quantiles
data_gdp <- data.com %>%
  group_by(Country) %>%
  summarise(Avg_GDP=mean(GDP_USD, na.rm = TRUE)) %>%
  mutate(GDP_Level = case_when(
    Avg_GDP <= quantiles[1] ~ "Low",
    Avg_GDP > quantiles[1] & Avg_GDP <= quantiles[2] ~ "Average",
    Avg_GDP > quantiles[2] ~ "High"
  ))
# Randomly select 3 countries from each GDP level
set.seed(42)
selected_countries <- data_gdp %>%
  group_by(GDP_Level) %>%
  sample_n(3) %>%
  pull(Country)

# Filter the dataset for selected countries and 5-year intervals
data3 <- data %>%
  filter(Country %in% selected_countries, Year %% 5 == 0) %>%
  group_by(Year, Country, GDP_USD) %>%
  summarise(Mean_BMI = mean(Mean_BMI_children, na.rm=TRUE), .groups = "drop")

# Filter the dataset for selected countries and 5-year intervals
data4 <- data %>%
  filter(Country %in% selected_countries, Year %% 5 == 0) %>%
  group_by(Year, Country, GDP_USD) %>%
  summarise(Mean_BMI = mean(Mean_BMI_adults, na.rm=TRUE), .groups = "drop")

# Filter the dataset for selected countries and 5-year intervals
data5 <- data.com %>%
  filter(Country %in% selected_countries, Year %% 5 == 0) %>%
  group_by(Year, Country, GDP_USD) %>%
  summarise(Mean_BMI = mean(Mean_BMI, na.rm=TRUE), .groups = "drop")

# Plot the data for GDP per year
ggplot(data5, aes(x=Year, y=GDP_USD, color=Country, group=Country)) +
  geom_line() +
  geom_point() +
  labs(title="How did GDP change over time (5-Year Intervals)",
       x="Year", y="Mean BMI (Children)") +
  scale_color_manual(values=c("blue", "red","black","orange","green","turquoise","purple","pink","yellow")) +
  theme_minimal()

# Plot the data to see the mean BMI against the GDP (for Children)
ggplot(data3, aes(y=GDP_USD, x=Mean_BMI, color=Country, group=Country)) +
  geom_line() +
  geom_point() +
  labs(title="Mean BMI Trends by GDP Group (5-Year Intervals)",
       x="BMI", y="GDP-USD") +
  scale_color_manual(values=c("blue", "red","black","orange","green","turquoise","purple","pink","yellow")) +
  theme_minimal()
# Plot the data to see the mean BMI against the GDP (for Adults)
ggplot(data4, aes(y=GDP_USD, x=Mean_BMI, color=Country, group=Country)) +
  geom_line() +
  geom_point() +
  labs(title="Mean BMI Trends by GDP Group (5-Year Intervals)",
       x="BMI", y="GDP-USD") +
  scale_color_manual(values=c("blue", "red","black","orange","green","turquoise","purple","pink","yellow")) +
  theme_minimal()
# Plot the data to see the mean BMI against the GDP (combined)
ggplot(data5, aes(y=GDP_USD, x=Mean_BMI, color=Country, group=Country)) +
  geom_line() +
  geom_point() +
  labs(title="Mean BMI Trends by GDP Group (5-Year Intervals)",
       x="BMI", y="GDP-USD") +
  scale_color_manual(values=c("blue", "red","black","orange","green","turquoise","purple","pink","yellow")) +
  theme_minimal()

-----------------------

#Hyopothesis Testing

----------------------
#hypothesis testing for child BMI
cor.test(data$Mean_BMI_children, data$GDP_USD, method = "pearson", conf.level = 0.95)
t.test(data$Mean_BMI_children, data$GDP_USD, var.equal = TRUE)
#hypothesis testing for adult BMI
cor.test(data$Mean_BMI_adults, data$GDP_USD, method = "pearson", conf.level = 0.95)
t.test(data$Mean_BMI_adults, data$GDP_USD, var.equal = TRUE )
#hypothesis testing for combined BMI
cor.test(data.com$Mean_BMI,data.com$GDP_USD, method = "pearson", conf.level = 0.95)
t.test(data.com$Mean_BMI,data.com$GDP_USD, var.equal = TRUE)









