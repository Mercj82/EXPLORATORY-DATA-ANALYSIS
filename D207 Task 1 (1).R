install.packages("tidyverse")
library(tidyverse)
install.packages("stats")
library(stats)
install.packages("vcd")
library(vcd)
install.packages("gmodels")
library(gmodels)
library(ggplot2)
MD <- read_csv("C:/Users/merce/Downloads/MD (1).csv")
View(MD)
#uploaded necessary packages and files
variables <- c("ReAdmis", "HighBlood", "Stroke", "Overweight", 
               "Arthritis", "Diabetes", "Hyperlipidemia", "BackPain", "Anxiety", 
               "Allergic_rhinitis", "Reflux_esophagitis", "Asthma", "Initial_days", "Additional_charges", "TotalCharge")
#select variables for correlation matrix
subset_data <- MD[, variables]
#create subset for variables
cor_matrix <- cor(subset_data, use = "pairwise.complete.obs")
#Calculate the correlation matrix
print(cor_matrix)
#print correlation matrix
str(MD[c("ReAdmis", "HighBlood", "Stroke", "Overweight", 
         "Arthritis", "Diabetes", "Hyperlipidemia", "BackPain", "Anxiety", 
         "Allergic_rhinitis", "Reflux_esophagitis", "Asthma", "Initial_days", "Additional_charges", "TotalCharge")])
#see variable types an examples
readmis_0 <- MD$Initial_days[MD$ReAdmis == 0]
readmis_1 <- MD$Initial_days[MD$ReAdmis == 1]
#subset the data
t_test <- t.test(readmis_0, readmis_1)
#perform the t-test
print(t_test)
#change ReAdmis to a factor
MD$ReAdmis <- factor(MD$ReAdmis)
#print test results
ggplot(MD, aes(x = ReAdmis, y = Initial_days)) +
  geom_boxplot() +
  xlab("ReAdmis") +
  ylab("Initial_days") +
  ggtitle("Box Plot of Initial Days by ReAdmis")
#create boxplot for Initial days and Readmis
MD <- read_csv("C:/Users/merce/OneDrive/medical_clean1.csv")
#File Import and Start of Univariate variables
summary(MD$Additional_charges)
#summarize stats of additional charges
hist(MD$Additional_charges)
#create histogram Additional Charges
summary(MD$Initial_days)
#summarize stats of Initial days
hist(MD$Initial_days)
#Histogram Initial days 
barplot(table(MD$Marital), main = "Distribution of Marital")
#Barplot for services
barplot(table(MD$Area), main = "Distribution of Area")
#barplot for complication risk
#Start Of bivariates
ggplot(MD, aes(x = Initial_days, y = Income)) +
  geom_point() +
  xlab("Initial Days") +
  ylab("Income") +
  ggtitle("Scatter Plot of Initial Days vs. Income")
#create a scatter plot Initial days and income
ggplot(MD, aes(x = HighBlood, fill = Stroke)) +
  geom_bar() +
  xlab("High Blood") +
  ylab("Count") +
  ggtitle("Stacked Bar Chart of High Blood and Stroke")
#create a stack bar chart for HB and stroke
