library(tidyverse)
#Read file
raw_data <- read_csv(file="lab_quiz_week2_data.csv")
#Change all 999s or missing values to NA 
raw_data <- read_csv(file = "lab_quiz_week2_data.csv", na=c("","NA","-999"))
#Tell R that univ, prog, and age are FACTORS (/categorical variables)
categorical_variables <- select(raw_data, univ, prog_year, age)
categorical_variables$age <- as.factor(categorical_variables$age)
#And when it applies, change numerical data to nominal 
categorical_variables$univ <- as.factor(categorical_variables$univ)
levels(categorical_variables$univ) <- list("Waterloo"=1,"Guelph"=2)
categorical_variables$prog_year <- as.factor(categorical_variables$prog_year)
levels(categorical_variables$prog_year) <- list("First Year"=1,"Second Year"=2, "Third Year"=3, "Fourth Year"=4, "Grad School"=5)
#Now, I'm going to create subsets of data for each scale 
positive_affect_items <- select (raw_data, PA1, PA2, PA3, PA4, PA5)
depression_items <- select (raw_data, D1, D2, D3, D4, D5)
prog_sat_items <- select (raw_data, PS1, PS2, PS3, PS4, PS5)
#Use descriptive data table to e.g., determine range 
psych::describe(positive_affect_items)
psych::describe(depression_items)
psych::describe(prog_sat_items)
#time to take out data that has been mis-entered 
is_bad_value <- positive_affect_items<1 | positive_affect_items>7
positive_affect_items[is_bad_value] <- NA 
is_bad_value <- depression_items<1 | depression_items>4
depression_items[is_bad_value] <- NA
is_bad_value <- prog_sat_items<1 | prog_sat_items>6
prog_sat_items[is_bad_value] <- NA
#Next, we fix reverse-scored items 
positive_affect_items <- mutate(positive_affect_items,PA1=8-PA1)
depression_items <- mutate(depression_items,D4=5-D4)
depression_items <- mutate(depression_items,D5=5-D5)
prog_sat_items <- mutate(prog_sat_items,PS1=7-PS1)
prog_sat_items <- mutate(prog_sat_items,PS2=7-PS2)
#Let's calculate the scale scores
pos_affect <- psych::alpha(as.data.frame(positive_affect_items),check.keys = FALSE)$scores
dep <- psych::alpha(as.data.frame(depression_items),check.keys = FALSE)$scores
prog_sat <- psych::alpha(as.data.frame(prog_sat_items),check.keys = FALSE)$scores
#Analytic data time 
analytic_data <- cbind(categorical_variables,pos_affect,dep,prog_sat)
analytic_data
#Let's export this data as a CSV file 
write_csv(analytic_data,path = "quiz1_analytic_data_PITEL.csv")
