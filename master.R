library(sqldf)
library(plyr)
library(dplyr)
library(data.table)
library(lazyeval)

source("/Users/davidgribbin/Desktop/RAND Prep/code_sample/utility_functions.R")

# Read and check data
df = read.csv("/Users/davidgribbin/Desktop/RAND Prep/code_sample/data/minority-report-data.txt", sep="\t")
lapply(df[c("FraudFound", "MonthClaimed", "WeekOfMonthClaimed", "DayOfWeekClaimed", "Year")], table)

# Create time column
df = df %>% rowwise() %>% mutate(DateTime = dcIMMDate(Year, MonthClaimed, WeekOfMonthClaimed, DayOfWeekClaimed))

# Convert FraudFound column to T/F
df = df %>% rowwise() %>% mutate(FraudFound = convertFraudColumn(FraudFound))

# Creating a lookup_table of nested data.tables
# The structure is [Feature][Group][logs] -> e.g. ["Sex"]["Male"][all logs by males indexed by time]
lookup_table = list()
for(name in names(df)){
  listdf = split(df, df[,name])
  for(group in names(listdf)){
    logs = listdf[[group]][,c("DateTime", "FraudFound")]
    hash_table = as.data.table(logs)
    setkey(hash_table, DateTime)
    lookup_table[[name]][[group]] = hash_table
  }
}

# Calculate features for each column
# Features are:
#   1) Number of fraud cases in the past year
#   2) Number of months that have a fraud case in the past year
#   3) Number of legitimate cases in the past year
#   4) Number of legitimate months in the past year
#   5) Ratio of fraud to legitimate cases over the past year
# An example calculated feature would be, "Males have had 344 cases of fraud over the 12 months prior to this claim."
# Estimated run time ~16 hours for 34 columns
for(name in names(df)){
  # Create empty data frame
  feature_df = data.frame(1:15419)
  names(feature_df) = c("an_index")
  
  # Select source data column a features
  subset_df = select(df, c(DateTime, get(name)))
  features = c("FraudCount", "FraudMonths", "LegitimateCount", "LegitimateMonths", "FraudRate")
  feature_names = c(feature_names, paste(name, features, sep=""))
  
  # Calculate features
  feature_df = calculateFeature(feature_df, subset_df, name, lookup_table, features)
  
  # Save results
  filename = sprintf("/Users/davidgribbin/Desktop/RAND Prep/code_sample/data/features_for_%s.Rda", name)
  save(feature_df, file = filename)
}