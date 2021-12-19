## load libraries
library(lubridate)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringi)
library(tidyselect)

# read in cleaned data
school_data = read_tsv("data/clean/school_data.tsv") %>%
  mutate(across(c("urban_centric_locale", "school_type", "charter"), 
                factor))

# split into train and test (set seed here if applicable)
set.seed(1)
n = nrow(school_data)
train_samples = sample(1:n, round(0.8*n))

school_train = school_data[train_samples,]
school_test = school_data[-train_samples,]

# save the train and test data
write_tsv(x = school_train, path = "data/clean/school_train.tsv")
write_tsv(x = school_test, path = "data/clean/school_test.tsv")
