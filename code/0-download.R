## school-level database
# contains data from the CCD, CRDC, US Dept of Edu, NHGIS

# load libraries
library(educationdata)
library(tidyverse)

# directory data from CCD
directory_data <- get_education_data(level = "schools",
                                     source = "ccd",
                                     topic = "directory",
                                     filters = list(year = 2015)) %>% 
  as_tibble()

write_tsv(directory_data, file = "data/raw/directory_data.tsv")

# teacher data from CRDC
teacher_data <- get_education_data(level = "schools",
                                   source = "crdc",
                                   topic = "teachers-staff",
                                   filters = list(year = 2015)) %>%
  as_tibble()
write_tsv(teacher_data, file = "data/raw/teacher_data.tsv")

# finance data from CRDC
finance_data <- get_education_data(level = "schools",
                                   source = "crdc",
                                   topic = "school-finance",
                                   filters = list(year = 2015)) %>%
  as_tibble()
write_tsv(finance_data, file = "data/raw/finance_data.tsv")

# graduation data from EDFACTs
grad_data <- get_education_data(level = "schools",
                                source = "edfacts",
                                topic = "grad-rates",
                                filters = list(year = 2015)) %>%
  as_tibble()
write_tsv(grad_data, file = "data/raw/grad_data.tsv")

offenses_data <- get_education_data(level = "schools",
                                    source = "crdc",
                                    topic = "offenses",
                                    filters = list(year = 2015))
write_tsv(offenses_data, file = "data/raw/offenses_data.tsv")

discipline_data <- get_education_data(level = "schools",
                                      source = "crdc",
                                      topic = "discipline-instances",
                                      filters = list(year = 2015))

write_tsv(discipline_data, file = "data/raw/discipline_data.tsv")

