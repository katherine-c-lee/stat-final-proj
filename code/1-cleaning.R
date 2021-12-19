## load libraries
library(lubridate)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringi)
library(tidyselect)

############################ School (Specific) Data ############################ 

directory_data <- read_tsv("data/raw/directory_data.tsv")
finance_data <- read_tsv("data/raw/finance_data.tsv")
teacher_data <- read_tsv("data/raw/teacher_data.tsv")
offenses_data <- read_tsv("data/raw/offenses_data.tsv")
discipline_data <- read_tsv("data/raw/discipline_data.tsv")

#names(directory_data)
#names(finance_data)
#names(teacher_data)
#names(offenses_data)
#names(discipline_data)

## clean individual data sets by dropping columns / rows with missing values
thresh = 0.15
directory_data[directory_data < 0] <- NA
directory_data_edit <- directory_data %>%
  select_if(~mean(is.na(.)) < thresh) %>%
  select(c("county_code", "year", "ncessch", "urban_centric_locale", 
           "school_type", "charter", "virtual", "free_or_reduced_price_lunch",
           "enrollment")) %>%
  # keep regular, special-ed, and vocational schools for interpretability
  filter(school_type %in% c(1,2,3)) %>%
  # filter non-virtual schools only
  filter(virtual == 0) %>%
  select(-c("virtual")) %>%
  na.omit() %>%
  filter(enrollment != 0) %>%
  mutate(across(c("urban_centric_locale", "school_type", "charter"), 
                           factor))

# collapse 'urban_centric_locale'
directory_data_edit$urban_centric_locale <- 
  recode(directory_data_edit$urban_centric_locale, "11" = "city", "12" = "city",
       "13" = "city", "21" = "suburb", "22" = "suburb", "23" = "suburb", 
       "31" = "town", "32" = "town", "33" = "town", "41" = "rural",
       "42" = "rural", "43" = "rural")

finance_data[finance_data < 0] <- NA
finance_data_edit <- finance_data %>%
  select_if(~mean(is.na(.)) < thresh) %>%
  select(-c("crdc_id", "leaid", "fips")) %>%
  select(c("year", "ncessch", "salaries_teachers", 
           "expenditures_nonpersonnel")) %>%
  na.omit() %>%
  distinct(ncessch, .keep_all = TRUE)

teacher_data[teacher_data < 0] <- NA
teacher_data_edit <- teacher_data %>%
  select_if(~mean(is.na(.)) < thresh) %>%
  select(-c("crdc_id", "leaid", "fips")) %>%
  na.omit() %>%
  distinct(ncessch, .keep_all = TRUE)

offenses_data[offenses_data < 0] <- NA
offenses_data_edit <- offenses_data %>%
  na.omit() %>%
  distinct(ncessch, .keep_all = TRUE) %>%
  group_by(ncessch) %>%
  # create aggregate crime feature
  mutate(total_crime = sum(rape_incidents, sexual_battery_incidents, 
                           robbery_w_weapon_incidents, 
                           robbery_w_firearm_incidents, 
                           robbery_no_weapon_incidents, 
                           attack_w_weapon_incidents,
                           attack_w_firearm_incidents, 
                           attack_no_weapon_incidents)) %>%
  select(-c("firearm_incident_ind", "homicide_ind", "rape_incidents",
            "sexual_battery_incidents", "robbery_w_weapon_incidents", 
            "robbery_w_firearm_incidents", "robbery_no_weapon_incidents",
            "attack_w_weapon_incidents", "attack_w_firearm_incidents",
            "attack_no_weapon_incidents", "leaid", "fips", "crdc_id"))

discipline_data[discipline_data < 0] <- NA
discipline_data_edit <- discipline_data %>%
  filter(suspensions_instances > -1) %>%
  select_if(~mean(is.na(.)) < thresh) %>%
  select(c("year", "ncessch", "suspensions_instances")) %>%
  na.omit() %>%
  group_by(ncessch, year) %>%
  summarise(avg_suspensions = mean(suspensions_instances))

## merge data sets by unique school identification numbers
mergedCols = c("ncessch", "year")
joined_set <- directory_data_edit %>%
  inner_join(teacher_data_edit, by = mergedCols) %>%
  inner_join(finance_data_edit, by = mergedCols) %>%
  inner_join(offenses_data_edit, by = mergedCols) %>%
  inner_join(discipline_data_edit, by = mergedCols) %>%
  mutate(free_reduced_lunch_per1000 = 
           (free_or_reduced_price_lunch * 1000 / enrollment)) %>%
  mutate(crimes_per1000 = (total_crime * 1000 / enrollment)) %>%
  select(-c("enrollment", "total_crime", "free_or_reduced_price_lunch"))

########################## County Level (General) Data ######################### 

education_data <- read_csv("data/raw/county level data/education.csv")
employment_data <- read_csv("data/raw/county level data/employment.csv")
population_data <- read_csv("data/raw/county level data/population.csv")
poverty_data <- read_csv("data/raw/county level data/poverty.csv")
wages_data <- read_csv("data/raw/county level data/wages.csv")

## clean education data: select 2015 features and rename features
education_data <- select(
  education_data, 
  c("FIPS Code","Percent of adults with less than a high school diploma, 2015-19",
    "Percent of adults with a high school diploma only, 2015-19",
    "Percent of adults completing some college or associate's degree, 2015-19",
    "Percent of adults with a bachelor's degree or higher, 2015-19")) %>% 
  rename("county_code" = "FIPS Code",
         "percent_no_highschool" = 
           "Percent of adults with less than a high school diploma, 2015-19",
         "percent_only_highschool" = 
           "Percent of adults with a high school diploma only, 2015-19", 
         "percent_some_college" = 
           "Percent of adults completing some college or associate's degree, 2015-19",
         "percent_all_college" = 
           "Percent of adults with a bachelor's degree or higher, 2015-19")

## clean employment data: select 2015 features
employment_data <- select(employment_data, 
                          c("FIPS_Code", 
                            "Civilian_labor_force_2015", 
                            "Unemployment_rate_2015")) %>%
  rename("county_code" = "FIPS_Code")
employment_data$county_code <- as.numeric(employment_data$county_code)  

## clean population data: select 2015 features, transform race to % population
population_data <- population_data %>% filter(year == 2015) %>%
  mutate(white_pop = white_pop / pop, 
         black_pop = black_pop / pop,
         asian_pop = asian_pop / pop,
         indian_pop = indian_pop / pop,
         pacific_pop = pacific_pop / pop,
         hisp_pop = hisp_pop / pop) %>%
  select(-c("STFIPS", "COFIPS", "year", "two_pop", "not_hisp_pop", 
            "state_abbrev", "state", "county")) %>%
  rename("county_code" = "FIPS")

## clean poverty data: select 2015 features
poverty_data <- poverty_data %>% select(c("FIPS", "PCTPOVALL_2019", 
                                          "PCTPOV017_2019", "PCTPOV517_2019", 
                                          "MEDHHINC_2019")) %>%
  rename("county_code" = "FIPS", 
         "Poverty_all_population" = "PCTPOVALL_2019", 
         "minor_poverty" = "PCTPOV017_2019", 
         "percent_school_age_children_poverty" = "PCTPOV517_2019", 
         "Household_income" = "MEDHHINC_2019")
poverty_data$county_code <- as.numeric(poverty_data$county_code)  

## clean wage data: average all wage estimates for the same county
wages_data <- wages_data %>%
  select(c("area_fips", "avg_wkly_wage")) %>%
  rename("county_code" = "area_fips") %>%
  group_by(county_code) %>%
  summarise(avg_wkly_wage = mean(avg_wkly_wage))

## join all county-level data
joined <- education_data %>% 
  inner_join(employment_data, by = "county_code") %>%
  inner_join(population_data, by = "county_code") %>%
  inner_join(poverty_data, by = "county_code") %>%
  inner_join(wages_data, by = "county_code") %>%
  mutate(labor_force_participation = Civilian_labor_force_2015 / pop)

################################## FINAL JOIN ################################## 
final_join <- joined_set %>% 
  left_join(joined, by = "county_code") %>% 
  drop_na() # %>%
  #filter(crimes_per1000 < 2000)
#write_tsv(final_join, path = "data/clean/school_data.tsv") # for mark's comp
write_tsv(final_join, "data/clean/school_data.tsv")

