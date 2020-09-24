# NOTE: I cannot provide the source data for this table myself.
# You must download it from IPUMS-USA https://usa.ipums.org/usa/index.shtml

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
library(tidyverse)
library(pollster)
library(labelled)
library(gt)

# This data was downloaded from IPUMS USA. It is the 2014-18 ACS microdata.
# The dataset is national, but it is filtered for people who are employed
ddi <- read_ipums_ddi("data/usa_00050.xml")
data <- read_ipums_micro(ddi)

# filter for full-time workers
fulltime <- data %>% 
  filter(UHRSWORK >= 37) %>%
  # create age category variable
  mutate(
    age_category = case_when(
      AGE < 35 ~ "Under 35",
      AGE %in% 35:49 ~ "35 to 50",
      AGE >= 50 ~ "50 plus"),
    age_category = factor(age_category,
                          levels = c("Under 35", "35 to 50", "50 plus")))

# calculate total workers in each occupation
total.jobs <- topline(fulltime, OCC2010, PERWT)

# a vector of the 20 most common jobs
top.jobs <- total.jobs %>%
  slice_max(Frequency, n = 20) %>% 
  mutate(Response = as.character(Response)) %>%
  pull(Response)

# fulltime workers in the top 20 occupations
fulltime.top.jobs <- fulltime %>%
  mutate(OCC2010 = to_character(OCC2010)) %>%
  filter(OCC2010 %in% top.jobs)

# calculate the median weighted wage for each job-gender-age combination
income.by.sex.age <- fulltime.top.jobs %>%
  mutate(SEX = to_factor(SEX)) %>%
  group_by(OCC2010, SEX, age_category) %>%
  summarise(median_wage = Hmisc::wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5)) %>%
  ungroup()
income.by.sex.age

# calculate the share of workers made up by each gender, along with...
# each profession's weighted and unweighted counts
jobs.by.sex <- crosstab(fulltime.top.jobs, OCC2010, SEX, PERWT,
                        unwt_n = T, format = "long") %>%
  mutate(age_category = "total")

write_csv(income.by.sex.age, "data/income_by_sex_and_age.csv")
write_csv(jobs.by.sex, "data/total_jobs_by_sex.csv")