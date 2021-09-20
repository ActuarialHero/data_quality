library(readr)
library(dplyr)
library(tidyr)
library(synthpop)
library(summarytools)
library(lubridate)
library(ggplot2)
library(data.table)

set.seed(68913189)

policy_count <- 25000

# Address data
addresses <- read_csv("./data_in/open_addresses/ks.csv") %>%
  select(LON, LAT, NUMBER, STREET, UNIT, CITY, STATE = REGION, POSTCODE)

# Household sizes from 2020 Census:
household_sizes <- c(36198, 44742, 19337, 16262, 7446, 2919, 1546)
household_sizes <- household_sizes / sum(household_sizes)

generate_policy_level_data <- function(year, n, number_start){
  
  print("Addresses")
  policy_addresses <- addresses[sample(c(1:nrow(addresses)), size = n, replace = TRUE),]
  print("Drivers")
  driver_counts <- sample(c(1:7), size = n, replace = TRUE, prob = household_sizes)
  
  print("Policies")
  policies <- cbind(policy_addresses, drivers = driver_counts) %>%
    rowwise() %>%
    mutate(effective_date = sample(seq(as.Date(paste0(year, "/01/01")),
                                       as.Date(paste0(year, "/12/31")),
                                       by = "day"), 1)) %>%
    ungroup() %>%
    mutate(
      pol_state = "KS",
      new_renew = "NEW",
      policy_term = 1,
      expiration_date = effective_date %m+% years(1),
      pol_num = c(number_start:(number_start + n - 1)))
  
  
  return(policies)
}

# Generate policies with cancels and renewals
for(i in 2013:2020) {
  print(i)
  if(i == 2013) {
    policies <- generate_policy_level_data(i, policy_count, 1)
  } else {
    next_pol_num <- max(policies$pol_num) + 1
    print(next_pol_num)
    
    renewals <- policies %>%
      filter(year(effective_date) == i - 1) %>%
      mutate(
        new_renew = "RENEW",
        policy_term = policy_term + 1,
        effective_date = effective_date %m+% years(1),
        expiration_date = expiration_date %m+% years(1)) %>%
      sample_frac(size = 0.82)

    new_business <- generate_policy_level_data(i, policy_count / 5, next_pol_num)
    
    policies <- bind_rows(
      policies,
      renewals,
      new_business)
    
  }
}

help(SD2011)                   # this will give you information about it
codebook.syn(SD2011)$tab       # get summary info about variables

synth_source <- SD2011[, c(1, 2, 6, 8, 10, 11, 21, 23, 25, 33, 34, 35)] 

# Generate drivers

distinct_policies <- policies %>%
  filter(policy_term == 1) %>%
  select(pol_num, drivers)

total_drivers <- sum(distinct_policies$drivers)
drivers <- syn(synth_source, cont.na = list(income = -8), k = total_drivers)$syn

drivers <- drivers %>%
  rename(
    education = edu,
    profession = socprof,
    homeowner = sport) %>%
  mutate(
    income = 20 * income,
    birth_year = 2021 - age,
    driver_id = row_number()) %>%
  rowwise() %>%
  mutate(
    license_state = sample(c("KS", "MO", "CO", "OK", "NE"), size = 1, prob = c(0.90, 0.01, 0.03, 0.03, 0.03)),
    birth_date = sample(c(
      rep(as.Date(paste0(birth_year, "/01/01")), 2),
      seq(as.Date(paste0(birth_year , "/01/01")),
          as.Date(paste0(birth_year, "/12/31")),
          by = "day")), 1)) %>%
  mutate(
    birth_month = month(birth_date),
    birth_day = day(birth_date))

names <- read_csv("./data_in/538/adjusted-name-combinations-list.csv") %>%
  select(first_name = FirstName, last_name = Surname, finalEstimate) %>%
  mutate(p = finalEstimate / sum(finalEstimate))

driver_names <- names[sample(c(1:400), size = total_drivers, replace = TRUE, prob = names$p),][, 1:2]

drivers <- cbind(drivers, driver_names)

distinct_policies[rep(seq_len(nrow(distinct_policies)), distinct_policies$drivers),]

assigned_drivers <- 
  as.data.frame(lapply(distinct_policies, rep, distinct_policies$drivers)) %>%
  bind_cols(drivers)

policy_driver <- policies %>%
  left_join(assigned_drivers)

violations <- policy_driver %>%
  group_by(driver_id) %>%
  summarize(
    years = max(policy_term) + 5,
    lookback_date = min(effective_date) %m-% years(5),
    max_eff_date = max(effective_date)) %>%
  rowwise() %>%
  mutate(violations = rpois(1, years* 0.05)) %>%
  filter(violations > 0)

# This can probably be re-written with purrr
for(i in 1:nrow(violations)) {
  if(i == 1) {
    violation_detail <- NULL
  }
  
  start_date <- violations$lookback_date[i]
  end_date <- violations$max_eff_date[i]
  violation_count <- violations$violations[i]
  
  violation_detail[[i]] <- data.frame(
    driver_id = violations$driver_id[i],
    violation_date = sample(seq(start_date, end_date, by = "day"), violation_count))
}

violation_detail <- rbindlist(violation_detail)

policy_driver_violation <- policy_driver %>%
  left_join(violation_detail) %>%
  filter(violation_date < effective_date) %>%
  group_by(driver_id, effective_date) %>%
  count(name = "violations")

policy_driver <- policy_driver %>%
  left_join(policy_driver_violation) %>%
  mutate(
    violations = replace_na(violations, 0))

policy_driver <- policy_driver %>%
  mutate(
    claim_freq = 0.03 * 
      (1 + .1 * (sex == "MALE")) +
      .2 * (age < 20) + 
      .1 * (age >= 20 & age < 25) +
      .1 * violation_count +
      .1 * (marital == "SINGLE") -
      .1 * (homeowner == "YES"),
    claim_freq = replace_na(claim_freq, 0.03)) %>%
  rowwise() %>%
  mutate(
    claim_count = rpois(1, claim_freq),
    claim_amount = sum(rgamma(claim_count, scale = 1000, shape = 2))) %>%
  select(-claim_freq)

write.table(policies, file = "./data_mid/auto_policy.csv", row.names = FALSE, sep = ",")
write.table(policy_driver, file = "./data_mid/auto_policy_driver.csv", row.names = FALSE, sep = ",")

# https://www.kaggle.com/fivethirtyeight/fivethirtyeight-most-common-name-dataset/version/108?select=aging-curve.csv
