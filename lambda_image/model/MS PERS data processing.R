##################################################################
##              Retirement & Separation Conditions              ##
##################################################################
#Current two tiers:
## Members joining before July 1, 2007 (tier 12)
## Members joining from July 1, 2007 to July 1, 2011 (tier 3)
## Members joining from July 1, 2011 to July 1, 2025 (tier 4)
## Members joining after July 1, 2025 (tier 5)

get_tier <- function(entry_year, age, yos){
  tier = if_else(entry_year < 2007,
                 case_when(
                   yos >= 25 | (age >= 60 & yos >= 4) ~ "tier_12_norm",
                   yos >= 4 ~ "tier_12_vested",
                   .default = "tier_12_non_vested"
                 ),
                 if_else(entry_year < 2011,
                         case_when(
                           yos >= 25 | (age >= 60 & yos >= 8) ~ "tier_3_norm",
                           yos >= 8 ~ "tier_3_vested",
                           .default = "tier_3_non_vested"
                         ),
                         if_else(entry_year < year_of_new_tier_start_,
                                 case_when(
                                   yos >= 30 | (age >= 65 & yos >= 8) ~ "tier_4_norm",
                                   yos < 30 & yos >= 8 & age < 65 & age >= 60 ~ "tier_4_early",
                                   yos >= 8 ~ "tier_4_vested",
                                   .default = "tier_4_non_vested"
                                 ),
                                 case_when(
                                   yos >= 30 | (age >= 65 & yos >= tier5_vesting_period_) ~ "tier_5_norm",
                                   yos < 30 & yos >= tier5_vesting_period_ & age < 65 & age >= 60 ~ "tier_5_early",
                                   yos >= tier5_vesting_period_ ~ "tier_5_vested",
                                   .default = "tier_5_non_vested"
                                 ))))
  return(tier)
}

#Separation type function
get_sep_type <- function(tier) {
  sep_type <- case_when(
    str_detect(tier, "early|norm|reduced") ~ "retire",
    str_detect(tier, "non_vested") ~ "non_vested",
    str_detect(tier, "vested") ~ "vested"
  )
}

check_retirement_eligibility <- function(tier){
  return(str_detect(tier, "early|norm|reduced"))
}

####################################
# MS-PERS Normal Cost/Benefit Model #
####################################

headcount <- headcount_matrix_ %>%
  pivot_longer(cols = -1, names_to = "yos", values_to = "count") %>%
  replace(is.na(.), 0) %>%
  mutate(
    yos = as.numeric(yos),
    current_year = year_of_latest_val_report_,
    entry_age = age - yos,
    entry_year = current_year - yos
  ) %>%
  filter(entry_age >= 18)

headcount_salary_entry <- headcount %>%
  filter(yos == 2) %>%
  left_join(salary_entry_, by="age") %>%
  mutate(
    entrant_dist = count / sum(count),
    start_salary = start_salary * salary_calibration_ratio
  ) %>%
  # rename(start_salary = salary) %>%
  select(entry_age, current_year = current_year.x, start_salary, count, entrant_dist)

all_entry_age <- as.vector(headcount_salary_entry$entry_age)

# lookup table for retirement type
memo_retirement_type <- CJ(age=age_range_, yos=yos_range_, entry_year=entry_year_range_)
memo_retirement_type[, tier := get_tier(entry_year, age, yos)]
memo_retirement_type[, is_retire_eligible := check_retirement_eligibility(tier)]
memo_retirement_type[, is_tier_5 := str_detect(tier, "tier_5")]


init_salary_projection <- memo_retirement_type %>% 
  mutate(
    entry_age = age - yos,
    year = entry_year + yos
  ) %>% 
  filter(entry_age %in% all_entry_age & age <= max_age_) %>%
  arrange(entry_year, entry_age, yos) %>%
  select(entry_year, entry_age, age, yos, year, tier, is_tier_5) %>%
  left_join(headcount_salary_entry, by = "entry_age") %>%
  left_join(salary_growth_rate_, by = "yos") %>%
  group_by(entry_year, entry_age) %>%
  mutate(
    salary = start_salary * cumprod(1 + lag(salary_growth_rate, default = 0)) * (1 + payroll_growth_)^(year - yos - year_of_latest_val_report_),
    fas_year = 4,
    final_avg_salary = get_roll_mean(salary, fas_year) # fix FAS at 4
  ) %>%
  ungroup() %>%
  replace(is.na(.), 0)

setDT(init_salary_projection)


#################################################################
##                    Mortality Assumptions                    ##
#################################################################

#Turn MP tables into long format and impute values for years after the max year in the MP tables
male_mp <- male_mp_ %>%
  pivot_longer(-age, names_to = "year", values_to = "male_mp_rate") %>%
  mutate(year = as.numeric(year))

male_ultimate_mp <- male_mp %>%        #ultimate rates = rates for the last year in the MP table
  filter(year == max(year)) %>%
  rename(male_ultimate_mp_rate = male_mp_rate) %>%
  select(-year)


male_final_mp <- expand_grid(age=age_range_, year = 1951:max_proj_year_) %>%
  left_join(male_mp, by = c("age", "year")) %>%
  left_join(male_ultimate_mp, by = "age") %>%
  mutate(
    male_final_mp_rate = ifelse(year > max(male_mp$year), male_ultimate_mp_rate, male_mp_rate)
  ) %>%
  group_by(age) %>%
  mutate(
    male_cumprod_mp_raw = cumprod(1 - male_final_mp_rate),
    male_cumprod_mp_adj = male_cumprod_mp_raw / male_cumprod_mp_raw[year == 2010]) %>%   #Adjust the mort improvement rates for the 2010 base year
  ungroup()


female_mp <- female_mp_ %>%
  pivot_longer(-age, names_to = "year", values_to = "female_mp_rate") %>%
  mutate(year = as.numeric(year))

female_ultimate_mp <- female_mp %>%        #ultimate rates = rates for the last year in the MP table
  filter(year == max(year)) %>%
  rename(female_ultimate_mp_rate = female_mp_rate) %>%
  select(-year)

female_final_mp <- expand_grid(age=age_range_, year = 1951:max_proj_year_) %>%
  left_join(female_mp, by = c("age", "year")) %>%
  left_join(female_ultimate_mp, by = "age") %>%
  mutate(
    female_final_mp_rate = ifelse(year > max(female_mp$year), female_ultimate_mp_rate, female_mp_rate)
    # the "ultimate rates" are used for all years
    # female_final_mp_rate = female_ultimate_mp_rate
  ) %>%
  group_by(age) %>%
  mutate(
    female_cumprod_mp_raw = cumprod(1 - female_final_mp_rate),
    female_cumprod_mp_adj = female_cumprod_mp_raw / female_cumprod_mp_raw[year == 2010]) %>%   #Adjust the mort improvement rates for the 2010 base year
  ungroup()


### Adjust the mortality improvement rates based on ages
### Find more information in val report's Schedule B about actuarial assumptions

dt_survival_rate <- data.table(survival_rate_)
dt_male_final_mp <- data.table(male_final_mp)
dt_female_final_mp <- data.table(female_final_mp)

# Define a lookup table for mortality rates
male_mort_table_age <- c(0, 60, 75, 76, 77, Inf)
male_mort_table_rate <- c(0.95, 0.95, 1.10, 1, 1.01, 1.01)

female_mort_table_age <- c(0, 72, Inf)
female_mort_table_rate <- c(0.84, 0.84, 1)

# Generate an initial mortality rate table
dt_mortality_rate <- CJ(entry_year = entry_year_range_, yos = yos_range_,
                        entry_age = all_entry_age, age = age_range_)[,`:=`(term_year = entry_year + yos, year = entry_year + age - entry_age)][term_year <= year]


# Join the mortality rate table with the tables of survival rate, male mp, and female mp
df_join1 <- dt_mortality_rate[dt_survival_rate, nomatch = 0, on = "age"]

dt_opt_mortality_rate <- df_join1[dt_male_final_mp, nomatch = 0, on = c("age", "year")][dt_female_final_mp, nomatch = 0, on = c("age", "year")]

setorder(dt_opt_mortality_rate, entry_year, entry_age, yos, age)

dt_opt_mortality_rate[, tier_at_dist_age := get_tier(entry_year, age, yos)]

dt_opt_mortality_rate[, is_tier_5 := str_detect(tier_at_dist_age, "tier_5")]

dt_opt_mortality_rate[, is_retirement := check_retirement_eligibility(tier_at_dist_age)]

dt_opt_mortality_rate[, mort_rate := calc_mort_rate_cpp(age, is_retirement, male_employee, male_healthy_retiree, male_cumprod_mp_adj,
                                                        female_employee, female_healthy_retiree, female_cumprod_mp_adj,
                                                        male_mort_table_age, male_mort_table_rate,
                                                        female_mort_table_age, female_mort_table_rate)]

dt_opt_mortality_rate[, survival_rate := cumprod(1 - shift(mort_rate, fill = 0, type = "lag")), by = .(entry_year, entry_age, yos)]
dt_opt_mortality_rate[, min_age := min(age), by = .(entry_year, entry_age, yos)]

selected_cols <- c("entry_year", "term_year", "year", "entry_age", "age", "min_age", "yos", "mort_rate", 
                   "tier_at_dist_age", "is_retirement", "survival_rate", "is_tier_5")
opt_mortality_rate <- dt_opt_mortality_rate[, ..selected_cols]


# Rprof("./Profiling-Outputs/optimized_mortality_rate_profile.out", interval = 0.1, append=FALSE)
# optimized_mortality_rate_func()
# Rprof(NULL)
# summaryRprof("./Profiling-Outputs/optimized_mortality_rate_profile.out")

mortality_rate_retire <- expand_grid(age = age_range_[age_range_ >= 40],
                                     year = year_range_[year_range_ >= year_of_latest_val_report_]) %>%
  left_join(survival_rate_, by = "age") %>%
  left_join(male_final_mp, by = c("age", "year")) %>%
  left_join(female_final_mp, by = c("age", "year")) %>%
  mutate(
    base_age = age - (year - year_of_latest_val_report_),
    male_mort_rate = male_healthy_retiree * male_cumprod_mp_adj,
    female_mort_rate = female_healthy_retiree * female_cumprod_mp_adj,
    mort_rate = (male_mort_rate + female_mort_rate) / 2
  ) %>%
  select(base_age, age, year, mort_rate) %>%
  filter(base_age >= 40) %>%
  arrange(base_age)




##############################################################################################################################
##################################################################
##                    Separation Assumptions                    ##
##################################################################

#Separation Rates (only apply to active members)
#Because separation rates are subject to retirement eligibility, which depends on entry years (tiers), the separation table needs to take entry years into account
male_separation_rate <- init_male_separation_rate_ %>%
  pivot_longer(cols=-1, names_to = "yos", values_to = "male_separation_rate") %>%
  mutate(
    age = as.numeric(age),
    yos = as.numeric(yos),
    entry_age = age - yos
  ) %>%
  filter(entry_age %in% all_entry_age) %>%
  select(age, yos, male_separation_rate)

female_separation_rate <- init_female_separation_rate_ %>%
  pivot_longer(cols=-1, names_to = "yos", values_to = "female_separation_rate") %>%
  mutate(
    age = as.numeric(age),
    yos = as.numeric(yos),
    entry_age = age - yos
  ) %>%
  filter(entry_age %in% all_entry_age) %>%
  select(age, yos, female_separation_rate)

########################## Separation rate #############################
# separation_rate <- expand_grid(age=age_range_, yos=yos_range_, entry_year=entry_year_range_) %>%
separation_rate <- memo_retirement_type %>%
  mutate(entry_age = age - yos) %>%
  filter(entry_age %in% all_entry_age) %>%
  arrange(entry_year, entry_age, age) %>%
  # left_join(memo_retirement_type, by=c("age", "yos", "entry_year")) %>%
  group_by(entry_year, entry_age) %>%
  left_join(male_separation_rate, by=c("age", "yos")) %>%
  left_join(female_separation_rate, by=c("age", "yos")) %>%
  left_join(init_retirement_rate_tier4_, by="age") %>%
  left_join(init_retirement_rate_tier123_, by="age") %>%
  replace(is.na(.), 0) %>%
  mutate(
    
    male_separation_rate_ = ifelse(is_retire_eligible == T & entry_year < 2011 & yos < 25, male_under_25yos,
                                   ifelse(is_retire_eligible == T & entry_year < 2011 & yos >= 25, male_over_25yos,
                                          ifelse(is_retire_eligible == T & entry_year >= 2011 & yos < 30, male_under_30yos,
                                                 ifelse(is_retire_eligible == T & entry_year >= 2011 & yos >= 30, male_over_30yos,
                                                        male_separation_rate)))),
    
    female_separation_rate_ = ifelse(is_retire_eligible == T & entry_year < 2011 & yos < 25, female_under_25yos,
                                     ifelse(is_retire_eligible == T & entry_year < 2011 & yos >= 25, female_over_25yos,
                                            ifelse(is_retire_eligible == T & entry_year >= 2011 & yos < 30, female_under_30yos,
                                                   ifelse(is_retire_eligible == T & entry_year >= 2011 & yos >= 30, female_over_30yos,
                                                          female_separation_rate)))),
    
    sep_rate = (male_separation_rate_ + female_separation_rate_)/2,
    
    remaining_prob = cumprod(1 - lag(sep_rate, default = 0)),
    separation_prob = lag(remaining_prob, default = 1) - remaining_prob) %>%
  ungroup() %>%
  select(entry_year, entry_age, age, yos, sep_rate, remaining_prob, separation_prob)



