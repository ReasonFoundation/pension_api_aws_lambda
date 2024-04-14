####################################
# MS-PERS Normal Cost/Benefit Model #
####################################

# SalaryMatrix_long <- test_salary_matrix %>%
#   pivot_longer(cols = -1, names_to = "yos", values_to = "salary")
# 
# HeadCountMatrix_long <- headcount_matrix %>%
#   pivot_longer(cols = -1, names_to = "yos", values_to = "count")
# 
# #Calculate salary cumulative growth
# SalaryGrowth <- salary_growth_rate %>%
#   mutate(sal_cum_growth = cumprod(1 + lag(salary_growth_rate, default = 0)))
# 
# #Joining salary data, headcount data, and estimate entry salaries for existing employees
# SalaryHeadCountData <- SalaryMatrix_long %>%
#   left_join(HeadCountMatrix_long) %>%
#   # replace(is.na(.), 0) %>%
#   mutate(yos = as.numeric(yos),
#          current_year = year_start,
#          entry_age = age - yos,
#          entry_year = current_year - yos) %>%
#   filter(salary > 0, entry_age >= 18) %>%
#   left_join(SalaryGrowth) %>%
#   #estimate the salart at entry-age
#   mutate(entry_salary = salary / sal_cum_growth) %>%
#   select(entry_year, entry_age, age, yos, count, entry_salary)


headcount <- headcount_matrix %>%
  pivot_longer(cols = -1, names_to = "yos", values_to = "count") %>%
  replace(is.na(.), 0) %>%
  mutate(
    yos = as.numeric(yos),
    current_year = year_start,
    entry_age = age - yos,
    entry_year = current_year - yos
  ) %>%
  filter(entry_age >= 18)

salary_entry <- headcount %>%
  filter(yos == 2) %>%
  left_join(salary_entry_2022, by="age") %>%
  mutate(entrant_dist = count / sum(count)) %>%
  rename(start_salary = salary) %>%
  select(entry_age, current_year = current_year.x, start_salary, count, entrant_dist)

all_entry_age <- as.vector(salary_entry$entry_age)

# ################
# # Main rule: Retirement Eligibility
# ################

#Determine retirement eligibility
# Look at page 25 in the member handbook
check_normal_retirement_eligibility <- function(age, yos, entry_year){
  check_ <- ifelse(
    (entry_year <= 2007 & yos >= 25) |
      (entry_year <= 2007 & age >= 60 & yos >= 4) |
      (entry_year > 2007 & entry_year < 2011 & yos >= 25) |
      (entry_year > 2007 & entry_year < 2011 & age >= 60 & yos >= 8) |
      (entry_year >= 2011 & yos >= 30) |
      (entry_year >= 2011 & age >= 65 & yos >= 8),
    TRUE, FALSE)
  return(check_)
}

check_early_retirement_eligibility <- function(age, yos, entry_year){
  check_ <- ifelse(
    (entry_year >= 2011 & yos < 30 & yos >= 8 & age < 65 & age >= 60), # YOS < 30 & Age < 65
    TRUE, FALSE)
  return(check_)
}


check_retirement_eligibility <- function(age, yos, entry_year){
  check_ <- ifelse(
    check_normal_retirement_eligibility(age, yos, entry_year) == T |
    check_early_retirement_eligibility(age, yos, entry_year) == T,
    TRUE, FALSE)
  return(check_)
}


#Determine retirement type
get_retirement_type <- function(age, yos, entry_year){
  retirement_type = ifelse(check_normal_retirement_eligibility(age, yos, entry_year) == T, "normal",
                 ifelse(check_early_retirement_eligibility(age, yos, entry_year) == T, "early", "none"))
  return(retirement_type)
}

# lookup table for retirement type
memo_retirement_type <- CJ(age=age_range, yos=yos_range, entry_year=entry_year_range)
memo_retirement_type[, retirement_type := get_retirement_type(age, yos, entry_year)]
memo_retirement_type[, is_retire_eligible := check_retirement_eligibility(age, yos, entry_year)]

#Determine separation type
check_separation_eligibility <- function(yos, entry_year){
  check_ <- ifelse(
    (entry_year <= 2007 & yos >= 4) | (entry_year > 2007 & yos >= 8), TRUE, FALSE
  )
  return(check_)
}

get_separation_type <- function(age, yos, entry_year){
  separation_type = ifelse(check_retirement_eligibility(age, yos, entry_year), 'retire',
                           ifelse(check_separation_eligibility(yos, entry_year), 'vested', 'non-vested'))
  return(separation_type)
}

##Transform base mortality rates and mortality improvement rates
#Turn MP tables into long format and impute values for years after the max year in the MP tables
male_mp_ <- male_mp %>%
  pivot_longer(-age, names_to = "year", values_to = "male_mp_rate") %>%
  mutate(year = as.numeric(year))

male_ultimate_mp <- male_mp_ %>%        #ultimate rates = rates for the last year in the MP table
  filter(year == max(year)) %>%
  rename(male_ultimate_mp_rate = male_mp_rate) %>%
  select(-year)


male_final_mp <- expand_grid(age=age_range, year = 1951:max_year) %>%
  left_join(male_mp_, by = c("age", "year")) %>%
  left_join(male_ultimate_mp, by = "age") %>%
  mutate(
    male_final_mp_rate = ifelse(year > max(male_mp_$year), male_ultimate_mp_rate, male_mp_rate)
    # the "ultimate rates" are used for all years
    # male_final_mp_rate = male_ultimate_mp_rate
  ) %>%
  group_by(age) %>%
  mutate(
    male_cumprod_mp_raw = cumprod(1 - male_final_mp_rate),
    male_cumprod_mp_adj = male_cumprod_mp_raw / male_cumprod_mp_raw[year == 2010]) %>%   #Adjust the mort improvement rates for the 2010 base year
  ungroup()


female_mp_ <- female_mp %>%
  pivot_longer(-age, names_to = "year", values_to = "female_mp_rate") %>%
  mutate(year = as.numeric(year))

female_ultimate_mp <- female_mp_ %>%        #ultimate rates = rates for the last year in the MP table
  filter(year == max(year)) %>%
  rename(female_ultimate_mp_rate = female_mp_rate) %>%
  select(-year)

female_final_mp <- expand_grid(age=age_range, year = 1951:max_year) %>%
  left_join(female_mp_, by = c("age", "year")) %>%
  left_join(female_ultimate_mp, by = "age") %>%
  mutate(
    female_final_mp_rate = ifelse(year > max(female_mp_$year), female_ultimate_mp_rate, female_mp_rate)
    # the "ultimate rates" are used for all years
    # female_final_mp_rate = female_ultimate_mp_rate
    ) %>%
  group_by(age) %>%
  mutate(
    female_cumprod_mp_raw = cumprod(1 - female_final_mp_rate),
    female_cumprod_mp_adj = female_cumprod_mp_raw / female_cumprod_mp_raw[year == 2010]) %>%   #Adjust the mort improvement rates for the 2010 base year
  ungroup()


################################# Mortality Rate #############################################
dt_survival_rate <- data.table(survival_rate)
dt_male_final_mp <- data.table(male_final_mp)
dt_female_final_mp <- data.table(female_final_mp)

# Define a lookup table for male mortality rates
male_mort_table <- data.table(age = c(0, 60, 75, 76, 77, Inf),
                              rate = c(0.95, 0.95, 1.10, 1, 1.01, 1.01))
male_mort_table_age <- c(0, 60, 75, 76, 77, Inf)
male_mort_table_rate <- c(0.95, 0.95, 1.10, 1, 1.01, 1.01)

# Define a lookup table for female mortality rates
female_mort_table <- data.table(age = c(0, 72, Inf),
                                rate = c(0.84, 0.84, 1))
female_mort_table_age <- c(0, 72, Inf)
female_mort_table_rate <- c(0.84, 0.84, 1)

# setkey(female_mort_table, age)

dt_mortality_rate <- CJ(entry_year = entry_year_range, yos = yos_range,
                        entry_age = all_entry_age, age = age_range)[,`:=`(term_year = entry_year + yos, year = entry_year + age - entry_age)][term_year <= year]

# setkey(dt_mortality_rate, age)
# setkey(dt_survival_rate, age)

df_join1 <- dt_mortality_rate[dt_survival_rate, nomatch = 0, on = "age"]

# setkey(df_join1, age, year)

dt_opt_mortality_rate <- df_join1[dt_male_final_mp, nomatch = 0, on = c("age", "year")][dt_female_final_mp, nomatch = 0, on = c("age", "year")]

setorder(dt_opt_mortality_rate, entry_year, entry_age, yos, age)

dt_opt_mortality_rate[, is_retirement := check_retirement_eligibility(age, yos, entry_year)]

dt_opt_mortality_rate[, mort_rate := calc_mort_rate_cpp(age, is_retirement, male_employee, male_healthy_retiree, male_cumprod_mp_adj,
                                                     female_employee, female_healthy_retiree, female_cumprod_mp_adj,
                                                     male_mort_table_age, male_mort_table_rate,
                                                     female_mort_table_age, female_mort_table_rate)]



selected_cols <- c("entry_year", "term_year", "year", "entry_age", "age", "yos", "mort_rate")
opt_mortality_rate <- dt_opt_mortality_rate[, ..selected_cols]

# Ready to annuity factor calculation
opt_mortality_rate[, survival_rate_ := cumprod(1 - shift(mort_rate, fill = 0, type = "lag")), by = .(entry_year, entry_age, yos)]
opt_mortality_rate[, min_age := min(age), by = .(entry_year, entry_age, yos)]

 
# Rprof("./Profiling-Outputs/optimized_mortality_rate_profile.out", interval = 0.1, append=FALSE)
# optimized_mortality_rate_func()
# Rprof(NULL)
# summaryRprof("./Profiling-Outputs/optimized_mortality_rate_profile.out")

mortality_rate_retire <- expand_grid(age = age_range[age_range >= 40],
                                     year = year_range[year_range >= year_start]) %>%
  left_join(survival_rate, by = "age") %>%
  left_join(male_final_mp, by = c("age", "year")) %>%
  left_join(female_final_mp, by = c("age", "year")) %>%
  mutate(
    base_age = age - (year - year_start),
    male_mort_rate = male_healthy_retiree * male_cumprod_mp_adj,
    female_mort_rate = female_healthy_retiree * female_cumprod_mp_adj,
    mort_rate = (male_mort_rate + female_mort_rate) / 2
  ) %>%
  select(base_age, age, year, mort_rate) %>%
  filter(base_age >= 40) %>%
  arrange(base_age)


#Separation Rates (only apply to active members)
#Because separation rates are subject to retirement eligibility, which depends on entry years (tiers), the separation table needs to take entry years into account
final_male_separation_rate <- init_male_separation_rate %>%
  pivot_longer(cols=-1, names_to = "yos", values_to = "male_separation_rate") %>%
  mutate(
    age = as.numeric(age),
    yos = as.numeric(yos),
    entry_age = age - yos
  ) %>%
  filter(entry_age %in% all_entry_age) %>%
  select(age, yos, male_separation_rate)

final_female_separation_rate <- init_female_separation_rate %>%
  pivot_longer(cols=-1, names_to = "yos", values_to = "female_separation_rate") %>%
  mutate(
    age = as.numeric(age),
    yos = as.numeric(yos),
    entry_age = age - yos
  ) %>%
  filter(entry_age %in% all_entry_age) %>%
  select(age, yos, female_separation_rate)

########################## Initial separation rate #############################
separation_rate <- expand_grid(age=age_range, yos=yos_range, entry_year=entry_year_range) %>%
  mutate(entry_age = age - yos) %>%
  filter(entry_age %in% all_entry_age) %>%
  arrange(entry_year, entry_age, age) %>%
  group_by(entry_year, entry_age) %>%
  left_join(final_male_separation_rate, by=c("age", "yos")) %>%
  left_join(final_female_separation_rate, by=c("age", "yos")) %>%
  left_join(init_retirement_rate_tier4, by="age") %>%
  left_join(init_retirement_rate_tier123, by="age") %>%
  replace(is.na(.), 0) %>%
  mutate(
    is_retirement = check_retirement_eligibility(age, yos, entry_year),

    male_separation_rate_ = ifelse(is_retirement == T & entry_year < 2011 & yos < 25, male_under_25yos,
                                  ifelse(is_retirement == T & entry_year < 2011 & yos >= 25, male_over_25yos,
                                         ifelse(is_retirement == T & entry_year >= 2011 & yos < 30, male_under_30yos,
                                                ifelse(is_retirement == T & entry_year >= 2011 & yos >= 30, male_over_30yos,
                                                       male_separation_rate)))),

    female_separation_rate_ = ifelse(is_retirement == T & entry_year < 2011 & yos < 25, female_under_25yos,
                                  ifelse(is_retirement == T & entry_year < 2011 & yos >= 25, female_over_25yos,
                                         ifelse(is_retirement == T & entry_year >= 2011 & yos < 30, female_under_30yos,
                                                ifelse(is_retirement == T & entry_year >= 2011 & yos >= 30, female_over_30yos,
                                                       female_separation_rate)))),

    sep_rate = (male_separation_rate_ + female_separation_rate_)/2,

    remaining_prob = cumprod(1 - lag(sep_rate, default = 0)),
    separation_prob = lag(remaining_prob, default = 1) - remaining_prob) %>%
  ungroup() %>%
  select(entry_year, entry_age, age, yos, sep_rate, remaining_prob, separation_prob)

# curr_dist_rate = 0.0755
# new_dist_rate = 0.0755
# cola = 0.03
# retire_refund_ratio = 0.6
# funding_policy = "status quo"
# analysis_type = "Deterministic"
# roa_scenario = "Assumption"

get_salary_projection <- function(salary_entry, salary_growth_rate){
  salary_projection_ <-
    expand_grid(
      entry_year = entry_year_range,
      entry_age = all_entry_age,
      yos = yos_range) %>%
    mutate(
      age = entry_age + yos,
      year = entry_year + yos) %>%
    filter(age <= max_age) %>%
    arrange(entry_year, entry_age, yos) %>%
    select(entry_year, entry_age, age, yos, year) %>%
    left_join(salary_entry, by = "entry_age") %>%
    left_join(salary_growth_rate, by = "yos") %>%
    group_by(entry_year, entry_age) %>%
    mutate(
      salary = start_salary * cumprod(1 + lag(salary_growth_rate, default = 0)) * (1 + payroll_growth_)^(year - yos - year_start),
      fas_year = 4,
      final_avg_salary = get_roll_mean(salary, fas_year), # fix FAS at 4
      employee_contribution = ee_contr_rate_ * salary,
      db_employee_balance = get_cum_fv(credited_interest_, employee_contribution),
    ) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  setDT(salary_projection_)
}

# get_salary_projection_memo <- memoise(get_salary_projection)

get_annuity_factor <- function(opt_mortality_rate, curr_dist_rate, new_dist_rate, cola){
  opt_mortality_rate[, discount_rate := if_else(entry_year <= year_start, curr_dist_rate, new_dist_rate)]
  opt_annuity_factor_tab <- opt_mortality_rate[, {
    # discount_rate = ifelse(entry_year <= year_start, curr_dist_rate, new_dist_rate)
    # survival_rate_ = cumprod(1 - shift(mort_rate, fill = 0, type = "lag"))
    # min_age = min(age)
    disc_survival_rate = survival_rate_ / (1 + discount_rate) ^ (age - min_age)
    disc_survival_rate_cola = disc_survival_rate * (1 + cola) ^ (age - min_age)
    disc_survival_rate_cola_ = cumsum(rev(disc_survival_rate_cola))
    annuity_factor = rev(disc_survival_rate_cola_) / disc_survival_rate_cola
    # annuity_factor = rev(cumsum(rev(disc_survival_rate_cola))) / disc_survival_rate_cola
    .(term_year, age, min_age, discount_rate, year, survival_rate_, disc_survival_rate, disc_survival_rate_cola, annuity_factor)
  }, by = .(entry_year, entry_age, yos)]
}

# get_annuity_factor_memo <- memoise(get_annuity_factor)


get_reduce_factor <- function(opt_annuity_factor_tab, memo_retirement_type){
  annuity_factor_tab_dt <- opt_annuity_factor_tab[memo_retirement_type, nomatch = 0, on = c('entry_year', 'age', 'yos')]
  
  early_retired <- annuity_factor_tab_dt[retirement_type == 'early' | retirement_type == 'normal' & age == 65 & entry_year >= 2011]
  normal_retired <- annuity_factor_tab_dt[retirement_type == 'normal' & entry_year < 2011 | retirement_type == 'normal' & age != 65 & entry_year >= 2011]
  no_retired <- annuity_factor_tab_dt[retirement_type == 'none']
  
  early_retired[retirement_type == 'normal', reduced_factor := 1]
  early_retired[, reduced_factor := annuity_factor[age == 65] * (disc_survival_rate[age == 65] / disc_survival_rate) / annuity_factor,
                by = c('entry_year', 'entry_age', 'yos')]
  
  early_retired <- early_retired[, c('entry_year', 'entry_age', 'yos', 'age', 'term_year',
                                     'retirement_type', 'reduced_factor', "year",
                                     'disc_survival_rate', 'annuity_factor')]
  
  no_retired <- annuity_factor_tab_dt[retirement_type == "none"]
  no_retired[, reduced_factor := 0]
  no_retired <- no_retired[, c('entry_year', 'entry_age', 'yos', 'age', 'term_year',
                               'retirement_type', 'reduced_factor', "year",
                               'disc_survival_rate', 'annuity_factor')]
  
  normal_retired[, reduced_factor := 1]
  normal_retired <- normal_retired[, c('entry_year', 'entry_age', 'yos', 'age', 'term_year',
                                       'retirement_type', 'reduced_factor', "year",
                                       'disc_survival_rate', 'annuity_factor')]
  
  opt_reduced_factor_tab <- rbindlist(list(early_retired, normal_retired, no_retired))
  setkey(opt_reduced_factor_tab, entry_year, entry_age, yos, age)
}

# get_reduce_factor_memo <- memoise(get_reduce_factor)

get_benefit_projection <- function(opt_reduced_factor_tab, memo_retirement_type, salary_projection){
  opt_benefit_projection <- opt_reduced_factor_tab[memo_retirement_type[, !c("retirement_type")], nomatch = 0, on = c("age", "yos", "entry_year")]

  opt_benefit_projection[, term_age := entry_age + yos]
  opt_benefit_projection <- opt_benefit_projection[salary_projection, nomatch = 0,
                                                   on = c("entry_year", "term_year" = "year", "entry_age", "term_age" = "age", "yos")]

  opt_benefit_projection <- opt_benefit_projection[, c("break_year", "min_benefit", "yos_mark") := {
    break_year = ifelse(entry_year < 2011, 1, 0)
    min_benefit = break_year * 120 * yos
    yos_mark = break_year * 25 + (1 - break_year) * 30
  }]

  opt_benefit_projection[, adj_annuity_factor := annuity_factor * disc_survival_rate]

  opt_benefit_projection[, c("before_yos_mark", "after_yos_mark", "benefit_multiplier") := {
    before_yos_mark = min(yos_mark, yos)
    after_yos_mark = max(yos - yos_mark, 0)
    benefit_multiplier = break_year * (0.02 * before_yos_mark + 0.025 * after_yos_mark) +
      (1 - break_year) * (0.02 * before_yos_mark + 0.025 * after_yos_mark)
  }, by = .(entry_year, entry_age, yos)]

  opt_benefit_projection <- ungroup(opt_benefit_projection)

  opt_benefit_projection[, pension_benefit := reduced_factor * final_avg_salary * benefit_multiplier]
  opt_benefit_projection[, pension_benefit_ := ifelse(reduced_factor == 1, pmax(pension_benefit, min_benefit), pension_benefit)]
  opt_benefit_projection[, pv_benefit := pension_benefit * adj_annuity_factor]

  setnames(opt_benefit_projection, "age", "retire_age")
  setorder(opt_benefit_projection, entry_year, entry_age, yos, retire_age)
}

get_benefit_data <- function(
    curr_dist_rate = curr_discount_rate_,
    new_dist_rate = new_discount_rate_,
    cola = cola_,
    retire_refund_ratio = retire_refund_ratio_
) {

  ############################### Salary Projection ############################### 
  salary_projection <- get_salary_projection(salary_entry, salary_growth_rate)

  opt_annuity_factor_tab <- get_annuity_factor(opt_mortality_rate, curr_dist_rate, new_dist_rate, cola)

  annuity_factor_retire <- mortality_rate_retire %>%
    group_by(base_age) %>%
    mutate(
      survival_rate_ = cumprod(1 - lag(mort_rate, default = 0)),
      disc_survival_rate = survival_rate_/(1 + curr_dist_rate)^(age - min(age)),
      disc_survival_rate_cola_retire = disc_survival_rate * (1+cola)^(age - min(age)),
      annuity_factor_retire = rev(cumsum(rev(disc_survival_rate_cola_retire)))/disc_survival_rate_cola_retire
    ) %>%
    ungroup()
  
  opt_reduced_factor_tab <- get_reduce_factor(opt_annuity_factor_tab, memo_retirement_type)

  ################################ Benefit Projection   ############################### 
  
  opt_benefit_projection <- get_benefit_projection(opt_reduced_factor_tab, memo_retirement_type, salary_projection)
  
  # # start_misc <- Sys.time()
  # # Calculate retire_age using data.table syntax
  max_age <- max(opt_benefit_projection$retire_age)
  retire_age_tab <- opt_benefit_projection[, .(retire_age = .N - sum(is_retire_eligible) + min(retire_age)), by = .(entry_year, entry_age, term_age)]
  retire_age_tab[, retire_age := ifelse(retire_age == max_age + 1, term_age, retire_age)]
  # 
  # # Remove unnecessary columns
  retire_age_tab <- retire_age_tab[, .(entry_year, entry_age, term_age, retire_age)]
  # 
  setkey(opt_benefit_projection, entry_year, entry_age, term_age, retire_age)
  setkey(retire_age_tab, entry_year, entry_age, term_age, retire_age)
  pension_benefit_tab <- opt_benefit_projection[retire_age_tab, .(entry_year, entry_age, term_age, retire_age, pv_benefit, adj_annuity_factor), nomatch = 0L]
  # 
  # mid_misc_1 <- Sys.time()
  
  ############################### Reestimate Payroll   ############################### 
  # Estimate salary for each group of age and yos based on decisions in optimum benefit table
  est_salary_2022 <- salary_projection %>%
    filter(year == 2022) %>%
    select(entry_year, entry_age, yos, year, salary)

  est_salary_headcount_2022 <- pension_benefit_tab %>%
    mutate(
      all_yos = term_age - entry_age,
      term_year = entry_year + all_yos,
      current_age = term_age - (term_year - 2022)
    ) %>%
    filter(term_year > 2022 & entry_year <= 2022) %>% # People enter before 2022 and terminate after 2022
    left_join(est_salary_2022, by=c("entry_year", "entry_age"))

  age_groups <- others$age_groups
  yos_groups <- others$yos_groups

  mat <- matrix(0, nrow=10, ncol=7)
  for(row in 1:10){
    for(col in 1:7){
      temp_df = est_salary_headcount_2022 %>%
        filter(age_groups[2*row-1] <= current_age & current_age <= age_groups[2*row] &
                 yos_groups[2*col-1] <= yos & yos <= yos_groups[2*col])
      avg_salary = mean(temp_df$salary)

      mat[row, col] = ifelse(is.na(avg_salary), 0, avg_salary)
    }
  }


  salary_matrix <- data.frame(headcount_matrix[,'age'])
  salary_matrix <- cbind(salary_matrix, as.data.frame(mat))
  colnames(salary_matrix) <- colnames(headcount_matrix)

  salary_matrix_long <- salary_matrix %>%
    pivot_longer(cols=-1, names_to="yos", values_to="salary")

  headcount_matrix_long <- headcount_matrix %>%
    pivot_longer(cols=-1, names_to="yos", values_to="count")

  salary_headcount <- salary_matrix_long %>%
    left_join(headcount_matrix_long) %>%
    replace(is.na(.), 0) %>%
    mutate(
      yos = as.numeric(yos),
      current_year = year_start,
      entry_age = age - yos,
      entry_year = current_year - yos
    ) %>%
    filter(salary > 0, entry_age >= 18)


  headcount_matrix_ <- data.matrix(headcount_matrix %>% replace(is.na(.), 0))

  payroll_matrix <- mat * headcount_matrix_[,2:8]
  est_payroll <- rowSums(payroll_matrix) / 1000 # divide by 1000 to plot scatter plot at the end
  
  # end_misc <- Sys.time()
  
  ############################### Final dataset   ############################### 
  # Benefit Accrual & Normal Cost # 
  #### Real Pension Wealth = Pension Wealth adjusted for inflation
  #### Actuarial PV of Pension Wealth = Pension Wealth discounted back to entry age, multiplied by separation probability
  #Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
  ##################################### - Must optimize this function - 2.188s/2.777s
  # start_final_data <- Sys.time()
  opt_final_data <- salary_projection %>%
    left_join(pension_benefit_tab, by = c("entry_year", "entry_age", "age" = "term_age")) %>%
    left_join(separation_rate, by = c("entry_year", "entry_age", "age", "yos"))

  opt_final_data <- opt_final_data %>%
    # group_by(entry_year, entry_age) %>%
    mutate(
      separation_type = get_separation_type(age = age, yos = yos, entry_year = entry_year),
      discount_rate = ifelse(entry_year <= year_start, curr_dist_rate, new_dist_rate),
      benefit_decision = ifelse(yos == 0, 'work',
                                ifelse(separation_type == 'retire', 'retire',
                                       ifelse(separation_type == 'vested', 'mix', 'refund'))),
      nominal_pension_wealth =
        ifelse(separation_type == 'retire', pv_benefit,
               ifelse(separation_type == 'non-vested', db_employee_balance,
                      retire_refund_ratio * pv_benefit + (1 - retire_refund_ratio) * db_employee_balance)
        ),
      real_pension_wealth = nominal_pension_wealth/(1 + assum_inflation_)^yos
      )

  opt_final_data_ <- opt_final_data %>%
    group_by(entry_year, entry_age) %>%
    mutate(
      #Calculate present value of future benefits (PVFB) for DB members
      pv_future_benefit = opt_PVFB_rcpp(sep_rate, discount_rate, nominal_pension_wealth),

      #Calculate present value of future salaries (PVFS)
      pv_future_salary = opt_PVFS_rcpp(discount_rate, remaining_prob, salary),

      #Calculate entry-age normal cost rate by dividing the PVFB by the PVFS at entry age
      normal_cost_rate = pv_future_benefit[yos == 0] / pv_future_salary[yos == 0],

      #Calculate present value of future normal costs (PVFNC)
      pv_future_normal_cost = pv_future_salary * normal_cost_rate
    ) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  # end_final_data <- Sys.time()


  # start_normal_cost <- Sys.time()
  #Calculate normal cost rate for each entry age in each entry year
  indv_normal_cost <- opt_final_data_ %>%
    filter(yos == 0) %>%
    select(entry_year, entry_age, normal_cost_rate)


  #Calculate the aggregate normal cost
  agg_normal_cost_tab <- indv_normal_cost %>%
    left_join(salary_headcount, by = c("entry_year", "entry_age")) %>%
    filter(!is.na(current_year)) %>%
    summarise(agg_normal_cost = sum(normal_cost_rate * salary * count) / sum(salary * count)) %>%
    pull()

  output <- list(
    ann_factor_tab = opt_annuity_factor_tab,
    annuity_factor_retire_tab = annuity_factor_retire,
    reduced_factor = opt_reduced_factor_tab,
    ben_tab = opt_benefit_projection,
    ben_retire_tab = pension_benefit_tab,
    final_tab = opt_final_data_,
    nc_tab = indv_normal_cost,
    nc_agg = agg_normal_cost_tab
  )
  
  # end_normal_cost = Sys.time()

  
  return(output)

}

# memoised_get_benefit_data <- memoise(get_benefit_data)


#Compare payroll distributions

# correct_payroll <- c(7476, 191368, 482530, 637260, 785830, 951304, 969208, 927712, 733019, 504672, 178065)
# 
# correct_payroll <- others$correct_payroll
# df <- data.frame(
#   group=c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69"),
#   correct_payroll, est_payroll)
# 
# ggplot(df, aes(x=group)) +
#   geom_point(aes(y=correct_payroll), color="blue") +
#   geom_point(aes(y=est_payroll), color="red") +
#   labs(title="Compare payroll distributions",
#        x="Group of ages", y = "Payroll")

  
################################ Mortality rate with pure data.table ################################ 
# calc_mort_rate <- function(age, is_retirement, male_employee, male_healthy_retiree, male_cumprod_mp_adj,
#                            female_employee, female_healthy_retiree, female_cumprod_mp_adj) {
# 
#   male_rate <- male_employee * male_mort_table$rate[findInterval(age, male_mort_table$age)]
#   male_mort_rate <- male_cumprod_mp_adj * (is_retirement * male_healthy_retiree + (!is_retirement)*male_rate)
# 
#   female_rate <- female_employee * female_mort_table$rate[findInterval(age, female_mort_table$age)]
#   female_mort_rate <- female_cumprod_mp_adj * (is_retirement * female_healthy_retiree + (!is_retirement)*female_rate)
# 
#   (male_mort_rate + female_mort_rate) / 2
# }

# library(parallel)

# calc_mort_rate_parallel <- function(age, is_retirement, male_employee, male_healthy_retiree, male_cumprod_mp_adj,
#                                     female_employee, female_healthy_retiree, female_cumprod_mp_adj) {
#   mort_rate <- mclapply(seq_along(age), function(i) {
#     male_rate <- male_employee[i] * male_mort_table$rate[findInterval(age[i], male_mort_table$age)]
#     male_mort_rate <- male_cumprod_mp_adj[i] * (is_retirement[i] * male_healthy_retiree[i] + (!is_retirement[i]) * male_rate)
# 
#     female_rate <- female_employee[i] * female_mort_table$rate[findInterval(age[i], female_mort_table$age)]
#     female_mort_rate <- female_cumprod_mp_adj[i] * (is_retirement[i] * female_healthy_retiree[i] + (!is_retirement[i]) * female_rate)
# 
#     (male_mort_rate + female_mort_rate) / 2
#   }, mc.cores = detectCores())
# 
#   unlist(mort_rate)
# }



