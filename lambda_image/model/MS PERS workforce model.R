# curr_dist_rate = curr_discount_rate_
# new_dist_rate = new_discount_rate_
# cola_for_tier_5 = cola_for_tier_5_
# retire_refund_ratio = retire_refund_ratio_
# stat_new_hire_EE_contr = stat_new_hire_EE_contr


get_wf_data <- function(
    curr_dist_rate = curr_discount_rate_,
    new_dist_rate = new_discount_rate_,
    cola_for_tier_5 = cola_for_tier_5_,
    retire_refund_ratio = retire_refund_ratio_,
    stat_new_hire_EE_contr = stat_new_hire_EE_contr_,
    benefit_data = benefit_data
    ){
  
  # benefit_data <- get_benefit_data(
  #   curr_dist_rate = curr_dist_rate,
  #   new_dist_rate = new_dist_rate,
  #   cola_for_tier_5 = cola_for_tier_5,
  #   retire_refund_ratio = retire_refund_ratio,
  #   stat_new_hire_EE_contr = stat_new_hire_EE_contr
  # )
  
  #Initialize empty workforce projection arrays
  entry_age <- headcount_salary_entry$entry_age
  age_projection <- age_range_
  # year_projection <- year_start:2130
  year_projection <- year_of_latest_val_report_:(year_of_latest_val_report_ + model_period_)
  term_year <- year_projection
  retire_year <- year_projection
  
  active_dim <- c(length(entry_age), length(age_projection), length(year_projection))
  active_dim_names <- list(entry_age, age_projection, year_projection)
  
  term_dim <- c(length(entry_age), length(age_projection), length(year_projection), length(term_year))
  term_dim_names <- list(entry_age, age_projection, year_projection, term_year)
  
  retire_dim <- c(length(entry_age), length(age_projection), 
                  length(year_projection), length(term_year), length(retire_year))
  retire_dim_names <- list(entry_age, age_projection, year_projection, term_year, retire_year)
  
  wf_active <- array(0, dim = active_dim, dimnames = active_dim_names)
  wf_term <- array(0, dim = term_dim, dimnames = term_dim_names)
  wf_refund <- wf_term
  wf_retire <- array(0, dim = retire_dim, dimnames = retire_dim_names)
  
  # Initial population
  active_int_df <- expand_grid(entry_age, age = age_projection) %>%
    left_join(headcount, by = c("entry_age", "age")) %>%
    replace(is.na(.), 0) %>%
    select(entry_age, age, count)
  
  active_int_matrix <- xtabs(count ~ entry_age + age, active_int_df)
  
  wf_active[,,1] <- active_int_matrix
  
  # active_int_pop <- c(2000, 8000, 8000, 7000, 8000, 9000, 8000, 7000, 6000, 5000, 3000)
  # 
  # active_int_ea <- data.frame(entry_age = salary_entry_2022$entry_age,
  #                              age = salary_entry_2022$entry_age,
  #                              n.active = active_int_pop)
  # 
  # active_int <- expand_grid(entry_age, age=age_projection) %>%
  #   left_join(active_int_ea) %>%
  #   replace(is.na(.), 0) %>%
  #   pivot_wider(names_from = age, values_from = n.active) %>%
  #   select(-1)
  
  # wf_active[,,1] <- as.matrix(active_int)
  
  #Position matrix to add new hires
  position_matrix <- expand_grid(entry_age, age=age_projection) %>%
    mutate(new = ifelse(entry_age == age, 1, 0))
  
  position_matrix <- xtabs(new ~ entry_age + age, position_matrix)
  
  
  ##Create probability array
  
  #Mortality probability array (4 dimensions)
  mort_df_term <- expand_grid(entry_age, age=age_projection, year=year_projection, term_year) %>%
    left_join(opt_mortality_rate, by=c("entry_age", "age", "year", "term_year")) %>%
    mutate(mort_rate = ifelse(is.na(mort_rate), 0, mort_rate))
  
  mort_array_term <- xtabs(mort_rate ~ entry_age + age + year + term_year, mort_df_term)
  
  #Separation probability array (3 dimensions): 
  sep_df <- expand_grid(entry_age, age=age_projection, year=year_projection) %>%
    mutate(entry_year = year - (age - entry_age)) %>%
    left_join(separation_rate, by=c("entry_age", "age", "entry_year")) %>%
    select(entry_age, age, year, sep_rate) %>%
    mutate(sep_rate = ifelse(is.na(sep_rate), 0, sep_rate))
  
  sep_array <- xtabs(sep_rate ~ entry_age + age + year, sep_df)
  
  #Refund and retirement probability arrays
  #Determine the optimal retirement age
  optimal_retire <- benefit_data$final_tab %>%
    rename(term_age = age) %>%
    # select(entry_year, entry_age, term_age, yos, retire_age, max_pv_benefit, db_employee_balance, is_refund) %>%
    select(entry_year, entry_age, term_age, yos, retire_age, benefit_decision) %>%
    mutate(
      refund = case_when(benefit_decision == 'refund' ~ 1,
                         benefit_decision == 'mix' ~ 1 - retire_refund_ratio,
                         TRUE ~ 0),
      retire = case_when(benefit_decision == 'retire' ~ 1,
                         benefit_decision == 'mix' ~ 1,
                         TRUE ~ 0),
      refund_age = term_age
    )
    # mutate(
    #   refund = case_when(is_refund == 'refund' ~ 1, TRUE ~ 0),
    #   retire = case_when(is_refund == 'retire' ~ 1, TRUE ~ 0),
    #   refund_age = term_age
    # )
  
  # optimal_retire <- optimum_benefit %>%
  #   left_join(benefit_projection %>% filter(present_value > 0),
  #             by=c("entry_year", "entry_age", "term_age", "max_benefit"="present_value")) %>%
  #   select(entry_year, entry_age, term_age, age, max_benefit) %>%
  #   rename(retire_age = age) %>%
  #   left_join(final_data, by=c("entry_year", "entry_age", "term_age"="age", "max_benefit")) %>%
  #   select(entry_year, entry_age, term_age, yos, retire_age, max_benefit, db_employee_balance) %>%
  #   mutate(
  #     ben_decision = ifelse(yos == 0, NA, ifelse(db_employee_balance > max_benefit, "refund", "retire")),
  #     refund = case_when(ben_decision == "refund" ~ 1, T ~ 0),
  #     retire = case_when(ben_decision == "retire" ~ 1, T ~ 0),
  #     refund_age = term_age
  #   )
  
    
  #Retire probability array (4 dimensions)
  retire_df <- expand_grid(entry_age, age=age_projection, year=year_projection, term_year) %>%
    mutate(
      entry_year = year - (age - entry_age),
      term_age = age - (year - term_year),
      yos = term_age - entry_age
    ) %>%
    filter(year - term_year >= 0, yos >= 0) %>%
    left_join(optimal_retire, by=c("entry_age", "age"="retire_age", "entry_year", "term_age", "yos")) %>%
    mutate(retire = ifelse(is.na(retire), 0, retire))
  
  retire_array <- xtabs(retire ~ entry_age + age + year + term_year, retire_df)
  
  #Refund probability array (4 dimensions). Note that employees get refunds in the same year they get terminated. 
  refund_df <- expand_grid(entry_age, age=age_projection, year=year_projection, term_year) %>%
    mutate(
      entry_year = year - (age - entry_age),
      term_age = age - (year - term_year),
      yos = term_age - entry_age
    ) %>%
    filter(year - term_year >= 0, yos >= 0) %>%
    left_join(optimal_retire, by=c("entry_age", "age"="refund_age", "entry_year", "term_age", "yos")) %>%
    mutate(refund = ifelse(is.na(refund), 0, refund))
  
  refund_array <- xtabs(refund ~ entry_age + age + year + term_year, refund_df)
  
  #Transition matrix to shift the population to the right by 1 age after 1 year
  TM <-  diag(length(age_projection) + 1)[-1, -(length(age_projection) + 1)]
  
  # Workforce projection
  for (i in 2:length(year_projection)) {
    active2term <- wf_active[,,i-1] * sep_array[,,i-1]   #calculate the # of newly terminated actives. 2-dimensional array
    
    wf_active[,,i] <- (wf_active[,,i-1] - active2term) %*% TM  #add new entrants later
    
    new_entrants <- add_new_entrants(g = pop_growth_, ne_dist = headcount_salary_entry$entrant_dist, wf1 = wf_active[,,i-1], 
                          wf2 = wf_active[,,i], ea = entry_age, age = age_projection, position_matrix = position_matrix)
    
    wf_active[,,i] = wf_active[,,i] + new_entrants  #add new entrants
    
    term2death <- wf_term[,,i-1,] * mort_array_term[,,i-1,] #3-dimensional array
    
    wf_term[,,i,] <- apply(wf_term[,,i-1,] - term2death, 3, function(x) x %*% TM) %>% array(term_dim[-3]) 
    
    wf_term[,,i,i] <- active2term %*% TM   #add newly terminated members the term population
    
    term2refund <- wf_term[,,i,i] * refund_array[,,i,i]  #calculate the # of newly refunded members. 2-dimensional array
    
    wf_term[,,i,i] <- wf_term[,,i,i] - term2refund
    
    wf_refund[,,i,i] <- term2refund
    
    term2retire <- wf_term[,,i,] * retire_array[,,i,]  #calculate the # of newly retired members. 3-dimensional array
    
    wf_term[,,i,] <- wf_term[,,i,] - term2retire
    
    retire2death <- apply(wf_retire[,,i-1,,], 4, function(x) x * mort_array_term[,,i-1,]) %>% array(retire_dim[-3])   #4-dimensional array
    
    wf_retire[,,i,,] <- apply(wf_retire[,,i-1,,] - retire2death, c(3,4), function(x) x %*% TM) %>% array(retire_dim[-3])
    
    wf_retire[,,i,,i] <- term2retire
    
  }
  
  #####Convert the multidimensional arrays into data frames 
  wf_active_df <- data.frame(
      expand.grid(entry_age = entry_age, age = age_projection, year = year_projection), 
      n.active = as.vector(wf_active)) %>%
    filter(age >= entry_age)
  
  wf_term_df <- data.frame(
      expand.grid(entry_age = entry_age, age = age_projection, year = year_projection, term_year = term_year),
      n.term = as.vector(wf_term)) %>%
    filter(age >= entry_age, year >= term_year)
  
  wf_refund_df <- data.frame(
      expand.grid(entry_age = entry_age, age = age_projection, year = year_projection, term_year = term_year),
      n.refund = as.vector(wf_refund)) %>%
    filter(age >= entry_age, year >= term_year)
  
  #Since the wf_retire array is too big to handle using the above method, we need to split it into smaller parts for processing
  wf_retire_list <- list()  #empty list to save retire workforce data in the for loop
  
  for(i in seq_along(headcount_salary_entry$entry_age)){
    wf_retire_name <- paste0("wf_retire_", headcount_salary_entry$entry_age[i])
    assign(wf_retire_name, wf_retire[i,,,,])
    wf_retire_i <- data.table(CJ(retire_year, term_year, year_projection, age_projection),
                              n.retire = as.vector(get(wf_retire_name)))[n.retire > 0] %>%
      mutate(entry_age = headcount_salary_entry$entry_age[i])
    assign(wf_retire_name, wf_retire_i)   #do this to save memory space
    wf_retire_list <- append(wf_retire_list, list(get(wf_retire_name)))
  }
  
  #Combine all retire data frames from the retire list into one retire data frame 
  wf_retire_df <- rbindlist(wf_retire_list) %>% 
    select(entry_age, age=age_projection, year=year_projection, term_year, retire_year, n.retire)
  
  
  wf_data <- list(wf_active_df = wf_active_df,
                  wf_term_df = wf_term_df,
                  wf_refund_df = wf_refund_df,
                  wf_retire_df = wf_retire_df)
  
  # saveRDS(wf_data, "./wf_data.rds")

}
  
  
  
