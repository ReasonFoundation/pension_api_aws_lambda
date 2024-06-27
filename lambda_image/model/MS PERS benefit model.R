##############################################################################################################################
##################################################################
##                    Benefit Model Function                    ##
##################################################################

get_salary_projection <- function(stat_curr_hire_EE_contr, stat_new_hire_EE_contr){
  
  salary_projection_ <- init_salary_projection %>% 
    group_by(entry_year, entry_age) %>%
    mutate(
      employee_contribution = salary * if_else(is_tier_5, stat_new_hire_EE_contr, stat_curr_hire_EE_contr),
      db_employee_balance = get_cum_fv(credited_interest_, employee_contribution),
    ) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  setDT(salary_projection_)
}


get_annuity_factor <- function(opt_mortality_rate, curr_dist_rate, new_dist_rate, 
                               cola_for_tier_1234, cola_for_tier_5){
  
  opt_mortality_rate[, discount_rate := if_else(entry_year < year_of_new_tier_start_, curr_dist_rate, new_dist_rate)]
  opt_mortality_rate[, cola := if_else(is_tier_5, cola_for_tier_5, cola_for_tier_1234)]
  opt_annuity_factor_tab <- opt_mortality_rate[, {
    # discount_rate = ifelse(entry_year <= year_start, curr_dist_rate, new_dist_rate)
    # survival_rate_ = cumprod(1 - shift(mort_rate, fill = 0, type = "lag"))
    # min_age = min(age)
    disc_survival_rate = survival_rate / (1 + discount_rate) ^ (age - min_age)
    disc_survival_rate_cola = disc_survival_rate * (1 + cola) ^ (age - min_age)
    disc_survival_rate_cola_ = cumsum(rev(disc_survival_rate_cola))
    annuity_factor = rev(disc_survival_rate_cola_) / disc_survival_rate_cola
    # annuity_factor = rev(cumsum(rev(disc_survival_rate_cola))) / disc_survival_rate_cola
    .(term_year, age, min_age, discount_rate, year, survival_rate, disc_survival_rate, disc_survival_rate_cola, annuity_factor, cola)
  }, by = .(entry_year, entry_age, yos)]
}


get_reduce_factor <- function(opt_annuity_factor_tab, memo_retirement_type){
  annuity_factor_tab_dt <- opt_annuity_factor_tab[memo_retirement_type, nomatch = 0, on = c('entry_year', 'age', 'yos')]
  
  early_retired <- annuity_factor_tab_dt[str_detect(tier, 'early') | str_detect(tier, 'norm') & age == 65 & entry_year >= 2011]
  normal_retired <- annuity_factor_tab_dt[str_detect(tier, 'norm') & entry_year < 2011 | str_detect(tier, 'norm') & age != 65 & entry_year >= 2011]
  # no_retired <- annuity_factor_tab_dt[tier != 'early' & tier != 'normal']
  
  early_retired[str_detect(tier, 'norm'), reduced_factor := 1]
  early_retired[, reduced_factor := annuity_factor[age == 65] * (disc_survival_rate[age == 65] / disc_survival_rate) / annuity_factor,
                by = c('entry_year', 'entry_age', 'yos')]
  
  early_retired <- early_retired[, c('entry_year', 'entry_age', 'yos', 'age', 'term_year',
                                     'tier', 'reduced_factor', "year",
                                     'disc_survival_rate', 'annuity_factor')]
  
  no_retired <- annuity_factor_tab_dt[!str_detect(tier, 'early|norm'), ]
  no_retired[, reduced_factor := 0]
  no_retired <- no_retired[, c('entry_year', 'entry_age', 'yos', 'age', 'term_year',
                               'tier', 'reduced_factor', "year",
                               'disc_survival_rate', 'annuity_factor')]
  
  normal_retired[, reduced_factor := 1]
  normal_retired <- normal_retired[, c('entry_year', 'entry_age', 'yos', 'age', 'term_year',
                                       'tier', 'reduced_factor', "year",
                                       'disc_survival_rate', 'annuity_factor')]
  
  opt_reduced_factor_tab <- rbindlist(list(early_retired, normal_retired, no_retired))
  setkey(opt_reduced_factor_tab, entry_year, entry_age, yos, age)
}

# opt_reduced_factor_tab <- get_reduce_factor(opt_annuity_factor_tab, memo_retirement_type)

get_benefit_projection <- function(opt_reduced_factor_tab, memo_retirement_type, salary_projection, cal_factor = nc_calibration){
  opt_benefit_projection <- opt_reduced_factor_tab[memo_retirement_type[, !c("tier")], nomatch = 0, on = c("age", "yos", "entry_year")]
  
  opt_benefit_projection[, term_age := entry_age + yos]
  opt_benefit_projection <- opt_benefit_projection[salary_projection, nomatch = 0,
                                                   on = c("entry_year", "term_year" = "year", "entry_age", "term_age" = "age", "yos")]
  
  opt_benefit_projection <- opt_benefit_projection[, c("is_break_year", "min_benefit", "yos_mark") := {
    is_break_year = ifelse(entry_year < 2011, 1, 0)
    min_benefit = is_break_year * 120 * yos
    yos_mark = is_break_year * 25 + (1 - is_break_year) * 30
  }]
  
  opt_benefit_projection[, adj_annuity_factor := annuity_factor * disc_survival_rate]
  
  opt_benefit_projection[, c("before_yos_mark", "after_yos_mark", "benefit_multiplier") := {
    before_yos_mark = min(yos_mark, yos)
    after_yos_mark = max(yos - yos_mark, 0)
    benefit_multiplier = is_break_year * (0.02 * before_yos_mark + 0.025 * after_yos_mark) +
      (1 - is_break_year) * (0.02 * before_yos_mark + 0.025 * after_yos_mark)
  }, by = .(entry_year, entry_age, yos)]
  
  opt_benefit_projection <- ungroup(opt_benefit_projection)
  
  opt_benefit_projection[, pension_benefit := reduced_factor * final_avg_salary * benefit_multiplier]
  opt_benefit_projection[, pension_benefit := ifelse(reduced_factor == 1, pmax(pension_benefit, min_benefit), pension_benefit)]
  opt_benefit_projection[, pension_benefit := pension_benefit * cal_factor]
  opt_benefit_projection[, pv_benefit := pension_benefit * adj_annuity_factor]
  
  setnames(opt_benefit_projection, "age", "retire_age")
  setorder(opt_benefit_projection, entry_year, entry_age, yos, retire_age)
}

# curr_dist_rate <- curr_discount_rate_
# new_dist_rate <- new_discount_rate_
# cola_for_tier_5 <- cola_for_tier_5_
# retire_refund_ratio <- retire_refund_ratio_
# stat_new_hire_EE_contr <- stat_new_hire_EE_contr_


get_benefit_data <- function(
    curr_dist_rate = curr_discount_rate_,
    new_dist_rate = new_discount_rate_,
    cola_for_tier_5 = cola_for_tier_5_,
    retire_refund_ratio = retire_refund_ratio_,
    stat_new_hire_EE_contr = stat_new_hire_EE_contr_
) {
  
  ############################### Salary Projection ############################### 
  salary_projection <- get_salary_projection(stat_curr_hire_EE_contr_, stat_new_hire_EE_contr)
  
  opt_annuity_factor_tab <- get_annuity_factor(opt_mortality_rate, curr_dist_rate, new_dist_rate, 
                                               cola_for_tier_1234_, cola_for_tier_5)
  
  
  annuity_factor_retire <- mortality_rate_retire %>%
    group_by(base_age) %>%
    mutate(
      survival_rate = cumprod(1 - lag(mort_rate, default = 0)),
      disc_survival_rate = survival_rate/(1 + curr_dist_rate)^(age - min(age)),
      disc_survival_rate_cola_retire = disc_survival_rate * (1+cola_for_tier_1234_)^(age - min(age)),
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
  # est_salary_of_latest_year_report <- salary_projection %>%
  #   filter(year == year_of_latest_val_report_) %>%
  #   select(entry_year, entry_age, yos, year, salary)
  # 
  # est_salary_headcount_of_latest_year_report <- pension_benefit_tab %>%
  #   mutate(
  #     all_yos = term_age - entry_age,
  #     term_year = entry_year + all_yos,
  #     current_age = term_age - (term_year - year_of_latest_val_report_)
  #   ) %>%
  #   filter(term_year > year_of_latest_val_report_ & entry_year <= year_of_latest_val_report_) %>% # People enter before 2022 and terminate after 2022
  #   left_join(est_salary_of_latest_year_report, by=c("entry_year", "entry_age"))
  # 
  # age_groups <- others_$age_groups
  # yos_groups <- others_$yos_groups
  # 
  # mat <- matrix(0, nrow=10, ncol=7)
  # for(row in 1:10){
  #   for(col in 1:7){
  #     temp_df = est_salary_headcount_of_latest_year_report %>%
  #       filter(age_groups[2*row-1] <= current_age & current_age <= age_groups[2*row] &
  #                yos_groups[2*col-1] <= yos & yos <= yos_groups[2*col])
  #     avg_salary = mean(temp_df$salary)
  #     
  #     mat[row, col] = ifelse(is.na(avg_salary), 0, avg_salary)
  #   }
  # }
  # 
  # 
  # salary_matrix <- data.frame(headcount_matrix_[,'age'])
  # salary_matrix <- cbind(salary_matrix, as.data.frame(mat))
  # colnames(salary_matrix) <- colnames(headcount_matrix_)
  # 
  # salary_matrix_long <- salary_matrix %>%
  #   pivot_longer(cols=-1, names_to="yos", values_to="salary")
  # 
  # headcount_matrix_long <- headcount_matrix_ %>%
  #   pivot_longer(cols=-1, names_to="yos", values_to="count")
  # 
  # salary_headcount <- salary_matrix_long %>%
  #   left_join(headcount_matrix_long) %>%
  #   replace(is.na(.), 0) %>%
  #   mutate(
  #     yos = as.numeric(yos),
  #     current_year = year_of_latest_val_report_,
  #     entry_age = age - yos,
  #     entry_year = current_year - yos
  #   ) %>%
  #   filter(salary > 0, entry_age >= 18)
  # 
  # 
  # new_headcount_matrix <- data.matrix(headcount_matrix_ %>% replace(is.na(.), 0))
  # 
  # payroll_matrix <- mat * new_headcount_matrix[,2:8]
  # est_payroll <- rowSums(payroll_matrix) / 1000 # divide by 1000 to plot scatter plot at the end
  
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
      separation_type = get_sep_type(tier),
      discount_rate = ifelse(entry_year < year_of_new_tier_start_, curr_dist_rate, new_dist_rate),
      benefit_decision = ifelse(yos == 0, 'work',
                                ifelse(separation_type == 'retire', 'retire',
                                       ifelse(separation_type == 'vested', 'mix', 'refund'))),
      nominal_pension_wealth =
        ifelse(separation_type == 'retire', pv_benefit,
               ifelse(separation_type == 'non_vested', db_employee_balance,
                      retire_refund_ratio * pv_benefit + (1 - retire_refund_ratio_) * db_employee_balance)
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
  # indv_normal_cost <- opt_final_data_ %>%
  #   filter(yos == 0) %>%
  #   select(entry_year, entry_age, normal_cost_rate)
  
  
  #Calculate the aggregate normal cost
  # agg_normal_cost_tab <- indv_normal_cost %>%
  #   left_join(salary_headcount, by = c("entry_year", "entry_age")) %>%
  #   filter(!is.na(current_year)) %>%
  #   summarise(agg_normal_cost = sum(normal_cost_rate * salary * count) / sum(salary * count)) %>%
  #   pull()
  
  output <- list(
    ann_factor_tab = opt_annuity_factor_tab,
    annuity_factor_retire_tab = annuity_factor_retire,
    reduced_factor = opt_reduced_factor_tab,
    ben_tab = opt_benefit_projection,
    ben_retire_tab = pension_benefit_tab,
    final_tab = opt_final_data_
    # nc_tab = indv_normal_cost,
    # nc_agg = agg_normal_cost_tab
  )
  
  # end_normal_cost = Sys.time()
  
  
  return(output)
  
}



