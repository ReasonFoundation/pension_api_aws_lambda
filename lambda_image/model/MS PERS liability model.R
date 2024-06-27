# Liability model

# curr_dist_rate = curr_discount_rate_
# new_dist_rate = new_discount_rate_
# cola_for_tier_5 = cola_for_tier_5_
# retire_refund_ratio = retire_refund_ratio_
# stat_new_hire_EE_contr = stat_new_hire_EE_contr_
# db_new_ratio = db_new_ratio_


get_liability_data <- function(
    curr_dist_rate = curr_discount_rate_,
    new_dist_rate = new_discount_rate_,
    cola_for_tier_5 = cola_for_tier_5_,
    retire_refund_ratio = retire_refund_ratio_,
    stat_new_hire_EE_contr = stat_new_hire_EE_contr_,
    db_new_ratio = db_new_ratio_
    ){
  
  benefit_data <- get_benefit_data(
    curr_dist_rate = curr_dist_rate,
    new_dist_rate = new_dist_rate,
    cola_for_tier_5 = cola_for_tier_5,
    retire_refund_ratio = retire_refund_ratio,
    stat_new_hire_EE_contr = stat_new_hire_EE_contr
  )
  
  wf_projection <- get_wf_data(
    curr_dist_rate = curr_dist_rate,
    new_dist_rate = new_dist_rate,
    cola_for_tier_5 = cola_for_tier_5,
    retire_refund_ratio = retire_refund_ratio,
    stat_new_hire_EE_contr = stat_new_hire_EE_contr,
    benefit_data = benefit_data
  )
  
  dc_new_ratio <- 1 - db_new_ratio
  
  # start_active <- Sys.time()
  #Join wf active table with normal cost and salary table to calculate the overall payroll and normal costs each year
  wf_active_df_final <- wf_projection$wf_active_df %>%
    filter(year <= year_of_latest_val_report_ + model_period_) %>%
    mutate(entry_year = year - (age - entry_age)) %>%
    left_join(benefit_data$final_tab, by = c("entry_age", "age", "year", "entry_year")) %>%
    select(entry_age, age, year, entry_year, n.active, normal_cost_rate,
           salary, pv_future_benefit, pv_future_normal_cost, pv_future_salary)  %>%
    # distinct() %>%
    replace(is.na(.), 0) %>%
    mutate(
      n.active_db_legacy = ifelse(entry_year < year_of_new_tier_start_, n.active, 0),
      n.active_db_new = ifelse(entry_year < year_of_new_tier_start_, 0, n.active * db_new_ratio),
      n.active_dc_new = ifelse(entry_year < year_of_new_tier_start_, 0, n.active * dc_new_ratio),
      ) %>%
    group_by(year) %>%
    summarise(
      # payroll
      payroll_db_legacy_est = sum(salary * n.active_db_legacy),
      payroll_db_new_est = sum(salary * n.active_db_new),
      payroll_dc_new_est = sum(salary * n.active_dc_new),
      payroll_est = sum(salary * n.active),
      # normal cost
      nc_rate_db_legacy_est = ifelse(payroll_db_legacy_est == 0, 0, sum(normal_cost_rate * salary * n.active_db_legacy) / sum(salary * n.active_db_legacy)),
      nc_rate_db_new_est = ifelse(payroll_db_new_est == 0, 0, sum(normal_cost_rate * salary * n.active_db_new) / sum(salary * n.active_db_new)),
      # avg_normal_cost = mean(normal_cost_rate),
      # avg_salary = mean(salary),
      # Present value of future benefits
      PVFB_db_legacy_est = sum(pv_future_benefit * n.active_db_legacy),
      PVFB_db_new_est = sum(pv_future_benefit * n.active_db_new),
      # Present value of future normal cost
      PVFNC_db_legacy_est = sum(pv_future_normal_cost * n.active_db_legacy),
      PVFNC_db_new_est = sum(pv_future_normal_cost * n.active_db_new),
      # Number of active members
      # sum_pv_future_salary = sum(pv_future_salary * n.active),
      sum_active = sum(n.active),
      sum_active_db_legacy = sum(n.active_db_legacy),
      sum_active_db_new = sum(n.active_db_new),
      sum_active_dc_new = sum(n.active_dc_new)
    ) %>%
    ungroup() %>%
    mutate(
      payroll_db_est = payroll_db_legacy_est + payroll_db_new_est,
      nc_rate_est = if_else(payroll_db_est == 0, 0, (nc_rate_db_legacy_est * payroll_db_legacy_est + nc_rate_db_new_est * payroll_db_new_est) / payroll_db_est),
      AAL_active_db_legacy_est = PVFB_db_legacy_est - PVFNC_db_legacy_est,
      AAL_active_db_new_est = PVFB_db_new_est - PVFNC_db_new_est
    ) %>%
    replace(is.na(.), 0)
    
  # end_active <- Sys.time()
  
  # start_term <- Sys.time()
  # Calculate PVFB for term vested members
  # The calculation of the PVFB for term vested CB members is more complicated than that for DB members
  # because actual investment returns and hence actual ICR can affect the cash balance after termination.
  wf_term_df_final <- wf_projection$wf_term_df %>%
    filter(year <= year_of_latest_val_report_ + model_period_) %>%
    filter(n.term > 0) %>%
    mutate(entry_year = year - (age - entry_age)) %>%
    #join FinalData to get DBWealth (the present value of benefits at termination)
    left_join(benefit_data$final_tab, by = c("entry_age", "term_year"="year", "entry_year")) %>%
    select(entry_age, age=age.x, year, term_year, entry_year, retire_age, n.term, pv_benefit) %>%
    #join BenefitsTable to get the surv_DR at current age
    left_join(benefit_data$ben_tab %>% select(-pv_benefit), by = c("entry_age", "age"="retire_age", "year", "term_year", "entry_year")) %>%
    select(entry_age, age, year, term_year, entry_year, retire_age, n.term, pv_benefit, disc_survival_rate) %>%
    #rename to clarify variables' meanings
    rename(
      disc_survival_rate_current = disc_survival_rate
    ) %>%
    left_join(benefit_data$ann_factor_tab, by = c("entry_age", "retire_age" = "age", "term_year", "entry_year")) %>%
    select(entry_age, age, year=year.x, term_year, entry_year, retire_age, n.term, pv_benefit,
           disc_survival_rate_current, disc_survival_rate, annuity_factor) %>%
    rename(
      disc_survival_rate_retire = disc_survival_rate
    ) %>%
    mutate(
      PVFB_db_term = pv_benefit / disc_survival_rate_current,
      n.term_db_legacy = ifelse(entry_year < year_of_new_tier_start_, n.term, 0),
      n.term_db_new = ifelse(entry_year < year_of_new_tier_start_, 0, n.term * db_new_ratio)
    ) %>%
    group_by(year) %>%
    summarise(
      AAL_term_db_legacy_est = sum(PVFB_db_term * n.term_db_legacy),
      AAL_term_db_new_est = sum(PVFB_db_term * n.term_db_new)
    ) %>%
    ungroup() %>%
    replace(is.na(.), 0)

    # end_term <- Sys.time()
  
  
  # start_refund <- Sys.time()
  #Join wf refund table with benefit table to calculate the overall refunds each year
  wf_refund_df_final <- wf_projection$wf_refund_df %>%
    filter(year <= year_of_latest_val_report_ + model_period_) %>%
    filter(n.refund > 0) %>%
    mutate(entry_year = year - (age - entry_age)) %>%
    left_join(benefit_data$ben_tab, by=c("entry_age", "age"="retire_age", "year", "term_year", "entry_year")) %>%
    select(entry_age, age, year, term_year, entry_year, n.refund, db_employee_balance) %>%
    mutate(
      n.refund_db_legacy = ifelse(entry_year < year_of_new_tier_start_, n.refund, 0),
      n.refund_db_new = ifelse(entry_year < year_of_new_tier_start_, 0, n.refund * db_new_ratio)
      ) %>%
    group_by(year) %>%
    summarise(
      refund_db_legacy_est = sum(db_employee_balance * n.refund_db_legacy),
      refund_db_new_est = sum(db_employee_balance * n.refund_db_new)
    ) %>%
    ungroup() %>%
    replace(is.na(.), 0)
    

  # end_refund <- Sys.time()
  
  # start_retire_final <- Sys.time()
  #Join wf retire table with benefit table to calculate the overall retirement benefits each year
  wf_retire_df_final <- wf_projection$wf_retire_df %>%
    filter(year <= year_of_latest_val_report_ + model_period_) %>%
    mutate(entry_year = year - (age - entry_age)) %>%
    left_join(benefit_data$ben_tab, by=c("entry_age", "entry_year", "term_year", "retire_year"="year")) %>%
    select(entry_age, age, year, term_year, retire_year, entry_year, n.retire, pension_benefit) %>%
    left_join(benefit_data$ann_factor_tab, by=c("entry_year", "entry_age", "term_year", "year")) %>%
    select(entry_age, age=age.x, year, term_year, retire_year, entry_year, n.retire, pension_benefit, annuity_factor, cola) %>%
    rename(base_benefit = pension_benefit) %>%
    mutate(
      db_benefit_final = base_benefit * (1 + cola)^(year - retire_year),

      n.retire_db_legacy = ifelse(entry_year < year_of_new_tier_start_, n.retire, 0),
      n.retire_db_new = ifelse(entry_year < year_of_new_tier_start_, 0, n.retire * db_new_ratio),

      PVFB_db_retire = db_benefit_final * (annuity_factor - 1),
    ) %>%
    group_by(year) %>%
    summarise(
      retire_ben_db_legacy_est = sum(db_benefit_final * n.retire_db_legacy),
      retire_ben_db_new_est = sum(db_benefit_final * n.retire_db_new),

      AAL_retire_db_legacy_est = sum(PVFB_db_retire * n.retire_db_legacy),
      AAL_retire_db_new_est = sum(PVFB_db_retire * n.retire_db_new)
    ) %>%
    ungroup() %>%
    replace(is.na(.), 0)

  # end_retire_final <- Sys.time()
  
  
  
  # start_misc <- Sys.time()
  #Project benefit payments for current retirees
  retire_current_int <- retiree_dist_ %>%
    select(age, n.retire_pct, total_benefit_pct) %>%
    mutate(
      n.retire_current = n.retire_pct * retiree_pop_current_,
      total_ben_current = total_benefit_pct * ben_payment_current_,
      avg_ben_current = total_ben_current / n.retire_current,
      year = year_of_latest_val_report_
    ) %>%
    replace(is.na(.), 0)

  wf_retire_current <- benefit_data$annuity_factor_retire_tab %>%
    filter(year <= year_of_latest_val_report_ + model_period_) %>%
    left_join(retire_current_int, by = c("age", "year")) %>%
    select(base_age:annuity_factor_retire, n.retire_current, avg_ben_current, total_ben_current) %>%
    mutate(cola = cola_for_tier_1234_) %>%
    group_by(base_age) %>%
    mutate(
      n.retire_current = recur_grow(n.retire_current, -mort_rate),
      avg_ben_current = recur_grow2(avg_ben_current, cola),
      total_ben_current = n.retire_current * avg_ben_current,
      PVFB_retire_current = avg_ben_current * (annuity_factor_retire - 1)
    ) %>%
    filter(!is.na(n.retire_current)) %>%
    ungroup() %>%
    replace(is.na(.), 0)

  wf_retire_current_final <- wf_retire_current %>%
    group_by(year) %>%
    summarise(
      retire_ben_current = sum(total_ben_current),
      AAL_retire_current = sum(n.retire_current * PVFB_retire_current)
    ) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  #Project benefit payments for current term vested members
  #New method: get the benefit payments by amortizing the aal_term such that the benefit payments will follow a bell curve path
  year <- year_of_latest_val_report_:(year_of_latest_val_report_ + model_period_)
  
  mid_amo_period_term <- amo_period_term_ / 2
  amo_spread_term <- amo_period_term_ / 5
  amo_seq_term <- seq(1, amo_period_term_, 1)
  amo_weights_term <- dnorm(amo_seq_term, mean = mid_amo_period_term, sd = amo_spread_term)
  ann_ratio_term <- amo_weights_term / amo_weights_term[1]
  #Note that we use the original "curr_discount_rate_" in calculating the benefit payments so that any discount rate adjustment can work properly (i.e. the discount rate adjustment only affects the PVFB, not the benefit payments)
  #Calculate the first benefit payment
  first_retire_ben_term <- PVFB_term_current / get_npv(curr_discount_rate_, ann_ratio_term)
  #Calculate the rest of the benefit payments
  retire_ben_term_est <- c(0,first_retire_ben_term * ann_ratio_term)
  #Evaluate the AALs along the benefit payment vector using the user-defined discount rate
  AAL_term_current_est <- roll_npv(rate = curr_dist_rate, cashflows = retire_ben_term_est)
  
  #Align the benefit payment schedule with the model's projection period
  length(retire_ben_term_est) <- length(year)
  retire_ben_term_est[is.na(retire_ben_term_est)] <- 0
  
  length(AAL_term_current_est) <- length(year)
  AAL_term_current_est[is.na(AAL_term_current_est)] <- 0
  
  #Put benefit payments produced above and aal projection together
  wf_term_current <- data.frame(year, retire_ben_term_est, AAL_term_current_est)

  
  # end_misc <- Sys.time()
  
  
  # start_funding_df <- Sys.time()
  #Mini funding model
  funding_df <- wf_active_df_final %>%
    left_join(wf_term_df_final) %>%
    left_join(wf_refund_df_final) %>%
    left_join(wf_retire_df_final) %>%
    left_join(wf_retire_current_final) %>%
    left_join(wf_term_current) %>%
    replace(is.na(.), 0) %>%
    mutate(
      AAL_legacy_est = AAL_active_db_legacy_est + AAL_term_db_legacy_est + AAL_retire_db_legacy_est +
                          AAL_retire_current + AAL_term_current_est,
      AAL_new_est = AAL_active_db_new_est + AAL_term_db_new_est + AAL_retire_db_new_est,
      AAL_est = AAL_legacy_est + AAL_new_est,

      total_ben_refund_legacy_est = refund_db_legacy_est + retire_ben_db_legacy_est + retire_ben_current + retire_ben_term_est,
      total_ben_refund_new_est = refund_db_new_est + retire_ben_db_new_est,
      total_ben_refund_est = total_ben_refund_legacy_est + total_ben_refund_new_est
    )

  # #Project AAL using the roll forward method
  funding_df$AAL_legacy_roll <- 0
  funding_df$AAL_new_roll <- 0
  funding_df$AAL_roll <- 0

  funding_df$liability_gain_loss_legacy_est <- 0
  funding_df$liability_gain_loss_new_est <- 0
  funding_df$liability_gain_loss_est <- 0


  for (i in 1:length(funding_df$AAL_roll)) {
    if (i == 1) {
      funding_df$liability_gain_loss_legacy_est[i] <- 0
      funding_df$liability_gain_loss_new_est[i] <- 0

      funding_df$AAL_legacy_roll[i] <- funding_df$AAL_legacy_est[i]
      funding_df$AAL_new_roll[i] <- funding_df$AAL_new_est[i]
    } else {
      funding_df$liability_gain_loss_legacy_est[i] <- round(funding_df$AAL_legacy_est[i] - (funding_df$AAL_legacy_est[i-1] * (1 + curr_dist_rate) +
                                                                                              funding_df$payroll_db_legacy_est[i-1] * funding_df$nc_rate_db_legacy_est[i-1] -
                                                                                              funding_df$total_ben_refund_legacy_est[i]),
                                                            digits = 1)
      funding_df$liability_gain_loss_new_est[i] <- round(funding_df$AAL_new_est[i] - (funding_df$AAL_new_est[i-1] * (1 + new_dist_rate) +
                                                                                        funding_df$payroll_db_new_est[i-1] * funding_df$nc_rate_db_new_est[i-1] -
                                                                                        funding_df$total_ben_refund_new_est[i]),
                                                         digits = 1)

      funding_df$AAL_legacy_roll[i] <- funding_df$AAL_legacy_roll[i-1] * (1 + curr_dist_rate) + funding_df$payroll_db_legacy_est[i-1] * funding_df$nc_rate_db_legacy_est[i-1] -
                                                                  funding_df$total_ben_refund_legacy_est[i] + funding_df$liability_gain_loss_legacy_est[i]
      funding_df$AAL_new_roll[i] <- funding_df$AAL_new_roll[i-1] * (1 + new_dist_rate) + funding_df$payroll_db_new_est[i-1] * funding_df$nc_rate_db_new_est[i-1] -
                                                                  funding_df$total_ben_refund_new_est[i] + funding_df$liability_gain_loss_new_est[i]

    }
  }

  funding_df$liability_gain_loss_est <- funding_df$liability_gain_loss_legacy_est + funding_df$liability_gain_loss_new_est
  funding_df$AAL_roll <- funding_df$AAL_legacy_roll + funding_df$AAL_new_roll
  
  # end_funding_df <- Sys.time()
  
  # output <- funding_df %>% select(year, refund_legacy_est, refund_new_est, 
  #                                 AAL_retire_legacy_est, AAL_retire_new_est)

  # PVFB_legacy_est = sum(pv_future_benefit * n.active_legacy),
  # PVFB_new_est = sum(pv_future_benefit * n.active_new),
  # # Present value of future normal cost
  # PVFNC_legacy_est = sum(pv_future_normal_cost * n.active_legacy),
  # PVFNC_new_est = sum(pv_future_normal_cost * n.active_new),
  
  # benefit_data$final_tab, by = c("entry_age", "age", "year", "entry_year")) %>%
  # select(entry_age, age, year, entry_year, n.active, normal_cost_rate,
  #        salary, pv_future_benefit, pv_future_normal_cost, pv_future_salary
  
  # output <- benefit_data$final_tab %>% select(year, age, entry_year, entry_age, pv_future_benefit, pv_future_salary,
                                          # normal_cost_rate, salary)
  # output <- wf_active_df_final %>% select(year, nc_rate_est, nc_rate_legacy_est, nc_rate_new_est,
  #                                         payroll_legacy_est, payroll_new_est, payroll_est, avg_normal_cost, avg_salary)
  return(funding_df)
  
  # saveRDS(funding_df, "./projected_liabilities.rds")

}

# liability_data <- get_liability_data()

# memoised_get_liability_data <- memoise(get_liability_data)
