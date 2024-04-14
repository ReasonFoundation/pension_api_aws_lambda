# Liability model

get_liability_data <- function(
    curr_dist_rate = curr_discount_rate_,
    new_dist_rate = new_discount_rate_,
    cola = cola_,
    retire_refund_ratio = retire_refund_ratio_
    ){

  # start_ben_proj <- Sys.time()
  benefit_data <- get_benefit_data(
    curr_dist_rate = curr_discount_rate_,
    new_dist_rate = new_discount_rate_,
    cola = cola_,
    retire_refund_ratio = retire_refund_ratio_
  )
  
  
  # benefit_data <- readRDS("./projected_benefit.rds")
  
  # start_active <- Sys.time()
  #Join wf active table with normal cost and salary table to calculate the overall payroll and normal costs each year
  wf_active_df_final <- wf_projection$wf_active_df %>%
    filter(year <= year_start + model_period) %>%
    mutate(entry_year = year - (age - entry_age)) %>%
    dplyr::left_join(benefit_data$final_tab, by = c("entry_age", "age", "year", "entry_year"), na_matches="na") %>%
    select(entry_age, age, year, entry_year, n.active, normal_cost_rate,
           salary, pv_future_benefit, pv_future_normal_cost, pv_future_salary)  %>%
    # distinct() %>%
    replace(is.na(.), 0) %>%
    mutate(
      n.active_legacy = ifelse(entry_year <= year_start, n.active, 0),
      n.active_new = ifelse(entry_year <= year_start, 0, n.active)
      ) %>%
    group_by(year) %>%
    summarise(
      # payroll
      payroll_legacy_est = sum(salary * n.active_legacy),
      payroll_new_est = sum(salary * n.active_new),
      payroll_est = sum(salary * n.active),
      # normal cost
      nc_rate_legacy_est = ifelse(payroll_legacy_est == 0, 0, sum(normal_cost_rate * salary * n.active_legacy) / sum(salary * n.active_legacy)),
      nc_rate_new_est = ifelse(payroll_new_est == 0, 0, sum(normal_cost_rate * salary * n.active_new) / sum(salary * n.active_new)),
      # avg_normal_cost = mean(normal_cost_rate),
      # avg_salary = mean(salary),
      # Present value of future benefits
      PVFB_legacy_est = sum(pv_future_benefit * n.active_legacy),
      PVFB_new_est = sum(pv_future_benefit * n.active_new),
      # Present value of future normal cost
      PVFNC_legacy_est = sum(pv_future_normal_cost * n.active_legacy),
      PVFNC_new_est = sum(pv_future_normal_cost * n.active_new),
      # Number of active members
      # sum_pv_future_salary = sum(pv_future_salary * n.active),
      sum_active = sum(n.active)
    ) %>%
    ungroup() %>%
    mutate(
      nc_rate_est = (nc_rate_legacy_est * payroll_legacy_est + nc_rate_new_est * payroll_new_est) / payroll_est,
      AAL_active_legacy_est = PVFB_legacy_est - PVFNC_legacy_est,
      AAL_active_new_est = PVFB_new_est - PVFNC_new_est
    ) %>%
    replace(is.na(.), 0)
    
  # end_active <- Sys.time()
  
  # start_term <- Sys.time()
  # Calculate PVFB for term vested members
  # The calculation of the PVFB for term vested CB members is more complicated than that for DB members
  # because actual investment returns and hence actual ICR can affect the cash balance after termination.
  wf_term_df_final <- wf_projection$wf_term_df %>%
    filter(year <= year_start + model_period) %>%
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
      PVFB_tern = pv_benefit / disc_survival_rate_current,
      n.term_legacy = ifelse(entry_year <= year_start, n.term, 0),
      n.term_new = ifelse(entry_year <= year_start, 0, n.term)
    ) %>%
    group_by(year) %>%
    summarise(
      AAL_term_legacy_est = sum(PVFB_tern * n.term_legacy),
      AAL_term_new_est = sum(PVFB_tern * n.term_new)
    ) %>%
    ungroup() %>%
    replace(is.na(.), 0)

    # end_term <- Sys.time()
  
  
  # start_refund <- Sys.time()
  #Join wf refund table with benefit table to calculate the overall refunds each year
  wf_refund_df_final <- wf_projection$wf_refund_df %>%
    filter(year <= year_start + model_period) %>%
    filter(n.refund > 0) %>%
    mutate(entry_year = year - (age - entry_age)) %>%
    left_join(benefit_data$ben_tab, by=c("entry_age", "age"="retire_age", "year", "term_year", "entry_year")) %>%
    select(entry_age, age, year, term_year, entry_year, n.refund, db_employee_balance) %>%
    mutate(
      n.refund_legacy = ifelse(entry_year <= year_start, n.refund, 0),
      n.refund_new = ifelse(entry_year <= year_start, 0, n.refund)
      ) %>%
    group_by(year) %>%
    summarise(
      refund_legacy_est = sum(db_employee_balance * n.refund_legacy),
      refund_new_est = sum(db_employee_balance * n.refund_new)
    ) %>%
    ungroup() %>%
    replace(is.na(.), 0)
    

  # end_refund <- Sys.time()
  
  # start_retire_final <- Sys.time()
  #Join wf retire table with benefit table to calculate the overall retirement benefits each year
  wf_retire_df_final <- wf_projection$wf_retire_df %>%
    filter(year <= year_start + model_period) %>%
    mutate(entry_year = year - (age - entry_age)) %>%
    left_join(benefit_data$ben_tab, by=c("entry_age", "entry_year", "term_year", "retire_year"="year")) %>%
    select(entry_age, age, year, term_year, retire_year, entry_year, n.retire, pension_benefit) %>%
    left_join(benefit_data$ann_factor_tab, by=c("entry_year", "entry_age", "term_year", "year")) %>%
    select(entry_age, age=age.x, year, term_year, retire_year, entry_year, n.retire, pension_benefit, annuity_factor) %>%
    rename(base_benefit = pension_benefit) %>%
    mutate(
      db_benefit_final = base_benefit * (1 + cola)^(year - retire_year),

      n.retire_legacy = ifelse(entry_year <= year_start, n.retire, 0),
      n.retire_new = ifelse(entry_year <= year_start, 0, n.retire),

      PVFB_retire = db_benefit_final * (annuity_factor - 1),
    ) %>%
    group_by(year) %>%
    summarise(
      retire_ben_legacy_est = sum(db_benefit_final * n.retire_legacy),
      retire_ben_new_est = sum(db_benefit_final * n.retire_new),

      AAL_retire_legacy_est = sum(PVFB_retire * n.retire_legacy),
      AAL_retire_new_est = sum(PVFB_retire * n.retire_new)
    ) %>%
    ungroup() %>%
    replace(is.na(.), 0)

  # end_retire_final <- Sys.time()
  
  
  
  # start_misc <- Sys.time()
  #Project benefit payments for current retirees
  retire_current_int <- retiree_dist %>%
    select(age, n.retire_pct, total_benefit_pct) %>%
    mutate(
      n.retire_current = n.retire_pct * retiree_pop_current_,
      total_ben_current = total_benefit_pct * ben_payment_current_,
      avg_ben_current = total_ben_current / n.retire_current,
      year = year_start
    ) %>%
    replace(is.na(.), 0)

  wf_retire_current <- benefit_data$annuity_factor_retire_tab %>%
    filter(year <= year_start + model_period) %>%
    left_join(retire_current_int, by = c("age", "year")) %>%
    select(base_age:annuity_factor_retire, n.retire_current, avg_ben_current, total_ben_current) %>%
    mutate(cola = 0.03) %>%
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
  retire_ben_term <- get_pmt(r = curr_dist_rate, nper = amo_period_term, pv = PVFB_term_current, g = payroll_growth_)

  year <- year_start:(year_start + model_period)
  amo_years_term <- (year_start + 1):(year_start + amo_period_term)
  retire_ben_term_est <- double(length = length(year))
  retire_ben_term_est[which(year %in% amo_years_term)] <- recur_grow3(retire_ben_term, payroll_growth_, amo_period_term)

  wf_term_current <- data.frame(year, retire_ben_term_est) %>%
    mutate(AAL_term_current_est = roll_pv(rate = curr_dist_rate, g = payroll_growth_,
                                          nper = amo_period_term, pmt_vec = retire_ben_term_est)) %>%
    replace(is.na(.), 0)
  
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
      AAL_legacy_est = AAL_active_legacy_est + AAL_term_legacy_est + AAL_retire_legacy_est +
                          AAL_retire_current + AAL_term_current_est,
      AAL_new_est = AAL_active_new_est + AAL_term_new_est + AAL_retire_new_est,
      AAL_est = AAL_legacy_est + AAL_new_est,

      total_ben_refund_legacy_est = refund_legacy_est + retire_ben_legacy_est + retire_ben_current + retire_ben_term_est,
      total_ben_refund_new_est = refund_new_est + retire_ben_new_est,
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
                                                                                              funding_df$payroll_legacy_est[i-1] * funding_df$nc_rate_legacy_est[i-1] -
                                                                                              funding_df$total_ben_refund_legacy_est[i]),
                                                            digits = 1)
      funding_df$liability_gain_loss_new_est[i] <- round(funding_df$AAL_new_est[i] - (funding_df$AAL_new_est[i-1] * (1 + new_dist_rate) +
                                                                                        funding_df$payroll_new_est[i-1] * funding_df$nc_rate_new_est[i-1] -
                                                                                        funding_df$total_ben_refund_new_est[i]),
                                                         digits = 1)

      funding_df$AAL_legacy_roll[i] <- funding_df$AAL_legacy_roll[i-1] * (1 + curr_dist_rate) + funding_df$payroll_legacy_est[i-1] * funding_df$nc_rate_legacy_est[i-1] -
                                                                  funding_df$total_ben_refund_legacy_est[i] + funding_df$liability_gain_loss_legacy_est[i]
      funding_df$AAL_new_roll[i] <- funding_df$AAL_new_roll[i-1] * (1 + new_dist_rate) + funding_df$payroll_new_est[i-1] * funding_df$nc_rate_new_est[i-1] -
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

# memoised_get_liability_data <- memoise(get_liability_data)
