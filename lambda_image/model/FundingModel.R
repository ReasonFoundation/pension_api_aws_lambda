get_funding_data <- function(
    curr_dist_rate = curr_discount_rate_,
    new_dist_rate = new_discount_rate_,
    cola = cola_,
    retire_refund_ratio = retire_refund_ratio_,
    funding_policy = funding_policy_,
    analysis_type = analysis_type_,
    roa_scenario = roa_scenario_
    ){
  
    liability_data <- get_liability_data(
      curr_dist_rate = curr_dist_rate,
      new_dist_rate = new_dist_rate,
      cola = cola,
      retire_refund_ratio = retire_refund_ratio)

    # liability_data <- get_liability_data(
    #   curr_discount_rate,
    #   new_discount_rate,
    #   cola,
    #   retire_refund_ratio)
  
    # liability_data <- readRDS("./projected_liabilities.rds")
    
    # Assign inputs
    # for(i in 1:nrow(numeric_inputs)){
    #   if(!is.na(numeric_inputs[i,2])){
    #     assign(as.character(numeric_inputs[i,2]), as.double(numeric_inputs[i,3]))
    #   }
    # }
    # 
    # for(i in 1:nrow(character_inputs)){
    #   if(!is.na(character_inputs[i,2])){
    #     assign(as.character(character_inputs[i,2]), as.character(character_inputs[i,3]))
    #   }
    # }
    
    # Create an empty matrix for the projection years
    empty_matrix <- matrix(0, (end_proj_year - start_proj_year + 1), 1)
    for(j in 1:length(colnames(historical_data))){
      temp_matrix <- rbind(as.matrix(historical_data[,j]), empty_matrix)
      assign(as.character(colnames(historical_data)[j]), temp_matrix)
    }
    
    # Assign values for projection years
    # FYE <- as.matrix(start_proj_year:end_proj_year)
    # colnames(FYE) <- "FYE"
    
    # Get start index
    period <- end_proj_year - start_hist_year + 1
    proj_length <- end_proj_year - start_proj_year + 1
    first_proj_yr_idx <- start_proj_year - start_hist_year + 1
    critical_year_indx <- period - (end_proj_year - critical_year)
    
    
    # Current vs New hire percentage. 9% new hire every year
    curr_hire_pct <- c(1, liability_data$payroll_legacy_est/liability_data$payroll_est)
    new_hire_pct <- 1 - curr_hire_pct
    
    # Discount rate
    curr_discount_rate[first_proj_yr_idx:length(curr_discount_rate)] <- rep(curr_dist_rate, 30)
    new_discount_rate[first_proj_yr_idx:length(new_discount_rate)] <- rep(new_dist_rate, 30)
    
    # Investment return scenario
    # scenario_index <- which(colnames(scenario_data) == as.character(ScenType))
    if(analysis_type == 'Stochastic'){
      roa_mva[first_proj_yr_idx:length(roa_mva)] <- rnorm(30, SimReturnAssumed, SimVolatility)
    } else if (analysis_type == 'Deterministic') {
      roa_mva[first_proj_yr_idx:length(roa_mva)] <- c(scenario_data[first_proj_yr_idx:length(roa_mva), roa_scenario][[roa_scenario]])
    }
    
    # roa_mva[first_proj_yr_idx:period] <- c(scenario_data[first_proj_yr_idx:period, roa_scenario][[roa_scenario]])
    
    # Payroll
    total_hire_payroll <- c(total_hire_payroll[1:(first_proj_yr_idx-2)], 
                       cumprod(c(total_hire_payroll[first_proj_yr_idx-1], rep(1 + payroll_growth_, 30))))
    curr_hire_payroll <- total_hire_payroll * curr_hire_pct
    new_hire_payroll <- total_hire_payroll * new_hire_pct
    
    payroll_matrix <- cbind(curr_hire_payroll, new_hire_payroll)
    
    # Benefit
    curr_hire_benefit <- c(curr_hire_benefit[1:(first_proj_yr_idx-3)], 
                                (liability_data$total_ben_refund_legacy_est - liability_data$refund_legacy_est) * (-10^(-6)))
    
    new_hire_benefit <- c(new_hire_benefit[1:(first_proj_yr_idx-3)],
                                (liability_data$total_ben_refund_new_est - liability_data$refund_new_est) * (-10^(-6)))
    
    total_hire_benefit <- curr_hire_benefit + new_hire_benefit
    
    # Refund
    curr_hire_refund <- c(curr_hire_refund[1:(first_proj_yr_idx-1)],
                                 liability_data$refund_legacy_est[2:31]*(-10^(-6)))
    new_hire_refund <- c(new_hire_refund[1:(first_proj_yr_idx-1)],
                                liability_data$refund_new_est[2:31]*(-10^(-6)))
    total_hire_refund <- curr_hire_refund + new_hire_refund
    
    # Admin exp
    curr_hire_admin_exp <- (-1) * curr_hire_payroll * Admin_Exp_Pct
    new_hire_admin_exp <- (-1) * new_hire_payroll * Admin_Exp_Pct
    total_hire_admin_exp <- curr_hire_admin_exp + new_hire_admin_exp
    
    
    # Normal cost
    curr_hire_nc_rate <- c(curr_hire_nc_rate_, liability_data$nc_rate_legacy_est * nc_calibration_ratio)
    new_hire_nc_rate <- c(0, liability_data$nc_rate_new_est * nc_calibration_ratio)
    
    curr_hire_nc_contr <- curr_hire_payroll * curr_hire_nc_rate
    new_hire_nc_contr <- new_hire_payroll * new_hire_nc_rate
    
    total_hire_nc_contr <- curr_hire_nc_contr + new_hire_nc_contr
  
    
    # AAL
    for(i in first_proj_yr_idx:length(curr_hire_aal)){
      curr_hire_aal[i] <- curr_hire_aal[i-1] * (1 + curr_discount_rate[i]) + 
                                  curr_hire_nc_contr[i] * (1 + curr_discount_rate[i])^(1/2) + 
                                  (curr_hire_benefit[i] + curr_hire_refund[i])*(1 + curr_discount_rate[i])^(1/2) +
                                  liability_data$liability_gain_loss_legacy_est[i-1]*(10^(-6))
    }
    
    for(i in first_proj_yr_idx:length(new_hire_aal)){
      new_hire_aal[i] <- new_hire_aal[i-1] * (1 + curr_discount_rate[i]) + 
                                new_hire_nc_contr[i] * (1 + curr_discount_rate[i])^(1/2) + 
                                (new_hire_benefit[i] + new_hire_refund[i])*(1 + curr_discount_rate[i])^(1/2) +
                                liability_data$liability_gain_loss_new_est[i-1]*(10^(-6))
    }
    
    total_hire_aal <- curr_hire_aal + new_hire_aal
    
    
    # Contribution rate
    # total_hire_gross_nc_rate <- total_hire_new_dr_nc_contr[first_proj_yr_idx:period] / total_hire_payroll[first_proj_yr_idx:period]
    curr_hire_gross_nc_rate[first_proj_yr_idx:length(curr_hire_gross_nc_rate)] <- curr_hire_nc_rate[first_proj_yr_idx:length(curr_hire_nc_rate)]
    new_hire_gross_nc_rate[first_proj_yr_idx:length(new_hire_gross_nc_rate)] <- new_hire_nc_rate[first_proj_yr_idx:length(new_hire_nc_rate)]
    
    # EE normal cost rate after excluding refund rate
    curr_hire_EE_nc_rate <- rep(stat_curr_hire_EE_contr_ - refund_rate_, length(curr_hire_gross_nc_rate))
    new_hire_EE_nc_rate <- rep(stat_new_hire_EE_contr_ - refund_rate_, length(new_hire_gross_nc_rate))
    
    # ER normal cost rate
    curr_hire_ER_nc_rate <- (curr_hire_gross_nc_rate - curr_hire_EE_nc_rate)
    new_hire_ER_nc_rate <- (new_hire_gross_nc_rate - new_hire_EE_nc_rate)
    
    # ER amo rate
    curr_hire_ER_amo_rate <- stat_ER_contr_rate_ - curr_hire_ER_nc_rate - Admin_Exp_Pct
    new_hire_ER_amo_rate <- stat_ER_contr_rate_ - new_hire_ER_nc_rate - Admin_Exp_Pct
    
    # EE normal cost contribution amount
    curr_hire_EE_nc_contr[first_proj_yr_idx:length(curr_hire_EE_nc_contr)] <- curr_hire_EE_nc_rate[first_proj_yr_idx:length(curr_hire_EE_nc_rate)] * curr_hire_payroll[first_proj_yr_idx:length(curr_hire_payroll)]
    new_hire_EE_nc_contr[first_proj_yr_idx:length(new_hire_EE_nc_contr)] <- new_hire_EE_nc_rate[first_proj_yr_idx:length(new_hire_EE_nc_rate)] * new_hire_payroll[first_proj_yr_idx:length(new_hire_payroll)]
    
    # Required ER normal cost contribution amount
    # curr_hire_ER_nc_contr[first_proj_yr_idx:period] <- req_er_contr_pct*(curr_hire_ER_nc_rate[first_proj_yr_idx:period] + Admin_Exp_Pct) * curr_hire_payroll[first_proj_yr_idx:period]
    # new_hire_ER_nc_contr[first_proj_yr_idx:period] <- req_er_contr_pct*(new_hire_ER_nc_rate[first_proj_yr_idx:period] + Admin_Exp_Pct) * new_hire_payroll[first_proj_yr_idx:period]

    curr_hire_ER_nc_contr[first_proj_yr_idx:length(curr_hire_ER_nc_contr)] <- (req_er_contr_pct*curr_hire_ER_nc_rate[first_proj_yr_idx:length(curr_hire_ER_nc_rate)] + Admin_Exp_Pct) * curr_hire_payroll[first_proj_yr_idx:length(curr_hire_payroll)]
    new_hire_ER_nc_contr[first_proj_yr_idx:length(new_hire_ER_nc_contr)] <- (req_er_contr_pct*new_hire_ER_nc_rate[first_proj_yr_idx:length(new_hire_ER_nc_rate)] + Admin_Exp_Pct) * new_hire_payroll[first_proj_yr_idx:length(new_hire_payroll)]
    
    
    # Cash flow excluding amortization
    cash_flow_excl_amo <- total_hire_benefit + total_hire_refund + total_hire_admin_exp + 
                          curr_hire_EE_nc_contr + new_hire_EE_nc_contr + 
                          curr_hire_ER_nc_contr + new_hire_ER_nc_contr
    
    
    # Current hires' remaining UAL
    curr_hire_remaining_ual_matrix <- matrix(0, nrow=period-1, ncol=30)
    curr_hire_remaining_ual_matrix[1,1:5] <- ual_data$remaining_ual # input remaining ual in 2018-2022
    
    # Current hires' amortization payment
    curr_hire_amo_payment_matrix <- matrix(0, nrow=period-2, ncol=30)
    
    # Current hires' amortization period. Its first line is the remaining amo period of legency debts
    curr_hire_amo_period_matrix <- rbind(c(25,24,23,22,26, rep(0,25)),
                               matrix(0,nrow=31,ncol=30))
    
    for(i in 2:nrow(curr_hire_amo_period_matrix)){
      for(j in 1:ncol(curr_hire_amo_period_matrix)){
        if(j == 1){
          curr_hire_amo_period_matrix[i,j] = 25
        } else {
          curr_hire_amo_period_matrix[i,j] = max(0, curr_hire_amo_period_matrix[i-1,j-1]-1)
        }
      }
    }
    
    # New hires' remaining UAL
    new_hire_remaining_ual_matrix <- matrix(0, nrow=period-1, ncol=30)
    
    # New hires' amortization payment
    new_hire_amo_payment_matrix <- matrix(0, nrow=period-2, ncol=30)
    
    # New hires' amortization period
    new_hire_amo_period_matrix <- matrix(0,nrow=32,ncol=30)
    
    for(i in 2:nrow(new_hire_amo_period_matrix)){
      for(j in 1:ncol(new_hire_amo_period_matrix)){
        if(j == 1){
          new_hire_amo_period_matrix[i,j] = 25
        } else {
          new_hire_amo_period_matrix[i,j] = max(0, new_hire_amo_period_matrix[i-1,j-1]-1)
        }
      }
    }
    
    total_hire_amo_payment_matrix <- matrix(0, nrow=period-2, ncol=30)
    
    
    aal_2047 <- tail(total_hire_aal[1:critical_year_indx], 1)

    end_col <- 5 # index in amo_payment_matrix, amo_period_matrix
    all_funded_ratio_2047 <- rep(0, 25)
    adc <- rep(0,25)
    adc_fcr_ratio <- rep(0,25)
    
  for(i in first_proj_yr_idx:period){
    # for(i in first_proj_yr_idx:first_proj_yr_idx){
      
    Year[i] <- Year[i-1] + 1
    
    # get amortization rates
    curr_hire_ER_amo_rate[i] <- curr_hire_ER_contr_rate[i-2] - curr_hire_ER_nc_rate[i] - Admin_Exp_Pct
    new_hire_ER_amo_rate[i] <- new_hire_ER_contr_rate[i-2] - new_hire_ER_nc_rate[i] - Admin_Exp_Pct
    
    # get amortization contribution amount
    curr_hire_ER_amo_contr[i] <- curr_hire_payroll[i] * curr_hire_ER_amo_rate[i]*req_er_contr_pct
    new_hire_ER_amo_contr[i] <- new_hire_payroll[i] * new_hire_ER_amo_rate[i]*req_er_contr_pct
    
    # get ER total contribution and cumulative ER total contribution
    total_hire_ER_contr[i] <- (curr_hire_ER_nc_contr[i] + new_hire_ER_nc_contr[i]) +
                              (curr_hire_ER_amo_contr[i] + new_hire_ER_amo_contr[i]) +
                              Admin_Exp_Pct * total_hire_payroll[i]
    
    total_hire_ER_contr_real[i] <- total_hire_ER_contr[i] / (1 + assum_inflation_)^(Year[i] - year_start)
    
    if (Year[i] == year_start + 1) {
      cum_total_hire_ER_contr_real[i] <- total_hire_ER_contr_real[i]
    } else {
      cum_total_hire_ER_contr_real[i] <- cum_total_hire_ER_contr_real[i-1] + total_hire_ER_contr_real[i]
    }

    # get net cash flow
    total_hire_net_cash_flow[i] <- total_hire_benefit[i] + total_hire_refund[i] + total_hire_admin_exp[i] + 
                                  (curr_hire_EE_nc_contr[i] + new_hire_EE_nc_contr[i]) +
                                  (curr_hire_ER_nc_contr[i] + new_hire_ER_nc_contr[i]) +
                                  (curr_hire_ER_amo_contr[i] + new_hire_ER_amo_contr[i])
    
    curr_hire_net_cash_flow[i] <- curr_hire_benefit[i] + curr_hire_refund[i] + curr_hire_admin_exp[i] + 
                                  curr_hire_EE_nc_contr[i] + curr_hire_ER_nc_contr[i] + curr_hire_ER_amo_contr[i]
    
    new_hire_net_cash_flow[i] <- new_hire_benefit[i] + new_hire_refund[i] + new_hire_admin_exp[i] +
                                  new_hire_EE_nc_contr[i] + new_hire_ER_nc_contr[i] + new_hire_ER_amo_contr[i]
    
    # get actual mva
    total_hire_actual_mva[i] <- total_hire_actual_mva[i-1]*(1 + roa_mva[i]) +
                                (total_hire_benefit[i] + total_hire_refund[i] + total_hire_admin_exp[i] +
                                 curr_hire_EE_nc_contr[i] + new_hire_EE_nc_contr[i] +
                                 curr_hire_ER_nc_contr[i] + new_hire_ER_nc_contr[i] +
                                 curr_hire_ER_amo_contr[i] + new_hire_ER_amo_contr[i])*(1 + roa_mva[i])^(1/2)
    
    curr_hire_actual_mva[i] <- curr_hire_actual_mva[i-1]*(1 + roa_mva[i]) + curr_hire_net_cash_flow[i]*(1 + roa_mva[i])^(1/2)
    new_hire_actual_mva[i] <- new_hire_actual_mva[i-1]*(1 + roa_mva[i]) + new_hire_net_cash_flow[i]*(1 + roa_mva[i])^(1/2)
    
    # get expected investment income
    total_hire_exp_invest_inco[i] <- total_hire_actual_mva[i-1]*new_discount_rate[i] + total_hire_net_cash_flow[i]*new_discount_rate[i]/2 
    curr_hire_exp_invest_inco[i] <- curr_hire_actual_mva[i-1]*new_discount_rate[i] + curr_hire_net_cash_flow[i]*new_discount_rate[i]/2 
    new_hire_exp_invest_inco[i] <- new_hire_actual_mva[i-1]*new_discount_rate[i] + new_hire_net_cash_flow[i]*new_discount_rate[i]/2 
    
    # get expected mva
    total_hire_exp_mva[i] <- total_hire_actual_mva[i-1] + total_hire_net_cash_flow[i] + total_hire_exp_invest_inco[i]
    curr_hire_exp_mva[i] <- curr_hire_actual_mva[i-1] + curr_hire_net_cash_flow[i] + curr_hire_exp_invest_inco[i]
    new_hire_exp_mva[i] <- new_hire_actual_mva[i-1] + new_hire_net_cash_flow[i] + new_hire_exp_invest_inco[i]
    
    # get gain and loss
    total_hire_gain_loss[i] <- total_hire_actual_mva[i] - total_hire_exp_mva[i]
    curr_hire_gain_loss[i] <- curr_hire_actual_mva[i] - curr_hire_exp_mva[i]
    new_hire_gain_loss[i] <- new_hire_actual_mva[i] - new_hire_exp_mva[i]
    
    # get deferred gain and loss
    total_hire_recog_gain_loss_1st_yr[i] <- total_hire_gain_loss[i]*0.2
    total_hire_recog_gain_loss_2nd_yr[i] <- total_hire_recog_gain_loss_1st_yr[i-1]
    total_hire_recog_gain_loss_3rd_yr[i] <- total_hire_recog_gain_loss_2nd_yr[i-1]
    total_hire_recog_gain_loss_4th_yr[i] <- total_hire_recog_gain_loss_3rd_yr[i-1]
    total_hire_recog_gain_loss_5th_yr[i] <- total_hire_recog_gain_loss_4th_yr[i-1]
    
    total_hire_total_recog_gain_loss[i] <- total_hire_recog_gain_loss_1st_yr[i] + 
                                            total_hire_recog_gain_loss_2nd_yr[i] +
                                            total_hire_recog_gain_loss_3rd_yr[i] +
                                            total_hire_recog_gain_loss_4th_yr[i] +
                                            total_hire_recog_gain_loss_5th_yr[i]
    
    curr_hire_recog_gain_loss_1st_yr[i] <- curr_hire_gain_loss[i]*0.2
    curr_hire_recog_gain_loss_2nd_yr[i] <- curr_hire_recog_gain_loss_1st_yr[i-1]
    curr_hire_recog_gain_loss_3rd_yr[i] <- curr_hire_recog_gain_loss_2nd_yr[i-1]
    curr_hire_recog_gain_loss_4th_yr[i] <- curr_hire_recog_gain_loss_3rd_yr[i-1]
    curr_hire_recog_gain_loss_5th_yr[i] <- curr_hire_recog_gain_loss_4th_yr[i-1]
    
    curr_hire_total_recog_gain_loss[i] <- curr_hire_recog_gain_loss_1st_yr[i] + 
                                          curr_hire_recog_gain_loss_2nd_yr[i] +
                                          curr_hire_recog_gain_loss_3rd_yr[i] +
                                          curr_hire_recog_gain_loss_4th_yr[i] +
                                          curr_hire_recog_gain_loss_5th_yr[i]
    
    new_hire_recog_gain_loss_1st_yr[i] <- new_hire_gain_loss[i]*0.2
    new_hire_recog_gain_loss_2nd_yr[i] <- new_hire_recog_gain_loss_1st_yr[i-1]
    new_hire_recog_gain_loss_3rd_yr[i] <- new_hire_recog_gain_loss_2nd_yr[i-1]
    new_hire_recog_gain_loss_4th_yr[i] <- new_hire_recog_gain_loss_3rd_yr[i-1]
    new_hire_recog_gain_loss_5th_yr[i] <- new_hire_recog_gain_loss_4th_yr[i-1]
    
    new_hire_total_recog_gain_loss[i] <- new_hire_recog_gain_loss_1st_yr[i] + 
                                          new_hire_recog_gain_loss_2nd_yr[i] +
                                          new_hire_recog_gain_loss_3rd_yr[i] +
                                          new_hire_recog_gain_loss_4th_yr[i] +
                                          new_hire_recog_gain_loss_5th_yr[i]
    
    # get ava
    total_hire_ava[i] <- min(total_hire_actual_mva[i] * 1.2,
                            max(total_hire_ava[i-1] + total_hire_net_cash_flow[i] + 
                                  total_hire_exp_invest_inco[i] + total_hire_total_recog_gain_loss[i],
                        total_hire_actual_mva[i] * 0.8))
    
    curr_hire_ava[i] <- min(curr_hire_actual_mva[i] * 1.2,
                             max(curr_hire_ava[i-1] + curr_hire_net_cash_flow[i] + 
                                   curr_hire_exp_invest_inco[i] + curr_hire_total_recog_gain_loss[i],
                                 curr_hire_actual_mva[i] * 0.8))
    
    new_hire_ava[i] <- min(new_hire_actual_mva[i] * 1.2,
                             max(new_hire_ava[i-1] + new_hire_net_cash_flow[i] + 
                                   new_hire_exp_invest_inco[i] + new_hire_total_recog_gain_loss[i],
                                 new_hire_actual_mva[i] * 0.8))
    
    # get ual_ava
    total_hire_ual_ava[i] <- total_hire_aal[i] - total_hire_ava[i]
    curr_hire_ual_ava[i] <- curr_hire_aal[i] - curr_hire_ava[i]
    new_hire_ual_ava[i] <- new_hire_aal[i] - new_hire_ava[i]
    
    # get ual_mva
    total_hire_ual_mva[i] <- total_hire_aal[i] - total_hire_actual_mva[i]
    curr_hire_ual_mva[i] <- curr_hire_aal[i] - curr_hire_actual_mva[i]
    new_hire_ual_mva[i] <- new_hire_aal[i] - new_hire_actual_mva[i]
    
    funded_ratio_mva[i] <- total_hire_actual_mva[i] / total_hire_aal[i]
    funded_ratio_ava[i] <- total_hire_ava[i] / total_hire_aal[i]
    
    # all-in employer cost analysis
    total_hire_ual_mva_real[i] <- total_hire_ual_mva[i] / (1 + assum_inflation_)^(Year[i] - year_start)
    total_hire_ER_all_in_cost_real[i] <- cum_total_hire_ER_contr_real[i] + total_hire_ual_mva_real[i]
      
    # amortization
    row_indx <- i - (start_proj_year - start_hist_year) # Adjust row index to fit three amo matrices
    for(j in 1:end_col){
      if(curr_hire_amo_period_matrix[row_indx,j] != 0){
        curr_hire_amo_payment_matrix[row_indx, j] <- get_pmt(
                                                  r=new_discount_rate[i],
                                                  g = AmoBaseInc_CurrentHire,
                                                  nper = curr_hire_amo_period_matrix[row_indx,j],
                                                  pv = curr_hire_remaining_ual_matrix[row_indx,j],
                                                  t=0.5)
      }
    }
    
    for(j in 1:end_col){
      if(new_hire_amo_period_matrix[row_indx,j] != 0){
        new_hire_amo_payment_matrix[row_indx, j] <- get_pmt(
                                                  r = new_discount_rate[i],
                                                  g = AmoBaseInc_CurrentHire,
                                                  nper = new_hire_amo_period_matrix[row_indx,j],
                                                  pv = new_hire_remaining_ual_matrix[row_indx,j],
                                                  t=0.5)
      }
    }
    
    end_col <- min(end_col + 1, 30)
    
    for(k in 2:end_col){
      curr_hire_remaining_ual_matrix[row_indx+1,k] <- curr_hire_remaining_ual_matrix[row_indx,k-1]*(1 + new_discount_rate[i]) - 
                                            curr_hire_amo_payment_matrix[row_indx,k-1]*(1 + new_discount_rate[i])^(1/2)
      
      new_hire_remaining_ual_matrix[row_indx+1,k] <- new_hire_remaining_ual_matrix[row_indx,k-1]*(1 + new_discount_rate[i]) - 
                                            new_hire_amo_payment_matrix[row_indx,k-1]*(1 + new_discount_rate[i])^(1/2)
    }
    
    curr_hire_remaining_ual_matrix[row_indx+1,1] <- curr_hire_ual_ava[i] - sum(curr_hire_remaining_ual_matrix[(row_indx+1),2:end_col])
    new_hire_remaining_ual_matrix[row_indx+1,1] <- new_hire_ual_ava[i] - sum(new_hire_remaining_ual_matrix[(row_indx+1),2:end_col])
    
    # get amortization contribution rate
    # curr_hire_actuarial_amo_rate <- get_actuarial_amo_rate(annual_amo_payment = sum(curr_hire_amo_payment_matrix[row_indx,]),
    #                                              annual_payroll = curr_hire_payroll[i])

    # new_hire_actuarial_amo_rate <- get_actuarial_amo_rate(annual_amo_payment = sum(new_hire_amo_payment_matrix[row_indx,]),
    #                                                        annual_payroll = new_hire_payroll[i])
    
    total_hire_actuarial_amo_rate <- get_actuarial_amo_rate(
                  annual_amo_payment = sum(curr_hire_amo_payment_matrix[row_indx,]) + sum(new_hire_amo_payment_matrix[row_indx,]),
                  annual_payroll = total_hire_payroll[i])
    
    # get ADC rates
    curr_hire_adc_rate <- get_adc_rate(
                                  # actuarial_amo_rate = curr_hire_actuarial_amo_rate,
                                  actuarial_amo_rate = total_hire_actuarial_amo_rate,
                                  ER_nc_rate = curr_hire_ER_nc_rate[i], admin_rate = Admin_Exp_Pct)
    
    new_hire_adc_rate <- get_adc_rate(
                                  # actuarial_amo_rate = new_hire_actuarial_amo_rate,
                                  actuarial_amo_rate = total_hire_actuarial_amo_rate,
                                  ER_nc_rate = new_hire_ER_nc_rate[i], admin_rate = Admin_Exp_Pct)
      
    if(funding_policy == 'StatusQuo'){
      # determine the total contribution rate next year
      next_year <- i + start_hist_year - 1
      
      if(next_year > 2047){
        
        curr_hire_ER_contr_rate[i] <- curr_hire_ER_contr_rate[i-1]
        new_hire_ER_contr_rate[i] <- new_hire_ER_contr_rate[i-1]
        total_hire_ER_contr_rate[i] <- (curr_hire_ER_contr_rate[i] * curr_hire_payroll[i] + 
                                          new_hire_ER_contr_rate[i] * new_hire_payroll[i]) / total_hire_payroll[i]
        
      } else {
        
        # get funded ratio in 2047
        payrolls <- payroll_matrix[i:critical_year_indx,]
        cash_flow_excl_amo_ <- cash_flow_excl_amo[i:critical_year_indx]
        
        proj_amo_contr <- get_amo_contr(amo_rates = c(curr_hire_ER_amo_rate[i-1], new_hire_ER_amo_rate[i-1]),
                                        payrolls = payrolls)
        
        proj_mva <- get_mva(prev_mva = total_hire_actual_mva[i], dr = new_discount_rate[i],
                            cash_flow_excl_amo = cash_flow_excl_amo_, amo_contr = proj_amo_contr)
        
        proj_mva_2047 <- tail(proj_mva, 1)
        
        funded_ratio_2047 <- get_funded_ratio_2047(mva_2047 = proj_mva_2047, aal_2047 = aal_2047)
        
        all_funded_ratio_2047[i-2] <- funded_ratio_2047
        
        # get adc/fcr ratio
        curr_hire_adc_fcr_ratio <- get_adc_fcr_ratio(adc = curr_hire_adc_rate,
                                                     fcr = curr_hire_ER_contr_rate[i-1])
        
        new_hire_adc_fcr_ratio <- get_adc_fcr_ratio(adc = new_hire_adc_rate,
                                                    fcr = new_hire_ER_contr_rate[i-1])
        
        adc[i-2] <- curr_hire_adc_rate
        adc_fcr_ratio[i-2] <- curr_hire_adc_fcr_ratio
        
        # check conditions
        curr_hire_metric_status <- is_red_status(fr_2047 = funded_ratio_2047,
                                                 adc_fcr_ratio = curr_hire_adc_fcr_ratio)
        
        new_hire_metric_status <- is_red_status(fr_2047 = funded_ratio_2047,
                                                adc_fcr_ratio = new_hire_adc_fcr_ratio)
        
        if(curr_hire_metric_status){
          # curr_hire_ER_contr_rate[i] <- curr_hire_actuarial_amo_rate + curr_hire_ER_nc_rate[i] + Admin_Exp_Pct
          curr_hire_ER_contr_rate[i] <- total_hire_actuarial_amo_rate + curr_hire_ER_nc_rate[i] + Admin_Exp_Pct
        } else{
          curr_hire_ER_contr_rate[i] <- curr_hire_ER_contr_rate[i-1]
        }
        
        if(new_hire_metric_status){
          # new_hire_ER_contr_rate[i] <- new_hire_actuarial_amo_rate + new_hire_ER_nc_rate[i] + Admin_Exp_Pct
          new_hire_ER_contr_rate[i] <- total_hire_actuarial_amo_rate + new_hire_ER_nc_rate[i] + Admin_Exp_Pct
        } else{
          new_hire_ER_contr_rate[i] <- new_hire_ER_contr_rate[i-1]
        }
        
        total_hire_ER_contr_rate[i] <- (curr_hire_ER_contr_rate[i] * curr_hire_payroll[i] + 
                                          new_hire_ER_contr_rate[i] * new_hire_payroll[i]) / total_hire_payroll[i]
        
      }
    } else if (funding_policy == 'ADC') {
      # curr_hire_ER_contr_rate[i] <- curr_hire_actuarial_amo_rate + curr_hire_ER_nc_rate[i] + Admin_Exp_Pct
      # new_hire_ER_contr_rate[i] <- new_hire_actuarial_amo_rate + new_hire_ER_nc_rate[i] + Admin_Exp_Pct
      
      curr_hire_ER_contr_rate[i] <- total_hire_actuarial_amo_rate + curr_hire_ER_nc_rate[i] + Admin_Exp_Pct
      new_hire_ER_contr_rate[i] <- total_hire_actuarial_amo_rate + new_hire_ER_nc_rate[i] + Admin_Exp_Pct
      
      total_hire_ER_contr_rate[i] <- (curr_hire_ER_contr_rate[i] * curr_hire_payroll[i] + 
                                        new_hire_ER_contr_rate[i] * new_hire_payroll[i]) / total_hire_payroll[i]
      
    }
  }
    output <- data.frame(sapply(colnames(historical_data), get, envir = sys.frame(sys.parent(0))))

    # target_col <- c("Year", "funded_ratio_mva", "total_hire_ER_contr_rate",
    #                 "total_hire_ER_contr", "total_hire_ual_mva",
    #                 "total_hire_ER_all_in_cost_real")
    # 
    # target_output <- output %>% select(all_of(target_col))
    # export_output <- liability_data %>% select(nc_rate_legacy_est, nc_rate_new_est)
    return(output)
}

# memoised_get_funding_data <- memoise(get_funding_data)

