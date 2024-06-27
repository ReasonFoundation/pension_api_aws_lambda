

# curr_dist_rate = curr_discount_rate_
# new_dist_rate = new_discount_rate_
# cola_for_tier_5 = cola_for_tier_5_
# retire_refund_ratio = retire_refund_ratio_
# funding_policy = funding_policy_
# analysis_type = analysis_type_
# roa_scenario = roa_scenario_
# stat_new_hire_EE_contr = stat_new_hire_EE_contr_
# is_fixed_ER_contr_rate = is_fixed_ER_contr_rate_
# fixed_ER_contr_rate_option = fixed_ER_contr_rate_option_
# db_new_ratio = db_new_ratio_
# er_dc_contr_rate = er_dc_contr_rate_
# amo_period_curr_hire_new_layer = amo_period_curr_hire_new_layer_
# amo_period_new_hire = amo_period_new_hire_
# amo_method_curr_hire_curr_layer = amo_method_curr_hire_curr_layer_
# amo_method_curr_hire_new_layer = amo_method_curr_hire_new_layer_
# amo_method_new_hire = amo_method_new_hire_
# ee_amo_cost_share_ratio = ee_amo_cost_share_ratio_
# one_time_er_sup = one_time_er_sup_
# er_sup_start_year = er_sup_start_year_

get_funding_data <- function(
    curr_dist_rate = curr_discount_rate_,
    new_dist_rate = new_discount_rate_,
    cola_for_tier_5 = cola_for_tier_5_,
    retire_refund_ratio = retire_refund_ratio_,
    funding_policy = funding_policy_,
    analysis_type = analysis_type_,
    roa_scenario = roa_scenario_,
    stat_new_hire_EE_contr = stat_new_hire_EE_contr_,
    is_fixed_ER_contr_rate = is_fixed_ER_contr_rate_,
    fixed_ER_contr_rate_option = fixed_ER_contr_rate_option_,
    db_new_ratio = db_new_ratio_,
    er_dc_contr_rate = er_dc_contr_rate_,
    amo_period_curr_hire_new_layer = amo_period_curr_hire_new_layer_,
    amo_period_new_hire = amo_period_new_hire_, 
    amo_method_curr_hire_curr_layer = amo_method_curr_hire_curr_layer_,
    amo_method_curr_hire_new_layer = amo_method_curr_hire_new_layer_,
    amo_method_new_hire = amo_method_new_hire_,
    ee_amo_cost_share_ratio = ee_amo_cost_share_ratio_,
    one_time_er_sup = one_time_er_sup_,
    er_sup_start_year = er_sup_start_year_
    ){
  
    liability_data <- get_liability_data(
      curr_dist_rate = curr_dist_rate,
      new_dist_rate = new_dist_rate,
      cola_for_tier_5 = cola_for_tier_5,
      retire_refund_ratio = retire_refund_ratio,
      stat_new_hire_EE_contr = stat_new_hire_EE_contr,
      db_new_ratio = db_new_ratio
      )
    
    # Create an empty matrix for the projection years
    empty_matrix <- matrix(0, (end_proj_year_ - start_proj_year_ + 1), 1)
    for(j in 1:length(colnames(historical_data_))){
      temp_matrix <- rbind(as.matrix(historical_data_[,j]), empty_matrix)
      assign(as.character(colnames(historical_data_)[j]), temp_matrix)
    }
    
    # Assign values for projection years
    # FYE <- as.matrix(start_proj_year_:end_proj_year)
    # colnames(FYE) <- "FYE"
    
    # Get start index
    period <- end_proj_year_ - start_hist_year_ + 1
    proj_length <- end_proj_year_ - start_proj_year_ + 1
    first_proj_yr_idx <- start_proj_year_ - start_hist_year_ + 1
    critical_year_indx <- period - (end_proj_year_ - critical_year_)
    
    
    # Current vs New hire percentage. 9% new hire every year
    db_curr_hire_pct <- c(1, liability_data$payroll_db_legacy_est/liability_data$payroll_est)
    db_new_hire_pct <- c(0, liability_data$payroll_db_new_est/liability_data$payroll_est)
    dc_new_hire_pct <- c(0, liability_data$payroll_dc_new_est/liability_data$payroll_est)
    
    # Discount rate
    curr_discount_rate[first_proj_yr_idx:length(curr_discount_rate)] <- rep(curr_dist_rate, proj_length)
    new_discount_rate[first_proj_yr_idx:length(new_discount_rate)] <- rep(new_dist_rate, proj_length)
    
    # Investment return scenario
    # scenario_index <- which(colnames(scenario_data) == as.character(ScenType))
    
    ##Set return values "assumption" scenario
    scenario_data_$assumption[scenario_data_$year > year_of_latest_val_report_] <- curr_dist_rate
    
    ##Set return scenario according to user input
    if(analysis_type == 'Stochastic'){
      roa_mva[first_proj_yr_idx:length(roa_mva)] <- rnorm(proj_length, SimReturnAssumed, SimVolatility)
    } else if (analysis_type == 'Deterministic') {
      roa_mva[first_proj_yr_idx:length(roa_mva)] <- c(scenario_data_[first_proj_yr_idx:length(roa_mva), roa_scenario][[roa_scenario]])
    }
    
    # roa_mva[first_proj_yr_idx:period] <- c(scenario_data[first_proj_yr_idx:period, roa_scenario][[roa_scenario]])
    
    # Payroll
    total_hire_payroll <- c(total_hire_payroll[1:(first_proj_yr_idx-2)], 
                       cumprod(c(total_hire_payroll[first_proj_yr_idx-1], rep(1 + payroll_growth_, proj_length))))
    db_curr_hire_payroll <- total_hire_payroll * db_curr_hire_pct
    db_new_hire_payroll <- total_hire_payroll * db_new_hire_pct
    dc_new_hire_payroll <- total_hire_payroll * dc_new_hire_pct
    
    payroll_matrix <- cbind(db_curr_hire_payroll, db_new_hire_payroll)
    
    # Benefit
    curr_hire_benefit <- c(curr_hire_benefit[1:(first_proj_yr_idx-3)], 
                                (liability_data$total_ben_refund_legacy_est - liability_data$refund_db_legacy_est) * (-10^(-6)))
    
    curr_hire_benefit[first_proj_yr_idx:period] <- curr_hire_benefit[first_proj_yr_idx:period] * benefit_calibration_ratio 
    
    new_hire_benefit <- c(new_hire_benefit[1:(first_proj_yr_idx-3)],
                                (liability_data$total_ben_refund_new_est - liability_data$refund_db_new_est) * (-10^(-6)))
    
    new_hire_benefit[first_proj_yr_idx:period] <- new_hire_benefit[first_proj_yr_idx:period] * benefit_calibration_ratio
    
    total_hire_benefit <- curr_hire_benefit + new_hire_benefit
    
    # Refund
    curr_hire_refund <- c(curr_hire_refund[1:(first_proj_yr_idx-1)],
                                 liability_data$refund_db_legacy_est[2:31]*(-10^(-6)))
    new_hire_refund <- c(new_hire_refund[1:(first_proj_yr_idx-1)],
                                liability_data$refund_db_new_est[2:31]*(-10^(-6)))
    total_hire_refund <- curr_hire_refund + new_hire_refund
    
    # Admin exp
    curr_hire_admin_exp[first_proj_yr_idx:period] <- (-1) * db_curr_hire_payroll[first_proj_yr_idx:period] * Admin_Exp_Pct
    new_hire_admin_exp[first_proj_yr_idx:period] <- (-1) * db_new_hire_payroll[first_proj_yr_idx:period] * Admin_Exp_Pct
    total_hire_admin_exp <- curr_hire_admin_exp + new_hire_admin_exp
    
    
    # Normal cost
    db_curr_hire_nc_rate <- c(curr_hire_nc_rate_, liability_data$nc_rate_db_legacy_est * nc_calibration_ratio)
    db_new_hire_nc_rate <- c(0, liability_data$nc_rate_db_new_est * nc_calibration_ratio)
    
    curr_hire_nc_contr <- db_curr_hire_payroll * db_curr_hire_nc_rate
    new_hire_nc_contr <- db_new_hire_payroll * db_new_hire_nc_rate
    
    total_hire_nc_contr <- curr_hire_nc_contr + new_hire_nc_contr
  
    curr_hire_aal[first_proj_yr_idx - 1] <- liability_data$AAL_legacy_est[1]*(10^(-6))
    new_hire_aal[first_proj_yr_idx - 1] <- liability_data$AAL_new_est[1]*(10^(-6))
    
    # AAL
    for(i in first_proj_yr_idx:length(curr_hire_aal)){
      curr_hire_aal[i] <- curr_hire_aal[i-1] * (1 + curr_discount_rate[i]) + 
                                  curr_hire_nc_contr[i] * (1 + curr_discount_rate[i])^(1/2) + 
                                  (curr_hire_benefit[i] + curr_hire_refund[i])*(1 + curr_discount_rate[i])^(1/2) +
                                  liability_data$liability_gain_loss_legacy_est[i-1]*(10^(-6))
    }
    
    for(i in first_proj_yr_idx:length(new_hire_aal)){
      new_hire_aal[i] <- new_hire_aal[i-1] * (1 + new_discount_rate[i]) + 
                                new_hire_nc_contr[i] * (1 + new_discount_rate[i])^(1/2) + 
                                (new_hire_benefit[i] + new_hire_refund[i])*(1 + new_discount_rate[i])^(1/2) +
                                liability_data$liability_gain_loss_new_est[i-1]*(10^(-6))
    }
    
    total_hire_aal <- curr_hire_aal + new_hire_aal
    
    total_hire_ual_ava[first_proj_yr_idx - 1] <- total_hire_aal[first_proj_yr_idx - 1] - total_hire_ava[first_proj_yr_idx - 1]
    curr_hire_ual_ava[first_proj_yr_idx - 1] <- curr_hire_aal[first_proj_yr_idx - 1] - curr_hire_ava[first_proj_yr_idx - 1]
    new_hire_ual_ava[first_proj_yr_idx - 1] <- new_hire_aal[first_proj_yr_idx - 1] - new_hire_ava[first_proj_yr_idx - 1]
    
    
    total_hire_ual_mva[first_proj_yr_idx - 1] <- total_hire_aal[first_proj_yr_idx - 1] - total_hire_actual_mva[first_proj_yr_idx - 1]
    curr_hire_ual_mva[first_proj_yr_idx - 1] <- curr_hire_aal[first_proj_yr_idx - 1] - curr_hire_actual_mva[first_proj_yr_idx - 1]
    new_hire_ual_mva[first_proj_yr_idx - 1] <- new_hire_aal[first_proj_yr_idx - 1] - new_hire_actual_mva[first_proj_yr_idx - 1]
    
    
    # Contribution rate
    # total_hire_gross_nc_rate <- total_hire_new_dr_nc_contr[first_proj_yr_idx:period] / total_hire_payroll[first_proj_yr_idx:period]
    curr_hire_gross_nc_rate[first_proj_yr_idx:length(curr_hire_gross_nc_rate)] <- db_curr_hire_nc_rate[first_proj_yr_idx:length(db_curr_hire_nc_rate)]
    new_hire_gross_nc_rate[first_proj_yr_idx:length(new_hire_gross_nc_rate)] <- db_new_hire_nc_rate[first_proj_yr_idx:length(db_new_hire_nc_rate)]
    
    # EE normal cost rate after excluding refund rate
    curr_hire_EE_nc_rate <- rep(stat_curr_hire_EE_contr_ - refund_rate_, length(curr_hire_gross_nc_rate))
    # 2024's new members still pay 9% contribution rate. A new contribution rate starts applying to tier 5 in 2025
    new_hire_EE_nc_rate <- c(rep(stat_curr_hire_EE_contr_ - refund_rate_, first_proj_yr_idx),
                             rep(stat_new_hire_EE_contr - refund_rate_, length(new_hire_gross_nc_rate) - first_proj_yr_idx))
      
    
    # ER normal cost rate
    curr_hire_ER_nc_rate <- (curr_hire_gross_nc_rate - curr_hire_EE_nc_rate)
    new_hire_ER_nc_rate <- (new_hire_gross_nc_rate - new_hire_EE_nc_rate)
    
    # Initial ER amo rate
    curr_hire_ER_amo_rate <- stat_ER_contr_rate_ - curr_hire_ER_nc_rate - Admin_Exp_Pct
    new_hire_ER_amo_rate <- stat_ER_contr_rate_ - new_hire_ER_nc_rate - Admin_Exp_Pct
    
    # EE normal cost contribution amount
    curr_hire_EE_nc_contr[first_proj_yr_idx:length(curr_hire_EE_nc_contr)] <- curr_hire_EE_nc_rate[first_proj_yr_idx:length(curr_hire_EE_nc_rate)] * db_curr_hire_payroll[first_proj_yr_idx:length(db_curr_hire_payroll)]
    new_hire_EE_nc_contr[first_proj_yr_idx:length(new_hire_EE_nc_contr)] <- new_hire_EE_nc_rate[first_proj_yr_idx:length(new_hire_EE_nc_rate)] * db_new_hire_payroll[first_proj_yr_idx:length(db_new_hire_payroll)]
    
    # Required ER normal cost contribution amount
    # curr_hire_ER_nc_contr[first_proj_yr_idx:period] <- req_er_contr_pct*(curr_hire_ER_nc_rate[first_proj_yr_idx:period] + Admin_Exp_Pct) * curr_hire_payroll[first_proj_yr_idx:period]
    # new_hire_ER_nc_contr[first_proj_yr_idx:period] <- req_er_contr_pct*(new_hire_ER_nc_rate[first_proj_yr_idx:period] + Admin_Exp_Pct) * new_hire_payroll[first_proj_yr_idx:period]

    curr_hire_ER_nc_contr[first_proj_yr_idx:length(curr_hire_ER_nc_contr)] <- (req_er_contr_pct*curr_hire_ER_nc_rate[first_proj_yr_idx:length(curr_hire_ER_nc_rate)] + Admin_Exp_Pct) * db_curr_hire_payroll[first_proj_yr_idx:length(db_curr_hire_payroll)]
    new_hire_ER_nc_contr[first_proj_yr_idx:length(new_hire_ER_nc_contr)] <- (req_er_contr_pct*new_hire_ER_nc_rate[first_proj_yr_idx:length(new_hire_ER_nc_rate)] + Admin_Exp_Pct) * db_new_hire_payroll[first_proj_yr_idx:length(db_new_hire_payroll)]
    
    
    # Cash flow excluding amortization
    cash_flow_excl_amo <- total_hire_benefit + total_hire_refund + total_hire_admin_exp + 
                          curr_hire_EE_nc_contr + new_hire_EE_nc_contr + 
                          curr_hire_ER_nc_contr + new_hire_ER_nc_contr
    
    
    # Employers' DC contribution
    total_er_dc_contr <- er_dc_contr_rate * dc_new_hire_payroll
    
    # Current hires' remaining UAL
    curr_hire_remaining_ual_matrix <- matrix(0, nrow = period-1, ncol = 30)
    curr_hire_remaining_ual_matrix[1,1:length(ual_data_$remaining_ual)] <- ual_data_$remaining_ual # input remaining ual in 2018-2022
    
    # Current hires' amortization payment
    curr_hire_amo_payment_matrix <- matrix(0, nrow = period-1, ncol = 30)
    
    # Current hires' amortization period. Its first line is the remaining amo period of legency debts
    curr_hire_amo_period_matrix <- rbind(c(25,24,23,22,21,25, rep(0,24)),
                               matrix(0, nrow = period-2, ncol = 30))
    
    for(i in 2:nrow(curr_hire_amo_period_matrix)){
      for(j in 1:ncol(curr_hire_amo_period_matrix)){
        if(j == 1){
          curr_hire_amo_period_matrix[i,j] = amo_period_curr_hire_new_layer
        } else {
          curr_hire_amo_period_matrix[i,j] = max(0, curr_hire_amo_period_matrix[i-1,j-1]-1)
        }
      }
    }
    
    # New hires' remaining UAL
    new_hire_remaining_ual_matrix <- matrix(0, nrow = period-1, ncol = 30)
    
    # New hires' amortization payment
    new_hire_amo_payment_matrix <- matrix(0, nrow = period-1, ncol = 30)
    
    # New hires' amortization period
    new_hire_amo_period_matrix <- matrix(0, nrow = period-1, ncol = 30)
    
    for(i in 2:nrow(new_hire_amo_period_matrix)){
      for(j in 1:ncol(new_hire_amo_period_matrix)){
        if(j == 1){
          new_hire_amo_period_matrix[i,j] = amo_period_new_hire
        } else {
          new_hire_amo_period_matrix[i,j] = max(0, new_hire_amo_period_matrix[i-1,j-1]-1)
        }
      }
    }
    
    total_hire_amo_payment_matrix <- matrix(0, nrow=period-2, ncol=30)
    
    
    aal_2047 <- tail(total_hire_aal[1:critical_year_indx], 1)

    end_col <- length(ual_data_$remaining_ual) # index of the last non-zero column in amo_payment_matrix, amo_period_matrix
    all_funded_ratio_2047 <- rep(0, 25)
    adc <- rep(0,25)
    adc_fcr_ratio <- rep(0,25)
    
    #Initialize the first amo payments for current hires
    for(j in 1:end_col){
      
      if (amo_method_curr_hire_curr_layer == "level percent") {
        amo_g_curr_hire <- AmoBaseInc_CurrentHire
      } else {
        amo_g_curr_hire <- 0
      }
      
      curr_hire_amo_payment_matrix[1, j] <- get_pmt(r = curr_discount_rate[first_proj_yr_idx - 1],
                                                    g = amo_g_curr_hire,
                                                    nper = curr_hire_amo_period_matrix[1,j],
                                                    pv = curr_hire_remaining_ual_matrix[1,j],
                                                    t = 0.5)
    }
    

    ######### Main funding loop #########
    for(i in first_proj_yr_idx:period){
      # for(i in first_proj_yr_idx:first_proj_yr_idx){
      # i <- first_proj_yr_idx
      
      row_indx <- i - (start_proj_year_ - start_hist_year_) + 1 # Adjust row index to fit amo matrices
      
      Year[i] <- Year[i-1] + 1
      
      # get actuarial amortization contribution rates
      if(funded_ratio_mva[i-1] < 1){
        
        if (db_curr_hire_payroll[i] == 0) {
          curr_hire_actuarial_amo_rate[i] <- 0
        } else {
          curr_hire_actuarial_amo_rate[i] <- get_actuarial_amo_rate(annual_amo_payment = sum(curr_hire_amo_payment_matrix[row_indx-1,]),
                                                                    annual_payroll = db_curr_hire_payroll[i])
        }
        
        if (db_new_hire_payroll[i] == 0) {
          new_hire_actuarial_amo_rate[i] <- 0
        } else {
          new_hire_actuarial_amo_rate[i] <- get_actuarial_amo_rate(annual_amo_payment = sum(new_hire_amo_payment_matrix[row_indx-1,]),
                                                                   annual_payroll = db_new_hire_payroll[i])
        }
          
        
        total_hire_actuarial_amo_rate <- get_actuarial_amo_rate(
          annual_amo_payment = sum(curr_hire_amo_payment_matrix[row_indx-1,]) + sum(new_hire_amo_payment_matrix[row_indx-1,]),
          annual_payroll = db_curr_hire_payroll[i] + db_new_hire_payroll[i])
        
      } else {
        curr_hire_actuarial_amo_rate[i] <- 0
        
        new_hire_actuarial_amo_rate[i] <- 0
        
        total_hire_actuarial_amo_rate <- 0
      }
      
      
      # get ADC rates: use the total amo rate for both current and new hires to facilitate the adc ratio later
      curr_hire_adc_rate <- get_adc_rate(
        # actuarial_amo_rate = curr_hire_actuarial_amo_rate,
        actuarial_amo_rate = total_hire_actuarial_amo_rate,
        ER_nc_rate = curr_hire_ER_nc_rate[i], admin_rate = Admin_Exp_Pct)
      
      new_hire_adc_rate <- get_adc_rate(
        # actuarial_amo_rate = new_hire_actuarial_amo_rate,
        actuarial_amo_rate = total_hire_actuarial_amo_rate,
        ER_nc_rate = new_hire_ER_nc_rate[i], admin_rate = Admin_Exp_Pct)
      
      # get employer contribution rates depending on the funding policy
      if(Year[i] < year_of_new_tier_start_){
        curr_hire_ER_contr_rate[i] <- fixed_ER_contr_rate_3[i]
        new_hire_ER_contr_rate[i] <- fixed_ER_contr_rate_3[i]
      } else {
        
        if(funding_policy == 'StatusQuo'){
          # determine the total contribution rate next year
          next_year <- i + start_hist_year_ - 1
          
          if (is_fixed_ER_contr_rate == TRUE){
            if (fixed_ER_contr_rate_option == "17.4 fixed") {
              curr_hire_ER_contr_rate[i] <- fixed_ER_contr_rate_1[i]
              new_hire_ER_contr_rate[i] <- fixed_ER_contr_rate_1[i]
              # total_hire_ER_contr_rate[i] <- (curr_hire_ER_contr_rate[i] * curr_hire_payroll[i] + 
              #                                   new_hire_ER_contr_rate[i] * new_hire_payroll[i]) / total_hire_payroll[i]
            } else if (fixed_ER_contr_rate_option == "increase to 19.4") {
              curr_hire_ER_contr_rate[i] <- fixed_ER_contr_rate_2[i]
              new_hire_ER_contr_rate[i] <- fixed_ER_contr_rate_2[i]
              # total_hire_ER_contr_rate[i] <- (curr_hire_ER_contr_rate[i] * curr_hire_payroll[i] + 
              #                                   new_hire_ER_contr_rate[i] * new_hire_payroll[i]) / total_hire_payroll[i]
            } else if (fixed_ER_contr_rate_option == "increase to 19.9 (SB 3231)") {
              curr_hire_ER_contr_rate[i] <- fixed_ER_contr_rate_3[i]
              new_hire_ER_contr_rate[i] <- fixed_ER_contr_rate_3[i]
              # total_hire_ER_contr_rate[i] <- (curr_hire_ER_contr_rate[i] * curr_hire_payroll[i] + 
              #                                   new_hire_ER_contr_rate[i] * new_hire_payroll[i]) / total_hire_payroll[i]
            } else {
              curr_hire_ER_contr_rate[i] <- fixed_ER_contr_rate_4[i]
              new_hire_ER_contr_rate[i] <- fixed_ER_contr_rate_4[i]
            }
            
          } else {
            if (next_year > 2047){
              
              if(funded_ratio_mva[i-1]<1){
                curr_hire_ER_contr_rate[i] <- curr_hire_ER_contr_rate[i-1]
                new_hire_ER_contr_rate[i] <- new_hire_ER_contr_rate[i-1]
                # total_hire_ER_contr_rate[i] <- (curr_hire_ER_contr_rate[i] * curr_hire_payroll[i] + 
                #                                   new_hire_ER_contr_rate[i] * new_hire_payroll[i]) / total_hire_payroll[i]
              } else {
                curr_hire_ER_contr_rate[i] <- 0 + curr_hire_ER_nc_rate[i] + Admin_Exp_Pct
                new_hire_ER_contr_rate[i] <- 0 + new_hire_ER_nc_rate[i] + Admin_Exp_Pct
                # total_hire_ER_contr_rate[i] <- (curr_hire_ER_contr_rate[i] * curr_hire_payroll[i] + 
                #                                   new_hire_ER_contr_rate[i] * new_hire_payroll[i]) / total_hire_payroll[i]
              }
              
            } else if (next_year <= 2029) {
              curr_hire_ER_contr_rate[i] <- phased_in_ER_contr_rate_[i]
              new_hire_ER_contr_rate[i] <- phased_in_ER_contr_rate_[i]
              # total_hire_ER_contr_rate[i] <- (curr_hire_ER_contr_rate[i] * curr_hire_payroll[i] + 
              #                                   new_hire_ER_contr_rate[i] * new_hire_payroll[i]) / total_hire_payroll[i]
            } else {
              
              # get funded ratio in 2047
              payrolls <- payroll_matrix[i:critical_year_indx,]
              cash_flow_excl_amo_ <- cash_flow_excl_amo[i:critical_year_indx]
              
              proj_amo_contr <- get_amo_contr(amo_rates = c(curr_hire_ER_amo_rate[i-1], new_hire_ER_amo_rate[i-1]),
                                              payrolls = payrolls)
              
              proj_mva <- get_mva(prev_mva = total_hire_actual_mva[i-1], dr = curr_discount_rate[i],
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
              
              # total_hire_ER_contr_rate[i] <- (curr_hire_ER_contr_rate[i] * curr_hire_payroll[i] + 
              #                                   new_hire_ER_contr_rate[i] * new_hire_payroll[i]) / total_hire_payroll[i]
              
            }
          }
          
          
        } else if (funding_policy == 'ADC') {
          # curr_hire_ER_contr_rate[i] <- curr_hire_actuarial_amo_rate + curr_hire_ER_nc_rate[i] + Admin_Exp_Pct
          # new_hire_ER_contr_rate[i] <- new_hire_actuarial_amo_rate + new_hire_ER_nc_rate[i] + Admin_Exp_Pct
          
          curr_hire_ER_contr_rate[i] <- curr_hire_actuarial_amo_rate[i] + curr_hire_ER_nc_rate[i] + Admin_Exp_Pct
          new_hire_ER_contr_rate[i] <- new_hire_actuarial_amo_rate[i]*(1 - ee_amo_cost_share_ratio) + new_hire_ER_nc_rate[i] + Admin_Exp_Pct
          
          # total_hire_ER_contr_rate[i] <- (curr_hire_ER_contr_rate[i] * curr_hire_payroll[i] + 
          #                                   new_hire_ER_contr_rate[i] * new_hire_payroll[i]) / total_hire_payroll[i]
          
        }
      }
      
      
      # get ER amortization rate
      curr_hire_ER_amo_rate[i] <- curr_hire_ER_contr_rate[i] - curr_hire_ER_nc_rate[i] - Admin_Exp_Pct
      new_hire_ER_amo_rate[i] <- new_hire_ER_contr_rate[i] - new_hire_ER_nc_rate[i] - Admin_Exp_Pct
      
      # get EE amortization rate
      new_hire_EE_amo_rate[i] <- new_hire_actuarial_amo_rate[i]*ee_amo_cost_share_ratio
      
      # get ER amortization contribution amount
      curr_hire_ER_amo_contr[i] <- db_curr_hire_payroll[i] * curr_hire_ER_amo_rate[i]*req_er_contr_pct
      new_hire_ER_amo_contr[i] <- db_new_hire_payroll[i] * new_hire_ER_amo_rate[i]*req_er_contr_pct
      
      # get EE amortization contribution amount
      new_hire_EE_amo_contr[i] <- db_new_hire_payroll[i] * new_hire_EE_amo_rate[i]
      
      #Supplemental employer contributions 
      # One-time supplement
      if (Year[i] == er_sup_start_year) {
        one_time_er_supplement[i] <- one_time_er_sup
      } else {
        one_time_er_supplement[i] <- 0
      }
      
      # get ER total contribution and cumulative ER total contribution
      total_hire_ER_contr[i] <- (curr_hire_ER_nc_contr[i] + new_hire_ER_nc_contr[i]) +
        (curr_hire_ER_amo_contr[i] + new_hire_ER_amo_contr[i]) + one_time_er_supplement[i]
      
      # total_hire_ER_contr_real[i] <- total_hire_ER_contr[i] / (1 + assum_inflation_)^(Year[i] - year_of_latest_val_report_)
      # 
      # if (Year[i] == year_of_latest_val_report_ + 1) {
      #   cum_total_hire_ER_contr_real[i] <- total_hire_ER_contr_real[i]
      # } else {
      #   cum_total_hire_ER_contr_real[i] <- cum_total_hire_ER_contr_real[i-1] + total_hire_ER_contr_real[i]
      # }
      
      # get net cash flow
      # total_hire_net_cash_flow[i] <- total_hire_benefit[i] + total_hire_refund[i] + total_hire_admin_exp[i] + 
      #                               (curr_hire_EE_nc_contr[i] + new_hire_EE_nc_contr[i]) +
      #                               (curr_hire_ER_nc_contr[i] + new_hire_ER_nc_contr[i]) +
      #                               (curr_hire_ER_amo_contr[i] + new_hire_ER_amo_contr[i])
      
      curr_hire_net_cash_flow[i] <- curr_hire_benefit[i] + curr_hire_refund[i] + curr_hire_admin_exp[i] + 
        curr_hire_EE_nc_contr[i] + curr_hire_ER_nc_contr[i] + curr_hire_ER_amo_contr[i] + one_time_er_supplement[i]
      
      new_hire_net_cash_flow[i] <- new_hire_benefit[i] + new_hire_refund[i] + new_hire_admin_exp[i] +
        new_hire_EE_nc_contr[i] + new_hire_ER_nc_contr[i] + new_hire_ER_amo_contr[i] + new_hire_EE_amo_contr[i]
      
      total_hire_net_cash_flow[i] <- curr_hire_net_cash_flow[i] + new_hire_net_cash_flow[i]
      
      
      #Calculate solvency contributions in case MVA goes negative
      total_solv_cont[i] <- max(-(total_hire_actual_mva[i-1] * (1 + roa_mva[i]) + total_hire_net_cash_flow[i] * (1 + roa_mva[i])^0.5) / (1 + roa_mva[i])^0.5, 0)
      solv_cont_legacy[i] <- total_solv_cont[i] * curr_hire_aal[i] / total_hire_aal[i]
      solv_cont_new[i] <- total_solv_cont[i] * new_hire_aal[i] / total_hire_aal[i]
      
      #Add solvency contributions and ER DC contributions to ER contributions
      total_hire_ER_contr[i] <- total_hire_ER_contr[i] + total_solv_cont[i] + total_er_dc_contr[i]
      
      #Total employer contribution rate
      total_hire_ER_contr_rate[i] <- total_hire_ER_contr[i] / total_hire_payroll[i]
      
      #Calculate the real value of ER contributions and cumulative ER contributions
      total_hire_ER_contr_real[i] <- total_hire_ER_contr[i] / (1 + assum_inflation_)^max((Year[i] - year_of_latest_val_report_), 0)
      
      if (Year[i] == year_of_latest_val_report_ + 1) {
        cum_total_hire_ER_contr_real[i] <- total_hire_ER_contr_real[i]
      } else {
        cum_total_hire_ER_contr_real[i] <- cum_total_hire_ER_contr_real[i-1] + total_hire_ER_contr_real[i]
      }
      
      # add solvency contributions back to net cash flow
      new_hire_net_cash_flow[i] <- new_hire_net_cash_flow[i] + solv_cont_new[i]
      curr_hire_net_cash_flow[i] <- curr_hire_net_cash_flow[i] + solv_cont_legacy[i]
      
      total_hire_net_cash_flow[i] <- curr_hire_net_cash_flow[i] + new_hire_net_cash_flow[i]
      
      # get actual mva
      # total_hire_actual_mva[i] <- total_hire_actual_mva[i-1]*(1 + roa_mva[i]) +
      #                             (total_hire_benefit[i] + total_hire_refund[i] + total_hire_admin_exp[i] +
      #                              curr_hire_EE_nc_contr[i] + new_hire_EE_nc_contr[i] +
      #                              curr_hire_ER_nc_contr[i] + new_hire_ER_nc_contr[i] +
      #                              curr_hire_ER_amo_contr[i] + new_hire_ER_amo_contr[i])*(1 + roa_mva[i])^(1/2)
      
      curr_hire_actual_mva[i] <- curr_hire_actual_mva[i-1]*(1 + roa_mva[i]) + curr_hire_net_cash_flow[i]*(1 + roa_mva[i])^(1/2)
      new_hire_actual_mva[i] <- new_hire_actual_mva[i-1]*(1 + roa_mva[i]) + new_hire_net_cash_flow[i]*(1 + roa_mva[i])^(1/2)
      
      total_hire_actual_mva[i] <- curr_hire_actual_mva[i] + new_hire_actual_mva[i]
      
      # get expected investment income
      curr_hire_exp_invest_inco[i] <- curr_hire_actual_mva[i-1]*curr_discount_rate[i] + curr_hire_net_cash_flow[i]*((1 + curr_discount_rate[i])^0.5 - 1) 
      new_hire_exp_invest_inco[i] <- new_hire_actual_mva[i-1]*new_discount_rate[i] + new_hire_net_cash_flow[i]*((1 + new_discount_rate[i])^0.5 - 1)
      
      # get expected mva
      curr_hire_exp_mva[i] <- curr_hire_actual_mva[i-1] + curr_hire_net_cash_flow[i] + curr_hire_exp_invest_inco[i]
      new_hire_exp_mva[i] <- new_hire_actual_mva[i-1] + new_hire_net_cash_flow[i] + new_hire_exp_invest_inco[i]
      
      # get gain and loss
      curr_hire_gain_loss[i] <- curr_hire_actual_mva[i] - curr_hire_exp_mva[i]
      new_hire_gain_loss[i] <- new_hire_actual_mva[i] - new_hire_exp_mva[i]
      
      # get deferred gain and loss
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
      curr_hire_ava[i] <- min(curr_hire_actual_mva[i] + abs(curr_hire_actual_mva[i]) * 0.2,
                              max(curr_hire_ava[i-1] + curr_hire_net_cash_flow[i] + 
                                    curr_hire_exp_invest_inco[i] + curr_hire_total_recog_gain_loss[i],
                                  curr_hire_actual_mva[i] - abs(curr_hire_actual_mva[i]) * 0.2))
      
      new_hire_ava[i] <- min(new_hire_actual_mva[i] + abs(new_hire_actual_mva[i]) * 0.2,
                             max(new_hire_ava[i-1] + new_hire_net_cash_flow[i] + 
                                   new_hire_exp_invest_inco[i] + new_hire_total_recog_gain_loss[i],
                                 new_hire_actual_mva[i] - abs(new_hire_actual_mva[i]) * 0.2))
      
      total_hire_ava[i] <- curr_hire_ava[i] + new_hire_ava[i]
      
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
      total_hire_ual_mva_real[i] <- total_hire_ual_mva[i] / (1 + assum_inflation_)^max((Year[i] - year_of_latest_val_report_), 0)
      total_hire_ER_all_in_cost_real[i] <- cum_total_hire_ER_contr_real[i] + total_hire_ual_mva_real[i]
      
      # amortization
      # update end_col for the next amortization
      end_col <- min(end_col + 1, 30)
      
      # project existing uaal layers forward and calculate new uaal layers
      for(k in 2:end_col){
        curr_hire_remaining_ual_matrix[row_indx,k] <- curr_hire_remaining_ual_matrix[row_indx-1,k-1]*(1 + curr_discount_rate[i]) - 
          curr_hire_amo_payment_matrix[row_indx-1,k-1]*(1 + curr_discount_rate[i])^(1/2)
        
        new_hire_remaining_ual_matrix[row_indx,k] <- new_hire_remaining_ual_matrix[row_indx-1,k-1]*(1 + new_discount_rate[i]) - 
          new_hire_amo_payment_matrix[row_indx-1,k-1]*(1 + new_discount_rate[i])^(1/2)
      }
      
      curr_hire_remaining_ual_matrix[row_indx,1] <- curr_hire_ual_ava[i] - sum(curr_hire_remaining_ual_matrix[row_indx,2:end_col])
      new_hire_remaining_ual_matrix[row_indx,1] <- new_hire_ual_ava[i] - sum(new_hire_remaining_ual_matrix[row_indx,2:end_col])
      
      # current hire plan amortization
      for(j in 1:end_col){
        if(curr_hire_amo_period_matrix[row_indx,j] != 0){
          # for current hire, current layers
          if (row_indx <= j) {
            if (amo_method_curr_hire_curr_layer == "level percent") {
              amo_g_curr_hire <- AmoBaseInc_CurrentHire
            } else {
              amo_g_curr_hire <- 0
            }
            # for current hire, new layers 
          } else {
            if (amo_method_curr_hire_new_layer == "level percent") {
              amo_g_curr_hire <- AmoBaseInc_CurrentHire
            } else {
              amo_g_curr_hire <- 0
            }
          }
          
          curr_hire_amo_payment_matrix[row_indx, j] <- get_pmt(r = curr_discount_rate[i],
                                                               g = amo_g_curr_hire,
                                                               nper = curr_hire_amo_period_matrix[row_indx,j],
                                                               pv = curr_hire_remaining_ual_matrix[row_indx,j],
                                                               t = 0.5)
        }
      }
      
      # new hire plan amortization
      for(j in 1:end_col){
        if(new_hire_amo_period_matrix[row_indx,j] != 0){
          if (amo_method_new_hire == "level percent") {
            amo_g_new_hire <- AmoBaseInc_NewHire
          } else {
            amo_g_new_hire <- 0
          }
          
          new_hire_amo_payment_matrix[row_indx, j] <- get_pmt(r = new_discount_rate[i],
                                                              g = amo_g_new_hire,
                                                              nper = new_hire_amo_period_matrix[row_indx,j],
                                                              pv = new_hire_remaining_ual_matrix[row_indx,j],
                                                              t = 0.5)
        }
      }
      
      
      
      
     
    }
    
    
    output <- data.frame(sapply(colnames(historical_data_), get, envir = sys.frame(sys.parent(0))))
    
    # output_list <- list(output = output,
    #                     curr_hire_remaining_ual_matrix = curr_hire_remaining_ual_matrix,
    #                     new_hire_remaining_ual_matrix = new_hire_remaining_ual_matrix,
    #                     curr_hire_amo_payment_matrix = curr_hire_amo_payment_matrix,
    #                     new_hire_amo_payment_matrix = new_hire_amo_payment_matrix)
    
    
    # target_col <- c("Year", "funded_ratio_mva", "total_hire_ER_contr_rate",
    #                 "total_hire_ER_contr", "total_hire_ual_mva",
    #                 "total_hire_ER_all_in_cost_real")
    # 
    # target_output <- output %>% select(all_of(target_col))
    # export_output <- liability_data %>% select(nc_rate_legacy_est, nc_rate_new_est)
    
    # return(output_list)
    
    return(output)
    
}

# 
# test_reform <- get_funding_data(db_new_ratio = 0,
#                                 funding_policy = "ADC")

# test_adc <- get_funding_data(funding_policy = "ADC",
#                              roa_scenario = "recur_recession",
#                              amo_period_curr_hire_new_layer = 15,
#                              amo_period_new_hire = 10,
#                              amo_method_curr_hire_new_layer = "level dollar",
#                              amo_method_new_hire = "level dollar")
# 
# funding_output <- test_adc$output
# round(test_adc$curr_hire_remaining_ual_matrix,0)
# round(test_adc$new_hire_remaining_ual_matrix,0)
# round(test_adc$curr_hire_amo_payment_matrix,0)
# round(test_adc$new_hire_amo_payment_matrix,0)


# baseline_funding <- get_funding_data(is_fixed_ER_contr_rate = FALSE)
# FCR_funding <- get_funding_data(is_fixed_ER_contr_rate = TRUE)

# memoised_get_funding_data <- memoise(get_funding_data)
