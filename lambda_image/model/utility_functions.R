
#Present Value function
get_pv <- function(rate, g = 0, nper, pmt, t = 1) {
  r <- (1 + rate)/(1 + g) - 1
  PV <- pmt/r * (1 - (1 / (1 + r)^nper)) / (1 + g) * (1 + rate)^(1 - t)
  return(PV)
}

#Rolling Present Value function. Note that the first value in the pmt_vec vector must be zero.
roll_pv <- function(rate, g = 0, nper, pmt_vec, t = 1) {
  pv_vec <- double(length(pmt_vec))
  for (i in 1:length(pv_vec)) {
    if (i == 1) {
      pv_vec[i] <- get_pv(rate, g, nper, pmt_vec[2], t)
    } else {
      pv_vec[i] <- pv_vec[i-1] * (1 + rate) - pmt_vec[i] * (1 + rate)^(1 - t)
    }
  }
  
  return(pv_vec)
}

#NPV function
get_npv = function(rate, cashflows) {
  for(i in 1:length(cashflows)){
    if(i == 1){
      net_present_value <- cashflows[i]/((1+rate)^(i))
    } else {
      net_present_value <- net_present_value + cashflows[i]/((1+rate)^(i))
    }
  }
  return(net_present_value)
}

get_npv_opt = function(rate, cashflows) {
  periods <- 1:length(cashflows)
  discount_factors <- 1 / ((1 + rate) ^ periods)
  net_present_value <- sum(cashflows * discount_factors)
  return(net_present_value)
}


#rolling NPV function
roll_npv <- function(rate, cashflows) {
  npv_vec <- double(length = length(cashflows))
  for (i in 1:(length(cashflows)-1)) {
    npv_vec[i] <- get_npv(rate, cashflows[(i+1):length(cashflows)])  
  }
  
  return(npv_vec)
}


#Function for calculating amo payments
#pmt0 = basic amo payment calculation, assuming payment beginning of period 
get_pmt0 <- function(r, nper, pv) {
  if (r == 0) {
    a <- pv/nper
  } else {
    a <- pv*r*(1+r)^(nper-1)/((1+r)^nper-1)  
  }
  
  # if(nper == 0){
  #   a <- 0
  # }
  
  return(a)
}

#pmt = amo payment function with growth rate and timing added; t = 1 for end of period payment, 0.5 for half period. 
get_pmt <- function(r, g = 0, nper, pv, t = 1) {
  a <- get_pmt0((1+r)/(1+g) - 1, nper, pv*(1+r)^t)
  return(a)
}


#Cumulative future values function (with interest being a single value)
get_cum_fv <- function(interest, cash_flow){
  cum_value <- double(length = length(cash_flow))
  for (i in 2:length(cum_value)) {
    cum_value[i] <- cum_value[i - 1]*(1 + interest) + cash_flow[i - 1]
  }
  return(cum_value)
}

#Cumulative future values function (with interest being a vector)
get_cum_fv_2 <- function(interest_vec, cashflow, first_value = 0){
  cumvalue <- double(length = length(cashflow))
  cumvalue[1] <- first_value
  for (i in 2:length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest_vec[i]) + cashflow[i - 1]
  }
  return(cumvalue)
}

#Rolling mean function (a lot faster than zoo's rollmean)
#note that this roll mean function assumes a "lagged" data vector
get_roll_mean <- function(data, window_vec) {
  n <- length(data)
  y <- double(n)
  for (i in 1:n) {
    window <- window_vec[i]
    if (i > window) {
      y[i] <- mean(data[(i-window):(i-1)]) 
    } else {
      y[i] <- 0
    }
  }
  return(y)
}

#Adding new entrants function
add_new_entrants <- function(g, ne_dist, wf1, wf2, ea, age, position_matrix){
  #g is the assumed population growth of the plan
  #ne_dist is a vector representing the distribution of new entrants for each entry age
  #wf1 is the population in period 1. wf2 is the wf1 population after decremented
  #ea and age are two vectors representing entry age and age for active members
  #position_matrix is the matrix that rearranges the new entrant numbers in the right positions to be added to the active workforce array
  ne <- sum(wf1)*(1 + g) - sum(wf2)
  ne_vec <- ne * ne_dist
  ne_matrix <- matrix(ne_vec, nrow = length(ea), ncol = length(age))
  ne_matrix_trans <- ne_matrix * position_matrix
  
  return(ne_matrix_trans)
}

#Recursive growing function (with lag)
recur_grow <- function(x, g) {
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      x[i] <- x[i-1] * (1 + g[i - 1])
    }
  }
  return(x)
}

#Recursive growing function (no lag)
recur_grow2 <- function(x, g) {
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      x[i] <- x[i-1] * (1 + g[i])
    }
  }
  return(x)
}

#Recursive growing function with a single base and a fixed growth rate
recur_grow3 <- function(x, g, nper) {
  x_vec <- double(length = nper)
  x_vec[1] <- x
  
  for (i in 2:length(x_vec)) {
    x_vec[i] <- x_vec[i-1] * (1 + g)
  }
  
  return(x_vec)
}

#Present Value of Future Benefits (PVFB) function (to be applied to a vector of "Pension Wealth") for active members
#sep_rate_vec is a vector containing separation rates. Interest is a single discount rate (ARR). value_vect is a vector containing the present values of pension benefits at separation ages.
#The purpose of this function is to calculate the PVFB at each active age (not just the entry age)
PVFB <- function(sep_rate_vec, interest, value_vec) {
  PVFB <- double(length = length(value_vec))
  for (i in 1:length(value_vec)) {
    sep_rate <- sep_rate_vec[i:length(sep_rate_vec)]
    sep_prob <- cumprod(1 - lag(sep_rate, n = 2, default = 0)) * lag(sep_rate, default = 0)
    value <- value_vec[i:length(value_vec)]
    value_adjusted <- value * sep_prob
    PVFB[i] <- get_npv(interest[i], value_adjusted[2:length(value_adjusted)])
  }
  return(PVFB)
}




#Present Value of Future Salaries (PVFS) function (to be applied to a vector of salaries)
#remaining_prob_vec is a vector containing the remaining probabilities. Interest is a single discount rate (ARR). sal_vec is a vector containing the salaries.
PVFS <- function(remaining_prob_vec, interest, sal_vec) {
  PVFS <- double(length = length(sal_vec))
  for (i in 1:length(sal_vec)) {
    remaining_prob_og <- remaining_prob_vec[i:length(remaining_prob_vec)]
    remaining_prob <- remaining_prob_og / remaining_prob_og[1]
    sal <- sal_vec[i:length(sal_vec)]
    sal_adjusted <- sal * remaining_prob
    PVFS[i] <- get_npv(interest[i], sal_adjusted)
  }
  return(PVFS)
}


get_actuarial_amo_rate <- function(annual_amo_payment, annual_payroll){
  actuarial_amo_rate <- annual_amo_payment / annual_payroll
  return(actuarial_amo_rate)
}

get_adc_rate <- function(actuarial_amo_rate, ER_nc_rate, admin_rate){
  adc_rate <- actuarial_amo_rate + ER_nc_rate + admin_rate
  return(adc_rate)
}

get_adc_fcr_ratio <- function(adc, fcr){
  adc_fcr_ratio <- adc / fcr
  return(adc_fcr_ratio)
}

# Get amortization contribution
get_amo_contr <- function(amo_rates, payrolls){
  amo_contr <- payrolls %*% matrix(amo_rates)
  return(amo_contr)
}

# test get_amo_contr
# simple_sum <- get_amo_contr(amo_rates = c(1,1), payroll_matrix)
# round(simple_sum - rowSums(payroll_matrix))

# Get MVA
get_mva <- function(prev_mva, dr, cash_flow_excl_amo, amo_contr){
  mva <- rep(0, length(cash_flow_excl_amo))
  for(i in 1:length(cash_flow_excl_amo)){
    if(i == 1){
      mva[i] <- prev_mva*(1+dr) + (cash_flow_excl_amo[i] + amo_contr[i])*(1+dr)^(1/2)
    } else {
      mva[i] <- mva[i-1]*(1+dr) + (cash_flow_excl_amo[i] + amo_contr[i])*(1+dr)^(1/2)
    }
  }
  return(mva)
}

# test get_mva
# simple_sum <- get_amo_contr(amo_rates = c(1,1), payroll_matrix)
# first_mva_wo_dr <- get_mva(prev_mva = 10, dr = 0, cash_flow_excl_amo, amo_contr=simple_sum)[1]
# round(first_mva_wo_dr - (10 + cash_flow_excl_amo[1] + simple_sum[1]))

# Get funded ratio
get_funded_ratio_2047 <- function(mva_2047, aal_2047){
  funded_ratio_2047 <- mva_2047/aal_2047
  return(funded_ratio_2047)
}

# test get_funded_ratio_2047
# amo_contr_1 <- get_amo_contr(amo_rates = c(0.158, 0.158), payroll_matrix)
# mva_1 <- get_mva(prev_mva = 30791, dr = 0.0755, cash_flow_excl_amo, amo_contr_1)
# mva_2047_1 <- tail(mva_1, 1)
# aal_2047 <- tail(total_hire_new_dr_aal[4:(length(total_hire_new_dr_aal) - (end_proj_year - 2047))], 1)
# fr_2047_1 <- get_funded_ratio_2047(mva_2047_1, aal_2047)

is_red_status <- function(fr_2047, adc_fcr_ratio){
  return(fr_2047 <= 0.65 || adc_fcr_ratio >= 1.10)
}





