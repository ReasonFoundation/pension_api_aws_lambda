#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector calc_mort_rate_cpp(NumericVector age, IntegerVector is_retirement, NumericVector male_employee, NumericVector male_healthy_retiree,
                                 NumericVector male_cumprod_mp_adj, NumericVector female_employee, NumericVector female_healthy_retiree,
                                 NumericVector female_cumprod_mp_adj, NumericVector male_mort_table_age, NumericVector male_mort_table_rate,
                                 NumericVector female_mort_table_age, NumericVector female_mort_table_rate) {
  
  // Initialize the output vector
  int n = age.size();
  NumericVector mort_rate(n);
  
  // Loop over the input vectors and compute the mortality rate for each element
  for (int i = 0; i < n; i++) {
    // Calculate male mortality rate
    double male_rate = 0.0;
    int nrow_male = male_mort_table_age.size();
    int male_index = std::lower_bound(male_mort_table_age.begin(), male_mort_table_age.end(), age[i]) - male_mort_table_age.begin();
    if (male_index < nrow_male) {
      male_rate = male_employee[i] * male_mort_table_rate[male_index];
    }
    double male_mort_rate = male_cumprod_mp_adj[i] * (is_retirement[i] * male_healthy_retiree[i] + (!is_retirement[i]) * male_rate);
    
    // Calculate female mortality rate
    double female_rate = 0.0;
    int nrow_female = female_mort_table_age.size();
    int female_index = std::lower_bound(female_mort_table_age.begin(), female_mort_table_age.end(), age[i]) - female_mort_table_age.begin();
    if (female_index < nrow_female) {
      female_rate = female_employee[i] * female_mort_table_rate[female_index];
    }
    double female_mort_rate = female_cumprod_mp_adj[i] * (is_retirement[i] * female_healthy_retiree[i] + (!is_retirement[i]) * female_rate);
    
    
    // Calculate the overall mortality rate as the average of male and female rates
    mort_rate[i] = (male_mort_rate + female_mort_rate) / 2.0;
  }
  
  // Return the vector of mortality rates
  return mort_rate;
}

// [[Rcpp::export]]
double get_npv_rcpp(double rate, NumericVector cashflows){
  int n = cashflows.size();
  NumericVector discount_factors(n);
  for(int i = 0; i < n; i++){
    discount_factors[i] = 1 / std::pow(1+rate, i+1);
  }
  double net_present_value = sum(cashflows * discount_factors);
  return(net_present_value);
}

// [[Rcpp::export]]
double get_npv_rcpp_log(double rate, NumericVector cashflows){
  int n = cashflows.size();
  NumericVector log_discount_factors(n);
  for(int i = 0; i < n; i++){
    log_discount_factors[i] = -log(1+rate) * (i+1);
  }
  NumericVector scaled_cashflows = cashflows * exp(log_discount_factors);
  double net_present_value = sum(scaled_cashflows);
  return(net_present_value);
}


// [[Rcpp::export]]
NumericVector lag_rcpp(NumericVector x, int k, double default_value) {
  int n = x.size();
  if (k >= n) {
    return rep(default_value, n);
  } else {
    NumericVector result(n);
    result[seq(k, n-1)] = NumericVector(x[seq(0, n-k-1)]);
    result[seq(0, k-1)] = NumericVector(k, default_value);
    return result;
  }
}

// [[Rcpp::export]]
NumericVector cumprodC(NumericVector x) {
  int n = x.size();
  NumericVector out(n);
  
  out[0] = x[0];
  for(int i = 1; i < n; ++i) {
    out[i]  = out[i - 1] * x[i];
  }
  return out;
}


// [[Rcpp::export]]
NumericVector opt_PVFB_rcpp(NumericVector sep_rate_vec, NumericVector interest, NumericVector value_vec) {
  int n = value_vec.size();
  NumericVector PVFB(n);
  for (int i = 0; i < n; i++) {
    NumericVector sep_rate_ = sep_rate_vec[seq(i, n-1)];
    // Rcpp::Rcout << "sep_rate_: " << sep_rate_ << std::endl;
    
    NumericVector sep_prob = cumprodC(1 - lag_rcpp(sep_rate_, 2, 0)) * lag_rcpp(sep_rate_, 1, 0);
    // Rcpp::Rcout << "sep_prob: " << sep_prob << std::endl;
    
    NumericVector value = value_vec[seq(i, n-1)];
    // Rcpp::Rcout << "value: " << value << std::endl;
    
    NumericVector value_adjusted = value * sep_prob;
    // Rcpp::Rcout << "value_adjusted: " << value_adjusted << std::endl;
    
    if (value_adjusted.size() > 1){
      PVFB[i] = get_npv_rcpp(interest[i], value_adjusted[seq(1, value_adjusted.size() - 1)]);
    } else{
      PVFB[i] = get_npv_rcpp(interest[i], value_adjusted);
    }
    
    // Rcpp::Rcout << "PVFB[" << i << "]: " << PVFB[i] << std::endl;
    
  }
  return PVFB;
}

// // [[Rcpp::export]]
// NumericVector opt_PVFS_rcpp(double interest, NumericVector remaining_prob_vec, NumericVector sal_vec){
//   int n = sal_vec.size();
//   Rcpp::NumericVector PVFS(n);
//   for (int i = 0; i < n; i++){
//     NumericVector sal = sal_vec[seq(i, n-1)];
//     NumericVector sal_adjusted = sal;
//     PVFS[i] = sum(sal_adjusted);
//   }
//   // Rcpp::NumericVector final_PVFS = ifelse(Rcpp::is_na(PVFS), 0, PVFS);
//   return PVFS;
// }

// [[Rcpp::export]]
NumericVector opt_PVFS_rcpp(NumericVector interest, NumericVector remaining_prob_vec, NumericVector sal_vec){
  int n = sal_vec.size();
  NumericVector PVFS(n);
  for (int i = 0; i < n; i++){
    NumericVector remaining_prob_og = remaining_prob_vec[seq(i, n)];
    NumericVector remaining_prob = remaining_prob_og / remaining_prob_og[0];
    NumericVector sal = sal_vec[seq(i, n)];
    NumericVector sal_adjusted = sal * remaining_prob;
    PVFS[i] = get_npv_rcpp(interest[i], sal_adjusted);
  }
  PVFS = ifelse(is_na(PVFS), 0, PVFS);
  return PVFS;
}

