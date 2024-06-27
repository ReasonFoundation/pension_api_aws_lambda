
# 1. Economic and Actuarial assumptions
payroll_growth_ <- 0.0265
pop_growth_ <- 0
assum_inflation_ <- 0.024
assum_inv_return_ <- 0.07
curr_discount_rate_ <- 0.07
new_discount_rate_ <- 0.07
retire_refund_ratio_ <- 0.65 # Page 7, Val Report
roa_scenario_ <-'assumption'
analysis_type_ <- 'Deterministic'
funding_policy_ <- 'StatusQuo'


# 4. Model assumptions
model_period_ <- 30    #Projection period (typically 30 years)
min_age_ <- 20          #Age of the typical youngest member
max_age_ <- 120         #Max age from mortality assumptions
year_of_latest_val_report_ <- 2023     #Year of the latest val report
min_proj_year_ <- 1990       # Current year - max(yos)
max_proj_year_ <- year_of_latest_val_report_ + model_period_ + max_age_ - min_age_
year_of_new_tier_start_ <- 2026


### Users' inputs
start_hist_year_ <- 2022
start_proj_year_ <- 2024
end_proj_year_ <- 2053
critical_year_ <- 2047

entry_year_range_ <- min_proj_year_:(year_of_latest_val_report_ + model_period_)
year_range_ <- min_proj_year_:max_proj_year_
age_range_ <- min_age_:max_age_
yos_range_ <- 0:70
retirement_age_range_ <- min_age_:max_age_


# 2. Plan design
credited_interest_ <- 0.02 #apply to employee contributions
# ee_contr_rate_ <- 0.09
cola_for_tier_1234_ <- 0.03
cola_for_tier_5_ <- 0.03
req_er_contr_pct <- 1 # Expected funded ratio in 2047
Admin_Exp_Pct <- 0.0026
AmoBaseInc_CurrentHire <- 0.0265
AmoBaseInc_NewHire <- 0.0265
phased_in_ER_contr_rate_ <- c(0.174, 0.174, 0.174, 0.194, 0.214, 0.234, 0.254, 0.274)
is_fixed_ER_contr_rate_ <- FALSE
#three fixed rate options: "17.4 fixed", "increase to 19.4", "increase to 19.9 (SB 3231)", "increase to 22.4"
fixed_ER_contr_rate_option_ <- "increase to 19.9 (SB 3231)"
fixed_ER_contr_rate_1 <- rep(0.174, end_proj_year_ - start_hist_year_ + 1)
fixed_ER_contr_rate_2 <- c(0.174, 0.174, 0.174, rep(0.194, end_proj_year_ - start_hist_year_ - 2))
fixed_ER_contr_rate_3 <- c(0.174, 0.174, 0.174, 0.179, 0.184, 0.189, 0.194, rep(0.199, end_proj_year_ - start_hist_year_ + 1 - 7))
fixed_ER_contr_rate_4 <- c(0.174, 0.174, 0.174, 0.194, 0.214, 0.224, rep(0.224, end_proj_year_ - start_hist_year_ + 1 - 6))


tier5_vesting_period_ <- 8

db_new_ratio_ <- 1       #the percent of new hires choosing DB (as opposed to DC)

er_dc_contr_rate_ <- 0.03

# 3. Empirical data
retiree_pop_current_ <- 97395
ben_payment_current_ <- 3237085000
curr_hire_nc_rate_ <- 0.1032
# new_hire_nc_rate_ <- 0.1136
stat_curr_hire_EE_contr_ <- 0.09
stat_new_hire_EE_contr_ <- 0.09
refund_rate_ <- 0
stat_ER_contr_rate_ <- 0.174

# Funding policy
amo_period_curr_hire_new_layer_ <- 25
amo_period_new_hire_ <- 25

amo_method_curr_hire_curr_layer_ <- "level percent"
amo_method_curr_hire_new_layer_ <- "level percent"
amo_method_new_hire_ <- "level percent"

ee_amo_cost_share_ratio_ <- 0


one_time_er_sup_ <- 110
er_sup_start_year_ <- 2025

#Import key data tables for benefit model
ben_file_name_ <- '/var/task/model/raw_inputs/BenefitModel-Inputs.xlsx'
# ben_file_name_ <- './raw_inputs/BenefitModel-Inputs.xlsx'

survival_rate_ <- read_excel(ben_file_name_, sheet = 'PubSH-2010-Mortality-Rate')
male_mp_ <- read_excel(ben_file_name_, sheet = 'MP-2020-Male')
female_mp_ <- read_excel(ben_file_name_, sheet = 'MP-2020-Female')

init_male_separation_rate_ <- read_excel(ben_file_name_, sheet = "Separation Rate Male") %>% replace(is.na(.), 0)
init_female_separation_rate_ <- read_excel(ben_file_name_, sheet = "Separation Rate Female") %>% replace(is.na(.), 0)

init_retirement_rate_tier123_ <- read_excel(ben_file_name_, sheet="Retirement Rate Tier123") %>% replace(is.na(.), 0)
init_retirement_rate_tier4_ <- read_excel(ben_file_name_, sheet="Retirement Rate Tier4") %>% replace(is.na(.), 0)

salary_growth_rate_ <- read_excel(ben_file_name_, sheet = "Salary Growth")
salary_entry_ <- read_excel(ben_file_name_, sheet = "Start Salary") #change the 1st entry age from 19 to 20 to match with separation rate tables
headcount_matrix_ <- read_excel(ben_file_name_, sheet = "HeadCount Matrix")
# test_salary_matrix_ <- read_excel(ben_file_name_, sheet = "TestSalaryMatrix")
headcount_ <- read_excel(ben_file_name_, sheet = "Headcount")
retiree_dist_ <- read_excel(ben_file_name_, sheet = "Retiree Dist")

others_ <- read_excel(ben_file_name_, sheet = "Others")


#Import key data tables for funding model
fund_file_name_ <- '/var/task/model/raw_inputs/FundingModel-Inputs.xlsx'
# fund_file_name_ <- './raw_inputs/FundingModel-Inputs.xlsx'

historical_data_ <- read_excel(fund_file_name_, sheet="Historical Data") %>% replace(is.na(.), 0)
scenario_data_ <- read_excel(fund_file_name_, sheet="Inv_Returns") %>% replace(is.na(.), 0)
ual_data_ <- read_excel(fund_file_name_, sheet = "Remaining UAL") %>% replace(is.na(.), 0)


# Model calibration
nc_calibration <- 0.858
nc_calibration_ratio <- 0.1136 / 0.11371464

salary_calibration_ratio <- 1.2
benefit_calibration_ratio <- 0.96


# Gap between AALs in the val report and the model
# See Page 16 Val Report
PVFB_term_current <- 3057713598 # 58148281981 - 55,090,568,383
# PVFB_term_current <- 0
amo_period_term_ <- 30
