
# 1. Economic and Actuarial assumptions
payroll_growth_ <- 0.0265
pop_growth_ <- 0
assum_inflation_ <- 0.024
assum_inv_return_ <- 0.0755
curr_discount_rate_ <- 0.0755
new_discount_rate_ <- 0.0755
retire_refund_ratio_ <- 0.6
roa_scenario_ <-'Assumption'
analysis_type_ <- 'Deterministic'
funding_policy_ <- 'StatusQuo'




# 2. Plan design
credited_interest_ <- 0.02
ee_contr_rate_ <- 0.09
cola_ <- 0.03
req_er_contr_pct <- 0.86
Admin_Exp_Pct <- 0.0028
AmoBaseInc_CurrentHire <- 0.0265
AmoBaseInc_NewHire <- 0.0265


# 3. Empirical data
retiree_pop_current_ <- 95976
ben_payment_current_ <- 3134860000
curr_hire_nc_rate_ <- 0.1032
new_hire_nc_rate_ <- 0.1032
stat_curr_hire_EE_contr_ <- 0.09
stat_new_hire_EE_contr_ <- 0.09
refund_rate_ <- 0.00104
stat_ER_contr_rate_ <- 0.174


# 4. Model assumptions
model_period <- 30    #Projection period (typically 30 years)
min_age <- 20          #Age of the typical youngest member
max_age <- 120         #Max age from mortality assumptions
year_start <- 2022     #Year of the latest val report
min_year <- 1990       # Current year - max(yos)
max_year <- year_start + model_period + max_age - min_age

### Users' inputs
start_hist_year <- 2021
start_proj_year <- 2023
end_proj_year <- 2052
critical_year <- 2047

entry_year_range <- min_year:(year_start + model_period)
year_range <- min_year:max_year
age_range <- min_age:max_age
yos_range <- 0:70
retirement_age_range <- min_age:max_age

# 5. Model calibration
nc_calibration <- 1
nc_calibration_ratio <- 0.897 # 0.1032 / 0.115

# Gap between AALs in the val report and the model
# See Page 16 Val Report
PVFB_term_current <- 2476015467 # 52000142746 - 49524127279
# PVFB_term_current <- 0
amo_period_term <- 50


#Import key data tables for benefit model
ben_file_name <- '/var/task/model/Inputs/BenefitModel-Inputs.xlsx'
# ben_file_name <- 'Inputs/BenefitModel-Inputs.xlsx'

survival_rate <- read_excel(ben_file_name, sheet = 'PubSH-2010-Mortality-Rate')
male_mp <- read_excel(ben_file_name, sheet = 'MP-2020-Male')
female_mp <- read_excel(ben_file_name, sheet = 'MP-2020-Female')

init_male_separation_rate <- read_excel(ben_file_name, sheet = "Separation Rate Male")
init_female_separation_rate <- read_excel(ben_file_name, sheet = "Separation Rate Female")

init_retirement_rate_tier123 <- read_excel(ben_file_name, sheet="Retirement Rate Tier123")
init_retirement_rate_tier4 <- read_excel(ben_file_name, sheet="Retirement Rate Tier4")

salary_growth_rate <- read_excel(ben_file_name, sheet = "Salary Growth")
salary_entry_2022 <- read_excel(ben_file_name, sheet = "Start Salary") #change the 1st entry age from 19 to 20 to match with separation rate tables
headcount_matrix <- read_excel(ben_file_name, sheet = "HeadCount Matrix")
test_salary_matrix <- read_excel(ben_file_name, sheet = "TestSalaryMatrix")
headcount_2022 <- read_excel(ben_file_name, sheet = "Headcount")
retiree_dist <- read_excel(ben_file_name, sheet = "Retiree Dist")

others <- read_excel(ben_file_name, sheet = "Others")


#Import key data tables for funding model
fund_file_name <- '/var/task/model/Inputs/FundingModel-Inputs.xlsx'
# fund_file_name <- 'Inputs/FundingModel-Inputs.xlsx'
# numeric_inputs <- read_excel(fund_file_name, sheet="Numeric Inputs")
# character_inputs <- read_excel(fund_file_name, sheet="Character Inputs")
historical_data <- read_excel(fund_file_name, sheet="Historical Data")
scenario_data <- read_excel(fund_file_name, sheet="Inv_Returns")
ual_data <- read_excel(fund_file_name, sheet = "Remaining UAL")


