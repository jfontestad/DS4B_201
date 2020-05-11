# Lib Load ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
    "tidyverse"
    ,"tidyquant"
    ,"readxl"
    ,"forcats"
    ,"stringr"
)

# Data Load ----
path_train <- "00_Data/telco_train.xlsx"
train_raw_tbl <- read_excel(path = path_train, sheet = 1)

# Data Subset ----
dept_job_role_tbl <- train_raw_tbl %>%
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)
dept_job_role_tbl

# Business Science Problem Framework ----

# 1A. View Business As Machine ----

# Isolate business unit (Dept and Job Role)
# Define objective: Retain High performers
# Assess Outcomes: TBD

dept_job_role_tbl %>%
    group_by(Attrition) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    mutate(pct = n / sum(n))

# 1B. Understand Drivers ----
# Investigate Objectivs: 16% Attrition
# Synthesize Outcomes: How does attrition change by dept and jobrole
# Hypothesize Drivers: Job Role and Dept

dept_job_role_tbl %>%
    group_by(Department, Attrition) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    # Get Dept Attrition
    group_by(Department) %>%
    mutate(pct = n / sum(n))
    
# Department ----
dept_job_role_tbl %>%
    group_by(Department, Attrition) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    # Get Dept Attrition
    group_by(Department) %>%
    mutate(pct = n / sum(n))

# Job Role ----
dept_job_role_tbl %>%
    group_by(Department, JobRole, Attrition) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    # Get Dept Attrition
    group_by(Department, JobRole) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    filter(Attrition == "Yes")

# 1C. Measure The Drivers ----

# Collect Information on Employee Attrition

# Develop KPI's: Industry KPI's 8.8%
dept_job_role_tbl %>%
    group_by(Department, JobRole, Attrition) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    # Get Dept Attrition
    group_by(Department, JobRole) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    filter(Attrition == "Yes") %>%
    arrange(desc(pct)) %>%
    mutate(
        above_industry_avg = case_when(
            pct > 0.088 ~ "Yes",
            TRUE ~ "No"
        )
    )

# 1D Uncover Prolems and Opportunities
calculate_attrition_cost <- function(
    
    # Employee
    n                          = 1
    , salary                   = 80000
    
    # Direct Cost
    , separation_cost          = 500
    , vacancy_cost             = 10000
    , acquisition_cost         = 4900
    , placement_cost           = 3500
    
    # Productivity costs
    , net_revenue_per_employee = 250000
    , workdays_per_year        = 240
    , workdays_position_open   = 40
    , workdays_onboarding      = 60
    , onboarding_efficiency    = 0.50
){
    
    # Direct Costs
    direct_cost <- sum(
        separation_cost
        , vacancy_cost
        , acquisition_cost
        , placement_cost
        )
    
    # Lost Productivyt Costs
    productivity_cost <- net_revenue_per_employee / workdays_per_year * 
        (workdays_position_open + workdays_onboarding * onboarding_efficiency)
    
    # Savings of Salary & Benefits (Cost Reduction)
    salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
    
    # Estimated Turnover for Employee
    cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
    
    # Total Cost of Employee Turnover
    total_cost <- n * cost_per_employee
    
    return(total_cost)
}
calculate_attrition_cost(n = 200)
