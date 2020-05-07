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

# Develop KPI's