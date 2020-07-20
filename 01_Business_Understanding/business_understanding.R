# Business Understanding ----
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

# Calculate Cost by Job Role ----
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
    ) %>%
    mutate(
        cost_of_attrition = calculate_attrition_cost(n = n)
    )

# Workflow of Attrition ----
count_to_pct <- function(data, ..., col = n) {
    
    grouping_vars_expr <- rlang::quos(...)
    col_expr <- rlang::enquo(col)
    
    ret <- data %>%
        group_by(!!! grouping_vars_expr) %>%
        mutate(pct = (!! col_expr) / sum(!! col_expr)) %>%
        ungroup()
    
    return(ret)
    
}

assess_attrition <- function(data, attrition_col, attrition_value, baseline_pct) {
    
    attrition_col_expr <- enquo(attrition_col)
    
    data %>%
        filter((!! attrition_col_expr) %in% attrition_value) %>%
        arrange(desc(pct)) %>%
        mutate(
            above_industry_avg = case_when(
                pct > baseline_pct ~ "Yes",
                TRUE ~ "No"
            )
        )
    
}

dept_job_role_tbl %>%
    count(Department, JobRole, Attrition) %>%
    count_to_pct(Department, JobRole) %>%
    assess_attrition(
        attrition_col = Attrition
        , attrition_value = "Yes"
        , baseline_pct = 0.088
    ) %>%    
    mutate(
        cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
    ) %>%
    
    # Data Manipulation
    mutate(name = str_c(Department, JobRole, sep = ": ") %>% as_factor()) %>%
    mutate(name = fct_reorder(name, cost_of_attrition)) %>%
    mutate(cost_text = str_c("$", format(cost_of_attrition / 1e6, digits = 2),
                             "M", sep = "")) %>%
    
    # Plotting
    ggplot(
        mapping = aes(
            x = cost_of_attrition
            , y = name
            )
        ) +
    geom_segment(
        mapping = aes(
            xend = 0
            , yend = name
            )
        , color = palette_light()[[1]]
        ) +
    geom_point(
        mapping = aes(
            size = cost_of_attrition
            )
        ) +
    scale_x_continuous(labels = scales::dollar) +
    geom_label(mapping = aes(
        label = cost_text
        , size = cost_of_attrition
        )
        , hjust = "inward"
        , color = palette_light()[[1]]
        ) +
    theme_tq() + 
    scale_size(range = c(3, 5)) +
    labs(
        title = "Estimated Cost of Attrition: By Dept and Job Role"
        , y = ""
        , x = "Cost of Attrition"
    ) +
    theme(
        legend.position = "none"
    )


