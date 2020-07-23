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

plot_attrition <- function(
    data
    , ...
    , .value
    , fct_reorder = TRUE
    , fct_rev = FALSE
    , include_lbl = TRUE
    , color = palette_light()[[1]]
    , units = c("0","K","M")
) {
    
    # Inputs
    group_vars_expr <- quos(...)
    if(length(group_vars_expr) == 0)
        group_vars_expr <- quos(rlang::sym(colnames(data)[[1]]))
    
    value_expr <- enquo(.value)
    value_name <- quo_name(value_expr)
    
    units_val <- switch(
        units[[1]]
        , "M" = 1e6
        , "K" = 1e3
        , "0" = 1
    )
    if(units[[1]] == "0") units <- ""
    
    # Data Manipulation
    usd <- scales::dollar_format(prefix = "$", largest_with_cents = 1e3)
    
    data_manipulated <- data %>%
        mutate(name = str_c(!!! group_vars_expr, sep = ": ") %>% as_factor()) %>%
        mutate(value_text = str_c(usd(!! value_expr / units_val),
                                 units[[1]], sep = ""))
    
    if(fct_reorder) {
        data_manipulated <- data_manipulated %>%
            mutate(name = forcats::fct_reorder(name, !! value_expr)) %>%
            arrange(name)
    }
    
    if(fct_rev) {
        data_manipulated <- data_manipulated %>%
            mutate(name = forcats::fct_rev(name, !! value_expr)) %>%
            arrange(name)
    }
    
    # Plotting
    g <- data_manipulated %>%
        ggplot(mapping = aes_string(x = value_name, y = "name")) +
        geom_segment(
            mapping = aes(
                xend = 0
                , yend = name
            )
            , color = color
        ) +
        geom_point(
            mapping = aes_string(
                size = value_name
            )
            , color = color
        ) +
        scale_x_continuous(labels = scales::dollar) +
        theme_tq() + 
        scale_size(range = c(3, 5)) +
        theme(legend.position = "none")
    
    if(include_lbl){
        g <- g +
            geom_label(mapping = aes_string(
                label = "value_text"
                , size = value_name
            )
            , hjust = "inward"
            , color = color
            )
    }
    
    return(g)
}

dept_job_role_tbl %>%
    count(Department, Attrition) %>%
    count_to_pct(Department) %>%
    assess_attrition(
        attrition_col = Attrition
        , attrition_value = "Yes"
        , baseline_pct = 0.088
    ) %>%    
    mutate(
        cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
    ) %>%
    plot_attrition(Department, .value = cost_of_attrition, units = "M")
