# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# TARGETED OVERTIME POLICY ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)


# Load Data
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
test_raw_tbl        <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)
definitions_raw_tbl <- definitions_raw_tbl %>%
    set_names("X__1","X__2")

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_mutate_at(JobLevel, StockOptionLevel, fn = as.factor) %>% 
    prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Models ----

h2o.init()

# Replace this with your model!!! (or rerun h2o.automl)
automl_leader <- h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_1_AutoML_1_20211020_121301")

automl_leader


# 3. Primer: Working With Threshold & Rates ----

performance_h2o <- automl_leader %>%
    h2o.performance(newdata = as.h2o(test_tbl))

performance_h2o %>%
    h2o.confusionMatrix()

rates_by_threshold_tbl <- performance_h2o %>%
    h2o.metric() %>%
    as_tibble()

rates_by_threshold_tbl %>% glimpse()

f1_score <- rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    filter(f1 == max(f1)) %>%
    slice(1)

rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    pivot_longer(cols = tnr:tpr, names_to = "metric") %>%
    mutate(metric = as.factor(metric)) %>%
    mutate(metric = fct_reorder2(metric, threshold, value)) %>%
    ggplot(aes(x = threshold, y = value, color = metric)) +
    geom_point() +
    geom_smooth() +
    geom_vline(xintercept = f1_score$threshold, size = 1) +
    theme_tq() +
    scale_color_tq() +
    theme(
        legend.position = "right"
    ) +
    labs(
        title = "Expected Values",
        x = "Threshold",
        y = "Values"
    )

# 4. Expected Value ----

# 4.1 Calculating Expected Value With OT ----

source("00_Scripts/assess_attrition.R")

predictions_with_ot_tbl <- automl_leader %>%
    h2o.predict(newdata = as.h2o(test_tbl)) %>%
    as_tibble() %>%
    bind_cols(
        test_tbl %>%
            select(EmployeeNumber, MonthlyIncome, OverTime)
    )

ev_with_ot_tbl <- predictions_with_ot_tbl %>%
    mutate(
        attrition_cost = calculate_attrition_cost(
            n = 1
            , salary = MonthlyIncome * 12
            , net_revenue_per_employee = 250000
        )
    ) %>%
    mutate(
        cost_of_policy_change = 0
    ) %>%
    mutate(
        expected_attrition_cost = Yes * (attrition_cost + cost_of_policy_change)
        + No  * (cost_of_policy_change)
    )

total_ev_with_ot_tbl <- ev_with_ot_tbl %>%
    summarise(
        total_expected_attrition_cost_0 = sum(expected_attrition_cost, na.rm = TRUE)
    )

# 4.2 Calculating Expected Value With Targeted OT ----

max_f1_tbl <- rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    filter(f1 == max(f1)) %>%
    slice(1)

tnr <- max_f1_tbl$tnr
tpr <- max_f1_tbl$tpr
fnr <- max_f1_tbl$fnr
fpr <- max_f1_tbl$fpr
threshold <- max_f1_tbl$threshold

test_targeted_ot_tbl <- test_tbl %>%
    add_column(Yes = predictions_with_ot_tbl$Yes) %>%
    mutate(
        OverTime = case_when(
            Yes >= threshold ~ factor("No", levels = levels(test_tbl$OverTime))
            , TRUE ~ OverTime
        )
    ) %>%
    select(-Yes)

predictions_targeted_ot_tbl <- automl_leader %>%
    h2o.predict(newdata = as.h2o(test_targeted_ot_tbl)) %>%
    as_tibble() %>%
    bind_cols(
        test_tbl %>%
            select(EmployeeNumber, MonthlyIncome, OverTime),
        test_targeted_ot_tbl %>%
            select(OverTime)
    ) %>%
    rename(OverTime_0 = `OverTime...6`,
           Overtime_1 = `OverTime...7`)

avg_ot_pct <- 0.10
ev_targeted_ot_tbl <- predictions_targeted_ot_tbl %>%
    mutate(
        attrition_cost = calculate_attrition_cost(
            n = 1,
            salary = MonthlyIncome * 12,
            net_revenue_per_employee = 250000
        )
    ) %>%
    mutate(
        cost_of_policy_change = case_when(
            OverTime_0 == "Yes" & Overtime_1 == "No" ~ attrition_cost * avg_ot_pct
            , TRUE ~ 0
        )
    ) %>%
    mutate(
        cb_tn = cost_of_policy_change,
        cb_fp = cost_of_policy_change,
        cb_tp = cost_of_policy_change + attrition_cost,
        cb_fn = attrition_cost + cost_of_policy_change,
        expected_attrition_cost = 
            Yes * (tpr*cb_tp + fnr*cb_fn) +
            No  * (tnr*cb_tn + fpr*cb_fp)
    )

total_ev_targeted_ot_tbl <- ev_targeted_ot_tbl %>%
    summarise(
        total_expected_attrition_cost_state_1 = sum(expected_attrition_cost)
    )

# 4.3 Savings Calculation ----

savings_tbl <- bind_cols(
    total_ev_with_ot_tbl,
    total_ev_targeted_ot_tbl
) %>%
    mutate(
        savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_state_1,
        pct_savings = savings/total_expected_attrition_cost_0
    )


# 5. Optimizing By Threshold ----

# 5.1 Create calculate_savings_by_threshold() ----

calculate_savings_by_threshold <- function(data, h2o_model, threshold = 0,
                                           tnr = 0, fpr = 1, fnr = 0, tpr = 1) {
    
    
    data_0_tbl <- as_tibble(data)
    
    # 4. Expected Value 
    
    # 4.1 Calculating Expected Value With OT 
    
    pred_0_tbl <- h2o_model %>%
        h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
        as_tibble() %>%
        bind_cols(
            data_0_tbl %>%
                select(EmployeeNumber, MonthlyIncome, OverTime)
        )
    
    ev_0_tbl <- pred_0_tbl %>%
        mutate(
            attrition_cost = calculate_attrition_cost(
                n = 1,
                salary = MonthlyIncome * 12,
                net_revenue_per_employee = 250000)
        ) %>%
        mutate(
            cost_of_policy_change = 0
        ) %>%
        mutate(
            expected_attrition_cost = 
                Yes * (attrition_cost + cost_of_policy_change) +
                No *  (cost_of_policy_change)
        )
    
    
    total_ev_0_tbl <- ev_0_tbl %>%
        summarise(
            total_expected_attrition_cost_0 = sum(expected_attrition_cost)
        )
    
    # 4.2 Calculating Expected Value With Targeted OT
    
    data_1_tbl <- data_0_tbl %>%
        add_column(Yes = pred_0_tbl$Yes) %>%
        mutate(
            OverTime = case_when(
                Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)),
                TRUE ~ OverTime
            )
        ) %>%
        select(-Yes) 
    
    pred_1_tbl <- h2o_model %>%
        h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
        as_tibble() %>%
        bind_cols(
            data_0_tbl %>%
                select(EmployeeNumber, MonthlyIncome, OverTime),
            data_1_tbl %>%
                select(OverTime)
        ) %>%
        rename(
            OverTime_0 = `OverTime...6`,
            OverTime_1 = `OverTime...7`
        )
    
    
    avg_overtime_pct <- 0.10
    
    ev_1_tbl <- pred_1_tbl %>%
        mutate(
            attrition_cost = calculate_attrition_cost(
                n = 1,
                salary = MonthlyIncome * 12,
                net_revenue_per_employee = 250000)
        ) %>%
        mutate(
            cost_of_policy_change = case_when(
                OverTime_1 == "No" & OverTime_0 == "Yes" 
                ~ attrition_cost * avg_overtime_pct,
                TRUE ~ 0
            ))%>%
        mutate(
            cb_tn = cost_of_policy_change,
            cb_fp = cost_of_policy_change,
            cb_fn = attrition_cost + cost_of_policy_change,
            cb_tp = attrition_cost + cost_of_policy_change,
            expected_attrition_cost = Yes * (tpr*cb_tp + fnr*cb_fn) + 
                No * (tnr*cb_tn + fpr*cb_fp)
        )
    
    
    total_ev_1_tbl <- ev_1_tbl %>%
        summarise(
            total_expected_attrition_cost_1 = sum(expected_attrition_cost)
        )
    
    
    # 4.3 Savings Calculation
    
    savings_tbl <- bind_cols(
        total_ev_0_tbl,
        total_ev_1_tbl
    ) %>%
        mutate(
            savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
            pct_savings = savings / total_expected_attrition_cost_0
        )
    
    return(savings_tbl$savings)
    
}

max_f1_savings <- calculate_savings_by_threshold(data = test_tbl, h2o_model = automl_leader,
                               threshold = max_f1_tbl$threshold,
                               tnr = max_f1_tbl$tnr,
                               tpr = max_f1_tbl$tpr,
                               fnr = max_f1_tbl$fnr,
                               fpr = max_f1_tbl$fpr)

# 5.2 Optimization ----

smpl <- seq(1, nrow(rates_by_threshold_tbl), length.out = 20) %>% round()

partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = automl_leader)

rates_by_threshold_optimized_tbl <- rates_by_threshold_tbl %>%
    select(threshold, tnr:tpr) %>%
    slice(smpl) %>%
    mutate(
        savings = pmap_dbl(
            .l = list(
                threshold = threshold,
                tnr = tnr,
                tpr = tpr,
                fnr = fnr,
                fpr = fpr
            ),
            .f = partial(
                calculate_savings_by_threshold
                , data = test_tbl
                , h2o_model = automl_leader
            )
        )
    )

rates_by_threshold_optimized_tbl %>%
    ggplot(aes(x = threshold, y = savings)) +
    geom_line() +
    geom_point() +
    # Optimal Point
    geom_point(shape = 21, size = 5, color = "red",
               data = rates_by_threshold_optimized_tbl %>%
                   filter(savings == max(savings)) %>%
                   slice(1)) +
    geom_label(aes(label = scales::dollar(savings)),
               data = rates_by_threshold_optimized_tbl %>%
                   filter(savings == max(savings)) %>%
                   slice(1),
               vjust = -1, color = "red") +
    
    # F1 Max
    geom_vline(xintercept = max_f1_tbl$threshold,
               color = "blue", size = 2, alpha = .382) +
    annotate(geom = "label", label = scales::dollar(max_f1_savings),
             x = max_f1_tbl$threshold,
             y = max_f1_savings,
             vjust = -1, color = "red") +
    
    # No OT Policy
    geom_point(shape = 21, size = 5, color = "red",
               data = rates_by_threshold_optimized_tbl %>%
                   filter(threshold == min(threshold)) %>%
                   slice(1)) +
    geom_label(aes(label = scales::dollar(savings)),
               data = rates_by_threshold_optimized_tbl %>%
                   filter(threshold == min(threshold)) %>%
                   slice(1),
               vjust = -1, color = "red") +
    
    # Do nothing Policy
    geom_point(shape = 21, size = 5, color = "red",
               data = rates_by_threshold_optimized_tbl %>%
                   filter(threshold == max(threshold)) %>%
                   slice(1)) +
    geom_label(aes(label = scales::dollar(savings)),
               data = rates_by_threshold_optimized_tbl %>%
                   filter(threshold == max(threshold)) %>%
                   slice(1),
               vjust = -1, color = "red") +
    
    # Aesthetics
    scale_y_continuous(labels = scales::dollar) +
    scale_x_continuous(labels = scales::percent,
                       breaks = seq(0, 1, by = 0.2)) +
    theme_tq() +
    expand_limits(x = c(-.01, 1.01), y = 8e5) +
    labs(
        title = "Optimization Results: Expected Savings Maximized At 13.1%",
        y = "Threshold",
        x = "Savings"
    )


# 6 Sensitivity Analysis ----

# 6.1 Create calculate_savings_by_threshold_2() ----

calculate_savings_by_threshold_2 <- function(data, h2o_model, threshold = 0,
                                             tnr = 0, fpr = 1, fnr = 0, tpr = 1,
                                             avg_overtime_pct = 0.10,
                                             net_revenue_per_employee = 250000){
    
    data_0_tbl <- as.tibble(data)
    
    
    # 4. Expected Value 
    
    # 4.1 Calculating Expected Value With OT 
    
    pred_0_tbl <- h2o_model %>%
        h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
        as.tibble() %>%
        bind_cols(
            data_0_tbl %>%
                select(EmployeeNumber, MonthlyIncome, OverTime)
        )
    
    ev_0_tbl <- pred_0_tbl %>%
        mutate(
            attrition_cost = calculate_attrition_cost(
                n = 1,
                salary = MonthlyIncome * 12,
                # Changed in _2 ----
                net_revenue_per_employee = net_revenue_per_employee) 
        ) %>%
        mutate(
            cost_of_policy_change = 0
        ) %>%
        mutate(
            expected_attrition_cost = 
                Yes * (attrition_cost + cost_of_policy_change) +
                No *  (cost_of_policy_change)
        )
    
    
    total_ev_0_tbl <- ev_0_tbl %>%
        summarise(
            total_expected_attrition_cost_0 = sum(expected_attrition_cost)
        )
    
    # 4.2 Calculating Expected Value With Targeted OT
    
    data_1_tbl <- data_0_tbl %>%
        add_column(Yes = pred_0_tbl$Yes) %>%
        mutate(
            OverTime = case_when(
                Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)),
                TRUE ~ OverTime
            )
        ) %>%
        select(-Yes) 
    
    pred_1_tbl <- h2o_model %>%
        h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
        as.tibble() %>%
        bind_cols(
            data_0_tbl %>%
                select(EmployeeNumber, MonthlyIncome, OverTime),
            data_1_tbl %>%
                select(OverTime)
        )  %>%
        rename(
            OverTime_0 = `OverTime...6`,
            OverTime_1 = `OverTime...7`
        )
    
    
    avg_overtime_pct <- avg_overtime_pct # Changed in _2 ----
    
    ev_1_tbl <- pred_1_tbl %>%
        mutate(
            attrition_cost = calculate_attrition_cost(
                n = 1,
                salary = MonthlyIncome * 12,
                # Changed in _2 ----
                net_revenue_per_employee = net_revenue_per_employee)
        ) %>%
        mutate(
            cost_of_policy_change = case_when(
                OverTime_1 == "No" & OverTime_0 == "Yes" 
                ~ attrition_cost * avg_overtime_pct,
                TRUE ~ 0
            ))%>%
        mutate(
            cb_tn = cost_of_policy_change,
            cb_fp = cost_of_policy_change,
            cb_fn = attrition_cost + cost_of_policy_change,
            cb_tp = attrition_cost + cost_of_policy_change,
            expected_attrition_cost = Yes * (tpr*cb_tp + fnr*cb_fn) + 
                No * (tnr*cb_tn + fpr*cb_fp)
        )
    
    
    total_ev_1_tbl <- ev_1_tbl %>%
        summarise(
            total_expected_attrition_cost_1 = sum(expected_attrition_cost)
        )
    
    
    # 4.3 Savings Calculation
    
    savings_tbl <- bind_cols(
        total_ev_0_tbl,
        total_ev_1_tbl
    ) %>%
        mutate(
            savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
            pct_savings = savings / total_expected_attrition_cost_0
        )
    
    return(savings_tbl$savings)
    
}

# 6.2 Sensitivity Analysis ----

max_savings_rates_tbl <- rates_by_threshold_optimized_tbl %>%
    filter(savings == max(savings))

calculate_savings_by_threshold_2(
    data = test_tbl,
    h2o_model = automl_leader,
    threshold = max_savings_rates_tbl$threshold,
    tnr = max_savings_rates_tbl$tnr,
    tpr = max_savings_rates_tbl$tpr,
    fnr = max_savings_rates_tbl$fnr,
    fpr = max_savings_rates_tbl$fpr
)

calculate_savings_by_threshold_2_preloaded <- partial(
    .f = calculate_savings_by_threshold_2,
    data = test_tbl,
    h2o_model = automl_leader,
    threshold = max_savings_rates_tbl$threshold,
    tnr = max_savings_rates_tbl$tnr,
    tpr = max_savings_rates_tbl$tpr,
    fnr = max_savings_rates_tbl$fnr,
    fpr = max_savings_rates_tbl$fpr
)

calculate_savings_by_threshold_2_preloaded(
    avg_overtime_pct = .10, 
    net_revenue_per_employee = 250000
)

sensitivity_tbl <- list(
    avg_overtime_pct = seq(0.05, 0.30, by = 0.05),
    net_revenue_per_employee = seq(200000, 400000, by = 50000)
) %>%
    cross_df() %>%
    mutate(
        savings = pmap_dbl(
            .l = list(
                avg_overtime_pct = avg_overtime_pct,
                net_revenue_per_employee = net_revenue_per_employee
            ),
            .f = calculate_savings_by_threshold_2_preloaded
        )
    )
