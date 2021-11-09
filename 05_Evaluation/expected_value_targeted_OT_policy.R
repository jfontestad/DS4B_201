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

# 4.3 Savings Calculation ----



# 5. Optimizing By Threshold ----

# 5.1 Create calculate_savings_by_threshold() ----

# 5.2 Optimization ----



# 6 Sensitivity Analysis ----

# 6.1 Create calculate_savings_by_threshold_2() ----

# 6.2 Sensitivity Analysis ----
