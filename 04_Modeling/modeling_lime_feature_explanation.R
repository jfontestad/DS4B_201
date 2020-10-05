# LIME FEATURE EXPLANATION ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(lime)

# Load Data
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
test_raw_tbl        <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE) %>%
    set_names("X__1","X__2")

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing Recipe 
factor_names <- c("JobLevel","StockOptionLevel")

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_mutate_at(factor_names, fn = as.factor) %>%
    #step_num2factor(JobLevel, StockOptionLevel) %>%
    prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Models ----

h2o.init()

automl_leader <- h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_AutoML_20201001_142408")
automl_leader


# LIME --------------------------------------------------------------------

# 3.1 Making Predictions ----

predictions_tbl <- automl_leader %>%
    h2o.predict(newdata = as.h2o(test_tbl)) %>%
    as_tibble() %>%
    bind_cols(
        test_tbl %>%
            select(Attrition, EmployeeNumber)
    )

test_tbl %>%
    slice(5) %>%
    glimpse()


# 3.2 Single Explanation --------------------------------------------------

explainer_obj <- train_tbl %>%
    select(-Attrition) %>%
    lime(
        model            = automl_leader
        , bin_continuous = TRUE
        , n_bins         = 4
        , quantile_bins  = TRUE
    ) 

explanation <- test_tbl %>%
    slice(5) %>%
    select(-Attrition) %>%
    lime::explain(
        explainer        = explainer_obj
        , n_labels       = 1
        , n_features     = 8
        , n_permutations = 5000
        , kernal_width   = 0.05
    )

explanation %>%
    as_tibble() %>%
    select(feature:prediction)

plot_features(explanation = explanation, ncol = 1)


# 3.3 Multiple Explanations -----------------------------------------------
explanation <- test_tbl %>%
    slice(1:20) %>%
    select(-Attrition) %>%
    lime::explain(
        explainer        = explainer_obj
        , n_labels       = 1
        , n_features     = 8
        , n_permutations = 5000
        , kernal_width   = 0.05
    )

plot_features(explanation = explanation, ncol = 4)

plot_explanations(explanation = explanation)

# Shutdown h2o ------------------------------------------------------------

h2o.shutdown()
