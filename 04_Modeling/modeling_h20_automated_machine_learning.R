
# H20 Modeling ------------------------------------------------------------


# 1. Setup ----------------------------------------------------------------


# Lib Load ----------------------------------------------------------------


if(!require(pacman)) {install.packages("pacman")}
pacman::p_load(
    "h2o"
    ,"recipes"
    ,"readxl"
    ,"tidyverse"
    ,"stringr"
    ,"forcats"
    ,"cowplot"
    ,"fs"
    ,"glue"
)


# Load Data ---------------------------------------------------------------

path_train <- "00_Data/telco_train.xlsx"
path_test  <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
test_raw_tbl  <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE) %>%
    set_names("X__1","X__2")


# Processing Pipeline -----------------------------------------------------

source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Processing -----------------------------------------------------------

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_mutate_at(JobLevel, StockOptionLevel, fn = as.factor) %>%
    prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)


# 2. Modeling -------------------------------------------------------------

h2o.init()

split_h2o <- train_tbl %>%
    as.h2o() %>%
    h2o.splitFrame(ratios = c(0.85), seed = 1234)

train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- test_tbl %>% as.h2o()

y <- "Attrition"
x <- setdiff(names(train_h2o), y)

automl_models_h20 <- h2o.automl(
    x = x,
    y = y,
    training_frame = train_h2o,
    validation_frame = valid_h2o,
    leaderboard_frame = test_h2o,
    max_runtime_secs = 30,
    nfolds = 5
)

automl_models_h20@leaderboard %>%
    as_tibble() %>%
    slice(1) %>%
    pull(model_id) %>%
    h2o.getModel()

extract_h20_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = TRUE) {
    
    model_name <- h2o_leaderboard %>%
        as_tibble() %>%
        slice(n) %>%
        pull(model_id)
    
    if(verbose) message(model_name)
    
    return(model_name)
    
}

automl_models_h20@leaderboard %>%
    extract_h20_model_name_by_position(n = 1) %>%
    h2o.getModel()
