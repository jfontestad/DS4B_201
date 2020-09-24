
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
    extract_h20_model_name_by_position(n = 6) %>%
    h2o.getModel() %>%
    h2o.saveModel(path = "04_Modeling/h2o_models/")

h2o.loadModel(path = "04_Modeling/h2o_models/DeepLearning_grid__1_AutoML_20200921_150741_model_1")

# Make Predictions ----
stacked_ensemble_h20 <- h2o.loadModel(path = "04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_AutoML_20200921_150741")

predictions <- h2o.predict(
    stacked_ensemble_h20,
    newdata = as.h2o(test_tbl)
)
predictions_tbl <- predictions %>%
    as_tibble()

stacked_ensemble_h20@allparameters


# Viz the leader board ----------------------------------------------------

data_transformed_tbl <- automl_models_h20@leaderboard %>%
    as_tibble() %>%
    mutate(model_type = str_split(model_id, "_", simplify = TRUE)[,1]) %>%
    slice(1:10) %>%
    rownames_to_column() %>%
    mutate(
        model_id = as_factor(model_id) %>% fct_reorder(auc)
        , model_type = as.factor(model_type)
    ) %>%
    select(rowname, model_id, auc, logloss, model_type) %>%
    pivot_longer(
        cols = c(auc, logloss)
        , names_to = "key"
        , values_to = "value"
        , names_ptypes = list(key = factor())
    )

data_transformed_tbl %>%
    ggplot(
        aes(
            x = value
            , y = model_id
            , color = model_type
        )
    ) +
    geom_point(size = 3) +
    geom_label(
        aes(
            label = round(value, 2)
            , hjust = "inward"
        )
    ) +
    facet_wrap(~ key, scales = "free_x") +
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "H2O Model Leader Board Metrics"
        , subtitle = "Order By AUC"
        , x = ""
        , y = ""
    )

plot_h2o_visualization <- function(
    h2o_leaderboard
    , order_by = c("auc","logloss","aucpr","mean_per_class_error","rmse","mse")
    , metrics_list = c("auc","logloss","aucpr","mean_per_class_error","rmse","mse")
    , n_max = 20
    , size = 4
    , include_lbl = TRUE
) {
    
    # Setup Inputs
    order_by     <- tolower(order_by[[1]])
    metrics_list <- tolower(metrics_list)
    
    leaderboard_tbl <- h2o_leaderboard %>%
        as_tibble() %>%
        mutate(model_type = str_split(model_id, "_", simplify = TRUE)[, 1]) %>%
        rownames_to_column(var = "rowname") %>%
        mutate(model_id = paste0(
            rowname
            , ". "
            , as.character(model_id)
            ) 
            %>% as.factor()
        ) %>%
        select(rowname, model_id, model_type, metrics_list, order_by)
    
    # Transformation
    if(order_by == "auc") {
        
        data_transformed_tbl <- leaderboard_tbl %>%
            slice(1:n_max) %>%
            mutate(
                model_id = as_factor(model_id) %>% 
                    fct_reorder(auc)
                , model_type = as.factor(model_type)
            ) %>%
            pivot_longer(
                cols = c(-model_id, -model_type, -rowname)
                , names_to = "key"
                , values_to = "value"
                , names_ptypes = list(key = factor())
            )
        
    } else if (order_by == "logloss") {
        data_transformed_tbl <- leaderboard_tbl %>%
            slice(1:n_max) %>%
            mutate(
                model_id = as_factor(model_id) %>% 
                    fct_reorder(logloss) %>% 
                    fct_rev()
                , model_type = as.factor(model_type)
            ) %>%
            pivot_longer(
                cols = c(-model_id, -model_type, -rowname)
                , names_to = "key"
                , values_to = "value"
                , names_ptypes = list(key = factor())
            )
    } else {
        stop(paste0("order_by = '", order_by, " is not currently supported. Use auc or logloss"))
    }
    
    # Viz
    g <- data_transformed_tbl %>%
        ggplot(
            aes(
                x = value
                , y = model_id
                , color = model_type
            )
        ) +
        geom_point(size = size) +
        facet_wrap(~ key, scales = "free_x") +
        theme_tq() +
        scale_color_tq() +
        labs(
            title = "H2O Leaderboard Metrics"
            , subtitle = paste0("Ordered by: ", toupper(order_by))
            , y = "Model Position and ID"
            , x = ""
        )
    
    if(include_lbl) g <- g +
            geom_label(
                aes(
                    label = round(value, 2)
                    , hjust = "inward"
                )
            )
    
    return(g)
}

plot_h2o_visualization(
    automl_models_h20@leaderboard
    , order_by = c("auc")
    , metrics_list = c("auc","aucpr")
    , size = 3
)
