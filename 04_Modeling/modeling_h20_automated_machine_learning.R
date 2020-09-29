
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

extract_h20_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = TRUE) {
    
    model_name <- h2o_leaderboard %>%
        as_tibble() %>%
        slice(n) %>%
        pull(model_id)
    
    if(verbose) message(model_name)
    
    return(model_name)
    
}

# automl_models_h20@leaderboard %>%
#     extract_h20_model_name_by_position(n = 6) %>%
#     h2o.getModel() %>%
#     h2o.saveModel(path = "04_Modeling/h2o_models/")

# Make Predictions ----
stacked_ensemble_h20 <- h2o.loadModel(path = "04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_AutoML_20200921_150741")

predictions <- h2o.predict(
    stacked_ensemble_h20,
    newdata = as.h2o(test_tbl)
)
predictions_tbl <- predictions %>%
    as_tibble()

# stacked_ensemble_h20@allparameters


# 3. Viz the leader board ----------------------------------------------------

# data_transformed_tbl <- automl_models_h20@leaderboard %>%
#     as_tibble() %>%
#     mutate(model_type = str_split(model_id, "_", simplify = TRUE)[,1]) %>%
#     slice(1:10) %>%
#     rownames_to_column() %>%
#     mutate(
#         model_id = as_factor(model_id) %>% fct_reorder(auc)
#         , model_type = as.factor(model_type)
#     ) %>%
#     select(rowname, model_id, auc, logloss, model_type) %>%
#     pivot_longer(
#         cols = c(auc, logloss)
#         , names_to = "key"
#         , values_to = "value"
#         , names_ptypes = list(key = factor())
#     )
# 
# data_transformed_tbl %>%
#     ggplot(
#         aes(
#             x = value
#             , y = model_id
#             , color = model_type
#         )
#     ) +
#     geom_point(size = 3) +
#     geom_label(
#         aes(
#             label = round(value, 2)
#             , hjust = "inward"
#         )
#     ) +
#     facet_wrap(~ key, scales = "free_x") +
#     theme_tq() +
#     scale_color_tq() +
#     labs(
#         title = "H2O Model Leader Board Metrics"
#         , subtitle = "Order By AUC"
#         , x = ""
#         , y = ""
#     )

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
        select(rowname, model_id, model_type, all_of(metrics_list), all_of(order_by))
    
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
    , n_max = 10
)


# Grid Search -------------------------------------------------------------

# h2o.performance(stacked_ensemble_h20, newdata = test_tbl %>% as.h2o())
stacked_ensemble_grid_01 <- h2o.grid(
    algorithm = "deeplearning"
    , x = x
    , y = y
    , grid_id = "stacked_ensemble_grid_01"
    , training_frame = train_h2o
    , validation_frame = valid_h2o
    , nfolds = 5
)


# 4. Assessing Performance ------------------------------------------------
deep_learning_model <- h2o.loadModel("04_Modeling/h2o_models/DeepLearning_grid__1_AutoML_20200921_150741_model_1")
glm_model <- h2o.loadModel("04_Modeling/h2o_models/GLM_1_AutoML_20200921_150741")
stacked_ensemble_model <- h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_AutoML_20200921_150741")

performance_h20 <- h2o.performance(deep_learning_model, newdata = test_h2o)
performance_h20 %>% slotNames()
performance_h20@metrics


# Classifier Summary Metrics ----------------------------------------------

h2o.auc(performance_h20)
h2o.giniCoef(performance_h20)
h2o.logloss(performance_h20)
h2o.confusionMatrix(performance_h20)
h2o.confusionMatrix(stacked_ensemble_model)

# 5. Performance Dashboard ------------------------------------------------

plot_h2o_performance <- function(
    h2o_leadergoard
    , newdata
    , order_by = c("auc","logloss")
    , max_models = 3
    , size = 1.5
) {
    
    # Inputs
    leaderboard_tbl <- h2o_leaderboard %>%
        as_tibble() %>%
        slice(1:max_models)
    
    newdata_tbl <- newdata %>%
        as_tibble()
    
    order_by <- tolower(order_by[[1]])
    order_by_expr <- rlang::sym(order_by)
    
    h2o.no_progress()
    
    # 1. Model Metrics ----
    get_model_performance_metrics <- function(model_id, test_tbl) {
        
        model_h2o <- h2o.getModel(model_id)
        perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
        
        perf_h2o %>%
            h2o.metric() %>%
            as_tibble() %>%
            select(threshold, tpr, fpr, precision, recall)
        
    }
    
    model_metrics_tbl <- leaderboard_tbl %>%
        mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>%
        unnest(metrics) %>%
        mutate(
            model_id = as_factor(model_id) %>%
                fct_reorder({{ order_by_expr }}, .desc = ifelse(order_by == "auc", TRUE, FALSE))
            , auc = auc %>%
                round(3) %>%
                as.character() %>%
                as_factor() %>%
                fct_reorder(as.numeric(model_id))
            , logloss = logloss %>%
                round(4) %>%
                as.character() %>%
                as_factor() %>%
                fct_reorder(as.numeric(model_id))
        )
    
    # 1A. ROC Plot ----
    p1 <- model_metrics_tbl %>%
        ggplot(
            mapping = aes_string(
                "fpr","tpr", color = "model_id", linetype = order_by
            )
        ) +
        geom_line(size = size) +
        theme_tq() +
        scale_color_tq() +
        labs(
            title = "ROC"
            , subtitle = "True Positive Rate vs False Positive Rate"
            , x = "FPR"
            , y = "TPR"
        ) +
        theme(legend.direction = "vertical")
    
    # 1B. Precision vs Recall ----
    p2 <- model_metrics_tbl %>%
        ggplot(
            mapping = aes_string(
                x = "recall"
                , y = "precision"
                , color = "model_id"
                , linetype = order_by
            )
        ) +
        geom_line(size = size) +
        geom_vline(
            xintercept = h2o.find_threshold_by_max_metric(performance_h20, "f1")
            , color = "green"
            , size = size
            , linetype = "dashed"
        ) +
        theme_tq() +
        scale_color_tq() +
        labs(
            title = "Precision vs Recall"
            , subtitle = "Green Line indicates Best Threshold"
            , x = "Recall"
            , y = "Precision"
        ) +
        theme(legend.position = "none")
    
    # 2. Gain / Lift ----
    
    get_gain_lift <- function(model_id, test_tbl) {
        
        model_h2o <- h2o.getModel(model_id)
        perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
        
        perf_h2o %>%
            h2o.gainsLift() %>%
            as_tibble() %>%
            select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)
        
    }
    
    gain_lift_tbl <- leaderboard_tbl %>%
        mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>%
        unnest(metrics) %>%
        mutate(
            model_id = as_factor(model_id) %>%
                fct_reorder({{ order_by_expr }}, .desc = ifelse(order_by == "auc", TRUE, FALSE))
            , auc = auc %>%
                round(3) %>%
                as.character() %>%
                as_factor() %>%
                fct_reorder(as.numeric(model_id))
            , logloss = logloss %>%
                round(4) %>%
                as.character() %>%
                as_factor() %>%
                fct_reorder(as.numeric(model_id))
        ) %>%
        rename(
            gain = cumulative_capture_rate
            , lift = cumulative_lift
        )
    
    # 2A. Gain Plot ----
    
    p3 <- gain_lift_tbl %>%
        ggplot(
            mapping = aes_string(
                x = "cumulative_data_fraction"
                , y = "gain"
                , color = "model_id"
                , linetype = order_by
            )
        ) +
        geom_line(size = size) +
        geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "black", size = size) +
        theme_tq() +
        scale_color_tq() +
        expand_limits(x = c(0,1), y = c(0,1)) +
        labs(
            title = "Gain"
            , subtitle = "Cumulative Lift"
            , x = "Cumulative Data Fraction"
            , y = "Gain"
        ) +
        theme(legend.position = "none")
    
    # 2B. Lift Plot ----
    
    p4 <- gain_lift_tbl %>%
        ggplot(
            mapping = aes_string(
                x = "cumulative_data_fraction"
                , y = "lift"
                , color = "model_id"
                , linetype = order_by
            )
        ) +
        geom_line(size = size) +
        geom_segment(x = 0, y = 1, xend = 1, yend = 1, color = "black", size = size) +
        theme_tq() +
        scale_color_tq() +
        expand_limits(x = c(0,1), y = c(0,1)) +
        labs(
            title = "Lift"
            , subtitle = "Model Value Add vs Baseline"
            , x = "Cumulative Data Fraction"
            , y = "Lift"
        ) +
        theme(legend.position = "none")
    
    # Combine using cowplot ----
    
    p_legend <- get_legend(p1)
    p1 <- p1 + theme(legend.position = "none")
    
    p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2)
    
    p_title <- ggdraw() +
        draw_label(
            "H2O Model Metrics"
            , size = 18
            , fontface = "bold"
            , color = palette_light()[[1]]
        )
    
    p_subtitle <- ggdraw() +
        draw_label(
            glue("Ordered by {toupper(order_by)}")
            , size = 10
            , color = palette_light()[[1]]
        )
    
    ret <- plot_grid(
        p_title
        , p_subtitle
        , p
        , p_legend
        , ncol = 1
        , rel_heights = c(0.05, 0.05, 1, 0.05 * max_models)
    )
    
    h2o.show_progress()
    
    return(ret)
    
}

plot_h2o_performance(
    h2o_leadergoard = automl_models_h20@leaderboard
    , newdata = test_tbl
    , order_by = "auc"
    , max_models = 3
)
