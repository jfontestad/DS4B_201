
# Data Preperation --------------------------------------------------------
# Machine Readable ----

# Lib Load ----------------------------------------------------------------

if(!require(pacman)) {install.packages("pacman")}
pacman::p_load(
    "recipes"
    , "readxl"
    , "tidyverse"
    , "tidyquant"
)


# Data Load ---------------------------------------------------------------

path_train <- "00_Data/telco_train.xlsx"
path_test <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
test_raw_tbl <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE) %>%
    set_names("X__1","X__2")


# Processing Pipeline -----------------------------------------------------

source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)


# Plot Faceted Histogram Function -----------------------------------------
.data <- train_raw_tbl
plot_hist_facet <- function(.data, .bins = 10, .ncol = 5, .fct_reorder = FALSE,
                            .fct_rev = FALSE, .fill = palette_light()[[3]],
                            .color = "white", .scale = "free") {
    
    data_factored <- .data %>%
        mutate_if(is.character, as.factor) %>%
        mutate_if(is.factor, as.numeric) %>%
        gather(key = key, value = value, factor_key = TRUE)
    
    if (.fct_reorder) {
        data_factored <- data_factored %>%
            mutate(key = as.character(key) %>% as.factor())
    }
    
    if (.fct_reorder) {
        data_factored <- data_factored %>%
            mutate(key = fct_rev(key))
    }
    
    g <- data_factored %>%
        ggplot(aes(x = value, group = key)) +
        geom_histogram(bins = .bins, fill = .fill, color = .color) +
        facet_wrap(~ key, ncol = .ncol, scale = .scale) +
        theme_tq()
    
    return(g)
    
}

train_raw_tbl %>%
    select(Attrition, everything()) %>%
    plot_hist_facet(.bins = 10, .ncol = 5)


# Data Preprocess with recipies -------------------------------------------

# Plan
# 1. Impute / Zero Var Features ----
rec_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors())

# 2. Transformations ----

skewed_feature_names <- train_readable_tbl %>%
    select_if(is.numeric) %>%
    map_df(skewness) %>%
    pivot_longer(cols = everything(), names_to = "key") %>%
    arrange(desc(value)) %>%
    filter(value >= 0.8) %>%
    filter(!key %in% c("JobLevel","StockOptionLevel")) %>%
    # You can also kick out those with a skewness score of
    # -0.5 <= x <= 0.5 if desired
    pull(key) %>%
    as.character()

!skewed_feature_names %in% c("JobLevel","StockOptionLevel")

train_readable_tbl %>%
    select(all_of(skewed_feature_names)) %>%
    plot_hist_facet()

factor_names <- c("JobLevel","StockOptionLevel")

rec_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_YeoJohnson(skewed_feature_names) %>%
    step_mutate_at(factor_names, fn = as.factor)

# 3. Discretize ----
# 4. Normalization / Center & Scaling ----
train_readable_tbl %>%
    select_if(is.numeric) %>%
    plot_hist_facet()

rec_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_YeoJohnson(skewed_feature_names) %>%
    step_mutate_at(factor_names, fn = as.factor) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric())

prepared_recipe <- prep(rec_obj)
prepared_recipe$steps[[4]]

prepared_recipe %>%
    bake(new_data = train_readable_tbl) %>%
    select_if(is.numeric) %>%
    plot_hist_facet()

# 5. Dummy Var ----
rec_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_YeoJohnson(skewed_feature_names) %>%
    step_mutate_at(factor_names, fn = as.factor) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_dummy(all_nominal())

# 6. Interaction Variable / Engineered Features ----
# 7. Multivariate Transformation ----


# Final Recipe ------------------------------------------------------------

rec_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_YeoJohnson(skewed_feature_names) %>%
    step_mutate_at(factor_names, fn = as.factor) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_dummy(all_nominal()) %>%
    prep()

# Train Data on
train_tbl <- bake(rec_obj, new_data = train_readable_tbl)

# Holdout set
test_tbl <- bake(rec_obj, new_data = test_readable_tbl)


# Correlation Analysis ----------------------------------------------------

get_cor <- function(data, target, use = "pairwise.complete.obs",
                      fct_reorder = FALSE, fct_rev = FALSE) {
    
    feature_expr <- rlang::enquo(target)
    feature_name <- rlang::quo_name(feature_expr)
    
    data_cor <- data %>%
        mutate_if(is.character, as.factor) %>%
        mutate_if(is.factor, as.numeric) %>%
        cor(use = use) %>%
        as_tibble() %>%
        mutate(feature = names(.)) %>%
        select(feature, !! feature_expr) %>%
        filter(!(feature == feature_name)) %>%
        mutate_if(is.character, as_factor)
    
    if(fct_reorder) {

        data_cor <- data_cor %>%
            mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
            arrange(feature)

    }

    if(fct_rev) {

        data_cor <- data_cor %>%
            mutate(feature = fct_rev(feature)) %>%
            arrange(feature)

    }
    
    return(data_cor)
    
}
get_cor(train_tbl, Attrition_Yes)

train_tbl %>%
    get_cor(target = Attrition_Yes, fct_reorder = TRUE, fct_rev = TRUE)

plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE,
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1,
                     color_pos = palette_light()[[1]],
                     color_neg = palette_light()[[2]]) {
    
    feature_expr <- enquo(target)
    feature_name <- quo_name(feature_expr)
    
    data_cor <- data %>%
        get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
        mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
        mutate(Correlation = case_when(
            (!! feature_expr) >= 0 ~ "Positive"
            , TRUE ~ "Negative"
        ) %>%
            as.factor()
        )
    
    g <- data_cor %>%
        ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
        geom_point(aes(color = Correlation), size = size) +
        geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
        geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
        expand_limits(x = c(-1,1)) +
        theme_tq() +
        scale_color_manual(values = c(color_neg, color_pos))
    
    if(include_lbl) g <- g + geom_label(
        aes(
            label = feature_name_text
        ),
        hjust = lbl_position
    )
    
    return(g)
}

train_tbl %>%
    select(Attrition_Yes, contains("satisfaction")) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)


# Correlation Evaluation --------------------------------------------------

# Explore Features by Category
train_tbl %>%
    select(Attrition_Yes, contains("jobrole")) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(Attrition_Yes, contains("joblevel")) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(Attrition_Yes
           , contains("jobinvolvement")
           , contains("jobsatisfaction")
           , contains("department")
           , contains("employeenumber")
          ) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(
        Attrition_Yes
        , contains("rate")
    ) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(
        Attrition_Yes
        , contains("income")
    ) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(Attrition_Yes
           , contains("life")) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(
        Attrition_Yes
        , contains("jobsatisfaction")
    ) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(Attrition_Yes, contains("performancerating")) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(
        Attrition_Yes
        , contains("jobinvolvement")
    ) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(
        Attrition_Yes
        , contains("worklifebalance")
    ) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(
        Attrition_Yes
        , contains("Education_")
    ) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(
        Attrition_Yes
        , contains("training")
    ) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(
        Attrition_Yes
        , contains("years")
    ) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(Attrition_Yes
           , contains("income")
           , contains("rate")
           , contains("salary")
           , contains("stock")
           ) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(
        Attrition_Yes
        , contains("gender")
        , contains("maritalstatus")
        , contains("numcompanies")
        , contains("Over18")
        , contains("DistanceFr")
    ) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(Attrition_Yes, contains("Satisfaction"), contains("life")) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(Attrition_Yes, contains("performance"), contains("involvement")) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(Attrition_Yes, contains("overtime"), contains("travel")) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(Attrition_Yes, contains("training"), contains("education")) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>%
    select(Attrition_Yes, contains("years")) %>%
    plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

