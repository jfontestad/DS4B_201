# RECOMMENDATION ALGORITHM ----

# 1.0 Setup ----

# Libraries
library(readxl)
library(tidyverse)
library(tidyquant)
library(recipes)    # Make sure v0.1.3 or later is installed. If not restart & install.packages("recipes") to update.


# Data
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
test_raw_tbl        <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE) %>%
    rename(`X__1` = `...1`) %>%
    rename(`X__2` = `...2`)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)



# 2.0 Correlation Analysis - Machine Readable ----
source("00_Scripts/plot_cor.R")

# 2.1 Recipes ----
# Factor Names
factor_names <- c("JobLevel","StockOptionLevel")

# Recipe
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_mutate_at(factor_names, fn = as.factor) %>%
    step_discretize(all_numeric(), options = list(min_unique = 1)) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    prep()

train_corr_tbl <- bake(recipe_obj, train_readable_tbl)
tidy(recipe_obj, 3)


# 2.2 Correlation Visualization ----

# Manipulate
train_corr_tbl %>%
    glimpse()

corr_level <- 0.06

correlation_results_tbl <- train_corr_tbl %>%
    select(-Attrition_No) %>%
    get_cor(target = Attrition_Yes, fct_reorder = TRUE, fct_rev = TRUE) %>%
    filter(abs(Attrition_Yes) >= corr_level) %>%
    mutate(
        relationship = case_when(
            Attrition_Yes > 0 ~ "Supports Attrition"
            , TRUE ~ "Contradicts Attrition"
        )
    ) %>%
    mutate(feature_text = as.character(feature)) %>%
    separate(feature_text, into = "feature_base", sep = "_", extra = "drop") %>%
    mutate(feature_base = as_factor(feature_base) %>% fct_rev())

length_unique_groups <- correlation_results_tbl %>% 
    pull(feature_base) %>%
    unique() %>%
    length()

# Create viz
correlation_results_tbl %>%
    ggplot(
        aes(
            x = Attrition_Yes,
            y = feature_base,
            color = relationship
        )
    ) +
    geom_point() +
    geom_label(
        aes(
            label = feature
        )
        , vjust = -0.5
    ) +
    expand_limits(x = c(-0.3, 0.3), y = c(1, length_unique_groups + 1)) +
    theme_tq() +
    scale_color_tq() +
    labs(
        color = "Relationship"
        , title = "Correlation Analysis: Recommendation Strategy Development"
        , subtitle = "Discretizing featues to help identify a strategy"
        , y = ""
        , x = "Attrition Yes"
    )




# 3.0 Recommendation Strategy Development Worksheet ----




# 4.0 Recommendation Algorithm Development ----

# 4.1 Personal Development (Mentorship, Education) ----


# 4.2 Professional Development (Promotion Readiness) ----


# 4.3 Work Life Balance ----




