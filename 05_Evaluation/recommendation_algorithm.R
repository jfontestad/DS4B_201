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

# YearsAtCompany
#  YAC - High - Likely to stay / YAC - Low - More Likely to leave
#  Tie promotion if low to advance faster

# TotalWorkingYears
#  TWY - High - Likely to stay / TWY - Low - More likely to leave
#  Tie low TWY to training and formation activities

# YearsInCurrentRole
#  More time in current role related to lower attrition	
#  Incentivize specialization or promote

# JobInvolvement	
#  High JI - Likely to stay	
#  create personal development plan

# JobSatisfaction	
#  Low JS - More likely to leave	
#  Low: create personal development

# PerformanceRating
#  Low: Personal Development Plan/ High Seek Leadership or Mentorship Roles

# Good Better Best Approach

# (Worst Case) Create Personal Development Plan: JobInvolvement, JobSatisfaction, PerformaceRating

# (Better Case) Promote Training and Formation: YearsAtCompany, TotalWorkingYears

# (Best Case 1) Seek Mentorship Role: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction

# (Best Case 2) Seek Leadership Role: JobInvolvement, JobSatisfaction, PerformanceRating

train_readable_tbl %>%
    select(YearsAtCompany, TotalWorkingYears, YearsInCurrentRole, JobInvolvement,
           JobSatisfaction, PerformanceRating) %>%
    mutate_if(is.factor, as.numeric) %>%
    mutate(
        personal_development_strategy = case_when(
            # (Worst Case) Create Personal Development Plan: JobInvolvement, JobSatisfaction, PerformaceRating
            PerformanceRating == 1 | 
                JobSatisfaction == 1 |
                JobInvolvement <= 2 ~ "Create Personal Development Plan",
            
            # (Better Case) Promote Training and Formation: YearsAtCompany, TotalWorkingYears
            YearsAtCompany < 3 |
                TotalWorkingYears < 6 ~ "Promote Training and Formation",
            
            # (Best Case 1) Seek Mentorship Role: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction
            (YearsInCurrentRole > 3 | YearsAtCompany >= 5) &
                PerformanceRating >= 3 &
                JobSatisfaction == 4 ~ "Seek Mentorship Role",
            
            # (Best Case 2) Seek Leadership Role: JobInvolvement, JobSatisfaction, PerformanceRating
            JobInvolvement >= 3 &
                JobSatisfaction >= 3 &
                PerformanceRating >= 3 ~ "Seek Leadership Role",
            
            # Catch All
            TRUE ~ "Retain and Maintain"
        )
    ) 

train_readable_tbl %>%
    pull(PerformanceRating) %>%
    levels()


# 4.2 Professional Development (Promotion Readiness) ----


# 4.3 Work Life Balance ----




