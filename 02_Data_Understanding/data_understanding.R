
# Data Understanding ------------------------------------------------------


# Lib Load ----------------------------------------------------------------

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
    "tidyverse"
    , "tidyquant"
    , "readxl"
    , "skimr"
    , "GGally"
)


# Load Data ---------------------------------------------------------------

path_train <- "00_Data/telco_train.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE) %>%
    set_names("X__1","X__2")

definitions_raw_tbl

train_raw_tbl
glimpse(train_raw_tbl)


# Step 1: Data summarization ----------------------------------------------

skim(train_raw_tbl)

# Char data
train_raw_tbl %>%
    select_if(is.character) %>%
    glimpse()

train_raw_tbl %>%
    select_if(is.character) %>%
    map(unique)

train_raw_tbl %>%
    select_if(is.character) %>%
    map(~ table(.) %>% prop.table())

# Num data
train_raw_tbl %>%
    select_if(is.numeric) %>%
    map(~ unique(.) %>% length())

train_raw_tbl %>%
    select_if(is.numeric) %>%
    map_df(~ unique(.) %>% length()) %>%
    gather() %>%
    arrange(value) %>%
    filter(value <= 10)

# Step 2: Data Visualization ----
train_raw_tbl %>%
    select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
    ggpairs()

train_raw_tbl %>%
    select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
    ggpairs(
        aes(color = Attrition)
        , lower = "blank"
        , legend = 1
        , diag = list(continuous = wrap("densityDiag", alpha = 0.5))
    ) +
    theme(legend.position = "bottom")

plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
    
    color_expr <- enquo(color)
    
    if(rlang::quo_is_null(color_expr)) {
        
        g <- data %>%
            ggpairs(lower = "blank")
        
    } else {
        
        color_name <- quo_name(color_expr)
        
        g <- data %>%
            ggpairs(
                aes_string(color = color_name)
                , lower = "blank"
                , legend = 1
                , diag = list(continuous = wrap("densityDiag", alpha = 0.5))
            ) +
            theme(legend.position = "bottom")
        
    }
    
    return(g)
}

train_raw_tbl %>%
    select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
    plot_ggpairs(color = Attrition)

# Explore Features by Category
