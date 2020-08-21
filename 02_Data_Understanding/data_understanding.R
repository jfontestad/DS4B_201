
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
