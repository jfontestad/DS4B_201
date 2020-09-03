# Data Preparation --------------------------------------------------------


# Human Readable ----------------------------------------------------------

if(!require(pacman)) {install.packages("pacman")}
pacman::p_load(
    "readxl"
    , "tidyverse"
    , "tidyquant"
    , "stringr"
    , "forcats"
)


# Data --------------------------------------------------------------------

path_train            <- "00_Data/telco_train.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet=1)
definitions_raw_tbl <- read_excel(
    path_data_definitions
    , sheet = 1
    , col_names = FALSE
  ) %>%
    set_names("X__1","X__2")


# Tidying The Data --------------------------------------------------------

train_raw_tbl %>% glimpse()

definitions_tbl <- definitions_raw_tbl %>%
  fill(X__1, .direction = "down") %>%
  filter(!is.na(X__2)) %>%
  separate(col = X__2, into = c("key","value"), remove = TRUE, sep = " '") %>%
  rename(column_name = X__1) %>%
  mutate(key = as.numeric(key)) %>%
  mutate(value = str_remove_all(value, "'"))

definitions_list <- definitions_tbl %>%
  split(.$column_name) %>%
  map(~ select(., - column_name)) %>%
  map(~ mutate(., value = as_factor(value)))
definitions_list[[1]]

for (i in seq_along(definitions_list)) {
  
  list_name <- names(definitions_list)[i]
  
  colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
  
}

data_merged_tbl <- list(HR_Data = train_raw_tbl) %>%
  append(definitions_list, after = 1) %>%
  reduce(left_join) %>%
  select(-one_of(names(definitions_list))) %>%
  set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>%
  select(sort(names(.)))
data_merged_tbl

data_merged_tbl %>%
  select_if(is.character) %>% 
  glimpse()

data_merged_tbl %>%
  distinct(BusinessTravel)

data_processed_tbl <- data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    BusinessTravel = BusinessTravel %>% 
      fct_relevel(
        "Non-Travel"
        , "Travel_Rarely"
        , "Travel_Frequently"
      )
    , MaritalStatus = MaritalStatus %>%
      fct_relevel(
        "Single"
        , "Married"
        , "Divorced"
      )
  )

data_processed_tbl %>%
  select_if(is.factor) %>%
  map(levels)


# Processing Pipeline -----------------------------------------------------

process_hr_data_readable <- function(
  .data
  , .definitions_tbl
) {
  
  definitions_list <- .definitions_tbl %>%
    fill(X__1, .direction = "down") %>%
    filter(!is.na(X__2)) %>%
    separate(col = X__2, into = c("key","value"), remove = TRUE, sep = " '") %>%
    rename(column_name = X__1) %>%
    mutate(key = as.numeric(key)) %>%
    mutate(value = str_remove_all(value, "'")) %>%
    split(.$column_name) %>%
    map(~ select(., - column_name)) %>%
    map(~ mutate(., value = as_factor(value)))
  
  for (i in seq_along(definitions_list)) {
    list_name <- names(definitions_list)[i]
    colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
    
  }
  
  data_merged_tbl <- list(HR_Data = .data) %>%
    append(definitions_list, after = 1) %>%
    reduce(left_join) %>%
    select(-one_of(names(definitions_list))) %>%
    set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>%
    select(sort(names(.)))
  
  data_processed_tbl <- data_merged_tbl %>%
    mutate_if(is.character, as.factor) %>%
    mutate(
      BusinessTravel = BusinessTravel %>% 
        fct_relevel(
          "Non-Travel"
          , "Travel_Rarely"
          , "Travel_Frequently"
        )
      , MaritalStatus = MaritalStatus %>%
        fct_relevel(
          "Single"
          , "Married"
          , "Divorced"
        )
    )
  
  return(data_processed_tbl)
  
}
process_hr_data_readable(.data = train_raw_tbl, .definitions_tbl = definitions_raw_tbl) %>%
  glimpse()
