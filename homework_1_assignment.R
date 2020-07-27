# HOMEWORK 1 ----

# Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Source Scripts ----
source("00_Scripts/assess_attrition.R")

# Data ----
path_train     <- "00_Data/telco_train.xlsx"
train_raw_tbl  <- read_excel(path_train, sheet = 1)

dept_jobrole_tbl <- train_raw_tbl %>%
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

kpi_industry_turnover_pct <- 0.088

# Productivity Cost by Role ----

productivity_cost_by_role_tbl <- read_excel("00_Data/productivity_cost_by_role.xlsx")
productivity_cost_by_role_tbl


# Q1: Which Job Role has the highest total cost of attrition? ----
dept_job_role_tbl %>%
    left_join(
        productivity_cost_by_role_tbl
        , by = c("Department" = "Department", "JobRole" = "JobRole")
    ) %>%
    count(JobRole, Attrition) %>%
    count_to_pct(JobRole)
    


# Q2: What is the total cost of attrition for the Research & Development: Research Scientist job role? ----



# Q3: What percentage do the top four Job Roles account for in terms of the total cost of attrition? ----



# Q4. Which Department has the highest total cost of attrition? ----



# Q5: What percentage does the top Department account for in terms of the total cost of attrition? ----


