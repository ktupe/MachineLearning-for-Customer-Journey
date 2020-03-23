
# 1.0 LIBRARIES & DATA ----

# Core
library(tidyverse)
library(tidyquant)
library(lubridate)
library(plotly)

# Big Data Helpers
library(dtplyr) # Big Data
library(data.table)
library(tictoc)

# EDA
library(skimr)
library(DataExplorer)

# Machine Learning
library(parsnip)
library(yardstick)
library(lime)

# DATA
query_tbl <- read_csv("data/bigquery_data.csv")

query_tbl

# 2.0 EDA ----
skim(query_tbl)

query_tbl %>% plot_missing()

query_tbl %>% plot_bar()

query_tbl %>% plot_density()

# 2.1 Explore Channel Grouping ----

query_tbl %>% count(channelGrouping, sort = TRUE)

# 2.1 Explore a visitor's purchase habits -----
query_tbl %>% count(fullVisitorId, sort = TRUE)

get_id <- function(data, id) {
    data %>%
        filter(fullVisitorId == id) %>%
        mutate(date = ymd(date)) %>%
        arrange(date) 
}

query_tbl %>% get_id(id = "1957458976293878100") %>% View()
query_tbl %>% get_id(id = "3608475193341679870") %>% View()
query_tbl %>% get_id(id = "0720311197761340948") %>% View()

query_tbl %>%
    get_id("1957458976293878100") %>%
    
    # Format data
    mutate(date = ymd(date)) %>%
    select(fullVisitorId, date, channelGrouping, total_transactions) %>%
    
    # Create flag if transaction > 0
    mutate(flag = case_when(
        total_transactions > 0 ~ 1,
        TRUE ~ 0
    )) %>%
    
    # Group by full Visitor Id
    group_by(fullVisitorId) %>%
    
    # Order by date
    arrange(date) %>%
    
    # Add paths by group
    mutate(paths = cumsum(flag)) %>%
    mutate(path_id = lag(paths, n = 1)) %>%
    
    ungroup() %>% # End Group by Full Visitor ID
    
    # Fill path_id NA's
    mutate(path_id = ifelse(is.na(path_id), 0, path_id)) %>%
    
    # Move from 0-base to 1-base index
    mutate(path_id = path_id + 1) %>%
    
    # Create visitor-path ids
    mutate(visitor_path_id = str_c(fullVisitorId, "_", path_id)) %>%
    
    # Group By All Unique Visitor-Paths - Flag if full path has a transaction
    group_by(visitor_path_id) %>%
    mutate(transaction_flag = ifelse(last(flag) == 1, 1, 0)) %>%
    ungroup() %>%
    
    View("Visitor 1957458976293878100")
    
# 2.2 Create Search Paths -----
# WARNING - LONG RUNNING SCRIPT 
# > toc()
# 16.98 sec elapsed (dtplyr)
# 51.763 sec elapsed (dplyr)

query_dt <- lazy_dt(query_tbl) 

query_paths_split_dt <- query_dt %>%
    
    # Format data
    mutate(date = ymd(date)) %>%
    select(fullVisitorId, date, channelGrouping, total_transactions) %>%
    
    # Create flag if transaction > 0
    mutate(flag = case_when(
        total_transactions > 0 ~ 1,
        TRUE ~ 0
    )) %>%
    
    # Group by full Visitor Id
    group_by(fullVisitorId) %>%
    
    # Order by date
    arrange(date) %>%
    
    # Add paths by group
    mutate(paths = cumsum(flag)) %>%
    mutate(path_id = lag(paths, n = 1)) %>%
    
    ungroup() %>% # End Group by Full Visitor ID
    
    # Fill path_id NA's
    mutate(path_id = ifelse(is.na(path_id), 0, path_id)) %>%
    
    # Move from 0-base to 1-base index
    mutate(path_id = path_id + 1) %>%
    
    # Create visitor-path ids
    mutate(visitor_path_id = str_c(fullVisitorId, "_", path_id)) %>%
    
    # Group By All Unique Visitor-Paths - Flag if full path has a transaction
    group_by(visitor_path_id) %>%
    mutate(transaction_flag = ifelse(last(flag) == 1, 1, 0)) %>%
    ungroup() 

tic()
query_paths_split_tbl <- query_paths_split_dt %>% as_tibble()
toc()

# Save and Load Point ----
# query_paths_split_tbl %>% write_rds("data/query_paths_split_tbl.rds")

query_paths_split_tbl <- read_rds("data/query_paths_split_tbl.rds")

query_paths_split_tbl %>% glimpse()


# 3.0 CUSTOMER JOURNEY EXPLORATION -----

customer_journey_tbl <- query_paths_split_tbl %>%
    select(visitor_path_id, transaction_flag, path_id, channelGrouping) %>%
    
    # Only evaluate successful paths
    filter(transaction_flag == 1) %>%
    
    # Count successful channels by path id
    group_by(channelGrouping, path_id) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    
    # Calculate successful channel composition by path id
    group_by(path_id) %>%
    mutate(prop = count / sum(count)) %>%
    ungroup() %>%
    
    mutate(channelGrouping = as_factor(channelGrouping) %>% fct_reorder(prop)) %>%
    ungroup() %>%
    
    # Filter out paths > 12 (few people have more than 12 transactions in 1 year)
    select(-count) %>%
    filter(path_id <= 12) %>%
    mutate(path_id = as.factor(path_id)) %>%
    
    # Trick to fill in missing observations with zeros
    pivot_wider(names_from  = channelGrouping, 
                values_from = prop, 
                values_fill = list(prop = 0)) %>%
    pivot_longer(cols = `(Other)`:Social, names_to = "channelGrouping", values_to = "prop") %>%
    
    mutate(channelGrouping = as_factor(channelGrouping) %>% fct_reorder(prop)) 

customer_journey_tbl

# 3.1 Path Plot ----
g <- customer_journey_tbl %>%
    
    ggplot(aes(channelGrouping, prop)) +
    geom_point(aes(frame = path_id, 
                   text  = str_glue("Path: {path_id}
                                    Channel: {channelGrouping}
                                    Prop: {scales::percent(prop)}")), 
               color = palette_light()[1], size = 5) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_tq() +
    labs(title = "Components of Customer Journey By Purchase Number",
         x = "", y = "Proportion")

ggplotly(g, tooltip = "text")

# 3.2 Customer Journey Heatmap ----
customer_journey_tbl %>%
    
    ggplot(aes(channelGrouping, path_id, fill = prop)) +
    geom_tile(color = "white", size = 0.1) +
    geom_text(aes(label = scales::percent(prop)), color = "white") +
    scale_fill_viridis_c(direction = 1) +
    theme_tq() +
    theme(legend.position = "none") +
    labs(title = "Components of Customer Journey By Purchase Number",
         subtitle = str_glue("As customers return, channel shifts from Referral and Organic
                             to Direct, Display, and Paid Search"),
         x = "", y = "Customer Transaction Path ID")


# PROBLEM - Does not take into account probability of conversion, only successes ----
# IDEA - Run ML to get Class Probability ----

# 4.0 MACHINE LEARNING ----

# 4.1 Aggregate Data ----
data_aggregated_tbl <- query_paths_split_tbl %>%
    select(visitor_path_id, transaction_flag, path_id, channelGrouping) %>%
    
    # Pivot Wide using data.table dcast() 
    mutate(freq = 1) %>%
    as.data.table() %>%
    dcast(visitor_path_id + path_id + transaction_flag ~ channelGrouping, 
          value.var     = "freq", 
          fun.aggregate = sum) %>%
    
    # Fix column names
    as_tibble() %>%
    rename_all(~ str_replace_all(., " ", "_") %>% 
                   str_to_lower() %>%
                   str_remove_all("\\(|\\)")) %>%
    mutate(transaction_flag = as.factor(transaction_flag)) %>%
    select(visitor_path_id, transaction_flag, everything())

data_aggregated_tbl %>% View("training_data")

skimr::skim(data_aggregated_tbl)

# 4.2 Modeling ----
# WARNING - LONG RUNNING SCRIPT - TAKES 7 MINUTES TO RUN ----
# > toc()
# 428.923 sec elapsed

tic()
model_xgboost <- boost_tree(mode = "classification",
               mtry = 12,
               trees = 1000,
               min_n = 3,
               tree_depth = 5,
               learn_rate = 0.01,
               loss_reduction = 0.01) %>%
    set_engine("xgboost") %>%
    fit.model_spec(transaction_flag ~ ., data = data_aggregated_tbl %>% select(-visitor_path_id))
toc()
model_xgboost %>% write_rds("models/model_xgboost.rds")

model_xgboost <- read_rds("models/model_xgboost.rds")

# 4.3 In-Sample Predictions ---- 
# > toc()
# 17.976 sec elapsed

tic()
predictions_tbl <- predict.model_fit(
    model_xgboost, 
    new_data = data_aggregated_tbl, 
    type     = "prob") %>%
    bind_cols(data_aggregated_tbl) 
toc()

# predictions_tbl %>% write_rds("data/prediction_tbl.rds")
predictions_tbl <- read_rds("data/prediction_tbl.rds")

# In-Sample AUC
predictions_tbl %>% roc_auc(transaction_flag, .pred_1)

predictions_tbl %>% arrange(desc(.pred_1)) 

predictions_tbl %>% arrange(desc(.pred_1)) %>% View("predictions")



# 5.0 LLPRO - EXPLAINABLE ML ----
source("LLPRO_lime_analysis.R")

# High Probability: 94%
query_paths_split_tbl %>% get_id(id = "1957458976293878100")
plot_model_explanation(id = "1957458976293878100_15", perm = 1e5)

# Medium Probability: 57%
query_paths_split_tbl %>% get_id(id = "0014262055593378383")
plot_model_explanation("0014262055593378383_4", perm = 1e5)

# Low Probability: 0.01%
query_paths_split_tbl %>% get_id(id = "8150969999547942207")
plot_model_explanation("8150969999547942207_1", perm = 1e5)


