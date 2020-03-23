
# 1. Create explainer with lime() 
# 2. Make explaination with explain()
# 3. Visualize with plot_features()

explainer <- data_aggregated_tbl %>%
    select(-transaction_flag, -visitor_path_id) %>%
    lime(
        model = model_xgboost,
        n_bins = 12, 
        quantile_bins = FALSE
    )

plot_model_explanation <- function(id, perm = 100000) {
    
    message(str_glue("Running {scales::comma(perm)} permutations..."))
    
    explanation <- data_aggregated_tbl %>%
        filter(visitor_path_id == id) %>%
        select(-transaction_flag, -visitor_path_id) %>%
        lime::explain(
            explainer = explainer,
            labels = "1",
            n_features = 8,
            n_permutations = perm,
            dist_fun       = "gower",
            gower_pow      = 1,
            feature_select = "auto" # if tree, requires xgboost
        )
    
    explanation %>% plot_features()
}

