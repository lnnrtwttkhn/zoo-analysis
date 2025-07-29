
get_decoding_main_current_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, node_current, node_classifier), .(
      mean_probability = mean(probability * 100),
      current_stim = as.factor(node_current == unique(node_classifier))
    )] %>%
    verify(current_stim %in% c(TRUE, FALSE)) %>%
    .[, current_stim := ifelse(current_stim == TRUE, "Current", "Other")] %>%
    .[, current_stim := factor(as.factor(current_stim), levels = c("Current", "Other"))] %>%
    .[, by = .(id, mask_test, current_stim), .(
      mean_probability = mean(mean_probability)
    )] %>%
    save_data(paths$source$decoding_main_current_mean)
}

get_decoding_main_current_stat <- function(cfg, path) {
  dt_input <- load_data(paths$source$decoding_main_current_mean) %>%
    .[, value := mean_probability]
  ttest_cfg <- list(
    lhs = "value",
    rhs = "current_stim",
    adjust_method = "bonferroni",
    paired = TRUE,
    mu = 0,
    alternative = "two.sided"
  )
  dt_output <- dt_input %>%
    .[, by = .(mask_test), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg) %>%
    save_data(paths$source$decoding_main_current_stat)
}

get_decoding_main_current_interval <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, interval_tr, node_current, node_classifier), .(
      num_trials = .N,
      mean_probability = mean(probability * 100),
      current_stim = as.factor(node_current == unique(node_classifier))
    )] %>%
    verify(current_stim %in% c(TRUE, FALSE)) %>%
    .[, current_stim := ifelse(current_stim == TRUE, "Current", "Other")] %>%
    .[, current_stim := factor(as.factor(current_stim), levels = c("Current", "Other"))] %>%
    verify(num_trials <= 20) %>%
    .[, num_trials := NULL] %>%
    .[, by = .(id, mask_test, interval_tr, current_stim), .(
      mean_probability = mean(mean_probability)
    )] %>%
    save_data(paths$source$decoding_main_current_interval)
}

analyze_decoding_main_current_interval = function() {
  dt_input = load_data(paths$source$decoding_main)
  dt_output = dt_input %>%
    .[, phase := data.table::fcase(
      (interval_tr %in% seq(1, 4)), "TRs 1 - 4\n(early)",
      (interval_tr %in% seq(5, 8)), "TRs 5 - 8\n(late)",
      (interval_tr > 8), "out"
    )] %>%
    .[interval_tr %in% seq(1, 8)] %>%
    .[, by = .(id, mask_test, order, graph, phase, dist_uni, dist_bi), .(
      mean_prob = mean(probability_norm * 100)
    )] %>%
    .[, dist_uni := ifelse(is.na(dist_uni), 0, dist_uni)] %>%
    .[, dist_bi := ifelse(is.na(dist_bi), 0, dist_bi)] %>%
    .[, dist_bi := ifelse(dist_bi == 1 & dist_uni == 5, -1, dist_bi)] %>%
    .[, dist_bi := ifelse(dist_bi == 2 & dist_uni == 4, -2, dist_bi)] %>%
    .[, dist_combined := paste0(dist_uni, " | ", dist_bi)] %>%
    .[, predictor := data.table::fcase(
      dist_uni == 0, NaN,
      dist_uni == 1, -2,
      dist_uni == 2, -1,
      dist_uni == 3, 0,
      dist_uni == 4, 1,
      dist_uni == 5, 2
    )] %>%
    .[, predictor_squared := predictor ^ 2] %>%
    .[, dist_uni := as.factor(dist_uni)] %>%
    .[, dist_bi := as.factor(abs(dist_bi))]
    # save_data(df = ., filename = "decoding_main_graph_probabilities", check = FALSE)
  return(dt_output)
}

analyze_decoding_main_current_interval = function() {
  # probability excluding the current node
  dt_input = load_data(file.path(paths$sourcedata, "zoo_sourcedata_decoding_main.csv"))
  dt_output = dt_input %>%
    # remove data of the current node:
    .[!(node_current == node_classifier),] %>%
    .[, dist_uni := ifelse(is.na(dist_uni), 0, dist_uni)] %>%
    .[, dist_bi := ifelse(is.na(dist_bi), 0, dist_bi)] %>%
    .[, dist_bi := ifelse(dist_bi == 1 & dist_uni == 5, -1, dist_bi)] %>%
    .[, dist_bi := ifelse(dist_bi == 2 & dist_uni == 4, -2, dist_bi)] %>%
    .[, dist_combined := paste0(dist_uni, " | ", dist_bi)] %>%
    .[, by = .(id, mask_test, graph, interval_tr, dist_combined), .(
      num_trials = .N,
      mean_prob = mean(probability_norm * 100)
    )] %>%
    verify(num_trials <= 120) %>%
    .[, num_trials := NULL] %>%
    save_data(df = ., filename = "decoding_main_graph_probabilities_distance", check = FALSE)
  return(dt_output)
}
