
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
