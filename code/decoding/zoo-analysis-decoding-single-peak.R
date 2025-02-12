
get_single_decoding_peak_accuracy <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_single_peak)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, session, run, run_index, trial_run, node), .(
      max_prob_label = class[which.max(probability)],
      num_classes = .N
    )] %>%
    verify(num_classes == cfg$num_nodes) %>%
    .[, num_classes := NULL] %>%
    .[, accuracy := ifelse(node == max_prob_label, 1, 0)] %>%
    save_data(paths$source$decoding_single_peak_accuracy)
}

get_single_decoding_peak_accuracy_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_single_peak_accuracy)
  dt_output <- dt_input %>%
    .[session == "ses-01"] %>%
    # .[, by = .(id, mask_test), .(num_trials = .N)] %>%
    verify(.[, by = .(id, mask_test), .(num_trials = .N)]$num_trials <= cfg$single$max_trials) %>%
    .[, by = .(id, mask_test), .(
      mean_accuracy = mean(accuracy) * 100
    )] %>%
    save_data(paths$source$decoding_single_peak_accuracy_mean)
}

get_single_decoding_peak_accuracy_stat <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_single_peak_accuracy_mean)
  ttest_cfg <- list(
    lhs = "value",
    rhs = "1",
    adjust_method = "bonferroni",
    paired = FALSE,
    mu = 100/cfg$num_nodes,
    alternative = "greater"
  )
  dt_output <- dt_input %>%
    .[, value := mean_accuracy] %>%
    .[, by = .(mask_test), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg)
}

get_single_decoding_peak_run <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_single_peak_accuracy)
  dt_output <- dt_input %>%
    verify(.[, by = .(id, mask_test, run_index), .(num_trials = .N)]$num_trials <= cfg$single$max_trials_run) %>%
    .[, by = .(id, mask_test, run_index), .(mean_accuracy = mean(accuracy) * 100)] %>%
    save_data(paths$source$decoding_single_peak_accuracy_run)
}

get_single_decoding_peak_run_lme <- function(cfg, paths, mask_input) {
  dt_input <- load_data(paths$source$decoding_single_peak_accuracy_run)
  model <- lmerTest::lmer(
    formula = mean_accuracy ~ run_index + (1 + run_index | id),
    data = dt_input %>% .[mask_test == mask_input, ],
    control = cfg$lcctrl,
    subset = NULL,
    weights = NULL,
    na.action = na.omit,
    offset = NULL,
    REML = TRUE
  )
  summary(model)
  model_stat <- broom::tidy(stats::anova(model))
  report_lme_model <- report_lme_stats(
    num_df = model_stat$NumDF,
    den_df = model_stat$DenDF,
    f_value = model_stat$statistic,
    p_value = model_stat$p.value
  )
}

get_single_decoding_peak_run_stat <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_single_peak_accuracy_run)
  ttest_cfg <- list(
    lhs = "value",
    rhs = "1",
    adjust_method = "bonferroni",
    paired = FALSE,
    mu = 100/cfg$num_nodes,
    alternative = "greater"
  )
  dt_output <- dt_input %>%
    .[, value := mean_accuracy] %>%
    .[, by = .(mask_test, run_index), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg)
  return(dt_output)
}
