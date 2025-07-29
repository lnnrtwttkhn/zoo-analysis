get_behavior_sequence_run <- function(cfg, paths) {
  # analyze mean learning effects across runs
  dt_input <- load_data(paths$source$behavior_task)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude)] %>%
    .[condition == "Sequence", ] %>%
    .[event_type == "response", ] %>%
    verify(.[, by = .(id, run), .(num_trials = .N)]$num_trials == cfg$sequence$num_trials_run) %>%
    .[, by = .(id, run), .(
      num_trials = .N,
      mean_response_time = mean(response_time[accuracy == 1]),
      mean_log_response_time = mean(response_time[accuracy == 1]),
      mean_accuracy = mean(accuracy) * 100
    )] %>%
    .[, by = .(id), run_index := as.integer(dplyr::row_number(run))] %>%
    save_data(paths$source$behavior_sequence_run)
}

get_behavior_sequence_run_stat <- function(cfg, paths) {
  # compare mean behavioral accuracy in sequence trials against baseline:
  dt_input <- load_data(paths$source$behavior_sequence_run)
  ttest_cfg <- list(
    lhs = "value",
    rhs = "1",
    adjust_method = "bonferroni",
    paired = FALSE,
    mu = 100/cfg$num_nodes,
    alternative = "greater"
  )
  dt_output <- dt_input %>%
    melt(id.vars = c("id", "run"), measure.vars = c("mean_accuracy", "mean_log_response_time")) %>%
    .[, by = .(run, variable), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg) %>%
    save_data(paths$source$behavior_sequence_run_stat)
}

get_behavior_sequence_run_lme <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sequence_run)
  # effect of task run on behavioral data in the main task (all data):
  lme_formula <- "mean_accuracy ~ run_index + (1 + run_index | id)"
  lme_data <- dt_input
  run_lme(lme_formula, lme_data)
  # effect of task run on behavioral data in the main task (exclude first run):
  lme_formula <- "mean_accuracy ~ run_index + (1 + run_index | id)"
  lme_data <- dt_input %>% .[run_index != 1, ]
  run_lme(lme_formula, lme_data)
  # effect of task run on response time in the main task (all data):
  lme_formula <- "mean_log_response_time ~ run_index + (1 + run_index | id)"
  lme_data <- dt_input
  run_lme(lme_formula, lme_data)
}

get_behavior_sequence_run_glm <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sequence_run)
  model_formulas <- c("mean_log_response_time ~ run_index")
  dt_output <- dt_input %>%
    .[, by = .(id), {
      model = lapply(model_formulas, run_glm, data = .SD, cfg = cfg, tidy = TRUE)
      model_formula = model_formulas
      model_number = seq_len(length(model_formulas))
      num_runs = .N
      list(model, model_formula, model_number, num_runs)
    }] %>%
    unnest(model) %>%
    setDT(.) %>%
    # .[, term := dplyr::case_when(
    #   stringr::str_detect(term, "(Intercept)") ~ "Intercept",
    #   stringr::str_detect(term, "prob_graph") ~ "1-step",
    #   stringr::str_detect(term, "prob_sr") ~ "SR",
    #   stringr::str_detect(term, "prob_stim") ~ "Stimulus"
    # )] %>%
    verify(num_runs == cfg$sequence$num_runs) %>%
    setnames(., old = "term", new = "predictor") %>%
    save_data(paths$source$behavior_sequence_run_glm)
}

get_behavior_sequence_halfrun_glm <- function(cfg, paths) {
  model_formulas <- c("mean_log_response_time ~ block_index")
  dt_input <- load_data(paths$source$behavior_task)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude)] %>%
    .[condition == "Sequence", ] %>%
    .[event_type == "response", ] %>%
    verify(.[, by = .(id, run), .(num_trials = .N)]$num_trials == cfg$sequence$num_trials_run) %>%
    .[, halfrun := ifelse(trial_run <= cfg$sequence$num_trials_run / 2, "first", "second")] %>%
    .[, by = .(run, halfrun), block := .GRP] %>%
    verify(block %in% seq(1, cfg$sequence$num_runs * 2)) %>%
    .[, graphblock := ifelse(block %in% seq(1, cfg$sequence$num_runs), "first graph", "second graph")] %>%
    .[accuracy == 1,] %>%
    .[, by = .(id, block, graphblock), .(
      num_trials = .N,
      mean_response_time = mean(response_time[accuracy == 1]),
      mean_log_response_time = mean(response_time[accuracy == 1]),
      mean_accuracy = mean(accuracy) * 100
    )] %>%
    .[, by = .(id, graphblock), block_index := as.integer(dplyr::row_number(block))] %>%
    .[, by = .(id, graphblock), {
      model = lapply(model_formulas, run_glm, data = .SD, cfg = cfg, tidy = TRUE)
      model_formula = model_formulas
      model_number = seq_len(length(model_formulas))
      num_blocks = .N
      list(model, model_formula, model_number, num_blocks)
    }] %>%
    unnest(model) %>%
    setDT(.) %>%
    verify(num_blocks == cfg$sequence$num_runs) %>%
    setnames(., old = "term", new = "predictor") %>%
    save_data(paths$source$behavior_sequence_halfrun_glm)
}

get_behavior_sequence_halfrun_glm <- function(cfg, paths) {
  model_formulas <- c("mean_log_response_time ~ halfrun")
  dt_input <- load_data(paths$source$behavior_task)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude)] %>%
    .[condition == "Sequence", ] %>%
    .[event_type == "response", ] %>%
    verify(.[, by = .(id, run), .(num_trials = .N)]$num_trials == cfg$sequence$num_trials_run) %>%
    .[, halfrun := ifelse(trial_run <= cfg$sequence$num_trials_run / 2, 1, 2)] %>%
    .[, by = .(run, halfrun), block := .GRP] %>%
    verify(block %in% seq(1, cfg$sequence$num_runs * 2)) %>%
    .[, graphblock := ifelse(block %in% seq(1, cfg$sequence$num_runs), "first graph", "second graph")] %>%
    .[, by = .(id, run, halfrun), .(
      num_trials = .N,
      mean_response_time = mean(response_time[accuracy == 1]),
      mean_log_response_time = mean(response_time[accuracy == 1]),
      mean_accuracy = mean(accuracy) * 100
    )] %>%
    verify(num_trials == cfg$sequence$num_trials_run / 2) %>%
    .[, by = .(id), {
      model = lapply(model_formulas, run_glm, data = .SD, cfg = cfg, tidy = TRUE)
      model_formula = model_formulas
      model_number = seq_len(length(model_formulas))
      num_halfruns = .N
      list(model, model_formula, model_number, num_halfruns)
    }] %>%
    unnest(model) %>%
    setDT(.) %>%
    verify(num_halfruns == cfg$sequence$num_runs * 2) %>%
    setnames(., old = "term", new = "predictor") %>%
    save_data(paths$source$behavior_sequence_halfrun_glm)
}

get_behavior_sequence_onestep <- function(cfg, paths) {
  # analyze response times and behavioral accuracy during one-step transitions:
  dt_input <- load_data(paths$source$behavior_task)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude)] %>%
    .[condition == "Sequence", ] %>%
    .[event_type == "response", ] %>%
    .[!(run == "run-03" & trial_run == 121), ] %>%
    .[trial_run != 1, ] %>%
    .[, by = .(id, graph, onestep), .(
      num_trials = .N,
      mean_accuracy = mean(accuracy) * 100,
      mean_log_response_time = mean(log_response_time[accuracy == 1], na.rm = TRUE)
    )] %>%
    save_data(paths$source$behavior_sequence_onestep)
}

get_behavior_sequence_onestep_stat <- function(cfg, paths) {
  # calculate paired t-tests for accuracy and response times on one-step transitions:
  dt_input <- load_data(paths$source$behavior_sequence_onestep)
  ttest_cfg <- list(
    lhs = "value",
    rhs = "onestep",
    adjust_method =  "bonferroni",
    paired = TRUE,
    mu = 0,
    alternative = "two.sided"
  )
  dt_output <- dt_input %>%
    melt(id.vars = c("id", "graph", "onestep"), measure.vars = c("mean_accuracy", "mean_log_response_time")) %>%
    .[, by = .(graph, variable), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg) %>%
    save_data(paths$source$behavior_sequence_onestep_stat)
}

get_behavior_sequence_onestep_run <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_task)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude)] %>%
    .[condition == "Sequence", ] %>%
    .[event_type == "response", ] %>%
    .[!(run == "run-03" & trial_run == 121), ] %>%
    .[trial_run != 1, ] %>%
    # rewrite the onestep variable (only high vs. low):
    .[, onestep := ifelse(grepl("High", onestep), "High", "Low")] %>%
    .[, onestep := factor(as.factor(onestep), levels = c("Low", "High"))] %>%
    .[, by = .(id, run, onestep), .(
      num_trials = .N,
      mean_accuracy = mean(accuracy) * 100,
      mean_log_response_time = mean(log_response_time[accuracy == 1], na.rm = TRUE)
    )] %>%
    .[, by = .(id, onestep), run_index := data.table::rleid(run)] %>%
    save_data(paths$source$behavior_sequence_onestep_run)
}
  
get_behavior_sequence_onestep_run_glm <- function(cfg, paths) {
  model_formulas <- c("mean_accuracy ~ run_index")
  dt_input <- load_data(paths$source$behavior_sequence_onestep_run)
  dt_output <- dt_input %>%
    .[, by = .(id, onestep), {
      model = lapply(model_formulas, run_glm, data = .SD, cfg = cfg, tidy = TRUE)
      model_formula = model_formulas
      model_number = seq_len(length(model_formulas))
      num_runs = .N
      list(model, model_formula, model_number, num_runs)
    }] %>%
    unnest(model) %>%
    setDT(.) %>%
    verify(num_runs == cfg$sequence$num_runs) %>%
    setnames(., old = "term", new = "predictor") %>%
    .[predictor == "run_index", ] %>%
    .[, c("id", "onestep", "estimate")] %>%
    pivot_wider(names_from = onestep, values_from = estimate) %>%
    setDT(.) %>%
    .[, slope_diff := Low - High] %>%
    save_data(paths$source$behavior_sequence_onestep_run_glm)
}

get_behavior_sequence_dist_run <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_task)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude)] %>%
    .[condition == "Sequence", ] %>%
    .[event_type == "response", ] %>%
    .[!(run == "run-03" & trial_run == 121), ] %>%
    .[trial_run != 1, ] %>%
    .[, by = .(id, run, dist_bi), .(
      num_trials = .N,
      mean_accuracy = mean(accuracy) * 100,
      mean_log_response_time = mean(log_response_time[accuracy == 1], na.rm = TRUE)
    )] %>%
    .[, by = .(id, dist_bi), run_index := data.table::rleid(run)] %>%
    melt(measure.vars = c("mean_log_response_time", "mean_accuracy")) %>%
    save_data(paths$source$behavior_sequence_dist_run)
}

get_behavior_sequence_dist_run_glm <- function(cfg, paths) {
  model_formulas <- c("value ~ run_index")
  dt_input <- load_data(paths$source$behavior_sequence_dist_run)
  dt_output <- dt_input %>%
    .[, by = .(id, dist_bi, variable), {
      model = lapply(model_formulas, run_glm, data = .SD, cfg = cfg, tidy = TRUE)
      model_formula = model_formulas
      model_number = seq_len(length(model_formulas))
      num_runs = .N
      list(model, model_formula, model_number, num_runs)
    }] %>%
    unnest(model) %>%
    setDT(.) %>%
    verify(num_runs == cfg$sequence$num_runs) %>%
    setnames(., old = "term", new = "predictor") %>%
    .[predictor == "run_index", ] %>%
    .[, dist_bi := paste0("dist", dist_bi) ] %>%
    .[, c("id", "dist_bi", "variable", "estimate")] %>%
    pivot_wider(names_from = dist_bi, values_from = estimate) %>%
    setDT(.) %>%
    save_data(paths$source$behavior_sequence_dist_run_glm)
}

get_behavior_sequence_graph <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_task)
  num_trials_run <- cfg$sequence$num_trials_run
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude)] %>%
    .[condition == "Sequence", ] %>%
    .[event_type == "response", ] %>%
    .[!(run == "run-03" & trial_run == 121), ] %>%
    .[trial_run != 1, ] %>%
    .[, by = .(id, sequence_detected, graph, dist_uni, dist_dir_label), unique_trials := .N] %>%
    .[, by = .(id, sequence_detected, order, graph, dist_uni, dist_dir_label), .(
      num_trials = .N,
      mean_error = sum(accuracy == 0) / unique(num_trials_run) * 100,
      mean_error_norm = sum(accuracy == 0) / unique(unique_trials) * 100,
      nun_unique_trials = unique(unique_trials),
      num_trial_run = unique(num_trials_run),
      mean_log_rt = mean(log_response_time[accuracy == 1], na.rm = TRUE)
    )] %>%
    setorder(id, sequence_detected, order, graph, dist_dir_label) %>%
    .[, predictor_linear := dplyr::case_when(
      dist_uni == 1 ~ -2,
      dist_uni == 2 ~ -1,
      dist_uni == 3 ~ 0,
      dist_uni == 4 ~ 1,
      dist_uni == 5 ~ 2
    )] %>%
    .[, predictor_quadratic := predictor_linear ^ 2] %>%
    save_data(paths$source$behavior_sequence_graph)
}

get_behavior_sequence_graph_conscious_stat <- function(cfg, paths) {
  # compare mean behavioral accuracy in sequence trials against baseline:
  dt_input <- load_data(paths$source$behavior_sequence_graph)
  ttest_cfg <- list(
    formula = "value ~ sequence_detected",
    adjust_method = "fdr",
    paired = FALSE,
    mu = 0,
    alternative = "two.sided"
  )
  dt_output <- dt_input %>%
    melt(id.vars = c("id", "sequence_detected", "order", "graph", "dist_dir_label"),
         measure.vars = c("mean_error", "mean_error_norm", "mean_log_rt")) %>%
    .[variable == "mean_log_rt", ] %>%
    .[, by = .(graph, dist_dir_label, variable), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg)
}

get_behavior_sequence_graph_run <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_task)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude)] %>%
    .[condition == "Sequence", ] %>%
    .[event_type == "response", ] %>%
    .[!(run == "run-03" & trial_run == 121), ] %>%
    .[trial_run != 1, ] %>%
    .[, by = .(id, run, graph, dist_dir_label), unique_trials := .N] %>%
    verify(.[, by = .(id, run, graph, dist_dir_label), .(num = length(unique(unique_trials)))]$num == 1) %>%
    .[, by = .(id, run, run_cond, graph, dist_uni, dist_bi, dist_dir_label, order), .(
      num_trials = .N,
      mean_error = sum(accuracy == 0) / unique(num_trials_run) * 100,
      mean_error_norm = sum(accuracy == 0) / unique(unique_trials) * 100,
      nun_unique_trials = unique(unique_trials),
      num_trial_run = unique(num_trials_run),
      mean_log_rt = mean(log_response_time[accuracy == 1], na.rm = TRUE)
    )] %>%
    .[, by = .(id), run_index := data.table::rleid(run)] %>%
    setorder(id, order, graph, dist_dir_label) %>%
    .[, predictor_linear := dplyr::case_when(
      dist_dir_label == "1|1\nu|b" ~ -2,
      dist_dir_label == "2|2\nu|b" ~ -1,
      dist_dir_label == "3|3\nu|b" ~ 0,
      dist_dir_label == "4|2\nu|b" ~ 1,
      dist_dir_label == "5|1\nu|b" ~ 2
    )] %>%
    .[, predictor_quadratic := predictor_linear ^ 2] %>%
    save_data(paths$source$behavior_sequence_graph_run)
}

get_behavior_sequence_graph_lme <- function() {
  dt_input <- load_data(paths$source$behavior_sequence_graph)
  # RESPONSE TIMES
  # effect of linear predictor in unidirectional data (all data):
  lme_formula = "mean_log_rt ~ predictor_linear + (1 + predictor_linear | id)"
  lme_data = dt_input %>% .[graph == "uni", ]
  run_lme(lme_formula, lme_data)
  # effect of linear predictor in unidirectional data (only 0.1 transitions):
  lme_formula = "mean_log_rt ~ predictor_linear + (1 + predictor_linear | id)"
  lme_data = dt_input %>% .[graph == "uni", ] %>% .[!(dist_dir_label == "1|1\nu|b"), ]
  run_lme(lme_formula, lme_data)
  # effect of quadratic predictor in bidirectional data (all data):
  lme_formula = "mean_log_rt ~ predictor_quadratic + (1 + predictor_quadratic | id)"
  lme_data = dt_input %>% .[graph == "bi", ]
  run_lme(lme_formula, lme_data)
  # effect of quadratic predictor in bidirectional data (only 0.1 transitions):
  lme_formula = "mean_log_rt ~ predictor_quadratic + (1 + predictor_quadratic | id)"
  lme_data = dt_input %>% .[graph == "bi", ] %>% .[!(dist_dir_label %in% c("1|1\nu|b", "5|1\nu|b")), ]
  run_lme(lme_formula, lme_data)
  # ACCURACY
  # effect of linear predictor in unidirectional data (all data):
  lme_formula = "mean_error_norm ~ predictor_linear + (1 + predictor_linear | id)"
  lme_data = dt_input %>% .[graph == "uni", ]
  run_lme(lme_formula, lme_data)
  # effect of linear predictor in unidirectional data (only 0.1 transitions):
  lme_formula = "mean_error_norm ~ predictor_linear + (1 + predictor_linear | id)"
  lme_data = dt_input %>% .[graph == "uni", ] %>% .[!(dist_dir_label == "1|1\nu|b"), ]
  run_lme(lme_formula, lme_data)
  # effect of quadratic predictor in bidirectional data (all data):
  lme_formula = "mean_error_norm ~ predictor_quadratic + (1 + predictor_quadratic | id)"
  lme_data = dt_input %>% .[graph == "bi", ]
  run_lme(lme_formula, lme_data)
  # effect of quadratic predictor in bidirectional data (only 0.1 transitions):
  lme_formula = "mean_error_norm ~ predictor_quadratic + (1 + predictor_quadratic | id)"
  lme_data = dt_input %>% .[graph == "bi", ] %>% .[!(dist_dir_label %in% c("1|1\nu|b", "5|1\nu|b")), ]
  run_lme(lme_formula, lme_data)
}
