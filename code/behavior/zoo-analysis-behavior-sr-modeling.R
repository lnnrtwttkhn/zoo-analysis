get_sr_grid <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_task)
  alpha <- 0.1
  gammas <- seq(0, 19, by = 1)
  colnames <- paste0("SR", gammas)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude), ] %>%
    .[event_type == "response", ] %>%
    .[trial_run > 1, ] %>%
    .[, transition := paste(node_previous, node, sep = "-")] %>%
    .[, by = .(id), (colnames) := (
      lapply(gammas / 20, function(x) sr_fun(node_previous, node, alpha = 0.1, gamma = x))
    )] %>%
    save_data(paths$source$behavior_sr_grid)
}

get_sr_grid_seq <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_grid)
  dt_output <- dt_input %>%
    .[condition == "Sequence",] %>%
    .[, halfrun := ifelse(trial_run <= cfg$sequence$num_trials_run / 2, "first", "second")] %>%
    .[, by = .(run, halfrun), block := .GRP] %>%
    .[, graphblock := ifelse(block %in% seq(1, cfg$sequence$num_runs), "first graph", "second graph")] %>%
    verify(block %in% seq(1, cfg$sequence$num_runs * 2)) %>%
    .[accuracy == 1,] %>%
    save_data(paths$source$behavior_sr_grid_seq)
}

get_sr_grid_seq_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_grid_seq)
  column_names <- c("log_response_time", "dist_uni", "prob_uni")
  dt_output <- dt_input %>%
    group_by(id, transition, block, graph, order) %>%
    summarise(across(all_of(column_names) | starts_with("SR"),
                     list(mean = ~ mean(.x, na.rm = TRUE)),
                     .names = "{.col}_{.fn}")) %>%
    setorder(., id, block, transition, dist_uni_mean) %>%
    save_data(paths$source$behavior_sr_grid_seq_mean)
}

fun_sr_lme <- function(dt_input, formula_text) {
  dt_output <- dt_input %>%
    .[, {
      N = .N
      model = lme4::lmer(
        formula = as.formula(formula_text),
        data = .SD,
        subset = NULL,
        weights = NULL,
        REML = TRUE,
        na.action = na.omit,
        offset = NULL,
        control = cfg$lcctrl
      )
      model_attributes = attributes(model)
      aic = AIC(model)
      optinfo_optimizer = model_attributes$optinfo$optimizer
      optinfo_message = model_attributes$optinfo$message
      optinfo_warnings = ifelse(length(model_attributes$optinfo$warnings) == 0, "none", model_attributes$optinfo$warnings)
      list(aic, N, optinfo_optimizer, optinfo_message, optinfo_warnings)
    }]
  return(dt_output)
}

fun_sr_add <- function(dt_input) {
  dt_output <- dt_input %>%
    .[, {list(
      SR_GAMMA = factor(SR_GAMMA, levels = gtools::mixedsort(SR_GAMMA)),
      gamma = as.numeric(seq(0, 19)/20),
      ranking = as.numeric(rank(as.numeric(aic))),
      aic = as.numeric(aic)
    )}]
  return(dt_output)
}

get_sr_grid_seq_lme <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_grid_seq)
  formula_text <- "log_response_time ~ block + bites * order * graph + (block + graph + bites | id)"
  dt_output <- dt_input %>%
    melt(measure.vars = patterns("^SR"), variable.name = "SR_GAMMA", value.name = "bites") %>%
    .[, by = .(SR_GAMMA), fun_sr_lme(.SD, formula_text)] %>%
    .[, fun_sr_add(.SD)] %>%
    save_data(paths$source$behavior_sr_grid_seq_lme)
}

get_sr_grid_seq_mean_lme <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_grid_seq_mean)
  formula_text <- "log_response_time_mean ~ block + bites * order * graph + (block + graph + bites | id)"
  dt_output <- dt_input %>%
    melt(measure.vars = patterns("^SR"), variable.name = "SR_GAMMA", value.name = "bites") %>%
    .[, by = .(SR_GAMMA), fun_sr_lme(.SD, formula_text)] %>%
    .[, fun_sr_add(.SD)] %>%
    save_data(paths$source$behavior_sr_grid_seq_mean_lme)
}

get_sr_grid_seq_graph_order_lme <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_grid_seq)
  formula_text <- "log_response_time ~ bites + (bites | id)"
  dt_output <- dt_input %>%
    melt(measure.vars = patterns("^SR"), variable.name = "SR_GAMMA", value.name = "bites") %>%
    .[, order_graph := paste0(order, " (", graph, ")")] %>%
    .[, order_graph := factor(as.factor(order_graph), levels = c(
      "uni - bi (uni)", "uni - bi (bi)",  "bi - uni (bi)", "bi - uni (uni)"))] %>%
    .[, by = .(SR_GAMMA, order_graph, graph), fun_sr_lme(.SD, formula_text)] %>%
    .[, by = .(order_graph, graph), fun_sr_add(.SD)] %>%
    save_data(paths$source$behavior_sr_grid_seq_graph_order_lme)
}

get_sr_grid_seq_graph_lme <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_grid_seq)
  formula_text <- "log_response_time ~ bites + (bites | id)"
  dt_output <- dt_input %>%
    melt(measure.vars = patterns("^SR"), variable.name = "SR_GAMMA", value.name = "bites") %>%
    .[, by = .(SR_GAMMA, graph), fun_sr_lme(.SD, formula_text)] %>%
    .[, by = .(graph), fun_sr_add(.SD)] %>%
    save_data(paths$source$behavior_sr_grid_seq_graph_lme)
}

get_sr_grid_seq_block_lme <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_grid_seq)
  formula_text <- "log_response_time ~ bites + (bites | id)"
  dt_output <- dt_input %>%
    melt(measure.vars = patterns("^SR"), variable.name = "SR_GAMMA", value.name = "bites") %>%
    .[, by = .(SR_GAMMA, block), fun_sr_lme(.SD, formula_text)] %>%
    .[, by = .(block), fun_sr_add(.SD)] %>%
    save_data(paths$source$behavior_sr_grid_seq_block_lme)
}

get_behavior_sr_fit_starting_values <- function(cfg, paths) {
  # check if starting values are random
  # check if starting values are consistent across iterations (using same starting values)
  dt_input <- load_data(paths$source$behavior_sr_fit_parameters)
  dt_output <- dt_input %>%
    .[mod == "model"] %>%
    .[variable %in% c("x0_alpha", "x0_gamma")] %>%
    save_data(paths$source$behavior_sr_fit_starting_values) %>%
    .[, .(n_diff_sv = length(unique(value)), n_sv = length(value)),
      by = .(id, iter, variable, model_name)] %>%
    assertr::verify(., n_diff_sv == 1) %>%
    .[, n_diff_sv := NULL]
}

get_behavior_sr_fit_parameter_dispersion <- function(cfg, paths) {
  # calculate the dispersion of parameter estimates
  dt_input <- load_data(paths$source$behavior_sr_fit_parameters)
  dt_output <- dt_input %>%
    .[mod == "model"] %>%
    .[variable %in% c("alpha", "gamma")] %>%
    .[, by = .(id, variable, model_name), .(sd_estimate = sd(value)), ] %>%
    save_data(paths$source$behavior_sr_fit_parameter_dispersion)
}

get_behavior_sr_fit_parameter_distribution <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_parameters)
  dt_output <- dt_input %>%
    .[mod == "model"] %>%
    .[variable %in% c("alpha", "gamma")] %>%
    .[iter == 1, ] %>%
    save_data(paths$source$behavior_sr_fit_parameter_distribution)
}

get_behavior_sr_fit_model_comparison <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_parameters)
  variables = c("aic", "bic")
  dt_output <- dt_input %>%
    .[process == "model_fitting", ] %>%
    .[iter == 1, ] %>%
    .[variable %in% variables, ] %>%
    .[, by = .(id, model_name, variable), .(
      value = unique(value),
      num_values = length(unique(value))
    )] %>%
    verify(.[, by = .(id, model_name), .(num_values = .N)]$num_values == length(variables)) %>%
    .[, by = .(id, variable), num_models := .N] %>%
    # .[num_models == 2, ] %>%
    save_data(paths$source$behavior_sr_fit_model_comparison)
}

get_behavior_sr_fit_model_comparison_stat <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_model_comparison)
  dt_output <-  dt_input %>%
    # .[, by = .(id, variable), num_models := .N] %>%
    # .[num_models == 2, ] %>%
    .[, num_params := ifelse(model_name == "Full", 2, NA)] %>%
    .[, num_params := ifelse(model_name == "Base", 1, num_params)] %>%
    .[variable == "aic", value := value + 2 * num_params] %>%
    .[, by = .(model_name, variable), .(
      num_subs = .N,
      sum_value = sum(value)
    )] %>%
    # TODO: FIX FOR SAME NUMBER OF PARTICIPANTS!
    # verify(num_subs == cfg$num_subs) %>%
    verify(length(unique(num_subs)) == 1)
  # ttest_cfg <- list(
  #   formula = "value ~ model_name",
  #   adjust_method = "none",
  #   paired = FALSE,
  #   mu = 0,
  #   alternative = "two.sided"
  # )
  # dt_output <- dt_input %>%
  #   .[, by = .(variable), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
  #   unnest(ttest) %>%
  #   get_pvalue_adjust(., ttest_cfg) %>%
  #   save_data(paths$source$behavior_sr_fit_model_comparison_stat)
}

get_behavior_sr_fit_parameter_conscious <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_parameter_distribution)
  ttest_cfg <- list(
    lhs = "value",
    rhs = "sequence_detected",
    adjust_method = "none",
    paired = FALSE,
    mu = 0,
    alternative = "two.sided"
  )
  dt_output <- dt_input %>%
    .[, by = .(model_name, variable), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg) %>%
    save_data(paths$source$behavior_sr_fit_parameter_conscious)
}

get_behavior_sr_fit_parameter_order <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_parameter_distribution)
  ttest_cfg <- list(
    lhs = "value",
    rhs = "order",
    adjust_method = "none",
    paired = FALSE,
    mu = 0,
    alternative = "two.sided"
  )
  dt_output <- dt_input %>%
    .[, by = .(model_name, variable), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg) %>%
    save_data(paths$source$behavior_sr_fit_parameter_order)
}

get_sr_matrices <- function(cfg, paths) {
  # get data to plot example SR matrices:
  dt_input <- load_data(paths$source$behavior_sr_fit_sr_matrices)
  # get sr matrix data for two example participants:
  dt_output <- dt_input %>%
    .[condition == "Sequence", ] %>%
    .[, by = .(id, run, graph, condition), last := trial_run == max(trial_run)] %>%
    .[last == 1, ] %>%
    .[, by = .(id), order := ifelse(any(run == "run-01" & graph == "uni"), "uni - bi", "bi - uni")] %>%
    verify(!is.na(order)) %>%
    .[, run_cond := sprintf("%s (%s)", run, graph)] %>%
    setorder(., run) %>%
    save_data(paths$source$behavior_sr_fit_sr_matrices_plot)
}

get_behavior_sr_fit_suprise_effect <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_parameters)
  dt_output <- dt_input %>%
    .[iter == 1, ] %>%
    .[mod == "reg_model",] %>%
    .[substr(variable, start = 0, stop = 2) == "p_",] %>%
    .[, variable := factor(variable, levels = c(
      'p_Intercept',
      'p_shannon_surprise',
      'p_trial_ses',
      'p_block',
      'p_hand_finger_pressedleft_middle',
      'p_hand_finger_pressedleft_ring',
      'p_hand_finger_pressedright_index',
      'p_hand_finger_pressedright_middle',
      'p_hand_finger_pressedright_ring')
    )] %>%
    .[, value_log_20 := log(value, base = 20)] %>%
    save_data(paths$source$behavior_sr_fit_suprise_effect)
}

get_behavior_sr_fit_suprise_effect_num <- function(cfg, paths) {
  alpha_level <- 0.05
  dt_input <- load_data(paths$source$behavior_sr_fit_suprise_effect)
  dt_output <- dt_input %>%
    .[variable == "p_shannon_surprise", ] %>%
    .[model_name == "Full",] %>%
    .[, significance := ifelse(value < alpha_level, "yes", "no")] %>%
    .[, by = .(significance), .(
      num_subs = .N
    )]
}

get_behavior_sr_fit_response_time_alpha <- function(cfg, paths) {
  # correlation between alpha and mean response time:
  dt_input <- load_data(paths$source$behavior_sr_fit_sr_matrices)
  # get number of considered trials (5 less, because trial 1 is removed from each run)
  num_trials <- cfg$sequence$num_trials_run * cfg$sequence$num_runs - cfg$sequence$num_runs
  dt_output <- dt_input %>%
    .[condition == "Sequence",] %>%
    distinct(id, condition, run, trial_run, response_time, alpha) %>%
    .[, by = .(id), .(
      num_trials = .N,
      alpha = unique(alpha),
      mean_response_time = mean(response_time, na.rm = TRUE),
      mean_log_response_time = mean(log(response_time), na.rm = TRUE)
    )] %>%
    verify(num_trials == num_trials) %>%
    save_data(paths$source$behavior_sr_fit_response_time_alpha)
}

get_behavior_sr_fit_response_time_alpha_stat <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_response_time_alpha)
  dt_output <- dt_input %>%
    .[, .(
      num_subs = .N,
      cor = list(broom::tidy(cor.test(alpha, mean_log_response_time, method = "pearson")))
    )] %>%
    verify(num_subs == cfg$num_subs) %>%
    unnest(cor) %>%
    setDT(.) %>%
    get_pvalue_adjust(., list(adjust_method = "fdr")) %>%
    save_data(paths$source$behavior_sr_fit_response_time_alpha_stat)
}

get_behavior_sr_fit_response_time_glm <- function(cfg, paths) {
  dt1 <- load_data(paths$source$behavior_sequence_run_glm) %>%
    .[predictor == "run_index", ]
  dt2 <- load_data(paths$source$behavior_sr_fit_parameters) %>%
    .[iter %in% seq(3)] %>%
    .[variable %in% c("alpha", "gamma"), ]
  dt_output <- merge(dt1, dt2) %>%
    .[, by = .(process, iter, model_name, variable), .(
      num_subs = .N,
      cor = list(broom::tidy(cor.test(estimate, value, method = "pearson")))
    )] %>%
    # verify(num_subs == cfg$num_subs) %>%
    unnest(cor) %>%
    setDT(.) %>%
    get_pvalue_adjust(., list(adjust_method = "fdr")) %>%
    save_data(paths$source$behavior_sr_fit_response_time_glm)
}

get_behavior_sr_fit_response_time_hafrun_glm <- function(cfg, paths) {
  dt1 <- load_data(paths$source$behavior_sequence_halfrun_glm) %>%
    .[predictor == "block_index", ] %>%
    .[graphblock == "first graph", ]
  dt2 <- load_data(paths$source$behavior_sr_fit_parameters) %>%
    .[iter %in% seq(3)] %>%
    .[variable %in% c("alpha", "gamma"), ]
  dt_output <- merge(dt1, dt2) %>%
    .[, by = .(process, iter, model_name, variable), .(
      num_subs = .N,
      cor = list(broom::tidy(cor.test(estimate, value, method = "pearson")))
    )] %>%
    # verify(num_subs == cfg$num_subs) %>%
    unnest(cor) %>%
    setDT(.) %>%
    get_pvalue_adjust(., list(adjust_method = "fdr"))
}

get_behavior_sr_fit_response_time_hafrun_glm <- function(cfg, paths) {
  dt1 <- load_data(paths$source$behavior_sequence_halfrun_glm) %>%
    .[predictor == "halfrun", ]
  dt2 <- load_data(paths$source$behavior_sr_fit_parameters) %>%
    .[iter %in% seq(3)] %>%
    .[variable %in% c("alpha", "gamma"), ]
  dt_output <- merge(dt1, dt2) %>%
    .[, by = .(process, iter, model_name, variable), .(
      num_subs = .N,
      cor = list(broom::tidy(cor.test(estimate, value, method = "pearson")))
    )] %>%
    # verify(num_subs == cfg$num_subs) %>%
    unnest(cor) %>%
    setDT(.) %>%
    get_pvalue_adjust(., list(adjust_method = "fdr"))
}

get_behavior_sr_fit_response_time_onestep <- function(cfg, paths) {
  dt1 <- load_data(paths$source$behavior_sequence_onestep)
  dt2 <- load_data(paths$source$behavior_sr_fit_parameters) %>%
    .[process == "model_fitting", ] %>%
    .[model_name == "Full", ] %>%
    .[iter %in% 1] %>%
    .[variable %in% c("gamma"), ]
  dt_output <- merge(dt1, dt2) %>%
    .[, by = .(graph, onestep), .(
      num_subs = .N,
      cor = list(broom::tidy(cor.test(mean_log_response_time, value, method = "pearson")))
    )] %>%
    verify(num_subs == cfg$num_subs) %>%
    unnest(cor) %>%
    setDT(.) %>%
    get_pvalue_adjust(., list(adjust_method = "fdr")) %>%
    save_data(paths$source$behavior_sr_fit_response_time_onestep)
}

get_behavior_sr_fit_response_time_onestep_diff <- function(cfg, paths) {
  dt1 <- load_data(paths$source$behavior_sequence_onestep_run_glm)
  dt2 <- load_data(paths$source$behavior_sr_fit_parameters) %>%
    .[process == "Model Fitting", ] %>%
    .[model_name == "Full", ] %>%
    .[iter %in% 1] %>%
    .[variable %in% c("alpha", "gamma"), ]
  dt_output <- merge(dt1, dt2) %>%
    save_data(paths$source$behavior_sr_fit_response_time_onestep_diff) %>%
    .[, by = .(variable), .(
      num_subs = .N,
      cor = list(broom::tidy(cor.test(slope_diff, value, method = "pearson")))
    )] %>%
    verify(num_subs == cfg$num_subs) %>%
    unnest(cor) %>%
    setDT(.) %>%
    get_pvalue_adjust(., list(adjust_method = "fdr")) %>%
    .[, result := sprintf("r = %.2f, p = %.2f", estimate, p.value)] %>%
    save_data(paths$source$behavior_sr_fit_response_time_onestep_run_stat)
}

get_behavior_sr_fit_parameter_recovery <- function() {
  dt_input <- dt_input_sr
  dt_output <- dt_input %>%
    .[iter == 1, ] %>%
    .[mod == "model", ] %>%
    .[variable %in% c("alpha", "gamma"), ] %>%
    .[, c("id", "process", "variable", "value")]
  ggplot(data = dt_output) +
    facet_wrap(~ variable) +
    geom_point(mapping = aes(x = process, y = value, color = id)) +
    geom_line(mapping = aes(x = process, y = value, group = id, color = id)) +
    theme(legend.position = "none")
  
  
}
