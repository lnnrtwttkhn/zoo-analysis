get_decoding_single_interval_trial <- function(cfg, paths) {
  # calculate the mean classifier probability for current vs. other stimuli
  dt_input <- load_data(paths$source$decoding_single_interval)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, node, class, interval_tr), .(
      num_trials = .N,
      mean_probability = mean(probability_norm * 100),
      current_stim = as.factor(class == unique(node))
    )] %>%
    verify(current_stim %in% c(TRUE, FALSE)) %>%
    .[, current_stim := ifelse(current_stim == TRUE, "Current", "Other")] %>%
    .[, current_stim := factor(as.factor(current_stim), levels = c("Current", "Other"))] %>%
    verify(num_trials <= cfg$decoding_single_interval$max_trials_run) %>%
    verify(.[, by = .(id, mask_test, interval_tr, current_stim), .(
      num_classes = .N
    )]$num_classes %in% c(30, 6)) %>%
    .[, by = .(id, mask_test, interval_tr, current_stim), .(
      mean_probability = mean(mean_probability)
    )] %>%
    save_data(paths$source$decoding_single_interval_trial)
}

get_decoding_single_interval_stat <- function(cfg, paths) {
  # compare the probability at the fourth TR between current vs. other stimuli
  dt_input <- load_data(paths$source$decoding_single_interval_trial) %>%
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
    .[interval_tr == 4, ] %>%
    .[, by = .(mask_test, interval_tr), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg)
  return(dt_output)
}

get_decoding_single_interval_trial_mean <- function(cfg, paths) {
  # calculate the mean classifier time course for current vs. other events across participants
  dt_input <- load_data(paths$source$decoding_single_interval_trial)
  dt_output <- dt_input %>%
    .[, time := (as.numeric(interval_tr) - 1) * cfg$tr] %>%
    .[, interval_tr := as.factor(interval_tr)] %>%
    .[, by = .(mask_test, interval_tr, time, current_stim), .(
      mean_probability = mean(mean_probability),
      num_subs = .N,
      sem_upper = (mean(mean_probability) + (sd(mean_probability)/sqrt(.N))),
      sem_lower = (mean(mean_probability) - (sd(mean_probability)/sqrt(.N)))
    )] %>%
    verify(num_subs == cfg$num_subs) %>%
    .[, num_subs := NULL] %>%
    save_data(paths$source$decoding_single_interval_trial_mean)
}

get_decoding_single_interval_node <- function(cfg, paths) {
  # calculate the mean classifier time course for each participant and for each node
  dt_input <- load_data(paths$source$decoding_single_interval)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, node, class, interval_tr), .(
      num_trials = .N,
      mean_probability = mean(probability),
      current_stim = as.factor(class == unique(node))
    )] %>%
    verify(current_stim %in% c(TRUE, FALSE)) %>%
    assertr::assert(., within_bounds(0, 100), mean_probability) %>%
    verify(num_trials <= cfg$decoding_single_interval$max_trials_run) %>%
    save_data(paths$source$decoding_single_interval_node)
}

get_decoding_single_fit <- function(cfg, paths){
  dt_input <- load_data(paths$source$decoding_single_interval_node) %>%
    .[current_stim == TRUE, ]
  tr_seq <- cfg$sine_params$time + 1
  dt_output <- dt_input %>%
    .[interval_tr %in% tr_seq, ] %>%
    verify(all(tr_seq %in% interval_tr)) %>%
    verify(.[, by = .(id, mask_test), .(
      num_nodes = length(unique(node))
    )]$num_nodes == cfg$num_nodes) %>%
    .[, by = .(id, mask_test, node), {
      results = nloptr::nloptr(
        x0 = cfg$sine_params$default_params,
        eval_f = sine_truncated_eval,
        time = cfg$sine_params$time,
        data = mean_probability,
        lb = cfg$sine_params$lower_bounds,
        ub = cfg$sine_params$upper_bounds,
        opts = cfg$sine_params$opts
      )
      list(results = list(results))}] %>%
    .[, by = .(id, mask_test, node), ":="(
      frequency = results[[1]]$solution[1],
      wavelength = 1/results[[1]]$solution[1],
      amplitude = results[[1]]$solution[2],
      shift = results[[1]]$solution[3],
      baseline = results[[1]]$solution[4]
    )] %>%
    .[, results := NULL] %>%
    setorder(., id, mask_test, node) %>%
    verify(.[, by = .(id, mask_test), .(
      num_nodes = length(unique(node)))]$num_nodes == cfg$num_nodes) %>%
    verify(.[, by = .(mask_test, node), .(
      num_subs = length(unique(id)))]$num_subs == cfg$num_subs) %>%
    save_data(paths$source$decoding_single_interval_sine_fit)
}

get_decoding_single_fit_eval <- function(cfg, paths) {
  # evaluate the sine function per participant and node:
  dt_input <- load_data(paths$source$decoding_single_interval_sine_fit)
  dt_output <- dt_input %>%
    .[, params := lapply(transpose(.SD), c), .SDcols = cfg$sine_params$names] %>%
    .[, by = .(id, mask_test, node), .(
      sine_probability = sine_truncated(params = unlist(params), time = cfg$sine_params$time_eval))] %>%
    .[, by = .(id, mask_test, node), time := cfg$sine_params$time_eval] %>%
    save_data(paths$source$decoding_single_interval_sine_fit_eval)
}

get_decoding_single_fit_eval_mean <- function(cfg, paths) {
  # evaluate the sine function per node (across participants):
  # note that here we average the parameters first and then evaluate the function.
  dt_input <- load_data(paths$source$decoding_single_interval_sine_fit)
  dt_output <- dt_input %>%
    .[, by = .(mask_test, node), .(
      num_subs = .N,
      frequency = mean(frequency),
      amplitude = mean(amplitude),
      shift = mean(shift),
      baseline = mean(baseline)
    )] %>%
    verify(all(num_subs == cfg$num_subs)) %>%
    .[, params := lapply(transpose(.SD), c), .SDcols = cfg$sine_params$names] %>%
    .[, by = .(mask_test, node), .(sine_probability = sine_truncated(params = unlist(params), time = cfg$sine_params$time_eval))] %>%
    .[, by = .(mask_test, node), time := cfg$sine_params$time_eval] %>%
    .[, node := paste("Node", node)] %>%
    save_data(paths$source$decoding_single_interval_sine_fit_eval_mean)
}

get_decoding_single_fit_sub <- function(cfg, paths) {
  # Evaluate the sine function per node (within participants, then average).
  # Note that here we evaluate the function first, then average across participants:
  dt_input <- load_data(paths$source$decoding_single_interval_sine_fit)
  dt_output <- dt_input %>%
    .[, params := lapply(transpose(.SD), c), .SDcols = cfg$sine_params$names] %>%
    .[, by = .(id, mask_test, node), .(sine_probability = sine_truncated(params = unlist(params), time = cfg$sine_params$time_eval))] %>%
    .[, by = .(id, mask_test, node), time := cfg$sine_params$time_eval] %>%
    .[, node := paste("Node", node)] %>%
    .[, sine_probability := sine_probability * 100] %>%
    save_data(paths$source$decoding_single_interval_sine_fit_sub)
}

get_decoding_single_fit_mean <- function(cfg, paths) {
  # calculate 
  dt_input <- load_data(paths$source$decoding_single_interval_sine_fit)
  dt_output <- dt_input %>%
    melt(., measure.vars = c("wavelength", cfg$sine_params$names),
         variable.name = "parameter") %>%
    .[, by = .(mask_test, parameter), .(
      mean_value = mean(value)
      # num_nodes = .N
    )] %>%
    # verify(num_nodes == cfg$num_nodes) %>%
    save_data(paths$source$decoding_single_interval_sine_fit_mean)
}

get_decoding_single_fit_mean_eval <- function(cfg, paths) {
  # evaluate the sine function per participant and node:
  dt_input <- load_data(paths$source$decoding_single_interval_sine_fit_mean)
  timeshift = (0.6 + 0.1) / cfg$tr
  dt_output <- dt_input %>%
    .[mask_test == "visual", ] %>%
    dcast(., mask_test ~ parameter, value.var = "mean_value") %>%
    .[rep(1:.N, each = cfg$num_nodes)] %>%
    .[, event := rep(1:cfg$num_nodes, length.out = .N)] %>%
    .[, shift := shift + timeshift * (event - 1)] %>%
    .[, onset := 0 + timeshift * (event - 1)] %>%
    .[, params := lapply(transpose(.SD), c), .SDcols = cfg$sine_params$names] %>%
    .[, by = .(mask_test, event, onset, baseline), .(
      sine_probability = sine_truncated(params = unlist(params), time = cfg$sine_params$time_eval))] %>%
    .[, sine_probability := sine_probability * 100] %>%
    .[, baseline := baseline * 100] %>%
    .[, by = .(mask_test, event, onset, baseline), time := cfg$sine_params$time_eval] %>%
    setorder(., mask_test, event, time) %>%
    save_data(paths$source$decoding_single_interval_sine_fit_mean_eval)
}
