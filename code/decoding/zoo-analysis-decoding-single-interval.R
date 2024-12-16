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
  dt_input <- load_data(paths$source$decoding_single_interval_trial)
  ttest_cfg <- list(
    formula = "mean_probability ~ current_stim",
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
  # calculate the mean classifier time course for each participant for each node, then reduce to the current node
  dt_input <- load_data(paths$source$decoding_single_interval)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, node, class, interval_tr), .(
      num_trials = .N,
      mean_probability = mean(probability),
      current_stim = as.factor(class == unique(node))
    )] %>%
    verify(current_stim %in% c(TRUE, FALSE)) %>%
    verify(num_trials <= cfg$decoding_single_interval$max_trials_run) %>%
    .[current_stim == TRUE, ] %>%
    .[, by = .(id, mask_test, interval_tr, node), .(
      mean_probability = mean(mean_probability)
    )] %>%
    save_data(paths$source$decoding_single_interval_node)
}

get_decoding_single_interval_node_all = function(cfg, paths) {
  # calculate the mean classifier time course for each participant for each node and consider all nodes
  dt_input <- load_data(paths$source$decoding_single_interval)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, node, class, interval_tr), .(
      num_trials = .N,
      mean_probability = mean(probability),
      current_stim = as.factor(class == unique(node))
    )] %>%
    assertr::assert(., within_bounds(0, 100), mean_probability) %>%
    verify(current_stim %in% c(TRUE, FALSE)) %>%
    verify(num_trials <= cfg$decoding_single_interval$max_trials_run) %>%
    save_data(paths$source$decoding_single_interval_node_all)
}

get_decoding_single_fit <- function(cfg, paths){
  dt_input <- load_data(paths$source$decoding_single_interval_node)
  tr_seq <- cfg$sine_params$time + 1
  dt_output <- dt_input %>%
    .[interval_tr %in% tr_seq, ] %>%
    verify(all(tr_seq %in% interval_tr)) %>%
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

get_decoding_single_fit_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_single_interval_sine_fit)
  dt_output <- dt_input %>%
    melt(., measure.vars = c("wavelength", cfg$sine_params$names),
        variable.name = "parameter") %>%
    .[, by = .(mask_test, parameter), .(
      mean_value = mean(value),
      num_nodes = .N
    )] %>%
    verify(num_nodes == cfg$num_nodes) %>%
    save_data(paths$source$decoding_single_interval_sine_fit_mean)
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

plot_decoding_single_fit <- function(cfg, paths){
  # visualize true data vs. modeling
  set.seed(19)
  dt_input1 <- load_data(paths$source$decoding_single_interval_sine_fit_eval)
  dt_input2 <- load_data(paths$source$decoding_single_interval_node)
  num_sub_select <- 3
  select_id <- sample(x = unique(dt_input1$id), size = num_sub_select)
  
  dt1 <- dt_input1 %>%
    .[id %in% select_id, ] %>%
    .[mask_test == "visual", ] %>%
    .[, sine_probability := sine_probability * 100] %>%
    .[, node := paste("Node", node)]
  
  dt2 <- dt_input2 %>%
    .[id %in% select_id, ] %>%
    .[mask_test == "visual", ] %>%
    .[interval_tr %in% seq(1, 10)] %>%
    .[, interval_tr := interval_tr - 1] %>%
    .[, mean_probability := mean_probability * 100] %>%
    .[, node := paste("Node", node)] %>%
    setorder(., "id", "mask_test", "node", "interval_tr")
  
  figure <- ggplot(data = dt1, aes(x = time, y = sine_probability)) +
    geom_point(aes(color = "Model"), alpha = 0.5) +
    geom_line(data = dt2, aes(x = interval_tr, y = mean_probability, color = "Data")) +
    geom_point(data = dt2, aes(x = interval_tr, y = mean_probability, color = "Data")) +
    scale_colour_manual(name = "", values = c("gray", "black")) +
    facet_grid(vars(as.factor(id)), vars(as.factor(node))) +
    coord_capped_cart(left = "both", bottom = "both", ylim = c(0, 80)) +
    xlab("Time from stimulus onset (in TRs; 1 TR = 1.25 s)") +
    ylab("Probability (%)") +
    scale_x_continuous(labels = label_fill(seq(1, 10, 1), mod = 3), breaks = seq(0, 9, 1)) +
    theme(legend.position = "top") +
    theme(legend.direction = "horizontal") +
    theme(legend.justification = "center") +
    theme(legend.margin = margin(0, 0 ,0, 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme_zoo()
  return(figure)
}

plot_decoding_single_fit_mean <- function(cfg, paths){
  
  dt_input1 <- load_data(paths$source$decoding_single_interval_node) %>%
    .[mask_test == "visual", ] %>%
    .[interval_tr %in% seq(1, 10)] %>%
    .[, interval_tr := interval_tr - 1] %>%
    .[, mean_probability := mean_probability * 100] %>%
    .[, node := paste("Node", node)] %>%
    setorder(., "id", "mask_test", "node", "interval_tr")
  
  dt_input2 <- load_data(paths$source$decoding_single_interval_sine_fit_eval_mean) %>%
    .[mask_test == "visual", ] %>%
    .[, sine_probability := sine_probability * 100]
  
  figure <- ggplot() +
    geom_line(data =  dt_input1, aes(
      x = interval_tr, y = mean_probability, group = as.factor(id), color = "Data"), alpha = 0.3) +
    geom_line(data = dt_input2, aes(x = time, y = sine_probability, color = "Model"), linewidth = 1) +
    facet_wrap(~ as.factor(node), nrow = 1) +
    coord_capped_cart(left = "both", bottom = "both", ylim = c(0, 80)) +
    xlab("Time from stimulus onset (in TRs; 1 TR = 1.25 s)") +
    ylab("Probability (%)") +
    scale_colour_manual(name = "", values = c("gray", "black"), guide = "none") +
    scale_x_continuous(labels = label_fill(seq(1, 10, 1), mod = 3), breaks = seq(0, 9, 1)) +
    theme_zoo()
  return(figure)
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

plot_decoding_single_fit_sub <- function(cfg, pats) {
  dt_input <- load_data(paths$source$decoding_single_interval_sine_fit_sub) %>%
    .[mask_test == "visual", ]
  figure <- ggplot(data = dt_input, aes(x = time, y = sine_probability)) +
    geom_line(aes(group = interaction(as.factor(id), as.factor(node)),
                  color = as.factor(node)), alpha = 0.1) +
    stat_summary(geom = "ribbon", fun.data = "mean_se", aes(fill = as.factor(node)), color = NA, alpha = 0.5) +
    stat_summary(geom = "line", fun = "mean", aes(color = as.factor(node))) +
    stat_summary(data = dt_input %>% .[time %in% seq(0, 9, 1)],
                 geom = "point", fun = "mean", aes(color = as.factor(node))) +
    facet_wrap(~ as.factor(node), nrow = 1) +
    coord_capped_cart(left = "both", bottom = "both", ylim = c(0, 80), xlim = c(0, 9)) +
    xlab("Time from stimulus onset (in TRs; 1 TR = 1.25 s)") +
    ylab("Probability (%)") +
    scale_colour_manual(name = "", values = cfg$colors$class, guide = "none") +
    scale_fill_manual(name = "", values = cfg$colors$class, guide = "none") +
    scale_x_continuous(labels = label_fill(seq(1, 10, 1), mod = 3), breaks = seq(0, 9, 1)) +
    theme_zoo()
  return(figure)
}

plot_decoding_single <- function(cfg, paths) {
  fig1 <- plot_decoding_single_fit(cfg, paths)
  fig2 <- plot_decoding_single_fit_mean(cfg, paths)
  fig3 <- plot_decoding_single_fit_sub(cfg, paths)
  figure <- plot_grid(
    fig1, fig2, fig3,
    ncol = 1, nrow = 3, rel_heights = c(4, 2, 2),
    labels = c("a", "b", "c")
  )
  save_figure(plot = figure, "decoding_single_sine_fit", width = 7, height = 9)
  return(figure)
}

get_decoding_single_fit_mean_eval <- function(cfg, paths) {
  # evaluate the sine function per participant and node:
  dt_input <- load_data(paths$source$decoding_single_interval_sine_fit_mean)
  timeshift = (0.512 + 0.1) / cfg$tr
  dt_output <- dt_input %>%
    .[mask_test == "visual", ] %>%
    dcast(., mask_test ~ parameter, value.var = "mean_value") %>%
    .[rep(1:.N, each = cfg$num_nodes)] %>%
    .[, event := rep(1:cfg$num_nodes, length.out = .N)] %>%
    .[, shift := shift + timeshift * event - 1] %>%
    .[, params := lapply(transpose(.SD), c), .SDcols = cfg$sine_params$names] %>%
    .[, by = .(mask_test, event), .(
      sine_probability = sine_truncated(params = unlist(params), time = cfg$sine_params$time_eval))] %>%
    .[, sine_probability := sine_probability * 100] %>%
    .[, by = .(mask_test, event), time := cfg$sine_params$time_eval] %>%
    setorder(., mask_test, event, time) %>%
    save_data(paths$source$decoding_single_interval_sine_fit_mean_eval)
}

plot_sequentiality_illustration <- function(cfg, paths) {
  rect_offset <- 0.25
  timepoints = c(3, 5)
  dt_input1 <- load_data(paths$source$decoding_single_interval_sine_fit_mean_eval)
  dt_input2 <- load_data(paths$source$decoding_single_interval_sine_fit_mean_eval) %>%
    .[time %in% timepoints, ]
  fig1 <- ggplot(data = NULL, aes(x = time, y = sine_probability)) +
    geom_line(data = dt_input1, aes(color = as.factor(event))) +
    geom_point(data = dt_input2, aes(color = as.factor(event))) +
    geom_rect(data = dt_input2 %>% .[event == 1, ], aes(
      xmin = time - rect_offset, xmax = time + rect_offset, ymin = 2.5, ymax = 45),
      fill = NA, color = "black", linetype = "dashed") + 
    coord_capped_cart(left = "both", bottom = "both", ylim = c(0, 50)) +
    xlab("Time (in TRs; 1 TR = 1.25 s)") +
    ylab("Probability (%)") +
    ggtitle("Classifier probability time courses") +
    scale_x_continuous(labels = label_fill(seq(1, 10, 1), mod = 3), breaks = seq(0, 9, 1)) +
    scale_colour_manual(name = "Event", values = cfg$colors$class) +
    scale_fill_manual(name = "Event", values = cfg$colors$class) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme_zoo()
  fig2 <- ggplot(data = dt_input2 %>% .[time == timepoints[1], ], aes(
    x = as.factor(event), y = sine_probability)) +
    geom_point(aes(color = as.factor(event))) +
    geom_smooth(aes(x = rev(event)), method = "lm", color = "black", se = FALSE) +
    coord_capped_cart(left = "both", bottom = "both", ylim = c(0, 50)) +
    xlab("Serial position (reverse)") +
    ylab("Probability (%)") +
    ggtitle("Forward sequentiality") +
    scale_colour_manual(values = cfg$colors$class, guide = "none") +
    scale_fill_manual(values = cfg$colors$class, guide = "none") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    scale_x_discrete(limits = rev(levels(as.factor(seq(1, 6))))) +
    theme_zoo()
  fig3 <- ggplot(data = dt_input2 %>% .[time == timepoints[2], ], aes(
    x = as.factor(event), y = sine_probability)) +
    geom_point(aes(color = as.factor(event))) +
    geom_smooth(aes(x = rev(event)), method = "lm", color = "black", se = FALSE) +
    coord_capped_cart(left = "both", bottom = "both", ylim = c(0, 50)) +
    xlab("Serial position (reverse)") +
    ylab("Probability (%)") +
    ggtitle("Backward sequentiality") +
    scale_colour_manual(values = cfg$colors$class, guide = "none") +
    scale_fill_manual(values = cfg$colors$class, guide = "none") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    scale_x_discrete(limits = rev(levels(as.factor(seq(1, 6))))) +
    theme_zoo()
  plot_legend = get_legend(fig1)
  figure <- plot_grid(
    fig1 + theme(legend.position = "none"),
    plot_grid(fig2, fig3, nrow = 2, ncol = 1, labels = c("b", "c")),
    plot_legend,
    ncol = 3, nrow = 1, rel_widths = c(0.55, 0.35, 0.1),
    labels = c("a")
  )
  save_figure(plot = figure, "sequentiality_illustration", width = 7, height = 5)
  return(figure)
}
