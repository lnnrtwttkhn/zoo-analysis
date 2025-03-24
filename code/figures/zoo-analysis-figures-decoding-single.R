plot_decoding_single_accuracy_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_single_peak_accuracy_mean)
  chance_level <- 100/6
  figure <- ggplot(data = dt_input, aes(x = mask_test, y = mean_accuracy)) +
    geom_hline(yintercept = chance_level, linetype = "dashed", color = "gray") +
    geom_beeswarm(aes(group = id), alpha = 0.3, color = "black") +
    geom_boxplot(width = 0.2, outlier.shape = NA) +
    stat_summary(geom = "point", fun = "mean", pch = 23) +#
    stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0) +
    # geom_text(data = dt_recall_acc_mean, aes(
    #  y = 100, label = paste("M = ", round(mean_acc, 2))),
    #  size = 2, angle = 90, hjust = 1, vjust = -1) +
    # geom_text(data = dt_recall_acc_mean_stat, aes(
    #   y = 120, label = p.value_adjust_round_label),
    #   size = 2, angle = 90, hjust = 1, vjust = 0) +
    theme_zoo() +
    ylim(c(0, 100)) +
    coord_capped_cart(expand = TRUE, left = "both", bottom = "both") +
    ylab("Decoding accuracy (in %)") +
    theme(axis.line.x = element_line(color = "white")) +
    theme(axis.ticks.x = element_line(color = "white")) +
    theme(axis.title.x = element_text(color = "white")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(legend.text = element_text(size = 6)) +
    theme(legend.title = element_text(size = 6)) +
    theme(legend.key.size = unit(0.4, "line")) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = -5, r = 0, b = 0, l = 0)) +
    theme(legend.position = "none") +
    annotate("text", x = 1.5, y = chance_level - 5,
             label = "Chance", color = "gray",
             size = rel(3), family = "Helvetica", fontface = "plain")
  save_figure(plot = figure, "decoding_single_accuracy_mean", width = 2, height = 3)
  return(figure)
}

plot_decoding_single_accuracy_run <- function(cfg, paths){
  chance_level <- 100/6
  dt <- load_data(paths$source$decoding_single_peak_accuracy_run)
  figure <- ggplot(data = dt, aes(x = as.factor(run_index), y = as.numeric(mean_accuracy))) +
    geom_hline(yintercept = chance_level, linetype = "dashed", color = "gray") +
    geom_beeswarm(aes(color = id), alpha = 0.3) +
    geom_boxplot(width = 0.2, outlier.shape = NA) +
    stat_summary(geom = "point", fun = "mean", pch = 23) +
    stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0) +
    #geom_text(data = dt_recall_mean, aes(
    #  y = 100, label = paste("M = ", round(mean_acc, 2))),
    #  size = 2, angle = 90, hjust = 1, vjust = -1) +
    # geom_text(data = dt_recall_stat, aes(
    #   y = 120, label = p.value_adjust_round_label),
    #   size = 2, angle = 90, hjust = 1, vjust = 0) +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, left = "both", bottom = "both") +
    facet_wrap(~ mask_test) +
    ylim(c(0, 100)) +
    ylab("Decoding accuracy (%)") +
    xlab("Run") +
    # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(legend.text = element_text(size = 6)) +
    theme(legend.title = element_text(size = 6)) +
    theme(legend.key.size = unit(0.4, "line")) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = -5, r = 0, b = 0, l = 0)) +
    theme(legend.position = "none") +
    annotate("text", x = 9, y = chance_level - 5, hjust = 1,
             label = "Chance", color = "gray",
             size = rel(3), family = "Helvetica", fontface = "plain")
  save_figure(plot = figure, "decoding_single_accuracy_run", width = 6, height = 4)
  return(figure)
}

plot_decoding_single_interval <- function(cfg, paths){
  chance_level <- 100/cfg$single$num_trs
  dt1 <- load_data(paths$source$decoding_single_interval_trial_mean)
  dt2 <- load_data(paths$source$decoding_single_interval_trial)
  figure <- ggplot(dt1, aes(x = as.factor(interval_tr), y = as.numeric(mean_probability))) +
    geom_hline(yintercept = chance_level, linetype = "dashed", color = "gray") +
    geom_line(data = dt2, aes(
      group = interaction(as.factor(id), as.factor(current_stim)),
      color = as.factor(current_stim)), alpha = 0.1) +
    facet_wrap(~ mask_test) +
    geom_ribbon(aes(
      group = as.factor(current_stim),
      ymin = sem_lower, ymax = sem_upper,
      fill = as.factor(current_stim)), alpha = 0.5) +
    geom_line(aes(
      group = as.factor(current_stim),
      color = as.factor(current_stim))) +
    theme_zoo() +
    xlab("Time from event onset (in TRs; 1 TR = 1.25 s)") +
    ylab("Probability (in %)") +
    coord_capped_cart(expand = TRUE, left = "both", bottom = "both") +
    scale_x_discrete(labels = label_fill(seq(1, 15, 1), mod = 7), breaks = seq(1, 15, 1)) +
    scale_color_manual(values = cfg$colors_decoding_current, name = "Events") +
    scale_fill_manual(values = cfg$colors_decoding_current, name = "Events") +
    theme(legend.position = "bottom") +
    theme(legend.box = "horizontal") +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = -5, r = 0, b = 0, l = 0)) +
    annotate("text", x = 15, y = chance_level - 5, hjust = 1,
             label = "Chance", color = "gray",
             size = rel(2.5), family = "Helvetica", fontface = "plain")
  save_figure(plot = figure, "decoding_single_interval", width = 6, height = 4)
 return(figure)
}

plot_decoding_single_interval_node <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_single_interval_node) %>%
    .[mask_test == "visual", ] %>%
    .[, node := paste("Node", node)] %>%
    .[, mean_probability := mean_probability * 100]
  figure <- ggplot(data = dt_input) +
    geom_line(aes(
      x = interval_tr, y = mean_probability, group = interaction(as.factor(id), as.factor(class)),
      color = as.factor(class)), alpha = 0.1) +
    stat_summary(geom = "ribbon", fun.data = "mean_se", aes(
      x = interval_tr, y = mean_probability, fill = as.factor(class)), alpha = 0.3) +
    stat_summary(geom = "line", fun = "mean", aes(
      x = interval_tr, y = mean_probability, color = as.factor(class))) +
    stat_summary(geom = "point", fun = "mean", aes(
      x = interval_tr, y = mean_probability, color = as.factor(class))) +
    facet_wrap(~ as.factor(node), nrow = 1) +
    coord_capped_cart(left = "both", bottom = "both", ylim = c(0, 80), xlim = c(1, 10)) +
    xlab("Time from stimulus onset (in TRs; 1 TR = 1.25 s)") +
    ylab("Probability (%)") +
    scale_colour_manual(name = "Class", values = cfg$colors_class) +
    scale_fill_manual(name = "Class", values = cfg$colors_class) +
    scale_x_continuous(labels = label_fill(seq(1, 10, 1), mod = 3), breaks = seq(1, 10, 1)) +
    theme_zoo() +
    guides(color = guide_legend(nrow = 1, title.position = "left")) +
    theme(legend.position = "bottom", legend.box = "horizontal")
  return(figure)
}

plot_decoding_single_fit <- function(cfg, paths){
  # visualize true data vs. modeling
  set.seed(19)
  dt_input1 <- load_data(paths$source$decoding_single_interval_sine_fit_eval)
  dt_input2 <- load_data(paths$source$decoding_single_interval_node) %>%
    .[current_stim == TRUE, ]
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
    .[current_stim == TRUE, ] %>%
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

plot_decoding_single_fit_sub <- function(cfg, paths) {
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
  fig4 <- plot_decoding_single_interval_node(cfg, paths)
  figure <- plot_grid(
    fig1, fig2, fig3, fig4,
    ncol = 1, nrow = 4, rel_heights = c(3.7, 2, 2, 2.2),
    labels = c("a", "b", "c", "d")
  )
  save_figure(plot = figure, "decoding_single_sine_fit", width = 7, height = 11)
  return(figure)
}

plot_sequentiality_illustration <- function(cfg, paths) {
  rect_offset <- 0.25
  timepoints <- c(3, 6)
  y_values <- c(3, 44)
  dt_input1 <- load_data(paths$source$decoding_single_interval_sine_fit_mean_eval)
  dt_input2 <- load_data(paths$source$decoding_single_interval_sine_fit_mean_eval) %>%
    .[time %in% timepoints, ]
  fig1 <- ggplot(data = NULL, aes(x = time, y = sine_probability)) +
    # add gray rectangles in the background:
    geom_rect(data = dt_input2 %>% .[event == 1, ], aes(
      xmin = time - rect_offset, xmax = time + rect_offset, ymin = y_values[1], ymax = y_values[2]),
      fill = "gray95") + 
    # add sine-waves:
    geom_line(data = dt_input1, aes(color = as.factor(event))) +
    # add dots to sine-wave:
    geom_point(data = dt_input2, aes(color = as.factor(event))) +
    # add black line around rectangles
    geom_rect(data = dt_input2 %>% .[event == 1, ], aes(
      xmin = time - rect_offset, xmax = time + rect_offset, ymin = y_values[1], ymax = y_values[2]),
      fill = NA, color = "black", linetype = "solid") + 
    # add text labels to event onsets:
    geom_text(data = dt_input2 %>% .[time == timepoints[1], ], aes(
      label = event, y = y_values[1] - 2, x = onset, color = as.factor(event)),
      show.legend = FALSE) +
    # add sticks to event onsets
    geom_segment(data = dt_input2 %>% .[time == timepoints[1], ], aes(
      x = onset, y = baseline, xend = onset, yend = baseline + 1.5, color = as.factor(event)),
      show.legend = FALSE) +
    # add arrows from rectangles:
    geom_curve(data = dt_input2 %>% .[time == timepoints[1], ], aes(
      x = time, y = y_values[2], xend = 9, yend = y_values[2]),
      arrow = arrow(length = unit(0.03, "npc")),
      curvature = -0.2) +
    geom_curve(data = dt_input2 %>% .[time == timepoints[2], ], aes(
      x = time, y = y_values[1], xend = 9, yend = y_values[1]),
      arrow = arrow(length = unit(0.03, "npc")),
      curvature = 0.3) +
    coord_capped_cart(left = "both", bottom = "both", ylim = c(0, 50)) +
    xlab("Time (in TRs; 1 TR = 1.25 s)") +
    ylab("Probability (%)") +
    ggtitle("Classifier probability time courses") +
    scale_x_continuous(labels = label_fill(seq(1, 10, 1), mod = 3), breaks = seq(0, 9, 1)) +
    scale_colour_manual(name = "Event", values = cfg$colors$class) +
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
  save_figure(plot = figure, "sequentiality_illustration", width = 7, height = 4.5)
  return(figure)
}
