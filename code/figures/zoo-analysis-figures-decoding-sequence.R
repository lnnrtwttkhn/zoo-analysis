plot_decoding_main_current_mean <- function(cfg, paths) {
  # chance_level <- 100/cfg$decoding_sequence$num_trs
  dt1 <- load_data(paths$source$decoding_main_current_mean)
  dt2 <- load_data(paths$source$decoding_main_current_stat)
  figure <- ggplot(data = dt1, aes(x = current_stim, y = mean_probability, color = current_stim)) +
    # geom_hline(aes(yintercept = chance_level), linetype = "dashed", color = "gray") +
    geom_beeswarm(dodge.width = 0.9, alpha = 0.3) +
    geom_boxplot(aes(group = current_stim, color = current_stim),
                 position = position_dodge(0.9), outlier.shape = NA, width = 0.5) +
    stat_summary(geom = "point", fun = "mean", position = position_dodge(0.9), pch = 23) +
    stat_summary(geom = "linerange", fun.data = "mean_se", position = position_dodge(0.9)) +
    # annotate("text", x = 0.5, y = 100 / 15, label = "Chance", hjust = -0.2, vjust = 2,
    #          color = "gray", size = rel(2.5), family = "Helvetica", fontface = "plain") +
    annotate(geom = "segment", x = 1, y = 17.5, xend = 2, yend = 17.5, color = "gray") +
    geom_text(data = dt2, aes(x = 1.5, y = 18, label = p.value_adjust_round_label),
              color = "gray", parse = FALSE, size = rel(2.5)) +
    facet_wrap(~ mask_test) +
    ylab("Probability (in %)") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, ylim = c(0, 20)) +
    scale_color_manual(values = cfg$colors_decoding_current, name = "Events") +
    scale_fill_manual(values = cfg$colors_decoding_current, name = "Events") +
    theme(legend.position = "bottom") +
    theme(legend.box = "horizontal") +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = -5, r = 0, b = 0, l = 0)) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.line.x = element_line(colour = "white")) +
    theme(axis.title.x = element_blank()) +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.text.x = element_blank())
  save_figure(plot = figure, "decoding_main_current_mean", width = 3, height = 4)
  return(figure)
}

plot_decoding_main_current_interval <- function(cfg, paths) {
  # chance_level <- 100/8
  dt <- load_data(paths$source$decoding_main_current_interval)
  figure <- ggplot(dt, aes(x = as.factor(interval_tr), y = as.numeric(mean_probability))) +
    # annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    # annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    # geom_hline(yintercept = chance_level, linetype = "dashed", color = "gray") +
    geom_line(aes(group = interaction(as.factor(id), as.factor(current_stim)),
    color = as.factor(current_stim)), alpha = 0.1) +
    stat_summary(aes(group = as.factor(current_stim), fill = as.factor(current_stim)),
                 geom = "ribbon", fun.data = "mean_se", alpha = 0.5) +
    stat_summary(aes(group = as.factor(current_stim), color = as.factor(current_stim)),
                 geom = "line", fun = "mean") +
    facet_wrap(~ mask_test) +
    xlab("Time from inter-trial interval onset") +
    ylab("Probability (in %)") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, left = "both", bottom = "both") +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    # scale_x_discrete(labels = label_fill(seq(1, 15, 1), mod = 7), breaks = seq(1, 15, 1)) +
    ylim(c(0, NA)) +
    scale_color_manual(values = cfg$colors_decoding_current, name = "Events") +
    scale_fill_manual(values = cfg$colors_decoding_current, name = "Events") +
    theme(legend.position = "bottom") +
    theme(legend.box = "horizontal") +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = -5, r = 0, b = 0, l = 0))
  save_figure(plot = figure, "decoding_main_current_interval", width = 6, height = 4)
  return(figure)
}

plot_decoding_main_class_probabilities = function() {
  chance_level = 100/15
  color_events = rev(hcl.colors(6, "Zissou 1"))
  dt = load_data(file.path(paths$sourcedata, "zoo_sourcedata_decoding_main_class_probabilities.csv")) %>%
    .[, mask_test := factor(as.factor(mask_test), levels = c("occipito-temporal", "motor"))]
  figure = ggplot(data = dt, aes(x = as.factor(interval_tr), y = as.numeric(mean_prob))) +
    facet_grid(vars(as.factor(mask_test)), vars(as.factor(node_current))) +
    annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = chance_level, linetype = "dashed", color = "gray") +
    geom_ribbon(aes(fill = as.factor(node_classifier), group = as.factor(node_classifier),
                    ymin = sem_lower, ymax = sem_upper), alpha = 0.3) +
    geom_line(aes(group = as.factor(node_classifier), color = as.factor(node_classifier))) +
    ylab("Probability (in %)") +
    xlab("Time from inter-trial interval onset (in TRs; 1 TR = 1.25 s)") +
    # ggtitle("Event on the current trial") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, ylim = c(0, NA)) +
    scale_x_discrete(labels = label_fill(seq(1, 15, 1), mod = 7), breaks = seq(1, 15, 1)) +
    scale_color_manual(name = "Class", values = color_events) +
    scale_fill_manual(name = "Class", values = color_events) +
    theme(plot.title = element_text(hjust = 0.5))
  return(figure)
}

plot_decoding_main_graph_probablities = function() {
  grays = colorRampPalette(c("lightgray", "black"))
  colors = c(hcl.colors(5, "Viridis"), grays(1))
  dt = load_data(file.path(paths$sourcedata, "zoo_sourcedata_decoding_main_graph_probabilities.csv")) %>%
    .[, mask_test := factor(as.factor(mask_test), levels = c("occipito-temporal", "motor"))] %>%
    .[, graph := factor(as.factor(graph), levels = c("uni", "bi"))] %>%
    .[, dist_combined := factor(as.factor(dist_combined), levels = c(
      "1 | 1", "2 | 2", "3 | 3", "4 | -2", "5 | -1", "0 | 0"
    ))] %>%
    .[!(dist_combined == "0 | 0")] %>%
    .[mask_test == "occipito-temporal",]
  figure = ggplot(dt, aes(x = as.factor(phase), y = mean_prob, group = as.factor(dist_combined))) +
    geom_point(aes(group = as.factor(dist_combined), fill = as.factor(dist_combined)),
               position = position_jitterdodge(dodge.width = 0.9, seed = 666),
               color = "black", pch = 21, alpha = 0.3, size = 0.5) +
    stat_summary(aes(group = as.factor(dist_combined), fill = as.factor(dist_combined)),
                 geom = "point", fun = "mean", position = position_dodge2(0.9),
                 pch = 23, size = rel(2)) +
    stat_summary(geom = "errorbar", fun.data = "mean_se",
                 position = position_dodge2(width = 0.9, padding = 1), color = "black") +
    facet_grid(vars(mask_test), vars(graph), scales = "free_y") +
    ylab("Probability (in %)") +
    xlab("Phase of the inter-trial interval") +
    theme_zoo() +
    theme(axis.line.x = element_line(colour = "white")) +
    theme(axis.ticks.x = element_line(colour = "white")) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_color_manual(values = colors, name = "Node distance (uni | bi)") +
    scale_fill_manual(values = colors, name = "Node distance (uni | bi)") +
    guides(color = guide_legend(nrow = 2, ncol = 3)) +
    guides(fill = guide_legend(title.position = "bottom", title.hjust = 0.5)) +
    guides(color = guide_legend(title.position = "bottom", title.hjust = 0.5)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0))
  return(figure)
}

plot_decoding_main_graph_probablities_distance = function() {
  dt = load_data(file.path(paths$sourcedata, "zoo_sourcedata_decoding_main_graph_probabilities_distance.csv")) %>%
    .[, mask_test := factor(as.factor(mask_test), levels = c("occipito-temporal", "motor"))] %>%
    .[, graph := factor(as.factor(graph), levels = c("uni", "bi"))] %>%
    .[interval_tr %in% seq(1, 8)] %>%
    .[mask_test == "occipito-temporal",]
  figure = ggplot(dt, aes(x = as.factor(interval_tr), y = mean_prob)) +
    annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    stat_summary(aes(group = as.factor(dist_combined), fill = as.factor(dist_combined)),
                 geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = as.factor(dist_combined), color = as.factor(dist_combined)),
                 geom = "line", fun = "mean") +
    facet_grid(vars(mask_test), vars(graph), scales = "free_y") +
    ylab("Probability (in %)") +
    xlab("Time from inter-trial interval onset (in TRs)") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    scale_color_viridis_d(name = "Node distance (uni | bi)", direction = 1) +
    scale_fill_viridis_d(name = "Node distance (uni | bi)", direction = 1) +
    guides(color = guide_legend(nrow = 1, ncol = 5)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title.position = "bottom", title.hjust = 0.5)) +
    guides(color = guide_legend(title.position = "bottom", title.hjust = 0.5)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0))
  # ybreaks = ggplot_build(figure)$layout$panel_params[[1]]$y.sec$breaks
  # label_pos = mean(c(ybreaks[length(ybreaks)], ybreaks[length(ybreaks) - 1]))
  # figure = figure +
  #   annotate(geom = "text", x = (8 - 1) / 2 + 1, y = ybreaks[length(ybreaks)] - 0.5,
  #            label = "Inter-trial interval", color = "darkgray", size = rel(2)) +
  #   annotate(geom = "text", x = (4.5 - 1) / 2 + 1, y = label_pos,
  #            label = "early", color = "darkgray", size = rel(2)) +
  #   annotate(geom = "text", x = (8 - 4.5) / 2 + 4.5, y = label_pos,
  #            label = "late", color = "darkgray", size = rel(2))
  return(figure)
}

plot_decoding_main_slope_seq_prob_phase = function() {
  dt = load_data(file.path(paths$sourcedata, "zoo_sourcedata_decoding_main_slope_seq_prob_phase.csv")) %>%
    .[, mask_test := factor(as.factor(mask_test), levels = c("occipito-temporal", "motor"))] %>%
    .[, graph := factor(as.factor(graph), levels = c("uni", "bi"))]
  figure = ggplot(data = dt, aes(x = phase, y = mean_slope)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    stat_summary(aes(fill = seq_ranking, group = seq_ranking), geom = "bar", fun = "mean",
                 position = position_dodge(0.9)) +
    stat_summary(aes(group = seq_ranking), geom = "linerange", fun.data = "mean_se",
                 position = position_dodge(0.9), color = "black") +
    geom_point(aes(fill = as.factor(seq_ranking)),
               position = position_jitterdodge(dodge.width = 0.9, seed = 666),
               color = "black", pch = 21, alpha = 0.1, size = 0.5) +
    facet_grid(vars(mask_test), vars(graph)) +
    ylab("Regression slope") +
    xlab("Phase of the inter-trial interval") +
    theme_zoo() +
    theme(axis.line.x = element_line(colour = "white")) +
    theme(axis.ticks.x = element_line(colour = "white")) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_color_viridis_d(name = "Sequence probability ranking",
                          direction = -1, option = "E") +
    scale_fill_viridis_d(name = "Sequence probability ranking",
                         direction = -1, option = "E") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    guides(color = guide_legend(
      nrow = 1, title.position = "bottom", title.hjust = 0.5,
      direction = "horizontal")) +
    guides(fill = guide_legend(
      nrow = 1, title.position = "bottom", title.hjust = 0.5,
      direction = "horizontal"))
  return(figure)
}

plot_decoding_main_slope_seq_prob_interval = function() {
  arrow_xpos = -1
  arrow_ymax = 0.005
  dt = load_data(file.path(paths$sourcedata, "zoo_sourcedata_decoding_main_slope_seq_prob_interval.csv")) %>%
    .[, mask_test := factor(as.factor(mask_test), levels = c("occipito-temporal", "motor"))] %>%
    .[, graph := factor(as.factor(graph), levels = c("uni", "bi"))] %>%
    .[interval_tr %in% seq(1, 8), ]
  figure = ggplot(data = dt, aes(x = as.numeric(interval_tr), y = as.numeric(mean_slope))) +
    facet_grid(rows = vars(mask_test), cols = vars(graph)) +
    annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(aes(yintercept = 0), color = "gray", linetype = "dashed") +
    geom_ribbon(aes(group = seq_ranking, fill = seq_ranking,
                    ymin = sem_lower, ymax = sem_upper), alpha = 0.3) +
    geom_line(aes(group = seq_ranking, color = seq_ranking)) +
    ylab("Regression slope") +
    xlab("Time from inter-trial interval onset (in TRs)") +
    theme_zoo() +
    scale_color_viridis_d(name = "Sequence probability ranking",
                          direction = -1, option = "E") +
    scale_fill_viridis_d(name = "Sequence probability ranking",
                         direction = -1, option = "E") +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both") +
    scale_x_continuous(
      limits = c(-2, 8),
      labels = label_fill(seq(1, 8, 1), mod = 1),
      breaks = seq(1, 8, 1)) +
    scale_y_continuous(
      limits = c(-0.0055, 0.005),
      labels = label_fill(seq(-0.005, 0.005, 0.0025), mod = 1),
      breaks = seq(-0.005, 0.005, 0.0025)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    guides(color = guide_legend(
      nrow = 1, title.position = "bottom", title.hjust = 0.5,
      direction = "horizontal")) +
    guides(fill = guide_legend(
      nrow = 1, title.position = "bottom", title.hjust = 0.5,
      direction = "horizontal"))
  ybreaks = ggplot_build(figure)$layout$panel_params[[1]]$y.sec$breaks
  label_pos = mean(c(ybreaks[length(ybreaks)], ybreaks[length(ybreaks) - 1]))
  figure = figure +
    geom_segment(aes(
      x = arrow_xpos, xend = arrow_xpos, y = 0, yend = arrow_ymax),
      arrow = arrow(length = unit(5, "pt")), color = "darkgray") +
    geom_segment(aes(
      x = arrow_xpos, xend = arrow_xpos, y = 0, yend = -arrow_ymax),
      arrow = arrow(length = unit(5, "pt")), color = "darkgray") +
    annotate(geom = "text", x = 0, y = arrow_ymax / 2, label = "Forward",
             color = "darkgray", angle = 90, size = rel(2)) +
    annotate(geom = "text", x = 0, y = -arrow_ymax / 2, label = "Backward",
             color = "darkgray", angle = 90, size = rel(2)) +
    # annotate(geom = "text", x = (8 - 1) / 2 + 1, y = ybreaks[length(ybreaks)] - 0.00125,
    #          label = "Inter-trial\ninterval", color = "darkgray", size = rel(2)) +
    annotate(geom = "text", x = (4.5 - 1) / 2 + 1, y = label_pos,
             label = "early", color = "darkgray", size = rel(2)) +
    annotate(geom = "text", x = (8 - 4.5) / 2 + 4.5, y = label_pos,
             label = "late", color = "darkgray", size = rel(2)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0))
  return(figure)
}

plot_decoding_main_graph_prob_model_comparison = function() {
  dt = load_data(file.path(paths$sourcedata, "zoo_sourcedata_decoding_main_graph_proba_model_comparison.csv")) %>%
    .[, mask_test := factor(as.factor(ROI), levels = c("occipito-temporal", "motor"))] %>%
    .[, graph := factor(as.factor(Graph), levels = c("uni", "bi"))]
  figure = ggplot(dt, aes(x = Phase, y = Diff)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    stat_summary(geom = "bar", fun = "identity", position = position_dodge2(0.9), width = 0.5) +
    facet_grid(vars(mask_test), vars(graph)) +
    ylab("Difference in AICs") +
    xlab("Phase of the inter-trial interval") +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(axis.line = element_line(colour = "black")) +
    theme(axis.line.x = element_line(colour = "white")) +
    theme(axis.ticks.x = element_line(colour = "white")) +
    theme(axis.text = element_text(colour = "black")) +
    theme(axis.title = element_text(colour = "black")) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, xlim = c(0.7, NA)) +
    scale_color_manual(values = cfg$graph_colors, name = "Predictor") +
    scale_fill_manual(values = cfg$graph_colors, name = "Predictor") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(legend.text = element_text(size = 10)) +
    theme(legend.title = element_text(size = 10)) +
    theme(legend.key.size = unit(0.6, "line")) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = -5, r = 0, b = 0, l = 0)) +
    theme(strip.text = element_text(margin = margin(b = 3, t = 3, r = 3, l = 3)))
  ybreaks = ggplot_build(figure)$layout$panel_params[[1]]$y.sec$breaks
  arrow_xpos = 0.5
  arrow_ymax = ybreaks[length(ybreaks)]
  figure = figure +
    geom_segment(aes(
      x = arrow_xpos, xend = arrow_xpos, y = 0, yend = arrow_ymax),
      arrow = arrow(length = unit(5, "pt")), color = "darkgray") +
    geom_segment(aes(
      x = arrow_xpos, xend = arrow_xpos, y = 0, yend = -arrow_ymax),
      arrow = arrow(length = unit(5, "pt")), color = "darkgray") +
    annotate(geom = "text", x = 0.3, y = arrow_ymax / 2, label = "Linear better",
             color = "darkgray", angle = 90, size = rel(2)) +
    annotate(geom = "text", x = 0.3, y = -arrow_ymax / 2, label = "Quadratic better",
             color = "darkgray", angle = 90, size = rel(2)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0))
  return(figure)
}

plot_decoding_main_graph_prob_aic_table = function() {
  dt = load_data(file.path(paths$sourcedata, "zoo_sourcedata_decoding_main_graph_proba_model_comparison.csv")) %>%
    .[, ROI := factor(as.factor(ROI), levels = c("occipito-temporal", "motor"))] %>%
    .[, Graph := factor(as.factor(Graph), levels = c("uni", "bi"))] %>%
    setorder(., ROI, Graph, Phase)
  figure = ggplot() +
    annotate(geom = "table", x = 0, y = 0, label = list(dt),
             table.theme = ttheme_gtlight, size = rel(2)) +
    theme_zoo() +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(axis.line = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.title = element_blank())
  return(figure)
}

plot_decoding_main_probabilities_sequence_distribution = function() {
  dt = load_data(file.path(paths$sourcedata, "zoo_sourcedata_decoding_probabilities_sequence_distribution.csv")) %>%
    .[, roi := factor(as.factor(roi), levels = c("occipito-temporal", "motor"))] %>%
    .[, graph := factor(as.factor(graph), levels = c("uni", "bi"))]
  xlabels = sort(unique(dt$seq_numbers))
  figure = ggplot(data = dt, aes(x = as.factor(seq_numbers), y = seq_freq)) +
    geom_bar(aes(color = as.numeric(seq_numbers)), stat = "identity") +
    geom_errorbar(aes(ymin = sem_lower, ymax = sem_upper, width = 0)) +
    geom_hline(yintercept = 1/120, color = "gray") +
    facet_grid(vars(roi), vars(graph)) +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both") +
    ylab("Observed sequence frequency") +
    ylim(c(0, NA)) +
    theme(axis.ticks.x = element_line(colour = "white")) +
    theme(axis.line.x = element_line(colour = "white")) +
    theme(axis.text.x = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(legend.margin = margin(t = -10, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    scale_colour_gradient(
      limits = c(min(xlabels), max(xlabels)),
      breaks = c(min(xlabels), xlabels[round(length(xlabels) / 2)], max(xlabels)),
      low = "#132B43", high = "#56B1F7", space = "Lab", guide = "colourbar", aesthetics = "colour") +
    guides(color = guide_colorbar(title = "Sequences", title.position = "bottom", title.hjust = 0.5, direction = "horizontal"))
  return(figure)
}

plot_decoding_main_probabilities_sequence_correlation = function() {
  dt1 = load_data(file.path(paths$sourcedata, "zoo_sourcedata_decoding_probabilities_sequence_distribution.csv")) %>%
    .[, roi := factor(as.factor(roi), levels = c("occipito-temporal", "motor"))] %>%
    .[, graph := factor(as.factor(graph), levels = c("uni", "bi"))]
  dt2 = load_data(file.path(paths$sourcedata, "zoo_sourcedata_decoding_probabilities_sequence_correlation.csv")) %>%
    .[, roi := factor(as.factor(roi), levels = c("occipito-temporal", "motor"))] %>%
    .[, graph := factor(as.factor(graph), levels = c("uni", "bi"))]
  figure = ggplot(dt1, aes(x = seq_probability_log, y = seq_freq)) +
    facet_grid(vars(roi), vars(graph)) +
    geom_point(color = "black", pch = 1) +
    geom_smooth(method = "lm", formula = "y ~ x", fullrange = TRUE, se = FALSE, color = "black") +
    xlab("Predicted sequence probability (log)") +
    ylab("Observed sequence frequency") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both") +
    scale_y_continuous(
      limits = c(0.0055, 0.011),
      labels = label_fill(seq(0.006, 0.011, 0.001), mod = 1),
      breaks = seq(0.006, 0.011, 0.001)) +
    scale_x_continuous(
      limits = c(-8, -1.5),
      labels = label_fill(seq(-8, -2, 2), mod = 1),
      breaks = seq(-8, -2, 2))
  ybreaks = ggplot_build(figure)$layout$panel_params[[1]]$y.sec$breaks
  ybreaks = ybreaks[!is.na(ybreaks)]
  xbreaks = ggplot_build(figure)$layout$panel_params[[1]]$x.sec$breaks
  xbreaks = xbreaks[!is.na(xbreaks)]
  figure = figure +
    geom_text(data = dt2, aes(label = p_estimate_label, x = xbreaks[length(xbreaks)], y = ybreaks[1]),
              hjust = 1, size = rel(2))
  return(figure)
}









plot_decoding_main_probabilities_model_estimates_dummy  <- function() {

  dt_load <- load_data(file.path(paths$sourcedata, "zoo_sourcedata_decoding_main_probability_models_dummy.csv")) %>%
    .[, graph := factor(as.factor(graph), levels = c("uni", "bi"))] %>%
    .[model_number == "Model 2", ]

  figure <- ggplot(data = dt_load, aes(x = as.factor(interval_tr), y = estimate)) +
    # geom_hline(yintercept = 0, color = "black") +
    geom_line(aes(color = graph, group = graph)) +
    facet_wrap(~ term, scales = "free_y") +
    ylab("Estimate") +
    xlab("Time from interval onset (in TRs; 1 TR = 1.25 s)") +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(axis.line = element_line(colour = "black")) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(name = "Class", values = cfg$graph_colors) +
    guides(fill = guide_legend(title = "Graph")) +
    guides(color = guide_legend(title = "Graph")) +
    scale_x_discrete(labels = label_fill(seq(1, 15, 1), mod = 7), breaks = seq(1, 15, 1)) +
    theme(strip.text.y = element_text(angle = 0)) +
    theme(axis.text = element_text(colour = "black")) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    theme(legend.text = element_text(size = 10)) +
    theme(legend.title = element_text(size = 10)) +
    theme(legend.key.size = unit(0.6, "line")) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = -5, r = 0, b = 0, l = 0)) +
    annotate("rect", xmin = 1, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray") +
    annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray")

  return(figure)
  
}