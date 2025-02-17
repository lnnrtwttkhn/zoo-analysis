plot_decoding_main_model_raw_prob <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_raw_prob) %>%
    .[graph == "uni"] %>%
    .[roi == "visual", ]
  figure <- ggplot(dt_input, aes(x = as.factor(interval_tr), y = mean_prob)) +
    annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    stat_summary(aes(group = as.factor(dist_graph), fill = as.factor(dist_graph)),
                 geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = as.factor(dist_graph), color = as.factor(dist_graph)),
                 geom = "line", fun = "mean") +
    stat_summary(aes(group = as.factor(dist_graph), color = as.factor(dist_graph)),
                 geom = "point", fun = "mean") +
    ylab("Probability (in %)") +
    xlab("Time from inter-trial interval onset (in TRs; 1 TR = 1.25 s)") +
    # facet_grid(vars(datatype), vars(graph), scales = "free_y") +
    facet_wrap(~ datatype, scales = "free_y") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    scale_color_manual(values = c("gray", cfg$colors_dist), name = "Node distance") +
    scale_fill_manual(values = c("gray", cfg$colors_dist), name = "Node distance") +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(nrow = 1, ncol = 6)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    # ggtitle("Classifier time courses vs. modeled stimulus time courses") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  return(figure)
}

plot_decoding_main_model_raw_prob_all <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_raw_prob) %>%
    .[graph == "uni"]
  figure <- ggplot(dt_input, aes(x = as.factor(interval_tr), y = mean_prob)) +
    annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    stat_summary(aes(group = as.factor(dist_graph), fill = as.factor(dist_graph)),
                 geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = as.factor(dist_graph), color = as.factor(dist_graph)),
                 geom = "line", fun = "mean") +
    stat_summary(aes(group = as.factor(dist_graph), color = as.factor(dist_graph)),
                 geom = "point", fun = "mean") +
    ylab("Probability (in %)") +
    xlab("Time from inter-trial interval onset (in TRs; 1 TR = 1.25 s)") +
    facet_grid(vars(roi), vars(datatype), scales = "free_y") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    scale_color_manual(values = c("gray", cfg$colors_dist), name = "Node distance") +
    scale_fill_manual(values = c("gray", cfg$colors_dist), name = "Node distance") +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(nrow = 1, ncol = 6)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    # ggtitle("Classifier time courses vs. modeled stimulus time courses") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  return(figure)
}

plot_decoding_main_model_residuals <- function(cfg, paths, roi_input, graph_input) {
  dt_input <- load_data(paths$source$decoding_main_model_residuals) %>%
    .[graph == graph_input, ] %>%
    .[ roi == roi_input, ] %>%
    .[model_number == 1, ]
  figure <- ggplot(dt_input, aes(x = as.factor(interval_tr), y = as.numeric(residual))) +
    # annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    # annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = 0, color = "black") +
    stat_summary(aes(group = as.factor(dist_graph), fill = as.factor(dist_graph)),
                 geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = as.factor(dist_graph), color = as.factor(dist_graph)),
                 geom = "line", fun = "mean") +
    stat_summary(aes(group = as.factor(dist_graph), color = as.factor(dist_graph)),
                 geom = "point", fun = "mean") +
    ylab("Residuals") +
    xlab("Time from inter-trial interval onset") +
    # facet_grid(vars(roi), vars(graph), scales = "free_y") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    scale_color_manual(values = cfg$colors_dist, name = "Node distance") +
    scale_fill_manual(values = cfg$colors_dist, name = "Node distance") +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(nrow = 1, ncol = 6)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    ggtitle("Residuals of stimulus model\n(unidirectional graph)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(legend.position = "none")
  return(figure)
}

plot_decoding_main_model_residuals_slope <- function(cfg, paths, roi_input, graph_input) {
  dt1 <- load_data(paths$source$decoding_main_model_residuals_slope) %>%
    .[graph == graph_input, ] %>%
    .[ roi == roi_input, ] %>%
    .[model_name == "Stimulus", ]
  dt2 <- load_data(paths$source$decoding_main_model_residuals_slope_stat) %>%
    .[graph == graph_input, ] %>%
    .[ roi == roi_input, ] %>%
    .[model_name == "Stimulus", ] %>%
    .[, p.value_significance := ifelse(p.value_significance == "n.s.", " ", p.value_significance)]
  arrow_ymax <- 0.01
  arrow_xpos <- 0.5
  figure <- ggplot(dt1, aes(x = as.numeric(interval_tr), y = as.numeric(mean_slope))) +
    # annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    # annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = 0, color = "gray") +
    stat_summary(aes(group = 1), geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(aes(group = 1), geom = "point", fun = "mean") +
    geom_point(data = dt2, aes(fill = p.value_significance, y = estimate), color = "black", pch = 21) +
    # geom_text(data = dt2, aes(label = paste("p =", p.value_adjust_round), y = as.numeric(estimate)), vjust = -5) +
    geom_text(data = dt2, aes(label = as.factor(p.value_significance), y = as.numeric(estimate)), vjust = -3) +
    ylab("Regression slope") +
    xlab("Time from inter-trial interval onset") +
    # facet_grid(vars(roi), vars(graph), scales = "free_y") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_continuous(limits = c(0, 8), labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    scale_fill_manual(values = c("black", "red")) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(nrow = 1, ncol = 6)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    ggtitle("Sequentiality in residuals\n(unidirectional graph)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(legend.position = "none") +
    annotate(geom = "segment",
             x = arrow_xpos, xend = arrow_xpos, y = 0 + arrow_ymax / 15, yend = arrow_ymax,
             arrow = arrow(length = unit(3, "pt"), type = "closed"), color = "darkgray") +
    annotate(geom = "segment",
             x = arrow_xpos, xend = arrow_xpos, y = 0 - arrow_ymax / 15, yend = -arrow_ymax,
             arrow = arrow(length = unit(3, "pt"), type = "closed"), color = "darkgray") +
    annotate(geom = "text", x = 0, y = arrow_ymax / 1.75, label = "Forward",
             color = "darkgray", angle = 90, fontface = "italic", size = rel(3)) +
    annotate(geom = "text", x = 0, y = -arrow_ymax / 1.75, label = "Backward",
             color = "darkgray", angle = 90, fontface = "italic", size = rel(3))
  return(figure)
}

plot_decoding_main_model_residuals_consciousness <- function(cfg, paths, roi_input, graph_input) {
  dt_input <- load_data(paths$source$decoding_main_model_residuals) %>%
    .[graph == graph_input, ] %>%
    .[ roi == roi_input, ] %>%
    .[model_number == 1, ] %>%
    .[, sequence_detected := ifelse(sequence_detected == "yes", "conscious knowledge", "no conscious knowledge")]
  figure <- ggplot(dt_input, aes(x = as.factor(interval_tr), y = as.numeric(residual))) +
    # annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    # annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = 0, color = "black") +
    stat_summary(aes(group = as.factor(dist_graph), fill = as.factor(dist_graph)),
                 geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = as.factor(dist_graph), color = as.factor(dist_graph)),
                 geom = "line", fun = "mean") +
    stat_summary(aes(group = as.factor(dist_graph), color = as.factor(dist_graph)),
                 geom = "point", fun = "mean") +
    ylab("Residuals") +
    xlab("Time from inter-trial interval onset") +
    facet_wrap(~ sequence_detected) +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    scale_color_manual(values = cfg$colors_dist, name = "Node distance") +
    scale_fill_manual(values = cfg$colors_dist, name = "Node distance") +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(nrow = 1, ncol = 6)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    ggtitle("Residuals of stimulus model\n(unidirectional graph)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  return(figure)
}

plot_decoding_main_model_residuals_slope_consciousness <- function(cfg, paths, roi_input, graph_input) {
  dt1 <- load_data(paths$source$decoding_main_model_residuals_slope) %>%
    .[graph == graph_input, ] %>%
    .[ roi == roi_input, ] %>%
    .[model_name == "Stimulus", ] %>%
    .[, sequence_detected := ifelse(sequence_detected == "yes", "conscious knowledge", "no conscious knowledge")]
  dt2 <- load_data(paths$source$decoding_main_model_residuals_slope_stat_consciousness) %>%
    .[graph == "uni", ] %>%
    .[ roi == roi_input, ] %>%
    .[model_name == "Stimulus", ] %>%
    .[, sequence_detected := ifelse(sequence_detected == "yes", "conscious knowledge", "no conscious knowledge")]
  arrow_ymax <- 0.01
  arrow_xpos <- 0.5
  figure <- ggplot(dt1, aes(x = as.numeric(interval_tr), y = as.numeric(mean_slope))) +
    # annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    # annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = 0, color = "gray") +
    stat_summary(aes(group = 1), geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(aes(group = 1), geom = "point", fun = "mean") +
    geom_point(data = dt2, aes(fill = p.value_significance, y = estimate), color = "black", pch = 21) +
    # geom_text(data = dt2, aes(label = paste("p =", p.value_adjust_round), y = as.numeric(estimate)), vjust = -5) +
    geom_text(data = dt2, aes(label = as.factor(p.value_significance), y = as.numeric(estimate)), vjust = -3) +
    ylab("Regression slope") +
    xlab("Time from inter-trial interval onset") +
    facet_wrap(~ sequence_detected) +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_continuous(limits = c(0, 8), labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    scale_fill_manual(values = c("red", "black")) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(nrow = 1, ncol = 6)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    ggtitle("Sequentiality in residuals\n(unidirectional graph)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(legend.position = "none") +
    annotate(geom = "segment",
      x = arrow_xpos, xend = arrow_xpos, y = 0 + arrow_ymax / 15, yend = arrow_ymax,
      arrow = arrow(length = unit(3, "pt"), type = "closed"), color = "darkgray") +
    annotate(geom = "segment",
      x = arrow_xpos, xend = arrow_xpos, y = 0 - arrow_ymax / 15, yend = -arrow_ymax,
      arrow = arrow(length = unit(3, "pt"), type = "closed"), color = "darkgray") +
    annotate(geom = "text", x = 0, y = arrow_ymax / 1.75, label = "Forward",
             color = "darkgray", angle = 90, fontface = "italic", size = rel(3)) +
    annotate(geom = "text", x = 0, y = -arrow_ymax / 1.75, label = "Backward",
             color = "darkgray", angle = 90, fontface = "italic", size = rel(3))
  return(figure)
}
