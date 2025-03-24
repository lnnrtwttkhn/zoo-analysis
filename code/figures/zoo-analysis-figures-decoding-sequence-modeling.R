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

plot_decoding_main_model_residuals <- function(cfg, paths, roi_input, graph_input, group = NULL) {
  group_name <- paste(group, collapse = "_")
  input_path <- paste(paths$source$decoding_main_model_residuals_mean, group_name, sep = "_")
  dt_input <- load_data(input_path) %>%
    .[graph == graph_input, ] %>%
    .[ roi == roi_input, ] %>%
    .[model_name == "Stimulus", ]
  if (!is.null(group)) {
    dt_input$group <- dt_input[, ..group]
  }
  title_text <- sprintf("Residuals of stimulus model\n(%sdirectional graph)", graph_input)
  figure <- ggplot(dt_input, aes(x = as.factor(interval_tr), y = as.numeric(mean_residual))) +
    # annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    # annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = 0, color = "gray") +
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
    ggtitle(title_text) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  if (!is.null(group)) {
    figure <- figure +
      geom_text(data = dt_input %>% .[, by = .(group), .(n = paste("n =", length(unique(id))))], aes(label = n), x = 8, y = -Inf, hjust = 1, vjust = -2) +
      facet_wrap(~ group)
  }
  return(figure)
}

plot_decoding_main_model_residuals_slope <- function(cfg, paths, roi_input, graph_input, group = NULL) {
  if (is.null(group)) {
    path_dt1 <- paths$source$decoding_main_model_residuals_slope_mean
    path_dt2 <- paths$source$decoding_main_model_residuals_slope_stat
  } else if (!is.null(group))  {
    path_dt1 <- paste(paths$source$decoding_main_model_residuals_slope_mean, group, sep = "_")
    path_dt2 <- paste(paths$source$decoding_main_model_residuals_slope_stat, group, sep = "_")
  }
  dt1 <- load_data(path_dt1) %>%
    .[graph == graph_input, ] %>%
    .[ roi == roi_input, ] %>%
    .[model_name == "Stimulus", ]
  dt2 <- load_data(path_dt2) %>%
    .[graph == graph_input, ] %>%
    .[ roi == roi_input, ] %>%
    .[model_name == "Stimulus", ] %>%
    .[, p.value_significance := ifelse(p.value_significance == "n.s.", " ", p.value_significance)]
  arrow_ymax <- 0.01
  arrow_xpos <- 0.5
  title_text <- sprintf("Sequentiality in residuals\n(%sdirectional graph)", graph_input)
  figure <- ggplot(dt1, aes(x = as.numeric(interval_tr), y = as.numeric(mean_slope))) +
    # annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    # annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = 0, color = "gray") +
    stat_summary(aes(group = 1), geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(aes(group = 1), geom = "point", fun = "mean") +
    geom_point(data = dt2, aes(fill = as.factor(p.value_significance), y = estimate), pch = 21) +
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
    ggtitle(title_text) +
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
  if (!is.null(group)) {
    figure <- figure +
      geom_text(data = dt1 %>% .[, by = .(group), .(n = length(unique(id)))], aes(label = paste("n =", n)), x = 8, y = -Inf, hjust = 1, vjust = -2) +
      facet_wrap(~ group)
  }
  return(figure)
}

plot_decoding_main_model_betas <- function(cfg, paths, roi_input, graph_input) {
  dt_input <- load_data(paths$source$decoding_main_model_betas) %>%
    .[roi == roi_input, ] %>%
    .[graph == graph_input, ] %>%
    .[model_number == 4, ] %>%
    .[predictor %in% c("SR", "1-step")]
  figure <- ggplot(dt_input, aes(x = as.factor(interval_tr), y = as.numeric(beta_id))) +
    # annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    # annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = 0, color = "black") +
    stat_summary(aes(group = as.factor(predictor), fill = as.factor(predictor)),
                 geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = as.factor(predictor), color = as.factor(predictor)),
                 geom = "line", fun = "mean") +
    stat_summary(aes(group = as.factor(predictor), color = as.factor(predictor)),
                 geom = "point", fun = "mean") +
    # geom_line(aes(color = as.factor(predictor), group = as.factor(predictor))) +
    # geom_point(aes(color = as.factor(predictor), group = as.factor(predictor),
    #                shape = as.factor(predictor))) +
    ylab("Beta") +
    xlab("Time from inter-trial interval onset") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    theme(legend.position.inside = c(0.2, 0.85)) +
    theme(legend.title = element_blank()) +
    theme(legend.key = element_blank()) + 
    # scale_color_manual(values = cfg$dist_colors, name = "Predictor") +
    # scale_fill_manual(values = cfg$dist_colors, name = "Predictor") +
    # scale_shape_manual(name = "Predictor") +
    labs(color = "Predictor", fill = "Predictor", shape = "Predictor") +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    ggtitle("Betas of SR + 1-step model\n(unidirectional graph)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  return(figure)
}

plot_decoding_main_model_betas_id <- function(cfg, paths, roi_input) {
  dt_input <- load_data(paths$source$decoding_main_model_betas_id) %>%
    .[roi == roi_input, ] %>%
    .[graph == "uni", ] %>%
    .[model_number == 4, ] %>%
    .[predictor %in% c("SR", "1-step")]
  figure <- ggplot(dt_input, aes(x = as.factor(interval_tr), y = as.numeric(statistic))) +
    # annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    # annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = 0, color = "black") +
    stat_summary(aes(group = as.factor(predictor), fill = as.factor(predictor)),
                 geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = as.factor(predictor), color = as.factor(predictor)),
                 geom = "line", fun = "mean") +
    stat_summary(aes(group = as.factor(predictor), color = as.factor(predictor)),
                 geom = "point", fun = "mean") +
    # geom_line(aes(color = as.factor(predictor), group = as.factor(predictor))) +
    # geom_point(aes(color = as.factor(predictor), group = as.factor(predictor),
    #                shape = as.factor(predictor))) +
    ylab("Beta") +
    xlab("Time from inter-trial interval onset") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    theme(legend.position = c(0.2, 0.85)) +
    theme(legend.title = element_blank()) +
    theme(legend.key = element_blank()) + 
    # scale_color_manual(values = cfg$dist_colors, name = "Predictor") +
    # scale_fill_manual(values = cfg$dist_colors, name = "Predictor") +
    # scale_shape_manual(name = "Predictor") +
    labs(color = "Predictor", fill = "Predictor", shape = "Predictor") +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    ggtitle("Betas of SR + 1-step model\n(unidirectional graph)") +
    # ggtitle("Betas of SR + 1-step model\n(bidirectional graph)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  return(figure)
}

plot_decoding_main_model_betas_all <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_betas) %>%
    # .[roi == "visual", ] %>%
    .[graph == "uni", ]
  figure <- ggplot(dt_input, aes(x = as.factor(interval_tr), y = as.numeric(beta))) +
    # annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    # annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = 0, color = "black") +
    geom_line(aes(color = as.factor(predictor), group = as.factor(predictor))) +
    geom_point(aes(color = as.factor(predictor), group = as.factor(predictor),
                   shape = as.factor(predictor))) +
    ylab("Beta") +
    xlab("Time from inter-trial interval onset") +
    facet_grid(vars(roi), vars(model_name), scales = "free_y") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    # scale_color_manual(values = cfg$dist_colors, name = "Predictor") +
    # scale_fill_manual(values = cfg$dist_colors, name = "Predictor") +
    # scale_shape_manual(name = "Predictor") +
    labs(color = "Predictor", fill = "Predictor", shape = "Predictor") +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(nrow = 4, ncol = 1)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0))
  return(figure)
}

plot_decoding_main_model_betas_behav <- function(cfg, paths, roi_input) {
  dt_input <- load_data(paths$source$decoding_main_model_betas_behav) %>%
    .[roi == "visual", ] %>%
    .[predictor != "Intercept"] %>%
    # .[predictor == "SR"] %>%
    .[interval_tr %in% c(7, 8), ]
  figure <- ggplot(dt_input, aes(x = as.numeric(gamma), y = as.numeric(estimate))) +
    geom_point(aes(color = as.factor(predictor), group = as.factor(id),
                   shape = as.factor(predictor))) +
    geom_smooth(method = "lm", aes(color = as.factor(predictor))) +
    facet_grid(vars(interval_tr), vars(graph)) +
    ylab("Beta") +
    xlab("Gamma") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    # scale_color_manual(values = cfg$dist_colors, name = "Predictor") +
    # scale_fill_manual(values = cfg$dist_colors, name = "Predictor") +
    # scale_shape_manual(name = "Predictor") +
    labs(color = "Predictor", fill = "Significance", shape = "Predictor") +
    ggtitle("Correlation between SR fits and betas of SR + 1-step model") +
    # ggtitle("Betas of SR + 1-step model\n(bidirectional graph)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  return(figure)
}

plot_decoding_main_model_betas_behav_cor <- function(cfg, paths, roi_input) {
  dt_input <- load_data(paths$source$decoding_main_model_betas_behav_cor) %>%
    .[roi == "visual", ] %>%
    .[beta == "estimate"] %>%
    .[predictor == "SR"] %>%
    .[graph == "uni", ]
  figure <- ggplot(dt_input, aes(x = as.factor(interval_tr), y = as.numeric(estimate))) +
    geom_hline(yintercept = 0, color = "black") +
    geom_line(aes(color = as.factor(predictor), group = as.factor(predictor))) +
    geom_point(aes(color = as.factor(predictor), group = as.factor(predictor),
                   shape = as.factor(predictor))) +
    # geom_point(data = dt_input, aes(fill = significance, y = estimate), color = "black", pch = 21) +
    # geom_text(data = dt_input, aes(label = as.factor(significance), y = as.numeric(estimate)), vjust = -3) +
    facet_wrap(~ sr_parameter) +
    ylab("Correlation (Pearson's r)") +
    xlab("Time from inter-trial interval onset") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, ylim = c(-0.5, 0.5)) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    scale_color_manual(name = "Predictor", values = cfg$colors_predictors) +
    scale_fill_manual(name = "Predictor", values = cfg$colors_predictors) +
    scale_shape_manual(name = "Predictor", values = cfg$shapes_predictors) +
    ggtitle("Correlation between SR fits\nand betas of SR + 1-step model") +
    # ggtitle("Betas of SR + 1-step model\n(bidirectional graph)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(legend.position = "none")
  return(figure)
}

plot_decoding_main_model_betas_behav_cor_mean <- function(cfg, paths, roi_input, graph_input, predictor_input = NULL, sr_input = NULL) {
  dt1 <- load_data(paths$source$decoding_main_model_betas_behav_cor_mean)
  dt2 <- load_data(paths$source$decoding_main_model_betas_behav_cor_mean_stat)
  if (!is.null(sr_input)) {
    dt1 <- dt1 %>% .[sr_parameter == sr_input]
  }
  dt1 <- dt1 %>%
    .[beta == "estimate_abs", ] %>%
    .[roi == roi_input, ] %>%
    .[predictor == predictor_input, ] %>%
    .[graph == graph_input, ] %>%
    .[, sr_parameter := dplyr::case_when(
      sr_parameter == "alpha" ~ cfg$alpha_utf,
      sr_parameter == "gamma" ~ cfg$gamma_utf
    )]
  dt2 <- dt2 %>%
    .[roi == roi_input, ] %>%
    .[beta == "estimate_abs", ] %>%
    .[roi == roi_input, ] %>%
    .[predictor == predictor_input, ] %>%
    .[graph == graph_input, ] %>%
    .[, sr_parameter := dplyr::case_when(
      sr_parameter == "alpha" ~ cfg$alpha_utf,
      sr_parameter == "gamma" ~ cfg$gamma_utf
    )]
  figure <- ggplot(dt1, aes(x = as.numeric(sr_parameter_value), y = as.numeric(beta_value))) +
    geom_point(aes(color = as.factor(predictor), group = as.factor(id),
                   shape = as.factor(predictor))) +
    geom_smooth(method = "lm", aes(color = as.factor(predictor),
                                   fill = as.factor(predictor))) +
    geom_text(data = dt2, aes(y = Inf, x = 0, label = result), vjust = 2, hjust = 0, size = 4) +
    ylab(sprintf("Absolute %s (of %s component)", cfg$beta_utf, predictor_input)) +
    xlab("Parameter estimate") +
    theme_zoo() +
    facet_wrap(~ sr_parameter) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_color_manual(name = "Predictor", values = cfg$colors_predictors) +
    scale_x_continuous(labels = label_fill(seq(0, 1, 0.25), mod = 2), breaks = seq(0, 1, 0.25)) +
    scale_fill_manual(name = "Predictor", values = cfg$colors_predictors) +
    scale_shape_manual(name = "Predictor", values = cfg$shapes_predictors) +
    ggtitle(sprintf("Relationship between SR parameters\nand absolute %s of %s (SR + 1-step model)", cfg$beta_utf, predictor_input)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(legend.position = "none")
  return(figure)
}

plot_decoding_main_model_betas_behav_cor_mean_rt <- function(cfg, paths) {
  dt1 <- load_data(paths$source$decoding_main_model_betas_behav_cor_mean) %>%
    .[roi == "visual", ] %>%
    .[graph == "uni", ] %>%
    .[model_name == "SR + 1-step"] %>%
    .[predictor == "1-step"] %>%
    .[beta == "estimate_abs", ] %>%
    .[sr_parameter == "alpha", ]
  dt2 <- load_data(paths$source$decoding_main_model_betas_behav_cor_mean_stat_rt) %>%
    # .[roi == "visual"] %>%
    .[p.value < 0.05]
  figure <- ggplot(dt1, aes(x = as.numeric(beta_value), y = as.numeric(slope_diff))) +
    geom_point() +
    geom_smooth(method = "lm") +
    geom_text(data = dt2, aes(y = Inf, x = Inf, label = result), vjust = 2, hjust = 1, size = 4) +
    ylab("Slope of response times for\nhigh probability one-step transitions") +
    xlab("Beta of 1-step regressor (SR + 1-step model)") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(legend.position = "none")
  return(figure)
}

plot_decoding_main_model_residuals_rt_cor <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_residuals_rt_cor)
  figure <- ggplot(dt_input, aes(x = as.numeric(mean_abs_slope), y = as.numeric(mean_rt))) +
    geom_point() +
    geom_smooth(method = "lm") +
    xlab("Mean absolute slope") +
    ylab("Mean response time (log ms)") +
    theme_zoo() +
    facet_wrap(~ rt_type) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_color_manual(name = "Predictor", values = cfg$colors_predictors) +
    # scale_x_continuous(labels = label_fill(seq(0, 1, 0.25), mod = 2), breaks = seq(0, 1, 0.25)) +
    scale_fill_manual(name = "Predictor", values = cfg$colors_predictors) +
    scale_shape_manual(name = "Predictor", values = cfg$shapes_predictors) +
    ggtitle("Relationship between RTs (mean of 5 trials)\nand mean absolute slope of residuals") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(legend.position = "none")
  save_figure(plot = figure, "model_residuals_rt_cor", width = 5, height = 4)
  return(figure)
}

plot_decoding_main_model_prediction <- function(cfg, paths, roi_input, graph_input) {
  dt_input <- load_data(paths$source$decoding_main_model_prediction) %>%
    .[graph == graph_input] %>%
    .[roi == roi_input, ] %>%
    .[model_name == "Stimulus", ]
  figure <- ggplot(dt_input, aes(x = as.factor(interval_tr), y = as.numeric(mean_prob) * 100)) +
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
    # facet_grid(vars(datatype), vars(model_name), scales = "free_y") +
    facet_wrap(~ datatype, scales = "free_y") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    # scale_color_manual(values = c("gray", cfg$dist_colors), name = "Node distance") +
    # scale_fill_manual(values = c("gray", cfg$dist_colors), name = "Node distance") +
    scale_color_manual(values = c(cfg$colors_dist), name = "Node distance") +
    scale_fill_manual(values = c(cfg$colors_dist), name = "Node distance") +
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

plot_decoding_main_model_results_stim <- function(cfg, paths, roi_input) {
  dt_input <- load_data(paths$source$decoding_main_model_results) %>%
    .[roi == roi_input, ] %>%
    .[model_number == 1, ]
  figure <- ggplot(data = dt_input, aes(x = as.factor(interval_tr), y = as.numeric(aic))) +
    geom_line(aes(group = 1)) +
    geom_point() +
    xlab("Time from inter-trial interval onset") +
    ylab("AIC") +
    theme_zoo() +
    # facet_wrap(~ roi, scales = "free_y") +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    theme(legend.key = element_blank()) + 
    ggtitle("AICs of stimulus model") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  return(figure)
}

plot_decoding_main_model_results_diff <- function(cfg, paths, group = NULL, roi_input = NULL) {
  group_name <- paste(group, collapse = "_")
  input_path <- paste(paths$source$decoding_main_model_results_diff, group_name, sep = "_")
  dt_input <- load_data(input_path) %>%
    .[model_name != "Stimulus", ]
  if (!is.null(roi_input)) {
    dt_input <- dt_input %>% .[roi == roi_input, ]
  }
  figure <- ggplot(data = dt_input, aes(x = as.numeric(interval_tr), y = as.numeric(aic_diff))) +
    geom_hline(yintercept = 0, color = "black") +
    geom_line(aes(color = as.factor(model_name), group = as.factor(model_name))) +
    geom_point(aes(color = as.factor(model_name), group = as.factor(model_name), shape = as.factor(model_name))) +
    ggtitle("AICs of replay models") +
    xlab("Time from inter-trial interval onset (1 TR = 1.25 s)") +
    ylab("Relative AIC") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_color_manual(values = cfg$colors_models) +
    scale_x_continuous(limits = c(0, 8), labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    # theme(legend.position = c(0.85, 0.15)) +
    theme(legend.title = element_blank()) +
    theme(legend.key = element_blank())
  if (!(all(group == "roi") & !is.null(roi_input))) {
    figure <- figure +
      facet_wrap(group)
  }
  figure <- set_yaxis(figure)
  figure <- add_arrows(figure)
  return(figure)
}

plot_decoding_main_model_results_diff_run_half <- function(cfg, paths, group, roi_input) {
  group_name <- paste(group, collapse = "_")
  input_path <- paste(paths$source$decoding_main_model_results_diff_mean, group_name, sep = "_")
  dt_input <- load_data(input_path) %>%
    .[roi == roi_input, ] %>%
    .[model_number == 4, ]
  arrow_y_change <- dt_input$aic_diff_abs_max[dt_input$run_half == 5]
  arrow_xpos <- 0.75
  arrow_ymax <- 5
  figure <- ggplot(data = dt_input, aes(x = as.numeric(run_half), y = as.numeric(aic_diff_abs_max))) +
    geom_hline(yintercept = 0, color = "black") +
    geom_line(aes(group = 1)) +
    geom_point(aes(group = 1)) +
    xlab("Run") +
    ylab("Relative AIC (SR-replay)") +
    ggtitle("Change in replay over runs") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, ylim = c(-6, 4)) +
    scale_x_continuous(breaks = c(1.5, 3.5, 5.5, 7.5, 9.5), labels = 1:5, limits = c(0, 10))
  figure <- add_arrows(figure)
  figure <- figure + 
    annotate(geom = "segment", x = 5.5, xend = 5.5, y = 2.5, yend = arrow_y_change,
      arrow = arrow(length = unit(3, "pt"), type = "closed"), color = "red") +
    annotate(geom = "text", x = 5.5, y = 3.5, label = "graph structure\nchange",
             color = "red", angle = 0, fontface = "plain", size = rel(3))
  return(figure)
}

plot_decoding_main_model_results_diff_run <- function(cfg, paths, group, roi_input) {
  group_name <- paste(group, collapse = "_")
  input_path <- paste(paths$source$decoding_main_model_results_diff_mean, group_name, sep = "_")
  dt_input <- load_data(input_path) %>%
    .[roi == roi_input, ] %>%
    .[model_number == 4, ]
  arrow_y_change <- dt_input$aic_diff_abs_max[dt_input$run == 3]
  arrow_xpos <- 0.75
  arrow_ymax <- 5
  figure <- ggplot(data = dt_input, aes(x = as.numeric(run), y = as.numeric(aic_diff_abs_max))) +
    geom_hline(yintercept = 0, color = "black") +
    geom_line(aes(group = 1)) +
    geom_point(aes(group = 1)) +
    xlab("Run") +
    ylab("Relative AIC (SR-replay)") +
    ggtitle("Change in replay over runs") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, ylim = c(-10, 5))
  figure <- add_arrows(figure)
  return(figure)
}

plot_decoding_main_model_results_diff_run_consciousness <- function(cfg, paths, group, roi_input) {
  group_name <- paste(group, collapse = "_")
  input_path <- paste(paths$source$decoding_main_model_results_diff_mean, group_name, sep = "_")
  dt_input <- load_data(input_path) %>%
    .[roi == roi_input, ] %>%
    .[model_number == 4, ]
  arrow_y_change <- dt_input$aic_diff_abs_max[dt_input$run == 3]
  arrow_xpos <- 0.75
  arrow_ymax <- 5
  figure <- ggplot(data = dt_input, aes(x = as.numeric(run), y = as.numeric(aic_diff_abs_max))) +
    geom_hline(yintercept = 0, color = "black") +
    geom_line(aes(group = 1)) +
    geom_point(aes(group = 1)) +
    facet_wrap(~ sequence_detected) +
    xlab("Run") +
    ylab("Relative AIC (SR-replay)") +
    ggtitle("Change in replay over runs") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, ylim = c(-10, 5))
  figure <- add_arrows(figure)
  return(figure)
}

plot_decoding_main_model_results_run_diff_all <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_diff_run) %>%
    .[model_number != 1, ]
  arrow_xpos <- 0.75
  arrow_ymax <- 10
  figure <- ggplot(data = dt_input, aes(x = as.numeric(run_half), y = as.numeric(aic_diff))) +
    geom_hline(yintercept = 0, color = "black") +
    geom_line(aes(color = as.factor(model_name), group = as.factor(model_name))) +
    geom_point(aes(color = as.factor(model_name), group = as.factor(model_name),
                   shape = as.factor(model_name))) +
    facet_wrap(~ roi) +
    xlab("Run") +
    ylab("Relative AIC") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_continuous(breaks = c(1.5, 3.5, 5.5, 7.5, 9.5), labels = 1:5) +
    theme(legend.title = element_blank()) +
    theme(legend.key = element_blank()) + 
    geom_segment(aes(
      x = arrow_xpos, xend = arrow_xpos, y = 0 + arrow_ymax / 15, yend = arrow_ymax),
      arrow = arrow(length = unit(3, "pt"), type = "closed"), color = "black") +
    geom_segment(aes(
      x = arrow_xpos, xend = arrow_xpos, y = 0 - arrow_ymax / 15, yend = -arrow_ymax),
      arrow = arrow(length = unit(3, "pt"), type = "closed"), color = "black") +
    annotate(geom = "text", x = 0, y = arrow_ymax / 1.75, label = "Stimulus model\nbetter",
             color = "black", angle = 90, fontface = "italic", size = rel(2.5)) +
    annotate(geom = "text", x = 0, y = -arrow_ymax / 1.75, label = "Replay model\nbetter",
             color = "black", angle = 90, fontface = "italic", size = rel(2.5)) +
    ggtitle("Change in replay over runs") +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(nrow = 1, ncol = 3)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  return(figure)
}

plot_decoding_main_model_no_evoked_prob <- function(cfg, paths, mask_input, graph_input) {
  dt <- load_data(paths$source$decoding_main_model_no_evoked) %>%
    .[mask_test == mask_input, ] %>%
    .[graph == graph_input]
  title_text <- sprintf("Classifier probabilities\n(%sdirectional graph)", graph_input)
  figure <- ggplot(dt, aes(x = as.factor(interval_tr), y = mean_prob)) +
    annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    stat_summary(aes(group = as.factor(dist_combined), fill = as.factor(dist_combined)),
                 geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = as.factor(dist_combined), color = as.factor(dist_combined)),
                 geom = "line", fun = "mean") +
    ylab("Probability (in %)") +
    xlab("Time from inter-trial interval onset") +
    theme_zoo() +
    ggtitle(title_text) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    scale_color_viridis_d(name = "Node distance (uni | bi)", direction = 1) +
    scale_fill_viridis_d(name = "Node distance (uni | bi)", direction = 1) +
    guides(color = guide_legend(nrow = 1, ncol = 5)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0))
  return(figure)
}

plot_decoding_main_model_no_evoked_slope <- function(cfg, paths, mask_input, graph_input) {
  dt1 <- load_data(paths$source$decoding_main_model_no_evoked_slope) %>%
    .[mask_test == mask_input, ] %>%
    .[graph == graph_input, ] %>%
    .[variable == "Slope", ]
  dt2 <- load_data(paths$source$decoding_main_model_no_evoked_slope_stat) %>%
    .[mask_test == mask_input, ] %>%
    .[graph == graph_input, ] %>%
    .[variable == "Slope", ] %>%
    .[, p.value_significance := ifelse(p.value_significance == "n.s.", " ", p.value_significance)]
  arrow_ymax <- 0.01
  arrow_xpos <- 0.5
  title_text <- sprintf("Sequentiality in probabilities\n(%sdirectional graph)", graph_input)
  figure <- ggplot(dt1, aes(x = as.numeric(interval_tr), y = as.numeric(value))) +
    # annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    # annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = 0, color = "gray") +
    stat_summary(aes(group = 1), geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    geom_point(data = dt2, aes(fill = p.value_significance, y = estimate), color = "black", pch = 21) +
    # geom_text(data = dt2, aes(label = paste("p =", p.value_adjust_round), y = as.numeric(estimate)), vjust = -5) +
    geom_text(data = dt2, aes(label = as.factor(p.value_significance), y = as.numeric(estimate)), vjust = -2.5) +
    ylab("Regression slope") +
    xlab("Time from inter-trial interval onset") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_continuous(limits = c(0, 8), labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    # scale_fill_manual(values = c("black")) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(nrow = 1, ncol = 6)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    ggtitle(title_text) +
    # ggtitle("Sequentiality in classifier probabilities") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("black", "red")) +
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

plot_decoding_main_model_no_evoked_slope_all <- function(cfg, paths, mask_input) {
  dt1 <- load_data(paths$source$decoding_main_model_no_evoked_slope) %>%
    .[mask_test == mask_input, ]
  dt2 <- load_data(paths$source$decoding_main_model_no_evoked_slope_stat) %>%
    .[mask_test == mask_input, ]
  figure <- ggplot(dt1, aes(x = as.numeric(interval_tr), y = as.numeric(value))) +
    # annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    # annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = 0, color = "black") +
    stat_summary(aes(group = 1), geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(aes(group = 1), geom = "point", fun = "mean") +
    geom_point(data = dt2, aes(fill = p.value_significance, y = estimate), color = "black", pch = 21) +
    # geom_text(data = dt2, aes(label = paste("p =", p.value_adjust_round), y = as.numeric(estimate)), vjust = -5) +
    geom_text(data = dt2, aes(label = as.factor(p.value_significance), y = as.numeric(estimate)), vjust = -3) +
    ylab("Regression slope") +
    xlab("Time from inter-trial interval onset (in TRs)") +
    theme_zoo() +
    facet_grid(vars(variable), vars(graph), scales = "free_y") +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_continuous(limits = c(0, 8), labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    # scale_fill_manual(values = c("black")) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(nrow = 1, ncol = 6)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    ggtitle("Sequentiality in classifier probabilities") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(legend.position = "none")
  return(figure)
}


plot_decoding_main_model_no_evoked_all <- function(cfg, paths) {
  dt <- load_data(paths$source$decoding_main_no_evoked)
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
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0))
  return(figure)
}

plot_decoding_main_model_no_evoked_num_class_trials <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_no_evoked_num_class_trials) %>%
    .[mask_test == "visual", ]
  figure <- ggplot(dt_input, aes(x = as.factor(interval_tr), y = as.numeric(num_trials))) +
    annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = 60, linetype = "dashed", color = "gray") +
    geom_line(aes(group = interaction(as.factor(id), as.factor(node_classifier)),
                  color = as.factor(node_classifier)), alpha = 0.05) +
    stat_summary(aes(group = as.factor(node_classifier), color = as.factor(node_classifier)),
                 geom = "line", fun = "mean") +
    stat_summary(aes(group = as.factor(node_classifier), color = as.factor(node_classifier)),
                 geom = "point", fun = "mean") +
    stat_summary(aes(label = after_stat(round(y, 0))),
                 geom = "text", fun = "mean", vjust = -3) +
    # facet_grid(vars(mask_test), vars(graph), scales = "free_y") +
    facet_wrap(~ graph) +
    ylab("Number of trials") +
    xlab("Time from inter-trial interval onset (in TRs)") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, ylim = c(0, 60)) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    scale_color_manual(name = "Class", values = cfg$colors_class) +
    scale_fill_manual(name = "Class", values = cfg$colors_class) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(color = guide_legend(title.position = "bottom", title.hjust = 0.5, nrow = 1, ncol = 6)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0))
  return(figure)
}

plot_decoding_main_model_no_evoked_num_dist_trials <- function(cfg, paths) {
  dt_input = load_data(paths$source$decoding_main_no_evoked_num_dist_trials) %>%
    .[mask_test == "visual",]
  figure <- ggplot(dt_input, aes(x = as.factor(interval_tr), y = as.numeric(num_trials))) +
    annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    geom_hline(yintercept = 60, linetype = "dashed", color = "gray") +
    geom_line(aes(group = interaction(as.factor(id), as.factor(dist_combined)),
                  color = as.factor(dist_combined)), alpha = 0.1) +
    stat_summary(aes(group = as.factor(dist_combined), color = as.factor(dist_combined)),
                 geom = "line", fun = "mean") +
    stat_summary(aes(group = as.factor(dist_combined), color = as.factor(dist_combined)),
                 geom = "point", fun = "mean") +
    stat_summary(aes(group = 1), geom = "line", fun = "mean", color = "black") +
    stat_summary(aes(group = 1), geom = "point", fun = "mean", color = "black") +
    # stat_summary(geom = "point", fun = "mean", color = "black") +
    stat_summary(aes(label = after_stat(round(y, 0))),
                 geom = "text", fun = "mean", vjust = -5) +
    # facet_grid(vars(mask_test), vars(graph), scales = "free_y") +
    facet_wrap(~ graph) +
    ylab("Number of trials") +
    xlab("Time from inter-trial interval onset (in TRs)") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, ylim = c(0, 80)) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    scale_color_viridis_d(name = "Node distance (uni | bi )") +
    scale_fill_viridis_d(name = "Node distance (uni | bi )") +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(color = guide_legend(title.position = "bottom", title.hjust = 0.5, nrow = 1, ncol = 6)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0))
  return(figure)
}

plot_decoding_main_model_no_evoked_consc <- function(paths) {
  dt <- load_data(paths$source$decoding_main_no_evoked) %>%
    .[, mask_test := factor(as.factor(mask_test), levels = c("visual", "motor"))] %>%
    .[, graph := factor(as.factor(graph), levels = c("uni", "bi"))] %>%
    .[interval_tr %in% seq(1, 8)] %>%
    .[mask_test == "visual", ]
  figure <- ggplot(dt, aes(x = as.factor(interval_tr), y = mean_prob)) +
    annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
    annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
    stat_summary(aes(group = as.factor(dist_combined), fill = as.factor(dist_combined)),
                 geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
    stat_summary(aes(group = as.factor(dist_combined), color = as.factor(dist_combined)),
                 geom = "line", fun = "mean") +
    facet_grid(vars(sequence_detected), vars(graph), scales = "free_y") +
    ylab("Probability (in %)") +
    xlab("Time from inter-trial interval onset (in TRs)") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_x_discrete(labels = label_fill(seq(1, 8, 1), mod = 1), breaks = seq(1, 8, 1)) +
    scale_color_viridis_d(name = "Node distance (uni | bi)", direction = 1) +
    scale_fill_viridis_d(name = "Node distance (uni | bi)", direction = 1) +
    guides(color = guide_legend(nrow = 1, ncol = 5)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    ggtitle("Visual cortex")
  return(figure)
}
