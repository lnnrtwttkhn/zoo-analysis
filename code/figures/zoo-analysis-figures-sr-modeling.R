plot_sr_grid_seq <- function(dt_input) {
  dt_best <- dt_input %>% .[ranking == 1, ]
  figure <- ggplot(data = dt_input, aes(x = gamma, y = aic)) +
    geom_vline(data = dt_best, aes(xintercept = gamma), color = "gray") +
    geom_line(aes(group = 1)) +
    geom_point(aes(color = as.factor(gamma))) +
    scale_color_manual(values = cfg$colors_sr) +
    scale_fill_manual(values = cfg$colors_graph) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    # scale_y_continuous(labels = unit_format(unit = "T", scale = 1e-6)) +
    theme_zoo() +
    scale_x_continuous(
      limits = c(0, 0.95),
      labels = label_fill(seq(0, 0.95, 0.05), mod = 3),
      breaks = seq(0, 0.95, 0.05)) +
    theme(legend.position = "none") +
    xlab(expression(gamma)) +
    ylab("AIC")
  return(figure)
}

plot_sr_grid_seq_lme <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_grid_seq_lme)
  figure <- plot_sr_grid_seq(dt_input)
  return(figure)
}

plot_sr_grid_seq_graph_lme <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_grid_seq_graph_lme)
  dt_best <- dt_input %>% .[ranking == 1, ]
  figure <- plot_sr_grid_seq(dt_input) +
    facet_wrap(~ graph, scales = "free_y") +
    geom_rect(data = dt_best, aes(fill = graph),
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.05)
  return(figure)
}

plot_sr_grid_seq_block_lme <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_grid_seq_block_lme)
  figure <- plot_sr_grid_seq(dt_input) +
    facet_wrap(~ block, scales = "free_y")
  return(figure)
}

plot_sr_grid_seq_graph_order_lme <- function(cfg, paths){
  dt_input <- load_data(paths$source$behavior_sr_grid_seq_graph_order_lme)
  dt_best <- dt_input %>% .[ranking == 1, ]
  figure <- plot_sr_grid_seq(dt_input) +
    facet_wrap(~ order_graph, scales = "free_y", nrow = 2, ncol = 2) +
    geom_rect(data = dt_best, aes(fill = graph),
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.05)
  return(figure)
}

plot_behavior_sr_fit_starting_values <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_starting_values)
  figure <- ggplot(data = dt_input, aes(x = value, group = as.factor(iter), color = as.factor(iter))) +
    geom_freqpoly(bins = 30, position = position_dodge2()) +
    facet_wrap(~ variable) +
    theme_zoo() +
    xlab("Starting values") +
    ylab("Number of participants") +
    ggtitle("Dispersion of starting values") +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(figure)
}

plot_behavior_sr_fit_parameter_dispersion <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_parameter_dispersion)
  figure <- ggplot(data = dt_input, aes(x = sd_estimate)) +
    geom_freqpoly(bins = 30) +
    # facet_wrap(~ variable) +
    facet_grid(vars(model_name), vars(variable)) +
    theme_zoo() +
    xlab("Standard deviation of parameter estimates") +
    ylab("Number of participants") +
    ggtitle("Dispersion of parameter estimates") +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE)
  return(figure)
}


plot_behavior_sr_fit_model_comparison <- function(cfg, paths) {
  dt1 <- load_data(paths$source$behavior_sr_fit_model_comparison)
  dt2 <- load_data(paths$source$behavior_sr_fit_model_comparison_stat)
  ypos <- max(dt1$value) + sd(dt1$value)
  figure <- ggplot(data = dt1, aes(x = model_name, y = value)) +
    geom_boxplot(outlier.shape = NA, width = 0.5, color = "black") +
    geom_beeswarm(alpha = 0.3) +
    stat_summary(geom = "point", fun = "mean", pch = 23) +
    stat_summary(geom = "linerange", fun.data = "mean_se", position = position_dodge(0.9)) +
    geom_segment(aes(x = 1, y = ypos, xend = 2, yend = ypos), color = "black") +
    geom_label(data = dt2,
               aes(x = 1.5, y = ypos, label = paste("p", p.value_adjust_round_label)),
               color = "black", parse = FALSE, size = rel(2.5)) +
    facet_wrap(~ variable) +
    ylab("Value") +
    xlab("Model") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(axis.line.x = element_line(color = "white")) +
    theme(axis.ticks.x = element_line(color = "white")) +
    theme(axis.title.x = element_text(colour = "black")) +
    theme(axis.text.x = element_text(colour = "black")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
  return(figure)
}

plot_behavior_sr_fit_model_comparison_sum_aic <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_model_comparison) %>%
    .[variable == "aic", ]
  ymax <- -102000
  figure <- ggplot(data = dt_input, aes(x = model_name, y = value)) +
    stat_summary(geom = "bar", fun = "sum") +
    ylab("AIC") +
    xlab("SR model") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", ylim = c(NA, ymax)) +
    theme(axis.line.x = element_line(color = "white")) +
    theme(axis.ticks.x = element_line(color = "white")) +
    theme(axis.title.x = element_text(colour = "black")) +
    theme(axis.text.x = element_text(colour = "black")) +
    theme(plot.title = element_text(hjust = 0.5))
  return(figure)
}

plot_behavior_sr_fit_model_comparison_sum_bic <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_model_comparison) %>%
    .[variable == "bic", ]
  ymax <- -100300
  figure <- ggplot(data = dt_input, aes(x = model_name, y = value)) +
    stat_summary(geom = "bar", fun = "sum") +
    ylab("BIC") +
    xlab("SR model") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", ylim = c(NA, ymax)) +
    theme(axis.line.x = element_line(color = "white")) +
    theme(axis.ticks.x = element_line(color = "white")) +
    theme(axis.title.x = element_text(colour = "black")) +
    theme(axis.text.x = element_text(colour = "black")) +
    theme(plot.title = element_text(hjust = 0.5))
  return(figure)
}

plot_behavior_sr_fit_parameter_distribution <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_parameter_distribution)
  figure <- ggplot(data = dt_input, aes(x = value)) +
    geom_histogram(bins = 10) +
    # facet_wrap(~ variable) +
    facet_grid(vars(model_name), vars(variable)) +
    theme_zoo() +
    xlab("Parameter estimates") +
    ylab("Number of participants") +
    ggtitle("Distribution of parameter estimates") +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(figure)
}

plot_behavior_sr_fit_parameter_distribution_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_parameter_distribution)
  figure <- ggplot(data = dt_input, aes(x = variable, y = value, fill = variable)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(position = position_jitter(width = 0.2, height = 0, seed = 666)) +
    stat_summary(fun = "mean", geom = "point", shape = 23, size = 2, fill = "white", color = "black") +
    xlab("Parameter") +
    ylab("Estimate") +
    facet_wrap(~ model_name) +
    ggtitle("Mean parameter estimates") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.line.x = element_line(colour = "white")) +
    theme(axis.ticks.x = element_blank()) +
    theme(legend.position = "none")
  return(figure)
}

plot_sr_matrix <- function() {
  dt_load <- load_data(file.path(paths$sourcedata, "zoo_sourcedata_behavior_sr.csv"))
  dt_plot <- dt_load %>%
    .[trial_run == 2, ] %>%
    .[, condition := as.factor(as.character(condition))] %>%
    .[, condition := factor(condition, levels = c("training", "recall", "main"))] %>%
    .[, label := paste(run, graph)] %>%
    .[condition %in% c("recall", "main"), ]
  plot_sub <- function(dt_sub) {
    id <- unique(dt_sub$id)
    title_string <- sprintf("Successor matrix at trial 2 (%s)", id)
    ggplot(dt_sub, aes(x = previous, y = current, fill = sr_prob)) +
      facet_grid(vars(condition), vars(run)) +
      geom_tile() +
      scale_fill_viridis(option = "inferno", name = "SR probability") +
      xlab("State") +
      ylab("State") +
      ggtitle(title_string) +
      theme_zoo() +
      coord_fixed() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.line = element_line(colour = "white")) +
      theme(axis.ticks = element_blank()) +
      theme(legend.position = "bottom")
  }
  num_subs <- 4
  set.seed(123)
  select_ids <- sample(unique(dt_plot$id), num_subs, replace = FALSE)
  plot_sub1 <- plot_sub(dt = dt_plot %>% .[id == select_ids[1]])
  plot_sub2 <- plot_sub(dt = dt_plot %>% .[id == select_ids[2]])
  plot_sub3 <- plot_sub(dt = dt_plot %>% .[id == select_ids[3]])
  plot_sub4 <- plot_sub(dt = dt_plot %>% .[id == select_ids[4]])
  figure <- plot_sub1 + plot_sub2 + plot_sub3 + plot_sub4
  save_figure(plot = figure, "sr_matrix", width = 12, height = 9)
  return(figure)
}

plot_sr_timecourse <- function() {
  
  dt_load <- load_data(file.path(paths$sourcedata, "zoo_sourcedata_behavior_sr.csv"))
  
  num_subs <- 1
  set.seed(123)
  select_ids <- sample(unique(dt_load$id), num_subs, replace = FALSE)
  dt_plot <- dt_load %>%
    .[id %in% select_ids, ] %>%
    .[, graph := factor(as.factor(graph), levels = c("uni", "bi"))] %>%
    .[condition == "main", ] %>%
    .[, label := paste(condition, run, graph, sep = "\n")] %>%
    # .[, label := fct_rev(label)] %>%
    .[previous != current, ] %>%
    .[dist_current != 1, ] %>%
    .[dist_current != 5, ]
  
  figure = ggplot(data = dt_plot, aes(x = trial_run, y = as.numeric(sr_prob))) +
    facet_grid(vars(as.factor(previous)), vars(as.factor(label))) +
    # geom_hline(yintercept = 0.1, linetype = "solid", color = "gray") +
    # geom_hline(yintercept = 0.35, linetype = "solid", color = "gray") +
    # geom_hline(yintercept = 0.7, linetype = "solid", color = "gray") +
    geom_line(aes(group = as.factor(dist_prob), color = as.factor(dist_prob))) +
    ylab("Successor probability (in %)") +
    xlab("Time (in trials)") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    guides(color = guide_legend(title = "Successor\nstate"))
  figure
  
}

plot_behavior_sr_fit_parameter_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_parameter_distribution) %>%
    .[model_name == "Full", ]
  figure <- ggplot(data = dt_input, aes(x = variable, y = value)) +
    geom_boxplot(outlier.shape = NA, width = 0.5, color = "black") +
    geom_beeswarm(alpha = 0.3) +
    stat_summary(geom = "point", fun = "mean", pch = 23) +
    stat_summary(geom = "linerange", fun.data = "mean_se", position = position_dodge(0.9)) +
    xlab("Parameter") +
    ylab("Parameter estimate") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(axis.line.x = element_line(color = "white")) +
    theme(axis.ticks.x = element_line(color = "white")) +
    theme(axis.title.x = element_text(colour = "black")) +
    theme(axis.text.x = element_text(colour = "black")) +
    theme(plot.title = element_text(hjust = 0.5))
  return(figure)
}

plot_behavior_sr_fit_parameter_order <- function(cfg, paths) {
  dt1 <- load_data(paths$source$behavior_sr_fit_parameter_distribution) %>%
    .[model_name == "Full", ]
  dt2 <- load_data(paths$source$behavior_sr_fit_parameter_order) %>%
    .[model_name == "Full", ]
  figure <- ggplot(data = dt1, aes(x = order, y = value)) +
    geom_boxplot(outlier.shape = NA, width = 0.5, color = "black") +
    geom_beeswarm(alpha = 0.3) +
    stat_summary(geom = "point", fun = "mean", pch = 23) +
    stat_summary(geom = "linerange", fun.data = "mean_se", position = position_dodge(0.9)) +
    annotate(geom = "segment", x = 1, y = 1.2, xend = 2, yend = 1.2, color = "black") +
    geom_text(data = dt2,
              aes(x = 1.5, y = 1.3, label = paste("p", p.value_adjust_round_label)),
              color = "black", parse = FALSE, size = rel(2.5)) +
    facet_wrap(~ variable) +
    ylab("Parameter estimate") +
    xlab("Graph order") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(axis.line.x = element_line(color = "white")) +
    theme(axis.ticks.x = element_line(color = "white")) +
    theme(axis.title.x = element_text(colour = "black")) +
    theme(axis.text.x = element_text(colour = "black")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
  return(figure)
}

plot_behavior_sr_fit_parameter_conscious <- function(cfg, paths) {
  dt1 <- load_data(paths$source$behavior_sr_fit_parameter_distribution) %>%
    .[model_name == "Full", ]
  dt2 <- load_data(paths$source$behavior_sr_fit_parameter_conscious) %>%
    .[model_name == "Full", ]
  figure <- ggplot(data = dt1, aes(x = sequence_detected, y = value)) +
    geom_boxplot(outlier.shape = NA, width = 0.5, color = "black") +
    geom_beeswarm(alpha = 0.3) +
    stat_summary(geom = "point", fun = "mean", pch = 23) +
    stat_summary(geom = "linerange", fun.data = "mean_se", position = position_dodge(0.9)) +
    annotate(geom = "segment", x = 1, y = 1.2, xend = 2, yend = 1.2, color = "black") +
    geom_text(data = dt2,
              aes(x = 1.5, y = 1.3, label = paste("p", p.value_adjust_round_label)),
              color = "black", parse = FALSE, size = rel(2.5)) +
    facet_wrap(~ variable) +
    ylab("Parameter estimate") +
    xlab('"Did you notice any\nsequential structure?"') +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(axis.line.x = element_line(color = "white")) +
    theme(axis.ticks.x = element_line(color = "white")) +
    theme(axis.title.x = element_text(colour = "black")) +
    theme(axis.text.x = element_text(colour = "black")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
  return(figure)
}

plot_behavior_sr_fit_suprise_effect <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_suprise_effect) %>%
    .[variable == "p_shannon_surprise", ] %>%
    .[model_name == "Full",]
  figure <- ggplot(data = dt_input, aes(x = variable, y = value_log_20)) +
    geom_hline(yintercept = log(0.05, base = 20), linetype = "dashed", color = "black") +
    geom_boxplot(outlier.shape = NA, width = 0.5, color = "black") +
    geom_beeswarm(alpha = 0.3) +
    ylab("p-value\n(base-20 log-transformed)") +
    theme_zoo() +
    coord_capped_cart(left = "both", expand = TRUE) +
    scale_y_continuous(limits = c(-20, 0)) +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.text.x = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(axis.line.x = element_blank())
  return(figure)
}

plot_behavior_sr_fit_response_time_alpha <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_response_time_alpha)
  figure <- ggplot(dt_input, aes(x = as.numeric(alpha), y = as.numeric(mean_log_response_time))) +
    geom_point(aes(group = as.factor(id))) +
    geom_smooth(method = "lm") +
    ylab("Response time (log ms)") +
    xlab("Alpha") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    # scale_color_manual(values = cfg$dist_colors, name = "Predictor") +
    # scale_fill_manual(values = cfg$dist_colors, name = "Predictor") +
    # scale_shape_manual(name = "Predictor") +
    ggtitle("Correlation between alpha and response time") +
    # ggtitle("Betas of SR + 1-step model\n(bidirectional graph)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  figure
  return(figure)
}

plot_sr_mat <- function(df) {
  id <- unique(df$id)
  order <- unique(df$order)
  alpha <- round(unique(df$alpha), 2)
  gamma <- round(unique(df$gamma), 2)
  str_id_order <- sprintf("%s (%s,", id, order)
  str_alpha <- sprintf("= %.2f,", alpha)
  str_gamma <-  sprintf("= %.2f)", gamma)
  title <- paste(str_id_order, "\u03B1", str_alpha, "\u03B3", str_gamma)
  figure <- ggplot(data = df, aes(x = previous, y = current)) +
    geom_tile(aes(fill = sr_prob)) +
    facet_wrap(~ run_cond, nrow = 1) +
    theme(axis.ticks = element_blank()) +
    xlab("Node at t - 1") +
    ylab("Node at trial t") +
    scale_fill_viridis_c(option = "magma", name = "SR probability") +
    scale_y_discrete(limits = rev) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(axis.line = element_blank()) +
    theme(axis.text = element_text(color = "black")) +
    theme(axis.title = element_text(color = "black")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title) +
    theme(plot.tag = element_text(face = "bold"))
  filename <- sprintf("behavior_sr_matrices_sub_%s", id)
  save_figure(plot = figure, filename, width = 8, height = 2)
  return(figure)
}

plot_sr_matrices <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_mat_plot)
  order_factor <- list()
  order_factor["uni - bi"] <- list(c("run-01 (uni)", "run-02 (uni)", "run-03 (uni)", "run-03 (bi)", "run-04 (bi)", "run-05 (bi)"))
  order_factor["bi - uni"] <- list(c("run-01 (bi)", "run-02 (bi)", "run-03 (bi)", "run-03 (uni)", "run-04 (uni)", "run-05 (uni)"))
  figures_all <- list()
  i <- 0
  for (order_index in c("uni - bi", "bi - uni")) {
    df_order <- dt_input %>%
      .[order == order_index, ] %>%
      .[, run_cond := factor(run_cond, levels = order_factor[order_index][[1]])]
    for (sub in unique(df_order$id)) {
      i <- i + 1
      df_sub <- df_order %>%
        .[id == sub]
      figures_all[[i]] <- plot_sr_mat(df_sub)
    }
  }
  figure <- plot_grid(plotlist = figures_all[1:20], ncol = 2)
  save_figure(plot = figure, filename = "behavior_sr_matrices_page1", width = 15, height = 20)
  figure <- plot_grid(plotlist = figures_all[20:39], ncol = 2)
  save_figure(plot = figure, filename = "behavior_sr_matrices_page2", width = 15, height = 20)
  return(figure)
}

plot_sr_matrices_select <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sr_fit_sr_matrices_plot) %>%
    .[id %in% c("sub-04", "sub-42")]
  order_factor <- list()
  order_factor["uni - bi"] <- list(c("run-01 (uni)", "run-02 (uni)", "run-03 (uni)", "run-03 (bi)", "run-04 (bi)", "run-05 (bi)"))
  order_factor["bi - uni"] <- list(c("run-01 (bi)", "run-02 (bi)", "run-03 (bi)", "run-03 (uni)", "run-04 (uni)", "run-05 (uni)"))
  figures_all <- list()
  i <- 0
  for (order_index in c("uni - bi", "bi - uni")) {
    df_order <- dt_input %>%
      .[order == order_index, ] %>%
      .[, run_cond := factor(run_cond, levels = order_factor[order_index][[1]])]
    for (sub in unique(df_order$id)) {
      i <- i + 1
      df_sub <- df_order %>%
        .[id == sub]
      figures_all[[i]] <- plot_sr_mat(df_sub)
    }
  }
  figure <- plot_grid(plotlist = figures_all, nrow = 2)
  return(figure)
}
