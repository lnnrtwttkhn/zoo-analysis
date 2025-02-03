plot_behavior_sequence_run_response_time <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sequence_run)
  figure <- ggplot(data = dt_input, aes(
    x = as.factor(run_index), y = mean_log_response_time)) +
    # geom_line(aes(group = id), alpha = 0.3, color = "gray") +
    geom_beeswarm(aes(group = id), alpha = 0.3,
                  dodge.width = 0.1, color = "gray") +
    geom_boxplot(width = 0.3, fill = NA, outlier.shape = NA) +
    stat_summary(geom = "point", fun = "mean", pch = 23, color = "black") +
    stat_summary(aes(group = 1), geom = "ribbon", fun.data = "mean_se", alpha = 0.3) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(geom = "errorbar", fun.data = "mean_se", color = "black", width = 0) +
    ylab("Response time (log ms)") +
    xlab("Run") +
    ggtitle("Sequence") +
    theme_zoo() +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    # theme(axis.line.x = element_line(color = "white")) +
    # theme(axis.ticks.x = element_line(color = "white")) +
    theme(legend.position = "none")
  return(figure)
}

plot_behavior_sequence_run_accuracy <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sequence_run)
  figure <- ggplot(data = dt_input, aes(x = as.factor(run_index), y = as.numeric(mean_accuracy))) +
    geom_hline(yintercept = 100/6, color = "gray", linetype = "dashed") +
    geom_line(aes(group = as.factor(id), color = as.factor(id)), alpha = 0.3, color = "gray") +
    geom_beeswarm(aes(group = as.factor(id), color = as.factor(id)), alpha = 0.3, dodge.width = 0.1, color = "gray") +
    geom_boxplot(width = 0.3, fill = NA, outlier.shape = NA) +
    stat_summary(geom = "point", fun = "mean", pch = 23, color = "black") +
    stat_summary(aes(group = 1), geom = "ribbon", fun.data = "mean_se", alpha = 0.3) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(geom = "errorbar", fun.data = "mean_se", color = "black", width = 0) +
    ylab("Behavioral accuracy (in %)") +
    xlab("Run") +
    ylim(c(0, 100)) +
    ggtitle("Sequence trials") +
    theme_zoo() +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    # theme(axis.line.x = element_line(color = "white")) +
    # theme(axis.ticks.x = element_line(color = "white")) +
    theme(legend.position = "none") + 
    annotate("text", x = 5, y = 100 / 6 - 5, hjust = 0.5,
           label = "Chance", color = "gray",
           size = rel(2.5), family = "Helvetica", fontface = "plain")
  return(figure)
}

plot_behavior_sequence_accuracy_mean <- function() {
  dt_input <-  load_data(paths$source$behavior_sequence_run) %>%
    .[, by = .(id), .(mean_accuracy = mean(mean_accuracy), num_runs = .N)] %>%
    verify(num_runs == 5)
  chance_level <- 100/6
  figure <- ggplot(data = dt_input, aes(x = as.factor(1), y = mean_accuracy)) +
    geom_hline(yintercept = chance_level, linetype = "dashed", color = "gray") +
    geom_beeswarm(aes(group = id), alpha = 0.3, color = "black", groupOnX = TRUE, cex = 0.5) +
    geom_boxplot(width = 0.3, outlier.shape = NA) +
    stat_summary(geom = "point", fun = "mean", pch = 23, color = "black") +
    stat_summary(geom = "errorbar", fun.data = "mean_se", color = "black", width = 0) +
    theme_zoo() +
    ylim(c(0, 100)) +
    coord_capped_cart(expand = TRUE, left = "both", bottom = "both") +
    ylab("Behavioral accuracy (in %)") +
    ggtitle("Sequence") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_zoo() +
    theme(axis.line.x = element_line(color = "white")) +
    theme(axis.ticks.x = element_line(color = "white")) +
    theme(axis.title.x = element_text(color = "white")) +
    theme(axis.text.x = element_text(color = "white")) +
    theme(legend.position = "none") +
    annotate("text", x = 1, y = chance_level - 7,
             label = "Chance", color = "gray",
             size = rel(2.0), family = "Helvetica", fontface = "plain")
  return(figure)
}

plot_behavior_sequence_accuracy_onestep <- function(cfg, paths) {
  dt1 <- load_data(paths$source$behavior_sequence_onestep)
  dt2 <- load_data(paths$source$behavior_sequence_onestep_stat) %>%
    .[variable == "mean_accuracy"]
  chance_level = 100/6
  figure <- ggplot(data = dt1, aes(x = as.factor(onestep), y = as.numeric(mean_accuracy))) +
    facet_wrap(~ graph, scales = "free_x") +
    geom_hline(yintercept = chance_level, linetype = "dashed", color = "gray") +
    geom_beeswarm(aes(group = id, color = onestep), alpha = 0.3, cex = 0.5) +
    annotate(geom = "segment", x = 1, y = 110, xend = 2, yend = 110, color = "gray") +
    geom_text(data = dt2, aes(x = 1.5, y = 115, label = paste("p", p.value_adjust_round_label)),
              color = "gray", parse = FALSE, size = rel(2.5)) +
    geom_boxplot(aes(color = onestep), width = 0.2, outlier.shape = NA) +
    stat_summary(aes(group = 1), geom = "ribbon", fun.data = "mean_se", alpha = 0.3) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(aes(color = onestep), geom = "point", fun = "mean", pch = 23, color = "black") +
    stat_summary(aes(color = onestep), geom = "errorbar", fun.data = "mean_se", color = "black", width = 0) +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both") +
    scale_y_continuous(
      limits = c(0, 120),
      labels = label_fill(seq(0, 100, 20), mod = 1),
      breaks = seq(0, 100, 20)) +
    xlab("Transition probability") +
    ylab("Behavioral accuracy (in %)") +
    scale_color_manual(values = cfg$colors_probability[1:3]) +
    scale_fill_manual(values = cfg$colors_probability[1:3]) +
    theme(axis.line.x = element_line(color = "white")) +
    theme(axis.ticks.x = element_line(color = "white")) +
    theme(legend.position = "none") +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    annotate("text", x = 1.5, y = chance_level - 7.5, label = "Chance", color = "gray", size = rel(2.5), family = "Helvetica", fontface = "plain")
  return(figure)
}

plot_behavior_sequence_response_time_onestep = function(cfg, paths) {
  dt1 <- load_data(paths$source$behavior_sequence_onestep)
  dt2 <- load_data(paths$source$behavior_sequence_onestep_stat) %>%
    .[variable == "mean_log_response_time"]
  figure <- ggplot(data = dt1, aes(x = onestep, y = mean_log_response_time)) +
    facet_wrap(~ graph, scales = "free_x") +
    geom_beeswarm(aes(group = id, color = onestep), alpha = 0.3, cex = 0.5) +
    geom_boxplot(aes(color = onestep), width = 0.2, outlier.shape = NA) +
    stat_summary(aes(group = 1), geom = "ribbon", fun.data = "mean_se", alpha = 0.3) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(aes(color = onestep), geom = "point", fun = "mean", pch = 23, color = "black") +
    stat_summary(aes(color = onestep), geom = "errorbar", fun.data = "mean_se", color = "black", width = 0) +
    annotate(geom = "segment", x = 1, y = -0.38, xend = 2, yend = -0.38, color = "gray") +
    geom_text(data = dt2, aes(x = 1.5, y = -0.37, label = paste("p", p.value_adjust_round_label)),
              color = "gray", parse = FALSE, size = rel(2.5)) +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, left = "both", bottom = "both") +
    xlab("Transition probability") +
    ylab("Response time (log ms)") +
    theme_zoo() +
    scale_color_manual(values = cfg$colors_probability[1:3]) +
    scale_fill_manual(values = cfg$colors_probability[1:3]) +
    theme(axis.line.x = element_line(color = "white")) +
    theme(axis.ticks.x = element_line(color = "white")) +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    theme(legend.position = "none")
  return(figure)
}

plot_behavior_sequence_response_time_onestep_run <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sequence_onestep_run)
  figure <- ggplot(dt_input, aes(x = run_index, y = as.numeric(mean_log_response_time))) +
    # geom_line(group = 1, aes(fill = interaction(id, onestep)), color = "black", alpha = 0.1) +
    geom_smooth(aes(color = onestep), method = "lm") +
    ylab("Response time (log ms)") +
    xlab("Run") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_color_manual(values = cfg$colors_probability[c(1, 4)], name = "Probability") +
    # scale_fill_manual(values = cfg$colors_probability, name = "Predictor") +
    # scale_shape_manual(name = "Predictor") +
    ggtitle("Decrease in response times with learning\ndepending on transition probability") +
    # ggtitle("Betas of SR + 1-step model\n(bidirectional graph)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    # theme(legend.position = "none")
  return(figure)
}

plot_behavior_sequence_response_time_distance <- function(cfg, paths) {
  dt <- load_data(paths$source$behavior_sequence_graph)
  figure <- ggplot(data = dt, aes(x = dist_dir_label, y = mean_log_rt)) +
    facet_wrap(~ graph) +
    geom_boxplot(aes(color = graph), outlier.shape = NA, width = 0.5) +
    geom_beeswarm(aes(color = graph), alpha = 0.2) +
    stat_summary(aes(group = 1, fill = graph), geom = "ribbon",
                 fun.data = "mean_se", alpha = 0.3, color = NA) +
    stat_summary(aes(group = 1, color = graph),
                 geom = "line", fun = "mean") +
    stat_summary(aes(fill = graph), pch = 23,
                 position = position_dodge(width = 0.5),
                 geom = "point", fun = "mean", color = "black") +
    stat_summary(geom = "errorbar", width = 0,
                 position = position_dodge(width = 0.5),
                 fun.data = "mean_se", color = "black") +
    scale_color_manual(values = cfg$colors_graph, name = "Graph") +
    scale_fill_manual(values = cfg$colors_graph, name = "Graph") +
    ylab("Response time (log ms)") +
    xlab("Node distance") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(hjust = 0.85)) +
    theme(panel.spacing = unit(2, "lines"))
  return(figure)
}

plot_behavior_sequence_response_time_distance_consciousness <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_sequence_graph)
  figure <- ggplot(data = dt_input, aes(x = dist_dir_label, y = mean_log_rt)) +
    facet_wrap(~ graph) +
    # geom_boxplot(aes(group = sequence_detected),
    #              position = position_dodge(width = 0.5),
    #              outlier.shape = NA, width = 0.5) +
    # geom_beeswarm(aes(color = graph, group = interaction(sequence_detected, dist_dir_label)), alpha = 0.2, position = position_dodge(width = 0.5)) +
    stat_summary(aes(group = sequence_detected, fill = sequence_detected), geom = "ribbon",
                 position = position_dodge(width = 0.5),
                 fun.data = "mean_se", alpha = 0.3, color = NA) +
    stat_summary(aes(group = sequence_detected, color = sequence_detected),
                 position = position_dodge(width = 0.5),
                 geom = "line", fun = "mean") +
    stat_summary(aes(fill = sequence_detected, group = sequence_detected), pch = 23,
                 position = position_dodge(width = 0.5),
                 geom = "point", fun = "mean", color = "black") +
    stat_summary(aes(group = sequence_detected), geom = "errorbar", width = 0,
                 position = position_dodge(width = 0.5),
                 fun.data = "mean_se", color = "black") +
    scale_color_manual(values = cfg$colors_conscious, name = "Did you notice any\nsequential structure?") +
    scale_fill_manual(values = cfg$colors_conscious, name = "Did you notice any\nsequential structure?") +
    ylab("Response time (log ms)") +
    xlab("Node distance") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both") +
    theme(legend.position = "bottom")
  return(figure)
}

plot_behavior_sequence_response_time_distance_order = function() {
  dt_input <- load_data(paths$source$behavior_sequence_graph) %>%
    .[, graph := factor(as.factor(graph), levels = c("uni", "bi"))]
  figure = ggplot(data = dt_input, aes(
    x = dist_dir_label, y = mean_log_rt, group = interaction(dist_dir_label, graph))) +
    facet_wrap(~ order) +
    geom_point(aes(color = graph),
               position = position_dodge(width = 0.5), alpha = 0.3) +
    geom_boxplot(aes(fill = graph), outlier.shape = NA, width = 0.5, color = "black") +
    stat_summary(aes(group = graph, fill = graph),
                 position = position_dodge(width = 0.5),
                 geom = "ribbon", fun.data = "mean_se", alpha = 0.5) +
    stat_summary(aes(group = graph, color = graph),
                 position = position_dodge(width = 0.5),
                 geom = "line", fun = "mean", color = "black") +
    stat_summary(aes(fill = graph), pch = 23,
                 position = position_dodge(width = 0.5),
                 geom = "point", fun = "mean", color = "black") +
    stat_summary(geom = "errorbar", width = 0,
                 position = position_dodge(width = 0.5),
                 fun.data = "mean_se", color = "black") +
    ylab("Log response time") +
    xlab("Node distance (assuming the graph structure)") +
    scale_color_manual(values = cfg$colors_graph, name = "Graph") +
    scale_fill_manual(values = cfg$colors_graph, name = "Graph") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both")
  return(figure)
}
