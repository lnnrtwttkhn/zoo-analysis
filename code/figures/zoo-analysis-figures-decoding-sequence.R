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
