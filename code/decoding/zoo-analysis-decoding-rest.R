get_decoding_rest_std <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest)
  dt_output <- dt_input %>%
    .[, by = .(id, session, run, mask, interval_tr), .(
      num_class = .N,
      std_prob = sd(probability),
      std_prob_norm = sd(probability_norm)
    )] %>%
    verify(num_class == cfg$num_nodes) %>%
    .[, num_class := NULL] %>%
    .[, by = .(id, session, run, mask), .(
      num_trs = .N,
      mean_std_prob = sd(std_prob),
      mean_std_prob_norm = sd(std_prob_norm)
    )] %>%
    verify(num_trs %in% cfg$rest$num_trs) %>%
    .[, ses_run := paste(session, run, sep = "_")] %>%
    save_data(., paths$decoding_rest_std)
}

plot_decoding_rest_std <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_std)
  ggplot(dt_input, aes(x = ses_run, y = mean_std_prob)) +
    geom_beeswarm(dodge.width = 0.9, alpha = 0.3) +
    geom_boxplot(outlier.shape = NA, width = 0.5) +
    stat_summary(geom = "point", fun = "mean", pch = 23) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(geom = "linerange", fun.data = "mean_se") +
    facet_wrap(~ mask) +
    ylab("SD of probability") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
