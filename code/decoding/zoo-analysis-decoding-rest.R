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

get_decoding_rest_slopes_session <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, session, ses_run, seq_id), .(
      num_trs = .N,
      mean_slope = mean(abs(slope))
    )] %>%
    verify(num_trs %in% cfg$rest$num_trs) %>%
    .[, num_trs := NULL] %>%
    .[, by = .(id, mask_test, session, ses_run), .(
      num_sequences = .N,
      mean_slope = mean(mean_slope)
    )] %>%
    verify(num_sequences == cfg$rest$num_seq) %>%
    .[, num_sequences := NULL] %>%
    .[, by = .(id, mask_test, session), .(
      mean_slope = mean(mean_slope)
    )] %>%
    save_data(., paths$decoding_rest_slopes_session)
  return(dt_output)
}

get_decoding_rest_slopes_session_stat <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_session) %>%
    .[, value := mean_slope]
  ttest_cfg <- list(
    formula = "value ~ session",
    adjust_method = "bonferroni",
    paired = TRUE,
    mu = 0,
    alternative = "two.sided"
  )
  dt_output <- dt_input %>%
    .[, by = .(mask_test), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg) %>%
    save_data(., paths$decoding_rest_slopes_session_stat)
  return(dt_output)
}

plot_decoding_rest_slopes_session <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_session)
  dt_stat <- load_data(paths$decoding_rest_slopes_session_stat)
  y_value <- 0.05
  p_value <- 
  figure <- ggplot(data = dt_input, aes(x = session, y = mean_slope)) +
    geom_beeswarm(dodge.width = 0.9, alpha = 0.3) +
    geom_boxplot(outlier.shape = NA, width = 0.5) +
    stat_summary(geom = "point", fun = "mean", position = position_dodge(0.9), pch = 23) +
    stat_summary(geom = "linerange", fun.data = "mean_se", position = position_dodge(0.9)) +
    xlab("Session") +
    ylab("Mean absolute slope") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, left = "both") +
    theme(axis.line.x = element_blank()) +
    theme(axis.ticks.x = element_blank())
  y_range <- layer_scales(figure)$y$range$range
  x_range <- layer_scales(figure)$x$range$range
  figure_stat <- figure + 
    geom_segment(x = 1, y = y_range[2] - 0.005, xend = 2, yend = y_range[2] - 0.005,
                 color = "gray") +
    geom_text(data = dt_stat,
              aes(x = 1.5, y = y_range[2], label = p.value_adjust_round_label),
              color = "gray", parse = FALSE, size = rel(2.5), vjust = 5)
  figure_stat
  return(figure)
}

get_decoding_rest_slopes_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, ses_run, seq_id), .(
      num_trs = .N,
      mean_slope = mean(abs(slope))
    )] %>%
    verify(num_trs %in% cfg$rest$num_trs) %>%
    .[, num_trs := NULL] %>%
    .[, by = .(id, mask_test, ses_run), .(
      num_sequences = .N,
      mean_slope = mean(mean_slope)
    )] %>%
    verify(num_sequences == cfg$rest$num_seq) %>%
    .[, num_sequences := NULL] %>%
    save_data(., paths$decoding_rest_slopes_mean)
  return(dt_output)
}

plot_decoding_rest_slopes_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_mean)
  figure <- ggplot(data = dt_input, aes(x = ses_run, y = mean_slope)) +
    # geom_beeswarm(dodge.width = 0.9, alpha = 0.3) +
    # geom_boxplot(outlier.shape = NA, width = 0.5) +
    stat_summary(geom = "point", fun = "mean", position = position_dodge(0.9), pch = 23) +
    stat_summary(geom = "linerange", fun.data = "mean_se", position = position_dodge(0.9)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Resting-state run") +
    ylab("Mean absolute slope") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both")
  return(figure)
}

get_decoding_rest_slopes_true <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes)
  dt_forward <- dt_input %>%
    .[seq_graph == 1, ]
  dt_backward <- dt_input %>%
    .[seq_graph == 1, ] %>%
    .[, seq_letters := reverse_string(seq_letters)] %>%
    .[, seq_numbers := reverse_number(seq_numbers)] %>%
    .[, slope := slope * (-1)] %>%
    .[, kendall := kendall * (-1)] %>%
    .[, pearson := pearson * (-1)]
  # combine forward and backward true sequences:
  dt_output <- rbind(dt_forward, dt_backward) %>%
    setorder(id, interval_tr, ses_run, mask_test) %>%
    .[, seq_dir := seq_direction(seq_numbers)] %>%
    save_data(., paths$decoding_rest_slopes_true)
  return(dt_output)
}

get_decoding_rest_slopes_true_time <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_true)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, ses_run, interval_tr, seq_dir), .(
      mean_slope = mean(slope)
    )] %>%
    save_data(., paths$decoding_rest_slopes_true_time)
}

plot_decoding_rest_slopes_true_time <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_true_time) %>%
    .[id == "sub-01", ]
  figure <- ggplot(data = dt_input, aes(x = interval_tr, y = mean_slope)) +
    facet_wrap(~ ses_run) +
    geom_hline(aes(yintercept = 0), color = "black") +
    geom_line(aes(color = seq_dir)) +
    ylab("Mean slope") +
    xlab("Time in TRs; 1 TR = 1.25 s)") +
    theme_zoo() +
    coord_capped_cart(expand = FALSE, bottom = "both", left = "both") +
    theme(strip.text.y = element_text(angle = 0))
  return(figure)
}

get_decoding_rest_slopes_true_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_true)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, ses_run, seq_dir), .(
      mean_slope = mean(slope)
    )] %>%
    save_data(., paths$decoding_rest_slopes_true_mean)
}

plot_decoding_rest_slopes_true_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_true_mean)
  figure <- ggplot(data = dt_input, aes(x = ses_run, y = mean_slope)) +
    facet_wrap(~ mask_test, scales = "free_y") +
    geom_hline(aes(yintercept = 0), color = "black") +
    geom_boxplot(aes(fill = seq_dir), outlier.shape = NA) +
    ylab("Mean slope") +
    xlab("Time in TRs; 1 TR = 1.25 s)") +
    theme_zoo() +
    coord_capped_cart(expand = FALSE, bottom = "both", left = "both")
    return(figure)
}

get_decoding_rest_slopes_max <- function(cfg, paths) {
  # we order the slopes
  dt_input <- load_data(paths$decoding_rest_slopes)
  dt_output <- dt_input %>%
  .[, by = .(id, mask_test, ses_run, interval_tr), ":="(
    slope_order = order(abs(slope)),
    num_sequences = .N
  )] %>%
  verify(.[, by = .(id, mask_test, ses_run, interval_tr), .(
    num_orders = length(unique(slope_order)))]$num_orders == 360) %>%
  verify(num_sequences == 360) %>%
  .[, num_sequences := NULL] %>%
    save_data(., paths$decoding_rest_slopes_max)
  return(dt_output)
}

plot_decoding_rest_slopes_max <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_max) %>%
    .[slope_order == 1, ] %>%
    .[id == "sub-01", ]
  figure <- ggplot(data = dt_input, aes(x = interval_tr, y = slope)) +
    facet_wrap(~ ses_run) +
    geom_hline(aes(yintercept = 0), color = "black") +
    geom_line() +
    ylab("Mean slope") +
    xlab("Time in TRs; 1 TR = 1.25 s)") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both")
  return(figure)
}

get_decoding_rest_slopes_max_diff <- function(cfg, paths) {
  # Check the mean sequence diff of those with the highest slope
  dt_input <- load_data(paths$decoding_rest_slopes_max)
  dt_output <- dt_input %>%
    .[slope_order == 1, ] %>%
    setorder(id, ses_run, interval_tr) %>%
    .[, by = .(id), ":="(
      run_index = rleid(ses_run)
    )] %>%
    verify(run_index %in% seq(1, 8)) %>%
    verify(.[, by = .(ses_run), .(num_indices = length(unique(run_index)))]$num_indices == 1) %>%
    .[, by = .(id, mask_test, ses_run), ":="(
      num_trs = .N
    )] %>%
    verify(num_trs %in% c(233, 137)) %>%
    .[, by = .(id, mask_test, ses_run, seq_diff_mean), .(
      seq_diff_mean_prop = .N/unique(num_trs) * 100
    )] %>%
    save_data(., paths$decoding_rest_slopes_max_diff)
  return(dt_output)
}

plot_decoding_rest_slopes_max_diff <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_max_diff)
  figure <- ggplot(data = dt_input, aes(x = ses_run, y = seq_diff_mean_prop)) +
    geom_boxplot(aes(group = interaction(ses_run, as.factor(seq_diff_mean)),
                     fill = as.factor(seq_diff_mean)),  
                 position = position_dodge2(0.9),
                 outlier.shape = NA) +
    ylab("Proportion of TRs (in %)") +
    xlab("Resting-state run") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(figure)
}

get_decoding_rest_slopes_mean_diff <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, ses_run, seq_diff_mean), .(
      mean_slope = mean(abs(slope))
    )] %>%
    .[, by = .(id, mask_test, ses_run, seq_diff_mean), .(
      mean_slope = mean(mean_slope)
    )] %>%
    .[, by = .(mask_test, ses_run, seq_diff_mean), .(
      mean_slope = mean(mean_slope),
      num_subs = .N,
      sem_upper = (mean(mean_slope) + (sd(mean_slope)/sqrt(.N))),
      sem_lower = (mean(mean_slope) - (sd(mean_slope)/sqrt(.N)))
    )] %>%
    save_data(., paths$decoding_rest_slopes_mean_diff)
}

get_decoding_rest_slopes_mean_diff <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_mean_diff)
  figure <-  ggplot(data = dt_input, aes(x = ses_run, y = mean_slope)) +
    geom_ribbon(aes(fill = seq_diff_mean, group = seq_diff_mean,
                    ymin = sem_lower, ymax = sem_upper), alpha = 0.1,
                position = position_dodge(0.9)) +
    geom_line(aes(color = seq_diff_mean, group = seq_diff_mean),
              position = position_dodge(0.9)) +
    geom_point(aes(color = seq_diff_mean, group = seq_diff_mean),
               position = position_dodge(0.9)) +
    # geom_errorbar(aes(color = seq_diff_mean, group = seq_diff_mean,
    #                 ymin = sem_lower, ymax = sem_upper), width = 0,
    #               position = position_dodge(0.9), alpha = 0.1) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Mean absolute slope") +
    xlab("Resting state run") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both")
  return(figure)
}
