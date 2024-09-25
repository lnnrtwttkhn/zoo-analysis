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
      mean_std_prob = mean(std_prob),
      mean_std_prob_norm = mean(std_prob_norm)
    )] %>%
    verify(num_trs %in% cfg$rest$num_trs) %>%
    .[, ses_run := paste(session, run, sep = "_")] %>%
    save_data(., paths$decoding_rest_std)
}

plot_decoding_rest_std <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_std)
  figure <- ggplot(dt_input, aes(x = ses_run, y = mean_std_prob)) +
    # geom_beeswarm(dodge.width = 0.9, alpha = 0.3) +
    # geom_boxplot(outlier.shape = NA, width = 0.5) +
    stat_summary(geom = "point", fun = "mean", pch = 23) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(geom = "linerange", fun.data = "mean_se") +
    facet_wrap(~ mask) +
    ylab("SD of probability") +
    xlab("Resting-state run") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  save_figure(figure, filename = "decoding-rest-std", width = 5, height = 4)
  return(figure)
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
  save_figure(figure, filename = "decoding-rest-slopes-mean", width = 5, height = 4)
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

get_decoding_rest_freq_spec <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes)
  num_trs_session1 <- max(cfg$rest$num_trs)
  num_trs_session2 <- min(cfg$rest$num_trs)
  smoothing_kernel <- cfg$rest$freq_smoothing_kernel
  set.seed(2937)
  dt_output <- dt_input %>%
    # take a random subsequence of the session 1 resting-state data
    # this subsequence has the same number of TRs as the session 2 resting state data
    # this is done to ensure that the frequency spectra are comparable
    .[!(ses_run == "ses-01_run-1" & !(interval_tr %in% random_subsequence(seq(num_trs_session1), num_trs_session2)))] %>%
    .[!(ses_run == "ses-01_run-2" & !(interval_tr %in% random_subsequence(seq(num_trs_session1), num_trs_session2)))] %>%
    .[, by = .(id, mask_test, ses_run, seq_id), {
      frequency_spectrum = lomb::lsp(
        x = slope,
        times = seq(1,.N),
        from = 0,
        to = 0.3,
        type = "frequency",
        plot = FALSE
      )
      list(
        num_trs = .N,
        freq_bins = frequency_spectrum$scanned,
        filter_width = round(smoothing_kernel / mean(diff(frequency_spectrum$scanned))),
        power = frequency_spectrum$power
      )}] %>%
    verify(all(num_trs %in% cfg$rest$num_trs)) %>%
    verify(.[, by = .(id, mask_test, ses_run, seq_id), .(
      num_filters = length(unique(filter_width))
    )]$num_filters == 1) %>%
    verify(.[, by = .(id, mask_test, ses_run), .(
      num_sequences = length(unique(seq_id))
    )]$num_sequences == 360) %>%
    # smooth the frequency spectra and frequency bins:
    .[, by = .(id, mask_test, ses_run, seq_id), ":="(
      power_smooth = stats::filter(x = power, filter = rep(1/unique(filter_width), unique(filter_width))),
      freq_bins_smooth = stats::filter(x = freq_bins, filter = rep(1/unique(filter_width), unique(filter_width)))
    )] %>%
    save_data(., paths$decoding_rest_freq_spec)
}

get_decoding_rest_freq_spec_power <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_freq_spec)
  baseline_run <- cfg$rest$freq_baseline_run
  dt_output <- dt_input %>%
    .[!is.na(power_smooth), ] %>%
    .[, power_smooth := as.numeric(power_smooth)] %>%
    # normalize the frequencies
    .[, by = .(id, mask_test, ses_run, seq_id), ":="(
      power_smooth_norm = as.numeric(power_smooth) / sum(as.numeric(power_smooth))
    )] %>%
    setorder(., id, mask_test, seq_id, ses_run) %>%
    # temporally add the power of ses-01 run-01 to the data.table:
    .[, by = .(id, mask_test, seq_id), ":="(
      num_ses_run = length(unique(ses_run)),
      power_smooth_baseline = rep(power_smooth[ses_run == baseline_run], length(unique(ses_run))),
      power_smooth_norm_baseline = rep(power_smooth_norm[ses_run == baseline_run], length(unique(ses_run))),
      power_smooth_norm_baseline1 = as.numeric(rep(power_smooth_norm[ses_run == "ses-01_run-1"], length(unique(ses_run)))),
      power_smooth_norm_baseline2 = as.numeric(rep(power_smooth_norm[ses_run == "ses-01_run-2"], length(unique(ses_run)))),
      power_smooth_norm_baseline3 = as.numeric(rep(power_smooth_norm[ses_run == "ses-02_run-1"], length(unique(ses_run))))
    )] %>%
    .[, by = .(id, mask_test, seq_id),
       power_smooth_norm_baseline_mean := rowMeans(.SD), .SDcols = c("power_smooth_norm_baseline1", "power_smooth_norm_baseline2", "power_smooth_norm_baseline3")
    ] %>%
    verify(num_ses_run == 8) %>%
    .[, num_ses_run := NULL] %>%
    # power relative to the baseline run data:
    .[, by = .(id, mask_test, ses_run, seq_id), ":="(
      power_smooth_rel = as.numeric(power_smooth - power_smooth_baseline),
      power_smooth_norm_rel = as.numeric(power_smooth_norm - power_smooth_norm_baseline),
      power_smooth_norm_rel_mean = as.numeric(power_smooth_norm - power_smooth_norm_baseline_mean)
    )] %>%
    .[, ses_run := as.factor(ses_run)] %>%
    .[, freq_bins_smooth := as.numeric(freq_bins_smooth)] %>%
    .[, condition := ifelse(ses_run %in% c("ses-01_run-1", "ses-01_run-2", "ses-02_run-1"), "Single", "Sequence")] %>%
    save_data(., paths$decoding_rest_freq_spec_power)
}

get_decoding_rest_freq_spec_power_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_freq_spec_power)
  dt_output <- dt_input %>%
    # average across all sequences:
    .[, by = .(id, mask_test, ses_run, freq_bins_smooth), .(
      num_sequences = .N,
      power_smooth = as.numeric(mean(power_smooth)),
      power_smooth_norm = as.numeric(mean(power_smooth_norm)),
      power_smooth_rel = as.numeric(mean(power_smooth_rel)),
      power_smooth_norm_rel = as.numeric(mean(power_smooth_norm_rel)),
      power_smooth_norm_rel_mean = as.numeric(mean(power_smooth_norm_rel_mean))
    )] %>%
    verify(num_sequences == 360) %>%
    save_data(., paths$decoding_rest_freq_spec_power_mean)
}

get_decoding_rest_freq_spec_power_mean_cond <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_freq_spec_power)
  dt_output <- dt_input %>%
    # average across all sequences:
    .[, by = .(id, mask_test, condition, freq_bins_smooth), .(
      num_sequences = .N,
      power_smooth = as.numeric(mean(power_smooth)),
      power_smooth_norm = as.numeric(mean(power_smooth_norm)),
      power_smooth_rel = as.numeric(mean(power_smooth_rel)),
      power_smooth_norm_rel = as.numeric(mean(power_smooth_norm_rel)),
      power_smooth_norm_rel_mean = as.numeric(mean(power_smooth_norm_rel_mean))
    )] %>%
    # verify(num_sequences == 360) %>%
    # .[, ses_run := as.factor(ses_run)] %>%
    .[, freq_bins_smooth := as.numeric(freq_bins_smooth)] %>%
    save_data(., paths$decoding_rest_freq_spec_power_mean_cond)
}

get_decoding_rest_freq_expect <- function(cfg, paths) {
  calc_delta <- function(speed_s) {
    # input speed_s: sequence speed (ISI), in seconds
    fitfreq <- 1 / 5.26
    stim_dur <- 0.0001
    num_seq_stim <- 6
    delta <- fitfreq / (1 + fitfreq * (num_seq_stim * stim_dur + (num_seq_stim - 1) * speed_s))
    return(delta)
  }
  # create a data table with the expected frequencies:
  frequency_expectation <- data.table(xintercept = c(
    calc_delta(0.032), calc_delta(2.048)),
    tITI = c("32 ms", "2048 ms")) %>%
    save_data(., paths$decoding_rest_freq_expect)
}

plot_decoding_rest_freq_spec_power_baseline <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_freq_spec_power_mean) %>%
    .[ses_run == "ses-01_run-1", ]
  figure <- ggplot(data = dt_input, aes(y = power_smooth_norm, x = freq_bins_smooth)) +
    geom_vline(data = frequency_expectation, aes(xintercept = xintercept),
               linetype = "dashed") +
    stat_summary(geom = "ribbon", fun.data = "mean_se", aes(fill = ses_run),
                 alpha = 0.3, color = NA) +
    stat_summary(geom = "line", fun = "mean", aes(color = ses_run)) +
    facet_wrap(~ mask_test) +
    xlab("Frequency") +
    ylab("Power") +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, xlim = c(0, 0.3)) +
    scale_x_continuous(labels = label_fill(seq(0, 0.3, by = 0.05), mod = 1),
                       breaks = seq(0, 0.3, by = 0.05)) +
    theme(panel.border = element_blank()) + 
    theme(axis.line = element_line(colour = "black")) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(panel.border = element_blank())
  return(figure)
}

plot_decoding_rest_freq_spec_power_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_freq_spec_power_mean)
  frequency_expectation <- load_data(paths$decoding_rest_freq_expect)
  figure <- ggplot(data = dt_input, aes(y = power_smooth_norm_rel_mean, x = freq_bins_smooth)) +
    geom_vline(data = frequency_expectation, aes(xintercept = xintercept), linetype = "dashed") +
    geom_text(data = frequency_expectation, aes(
      x = xintercept, y = Inf, label = paste(round(xintercept, 2))), hjust = -0.3, vjust = 2) +
    geom_vline(data = frequency_expectation, aes(xintercept = xintercept),
               linetype = "dashed") +
    stat_summary(geom = "ribbon", fun.data = "mean_se", aes(fill = ses_run),
                 alpha = 0.3, color = NA) +
    stat_summary(geom = "line", fun = "mean", aes(color = ses_run)) +
    facet_wrap(~ mask_test) +
    xlab("Frequency") +
    ylab("Power") +
    theme_zoo() +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, xlim = c(0, 0.3)) +
    scale_x_continuous(labels = label_fill(seq(0, 0.3, by = 0.05), mod = 1),
                       breaks = seq(0, 0.3, by = 0.05)) +
    scale_color_viridis_d(name = "Resting-state run") +
    scale_fill_viridis_d(name = "Resting-state run") +
    guides(color = guide_legend(ncol = 3, byrow = TRUE)) +
    theme(legend.title = element_text(hjust = 0.5)) +
    theme(legend.position = "bottom") +
    theme(legend.title.position = "top") +
    theme(legend.justification = "center") +
    theme(legend.margin = margin(0, 0, 0, 0))
  save_figure(figure, filename = "decoding-rest-freq-spec-power-mean", width = 5, height = 5)
  return(figure)
}

plot_decoding_rest_freq_spec_power_mean_cond <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_freq_spec_power_mean_cond)
  frequency_expectation <- load_data(paths$decoding_rest_freq_expect)
  figure <- ggplot(data = dt_input, aes(y = power_smooth_norm_rel_mean, x = freq_bins_smooth)) +
    geom_vline(data = frequency_expectation, aes(xintercept = xintercept), linetype = "dashed") +
    geom_text(data = frequency_expectation, aes(
      x = xintercept, y = Inf, label = paste(round(xintercept, 2))), hjust = -0.3, vjust = 2) +
    stat_summary(geom = "ribbon", fun.data = "mean_se", aes(fill = condition),
                 alpha = 0.3, color = NA) +
    stat_summary(geom = "line", fun = "mean", aes(color = condition)) +
    facet_wrap(~ mask_test) +
    xlab("Frequency") +
    ylab("Relative power") +
    theme_zoo() +
    scale_color_manual(values = c("red", "blue"), name = "Condition") +
    scale_fill_manual(values = c("red", "blue"), name = "Condition") +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, xlim = c(0, 0.3)) +
    scale_x_continuous(labels = label_fill(seq(0, 0.3, by = 0.05), mod = 1),
                       breaks = seq(0, 0.3, by = 0.05)) +
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.justification = "center") +
    theme(legend.margin = margin(0, 0, 0, 0))
  save_figure(figure, filename = "decoding-rest-freq-spec-power-mean-cond-rel", width = 3.5, height = 4)
  return(figure)
}

get_decoding_rest_freq_spec_power_expect <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_freq_spec_power)
  frequency_expectation <- load_data(paths$decoding_rest_freq_expect)
  fast_freq <- frequency_expectation %>%
    .[tITI == "32 ms"] %>% .$xintercept
  slow_freq <- frequency_expectation %>%
    .[tITI == "2048 ms"] %>% .$xintercept
  dt_output <- dt_input %>%
    .[, ses_run := as.factor(ses_run)] %>%
    .[, freq_bins_smooth := as.numeric(freq_bins_smooth)] %>%
    .[, by = .(id, mask_test, ses_run, seq_id), .(
      power_index_fast = power_smooth_norm[which.min(abs(freq_bins_smooth - fast_freq))],
      power_index_slow = power_smooth_norm[which.min(abs(freq_bins_smooth - slow_freq))]
    )] %>%
    # melt all index variables into one column:
    gather(grep("power", names(.), fixed = TRUE), key = "index", value = "power") %>%
    setDT(.) %>%
    .[grepl("index_fast", index), label := paste0("Fast (", round(fast_freq, 2), ")")] %>%
    .[grepl("index_slow", index), label := paste0("Slow (", round(slow_freq, 2), ")")] %>%
    # average across sequences for seen and unseen sequences:
    .[, by = .(id, mask_test, ses_run, index, label), .(
      num_seqs = .N,
      power = mean(power)
    )] %>%
    verify(num_seqs %in% c(360)) %>%
    setorder(., id, mask_test, ses_run) %>%
    save_data(., paths$decoding_rest_freq_spec_power_expect)
}

get_decoding_rest_freq_spec_power_expect_cond <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_freq_spec_power)
  frequency_expectation <- load_data(paths$decoding_rest_freq_expect)
  fast_freq <- frequency_expectation %>%
    .[tITI == "32 ms"] %>% .$xintercept
  slow_freq <- frequency_expectation %>%
    .[tITI == "2048 ms"] %>% .$xintercept
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, condition, seq_id), .(
      power_index_fast = power_smooth_norm[which.min(abs(freq_bins_smooth - fast_freq))],
      power_index_slow = power_smooth_norm[which.min(abs(freq_bins_smooth - slow_freq))]
    )] %>%
    # melt all index variables into one column:
    gather(grep("power", names(.), fixed = TRUE), key = "index", value = "power") %>%
    setDT(.) %>%
    .[grepl("index_fast", index), label := paste0("Fast (", round(fast_freq, 2), ")")] %>%
    .[grepl("index_slow", index), label := paste0("Slow (", round(slow_freq, 2), ")")] %>%
    # average across sequences for seen and unseen sequences:
    .[, by = .(id, mask_test, condition, index, label), .(
      num_seqs = .N,
      power = mean(power)
    )] %>%
    verify(num_seqs %in% c(360)) %>%
    save_data(., paths$decoding_rest_freq_spec_power_expect_cond)
}

get_decoding_rest_freq_spec_power_expect_cond_stat <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_freq_spec_power_expect_cond)
  ttest_cfg <- list(
    formula = "value ~ condition",
    adjust_method = "bonferroni",
    paired = TRUE,
    mu = 0,
    alternative = "two.sided"
  )
  dt_output <- dt_input %>%
    .[, value := power] %>%
    .[, by = .(label), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg) %>%
    save_data(., paths$decoding_rest_freq_spec_power_expect_cond_stat)
}

plot_decoding_rest_freq_spec_power_expect <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_freq_spec_power_expect)
  # dt_input_stat <- load_data(paths$decoding_rest_freq_spec_power_expect_cond_stat)
  figure <- ggplot(data = dt_input, aes(y = power, x = fct_rev(label))) +
    geom_beeswarm(aes(fill = ses_run), alpha = 0.3, dodge.width = 0.9, pch = 21, color = "black") +
    geom_boxplot(aes(fill = ses_run), outlier.shape = NA, width = 0.5, position = position_dodge(0.9), color = "black") +
    stat_summary(aes(fill = ses_run), geom = "point", fun = "mean", pch = 23, position = position_dodge(0.9), color = "black") +
    stat_summary(aes(group = ses_run), geom = "linerange", fun.data = "mean_se", position = position_dodge(0.9), color = "black") +
    # geom_label(data = dt_input_stat, aes(y = 0.06, label = paste("p", p.value_adjust_round_label)),
    #            color = "gray", parse = FALSE, size = rel(2.5)) +
    facet_wrap(~ mask_test) +
    xlab("Predicted frequency") +
    ylab("Power") +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, ylim = c(0, 0.06)) +
    theme_zoo() +
    theme(axis.ticks.x = element_line(color = "white")) +
    theme(axis.line.x = element_line(color = "white")) +
    scale_color_viridis_d(name = "Resting-state run") +
    scale_fill_viridis_d(name = "Resting-state run")
  save_figure(figure, filename = "decoding-rest-freq-spec-power-expect", width = 6, height = 4)
  return(figure)
}

plot_decoding_rest_freq_spec_power_expect_cond <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_freq_spec_power_expect_cond)
  dt_input_stat <- load_data(paths$decoding_rest_freq_spec_power_expect_cond_stat)
  figure <- ggplot(data = dt_input, aes(y = power, x = fct_rev(label))) +
    geom_boxplot(aes(fill = condition), outlier.shape = NA, width = 0.5, position = position_dodge(0.9), color = "black", alpha = 0.3) +
    geom_beeswarm(aes(fill = condition), alpha = 0.3, dodge.width = 0.9, pch = 21, color = "black") +
    stat_summary(aes(fill = condition), geom = "point", fun = "mean", pch = 23, position = position_dodge(0.9), color = "black") +
    stat_summary(aes(group = condition), geom = "linerange", fun.data = "mean_se", position = position_dodge(0.9), color = "black") +
    geom_label(data = dt_input_stat, aes(y = 0.06, label = paste("p", p.value_adjust_round_label)),
              color = "gray", parse = FALSE, size = rel(2.5)) +
    facet_wrap(~ mask_test) +
    xlab("Predicted frequency") +
    ylab("Power") +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE, ylim = c(0, 0.06)) +
    theme_zoo() +
    theme(axis.ticks.x = element_line(color = "white")) +
    theme(axis.line.x = element_line(color = "white")) +
    scale_color_manual(values = c("red", "blue"), name = "Condition") +
    scale_fill_manual(values = c("red", "blue"), name = "Condition") +
    theme(legend.position = "bottom") +
    theme(legend.direction = "horizontal") +
    theme(legend.justification = "center") +
    theme(legend.margin = margin(0, 0, 0, 0)) +
    theme(legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0))
  save_figure(figure, filename = "decoding-rest-freq-spec-power-expect-cond", width = 3.5, height = 4)
  return(figure)
}

get_decoding_rest_slopes_sr <- function(cfg, paths) {
  dt_rest <- load_data(paths$decoding_rest) %>%
    .[session == "ses-02", ] %>%
    .[mask == "vis", ] %>%
    .[, run := dplyr::case_when(
      run == "run-1" ~ "run-09",
      run == "run-2" ~ "run-01",
      run == "run-3" ~ "run-02",
      run == "run-4" ~ "run-03",
      run == "run-5" ~ "run-04",
      run == "run-6" ~ "run-05"
    )] %>%
    .[, run := factor(as.factor(run), levels = c(
      "run-09", "run-01", "run-02", "run-03", "run-04", "run-05"))]
  dt_sr_mat_rest <- load_data(paths$behav_sr_mat_rest)
  grouping_variables = c("id", "classification", "mask_test", "train_set", "session", "run", "node_previous", "trial_index", "interval_tr")
  dt_output <- dt_rest %>%
    merge.data.table(x = ., y = dt_sr_mat_rest,
                     by.x = c("id", "run", "node"), by.y = c("id", "run", "current")) %>%
    .[, by = grouping_variables, .(
      slope = coef(lm(probability ~ mean_sr_prob))[2] * (-1),
      slope_rank = coef(lm(probability ~ rank(mean_sr_prob)))[2] * (-1),
      slope_norm = coef(lm(probability_norm ~ mean_sr_prob))[2] * (-1),
      kendall = cor.test(mean_sr_prob, probability, method = "kendall")$estimate * (-1),
      kendall_norm = cor.test(mean_sr_prob, probability_norm, method = "kendall")$estimate * (-1),
      pearson = cor.test(mean_sr_prob, probability, method = "pearson")$estimate * (-1),
      pearson_norm = cor.test(mean_sr_prob, probability_norm, method = "pearson")$estimate * (-1),
      sd_sr_prob = sd(mean_sr_prob),
      sum_sr_prob = sum(mean_sr_prob),
      # seq_numbers = paste(x, collapse = ""),
      # seq_letters = paste(node[x], collapse = ""),
      # seq_start = node[x][1],
      # seq_diff_mean = mean(diff(x)),
      # seq_diff_sd = sd(diff(x)),
      seq_length = .N
      # seq_graph = as.numeric(all(abs(diff(x)) %in% c(1, 5)))
    )] %>%
    # verify that there are six values per slope (one for each node):
    verify(seq_length == cfg$num_nodes) %>%
    # verify that the SR probabilities sum to 1 (with tolerance):
    verify(near(1, sum_sr_prob, tol = 1e-15)) %>%
    save_data(paths$decoding_rest_slopes_sr)
}

get_decoding_rest_sd_sr_prob <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_sr)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, run), .(
      num_trs = .N,
      mean_sd_sr_prob = mean(sd_sr_prob)
    )] %>%
    verify(num_trs %in% cfg$rest$num_trs) %>%
    .[, num_trs := NULL] %>%
    save_data(paths$decoding_sd_sr_prob)
}

plot_decoding_rest_sd_sr_prob <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_sd_sr_prob)
  figure <- ggplot(data = dt_input, aes(x = run, y = mean_sd_sr_prob)) +
    # geom_beeswarm(dodge.width = 0.9, alpha = 0.3) +
    # geom_boxplot(outlier.shape = NA, width = 0.5) +
    stat_summary(geom = "point", fun = "mean", position = position_dodge(0.9), pch = 23) +
    stat_summary(geom = "linerange", fun.data = "mean_se", position = position_dodge(0.9)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Resting-state run (in Session 02)") +
    ylab("Mean SD SR Probability") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both")
  save_figure(figure, filename = "decoding-rest-sd-sr-mean", width = 5, height = 4)
  return(figure)
}

get_decoding_rest_slopes_sr_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_sr)
  dt_sr_params <- load_data(paths$behav_sr_params) %>%
    .[model_name == "Full", ] %>%
    .[mod == "model", ] %>%
    .[iter == 1, ] %>%
    .[variable %in% c("alpha", "gamma"), ] %>%
    .[, c("id", "variable", "value")] %>%
    pivot_wider(id_cols = c("id"), names_from = "variable")
  dt_output <- dt_input %>%
    merge.data.table(x = ., y = dt_sr_params, by = c("id")) %>%
    .[, by = .(id, mask_test, run, alpha, gamma), .(
      num_trs = .N,
      mean_slope = mean(abs(slope)),
      mean_slope_rank = mean(abs(slope_rank))
    )] %>%
    verify(num_trs %in% cfg$rest$num_trs) %>%
    .[, num_trs := NULL] %>%
    save_data(paths$decoding_rest_slopes_sr_mean)
}

get_decoding_rest_slopes_sr_gamma_corr <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_sr_mean)
  dt_output <- dt_input %>%
    .[, by = .(mask_test, run), .(
      num_subs = .N,
      cor = list(broom::tidy(cor.test(mean_slope, gamma, method = "pearson")))
    )] %>%
    unnest(cor) %>%
    setDT(.) %>%
    .[, label := sprintf("r = %.2f, p = %.2f", estimate, p.value)] %>%
    save_data(paths$decoding_rest_slopes_sr_gamma_corr)
}

plot_decoding_rest_slopes_sr_gamma_corr <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_sr_mean)
  dt_input_stat <- load_data(paths$decoding_rest_slopes_sr_gamma_corr)
  figure <- ggplot(data = dt_input, aes(x = gamma, y = mean_slope)) +
    facet_wrap(~ run) +
    geom_point(color = "darkgray") +
    geom_smooth(method = "lm", formula = y~x, color = "black") +
    geom_text(data = dt_input_stat, aes(label = label), y = 90, x = 0.5) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(expression(gamma)) +
    ylab("Mean absolute slope") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both")
  figure
  return(figure)
}

plot_decoding_rest_slopes_sr_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_sr_mean)
  figure <- ggplot(data = dt_input, aes(x = run, y = mean_slope)) +
  # figure <- ggplot(data = dt_input, aes(x = run, y = mean_slope_rank)) +
    # geom_beeswarm(dodge.width = 0.9, alpha = 0.3) +
    # geom_boxplot(outlier.shape = NA, width = 0.5) +
    stat_summary(geom = "point", fun = "mean", position = position_dodge(0.9), pch = 23) +
    stat_summary(geom = "linerange", fun.data = "mean_se", position = position_dodge(0.9)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Resting-state run (in Session 02)") +
    ylab("Mean absolute slope") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both")
  save_figure(figure, filename = "decoding-rest-slopes-sr-mean", width = 5, height = 4)
  return(figure)
}

get_decoding_rest_slopes_sr_mean_phase <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_sr)
  dt_output <- dt_input %>%
    .[, phase := dplyr::case_when(
      run == "run-09" ~ "Pre Seq",
      run %in% c("run-01", "run-02") ~ "Seq 1",
      run %in% c("run-03", "run-04", "run-05", "run-06") ~ "Seq 2",
    )] %>%
    .[, run := factor(as.factor(run), levels = c(
      "Pre Seq", "Seq 1", "Seq 2"))] %>%
    .[, by = .(id, mask_test, phase), .(
      num_trs = .N,
      mean_slope = mean(abs(slope))
    )] %>%
    save_data(paths$decoding_rest_slopes_sr_mean_phase)
}

plot_decoding_rest_slopes_sr_mean_phase <- function(cfg, paths) {
  dt_input <- load_data(paths$decoding_rest_slopes_sr_mean_phase)
  figure <- ggplot(data = dt_input, aes(x = phase, y = mean_slope)) +
    geom_beeswarm(dodge.width = 0.9, alpha = 0.3) +
    geom_boxplot(outlier.shape = NA, width = 0.5) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(geom = "point", fun = "mean", pch = 23) +
    stat_summary(geom = "linerange", fun.data = "mean_se") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Resting-state phase (in Session 02)") +
    ylab("Mean absolute slope") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both")
  return(figure)
}

plot_decoding_rest_surprise_timecourse <- function(cfg, paths) {
  dt_input <- load_data(paths$behav_sr_mat)
  set.seed(498)
  random_id <- sample(unique(dt_input$id), 3)
  random_id <- c("sub-04", "sub-42")
  dt_output <- dt_input %>%
    .[id %in% random_id, ] %>%
    distinct(., pick(contains(c("id", "session", "run", "condition", "trial_run", "node", "bits")))) %>%
    setDT(.) %>%
    .[, trial_study := .GRP, by = .(session, condition, run, trial_run)] %>%
    .[, condition_session_run := sprintf("%s (%s %s)", condition, session, run)] %>%
    .[, by = .(id, condition, run), min_trial_run := unique(trial_study[trial_study == min(trial_study)])] %>%
    .[, by = .(id, condition, run), max_trial_run := unique(trial_study[trial_study == max(trial_study)])] %>%
    .[condition == "Sequence" & run == "run-03", by = .(id, condition, run), graph_change := median(c(unique(min_trial_run), unique(max_trial_run)))]
  figure <- ggplot(data = dt_output, aes(x = trial_study, y = bits)) +
    facet_grid(vars(id), vars(session), scales = "free") +
    geom_vline(aes(xintercept = min_trial_run, color = condition), linewidth = 0.75) +
    scale_color_viridis_d(option = "viridis", name = "Task Condition") +
    new_scale_color() +
    geom_vline(aes(xintercept = graph_change, color = "Graph Change"), linewidth = 1) +
    scale_color_manual(values = "red", name = "") +
    # geom_rect(data = dt_rect, aes(
    #   xmin = min_trial_run, xmax = max_trial_run,
    #   ymin = -Inf, ymax = Inf, fill = condition),
    #   linewidth = 0.75, color = "black", alpha = 0.1) +
    # geoms below will use another color scale
    new_scale_color() +
    geom_line(aes(color = node)) +
    scale_color_viridis_d(option = "C", name = "Stimuli") +
    # geom_smooth(method = "lm", formula = y~x, color = "black") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Time (in Trials)") +
    ylab("SR-based Shannon surprise") +
    theme_zoo() +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both")
  save_figure(figure, filename = "decoding-rest-rest-surprise-timecourse", width = 8, height = 5)
  return(figure)
}
