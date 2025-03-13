get_decoding_main_sine <- function(cfg, paths) {
  # convolve the sine wave with the stimulus onsets
  dt_sine <- load_data(paths$source$decoding_single_interval_sine_fit)
  dt_main <- load_data(paths$source$decoding_main)
  max_num_stim <- cfg$decoding_sequence$max_stim_evoked
  dt_main_sine <- dt_main %>%
    .[, c("id", "run", "trial_run", "mask_test", "seq_total", "seq_total_onsets")] %>%
    unique(.) %>%
    .[, by = .(id, run, trial_run, mask_test), .(
      node = unlist(strsplit(seq_total, "-")),
      onset = unlist(strsplit(seq_total_onsets, "-"))
    )] %>%
    .[, node := ifelse(node == "NA", NA, as.character(node))] %>%
    .[, onset := as.numeric(ifelse(onset == "NA", NA, onset))] %>%
    .[, by = .(id, run, trial_run, mask_test), stim_num := seq_len(.N)] %>%
    .[, by = .(id, onset, mask_test, run, trial_run, node),
      interval_location := ifelse(stim_num > cfg$num_next, "after", "before")] %>%
    verify(.[, by = .(id, run, trial_run, mask_test), .(max_counter = max(stim_num))]$max_counter == max_num_stim) %>%
    # add fitted parameters of the sine modeling:
    merge.data.table(x = ., y = dt_sine, by = c("id", "mask_test", "node")) %>%
    setorder(., id, mask_test, run, trial_run, stim_num) %>%
    .[, params := lapply(transpose(.SD), c), .SDcols = cfg$sine_params$names] %>%
    # fit sine waves to sequence items:
    .[, by = .(id, run, trial_run, mask_test, node, stim_num, onset, interval_location), .(
      probability_sine = sine_truncated(params = unlist(params), cfg$sine_params$time_eval)
    )] %>%
    verify(!(is.na(probability_sine))) %>%
    # calculate onsets for sine wave from onset of current node:
    .[, by = .(id, mask_test, run, trial_run, node, stim_num, onset, interval_location),
      onset := onset + cfg$sine_params$time_eval
    ] %>%
    # get onsets in TR:
    .[, by = .(id, onset, mask_test, run, trial_run, node), ":="(
      onset_tr = onset / cfg$tr,
      onset_tr_round = floor(onset / cfg$tr) + 1
    )] %>%
    save_data(paths$source$decoding_main_sine)
}

get_decoding_main_sine_mean <- function(cfg, paths) {
  # calculate the mean activation from the convolving
  dt_input <- load_data(paths$source$decoding_main_sine)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, run, node, stim_num, onset_tr_round), .(
      probability_sine = mean(probability_sine)
    )] %>%
    .[, by = .(id, mask_test, run, node, onset_tr_round), .(
      probability_sine_mean = mean(probability_sine),
      probability_sine_sum = sum(probability_sine)
    )] %>%
    setorder(., id, mask_test, run, node, onset_tr_round) %>%
    save_data(paths$source$decoding_main_sine_mean)
}

get_decoding_main_modeled <- function(cfg, paths) {
  # create dataframe that combines main data and stim modeled data
  dt_main <- load_data(paths$source$decoding_main)
  dt_main_sine_mean <- load_data(paths$source$decoding_main_sine_mean)
  column_names <- c(
    "id", "mask_test", "run", "trial_index", "trial_index_run", "trial_run", "node_classifier", "node",
    "node_dist_time", "node_dist_trial", "class_dist_trial", "class_dist_time", "graph", "prob_uni", "prob_bi",
    "dist_uni", "dist_bi", "prob_graph", "dist_graph", "interval_tr",
    "onset_interval", "onset", "probability", "probability_norm", "probability_sine_sum")
  dt_main_modeled <- dt_main %>%
    merge.data.table(x = ., y = dt_main_sine_mean,
                     by.x = c("id", "mask_test", "run", "node_classifier", "onset_interval_tr"),
                     by.y = c("id", "mask_test", "run", "node", "onset_tr_round"),
                     all.x = TRUE) %>%
    .[, ..column_names] %>%
    setorder(., id, mask_test, run, trial_run, trial_index, trial_index_run, node_classifier, node, interval_tr, onset_interval, onset) %>%
    .[, probability_modeled := as.numeric(ifelse(is.na(probability_sine_sum), 0, probability_sine_sum))] %>%
    verify(!(is.na(probability_modeled))) %>%
    .[, probability_sine_sum := NULL] %>%
    # .[, dist_uni := ifelse(is.na(dist_uni), 0, dist_uni)] %>%
    # .[, dist_bi := ifelse(is.na(dist_bi), 0, dist_bi)] %>%
    # .[, dist_bi := ifelse(dist_bi == 1 & dist_uni == 5, -1, dist_bi)] %>%
    # .[, dist_bi := ifelse(dist_bi == 2 & dist_uni == 4, -2, dist_bi)] %>%
    .[, dist_combined := as.factor(paste0(dist_uni, " | ", dist_bi))] %>%
    .[, phase := ifelse(interval_tr %in% seq(1, 4), "TRs 1 - 4\n(early)", "TRs 5 - 8\n(late)")] %>%
    .[, phase := as.factor(phase)] %>%
    # .[, dummy := 1] %>%
    # pivot_wider(names_from = dist_graph, values_from = dummy, values_fill = 0, names_prefix = "dist_") %>%
    save_data(paths$source$decoding_main_stim_modeled)
}

get_decoding_main_model_input <- function(cfg, paths) {
  col_names_x <- c("id", "run", "trial_run", "graph", "node", "node_classifier",
                   "prob_uni", "dist_uni", "prob_bi", "dist_bi")
  col_names_y <- c("id", "run", "trial_run", "graph", "previous", "current",
                   "prob_uni", "dist_uni", "prob_bi", "dist_bi")
  dt_main_stim <- load_data(paths$source$decoding_main_stim_modeled)
  dt_behav_sequence <- load_data(paths$source$behavior_task) %>%
    .[!(id %in% cfg$sub_exclude)] %>%
    .[condition == "Sequence", ] %>%
    .[event_type == "response", ] %>%
    verify(.[, by = .(id, run), .(num_trials = .N)]$num_trials == cfg$sequence$num_trials_run) %>%
    .[, by = .(id, run), mean_rt_prev := sapply(seq_len(.N), function(i) mean(log_response_time[max(1, i-5):i], na.rm = TRUE))] %>%
    .[, by = .(id, run), mean_rt_next := sapply(seq_len(.N), function(i) mean(log_response_time[i:min(.N, i+5)], na.rm = TRUE))] %>%
    .[long_interval == TRUE, ] %>%
    verify(.[, by = .(id), .(num_long_intervals = .N)]$num_long_intervals == cfg$decoding_sequence$max_trials) %>%
    .[, c("id", "run", "trial_run", "mean_rt_prev", "mean_rt_next")]
  dt_demographics <- load_data(paths$source$demographics) %>%
    .[, c("id", "sequence_detected")]
  dt_behav_sr <- load_data(paths$source$behavior_sr_fit_sr_matrices) %>%
    .[condition == "Sequence", ]
  dt_behav_sr_data <- load_data(paths$source$behavior_sr_fit_data) %>%
    .[model_name == "SR + 1-step", ] %>%
    .[process == "Model Fitting", ] %>%
    .[, c("id", "run", "trial_run", "shannon_surprise", "mean_surprise_last_5")]
  dt_questionnaire_ratings <- load_data(paths$source$questionnaire_prob_ratings_cor_sub) %>%
    .[, c("id", "rating_group", "knowledge_group")]
  dt_model <- dt_main_stim %>%
    merge.data.table(., dt_behav_sr, by.x = col_names_x, by.y = col_names_y, all.x = TRUE) %>%
    merge.data.table(., dt_demographics, by = c("id")) %>%
    merge.data.table(., dt_questionnaire_ratings, by = c("id")) %>%
    merge.data.table(., dt_behav_sr_data, by = c("id", "run", "trial_run")) %>%
    merge.data.table(., dt_behav_sequence, by = c("id", "run", "trial_run")) %>%
    .[!(id %in% cfg$sub_exclude), ] %>%
    .[interval_tr %in% seq(1, 8), ] %>%
    .[trial_run > 1, ] %>%
    .[, run_half := (as.numeric(substr(run, 6, 6)) - 1) * 2 + ceiling(trial_run / 120)] %>%
    .[, session_half := ifelse(run_half <= 5, "Before", "After")] %>%
    .[, onset_interval := NULL] %>%
    .[, onset := NULL] %>%
    .[, dist_flat := NULL] %>%
    .[, prob_flat := NULL] %>%
    .[, condition := NULL] %>%
    setnames(., "mask_test", "roi") %>%
    setnames(., "probability", "prob_class") %>%
    setnames(., "probability_norm", "prob_class_norm") %>%
    setnames(., "probability_modeled", "prob_stim") %>%
    setnames(., "sr_prob", "prob_sr") %>%
    .[, prob_stim_norm := exp(prob_stim * 1) / sum(exp(prob_stim * 1)), by = .(id, roi, trial_index, interval_tr)] %>%
    .[, dist_graph_scale := scale(dist_graph, center = TRUE, scale = TRUE), by = .(id, roi)] %>%
    .[, dist_graph_scale := scale(dist_graph, center = TRUE, scale = TRUE), by = .(id, roi)] %>%
    .[, prob_graph_scale := scale(prob_graph, center = TRUE, scale = TRUE), by = .(id, roi)] %>%
    .[, prob_class_scale := scale(prob_class, center = TRUE, scale = TRUE), by = .(id, roi)] %>%
    .[, prob_stim_scale := scale(prob_stim, center = TRUE, scale = TRUE), by = .(id, roi)] %>%
    .[, prob_sr_scale := scale(prob_sr, center = TRUE, scale = TRUE), by = .(id, roi)] %>%
    .[, sequence_detected := ifelse(sequence_detected == "yes", "conscious knowledge", "no conscious knowledge")] %>%
    setcolorder(., c(
      "id", "run", "run_half", "trial_run", "trial_index", "graph", "node", "prob_graph", "dist_graph", "roi", "node_classifier", "interval_tr"
    )) %>%
    setorder(id, run, run_half, trial_run, node_classifier, interval_tr) %>%
    save_data(paths$source$decoding_main_model_input)
}

get_decoding_main_model_raw_prob <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_input) %>%
    .[, dist_graph := ifelse(node_classifier == node, 0, dist_graph)] %>%
    melt(.,
         id.vars = c("id", "roi", "interval_tr", "graph", "dist_graph"),
         measure.vars = c("prob_class", "prob_stim_norm"),
         variable.name = "datatype",
         value.name = "prob") %>%
    .[, datatype := dplyr::case_when(
      datatype == "prob_class" ~ "Data",
      datatype == "prob_stim_norm" ~ "Stimulus Model"
    )] %>%
    .[, by = .(id, roi, interval_tr, graph, dist_graph, datatype), .(
      mean_prob = mean(prob)
    )] %>%
    save_data(paths$source$decoding_main_model_raw_prob)
}

get_decoding_main_model_p_stim <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_results)
  dt_output <- dt_input %>%
    .[interval_tr == 4, ] %>%
    .[model_name == "Stimulus", ] %>%
    .[roi == "visual", ] %>%
    .[term == "prob_stim", ]
  print(format_pvalue(dt_output$p.value))
}

get_decoding_main_model_residuals <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_input)
  model_formulas <- cfg$decoding_sequence$models$model_formulas
  model_names <- cfg$decoding_sequence$models$model_names
  dt_output <- dt_input %>%
    .[!(node_classifier == node), ] %>%
    .[, by = .(roi, graph, interval_tr), {
      model = lapply(model_formulas, run_lmer, data = .SD, cfg = cfg, tidy = FALSE)
      model_formula = model_formulas
      model_name = model_names
      model_number = seq_len(length(model_formulas))
      residual = lapply(model, residuals)
      prediction = lapply(model, predict)
      dist_graph = list(dist_graph)
      id = list(id)
      alpha = list(alpha)
      gamma = list(gamma)
      sequence_detected = list(as.character(sequence_detected))
      rating_group = list(as.character(rating_group))
      knowledge_group = list(as.character(knowledge_group))
      shannon_surprise = list(shannon_surprise)
      mean_surprise_last_5 = list(mean_surprise_last_5)
      mean_rt_prev = list(mean_rt_prev)
      mean_rt_next = list(mean_rt_next)
      trial_index = list(trial_index)
      N = .N
      list(model_formula, model_name, model_number, residual, prediction, dist_graph, id, alpha, gamma, sequence_detected, rating_group, knowledge_group, shannon_surprise, mean_surprise_last_5, mean_rt_prev, mean_rt_next, trial_index, N)
    }] %>%
    unnest(., c(residual, prediction, dist_graph, id, alpha, gamma, sequence_detected, shannon_surprise, rating_group, knowledge_group, mean_surprise_last_5, mean_rt_prev, mean_rt_next, trial_index)) %>%
    setDT(.) %>%
    .[, by = .(id), surprise_group := dplyr::case_when(
      shannon_surprise <= median(shannon_surprise) ~ "Low SR-based surprise",
      shannon_surprise > median(shannon_surprise) ~ "High SR-based surprise"
    )] %>%
    .[, alpha_group := dplyr::case_when(
      round(alpha, 2) == 0.01 ~ sprintf("%s ~ 0.01", cfg$alpha_utf),
      round(alpha, 2) > 0.01 & round(alpha, 2) < 1 ~ sprintf("0.01 < %s < 1.00", cfg$alpha_utf),
      round(alpha, 2) == 1 ~ sprintf("%s ~ 1.00", cfg$alpha_utf)
    )] %>%
    .[, gamma_group := dplyr::case_when(
      round(gamma, 2) == 0.01 ~ sprintf("%s ~ 0.01", cfg$gamma_utf),
      round(gamma, 2) > 0.01 & round(gamma, 2) < 1  ~ sprintf("0.01 < %s < 1.00", cfg$gamma_utf),
      round(gamma, 2) == 1 ~ sprintf("%s ~ 1.00", cfg$gamma_utf)
    )] %>%
    .[, model_label := paste("Model", model_number)] %>%
    save_data(paths$source$decoding_main_model_residuals)
}

get_decoding_main_model_residuals_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_residuals)
  dt_output <- dt_input %>%
    .[, by = .(id, roi, sequence_detected, alpha_group, gamma_group, surprise_group, rating_group, knowledge_group, model_name, graph, interval_tr, dist_graph), .(
      num_trials = .N,
      mean_residual = mean(residual)
    )] %>%
    verify(num_trials <= cfg$decoding_sequence$max_trials_graph * 2) %>%
    .[, num_trials := NULL] %>%
    save_data(paths$source$decoding_main_model_residuals_mean)
}

get_decoding_main_model_residuals_slope <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_residuals)
  dt_output <- dt_input %>%
    .[, by = .(id, roi, sequence_detected, shannon_surprise, alpha_group, gamma_group, surprise_group, rating_group, knowledge_group, mean_rt_prev, mean_rt_next, model_name, graph, trial_index, interval_tr), .(
      num_nodes = .N,
      slope = coef(lm(residual ~ dist_graph))[2] * (-1)
    )] %>%
    verify(num_nodes == cfg$num_nodes - 1) %>%
    .[, num_nodes := NULL] %>%
    save_data(paths$source$decoding_main_model_residuals_slope)
}

get_decoding_main_model_residuals_slope_mean <- function(cfg, paths, group = NULL) {
  save_path <- paste(paths$source$decoding_main_model_residuals_slope_mean, group, sep = "_")
  dt_input <- load_data(paths$source$decoding_main_model_residuals_slope)
  dt_input$group <- dt_input[, ..group]
  dt_output <- dt_input %>%
    .[, by = .(id, roi, model_name, graph, interval_tr, group), .(
      num_trials = .N,
      mean_slope = mean(slope)
    )] %>%
    verify(num_trials <= cfg$decoding_sequence$max_trials_graph) %>%
    .[, num_trials := NULL] %>%
    save_data(save_path)
}

get_decoding_main_model_residuals_slope_stat <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_residuals_slope_mean)
  ttest_cfg <- list(
    lhs = "value",
    rhs = "1",
    adjust_method = "none",
    paired = FALSE,
    mu = 0,
    alternative = "two.sided"
  )
  dt_output <- dt_input %>%
    melt(id.vars = c("id", "roi", "model_name", "graph", "interval_tr"), measure.vars = c("mean_slope")) %>%
    .[model_name == "Stimulus", ] %>%
    .[, by = .(roi, model_name, graph, interval_tr, variable), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg) %>%
    setorder(., roi, model_name, graph, interval_tr) %>%
    save_data(paths$source$decoding_main_model_residuals_slope_stat)
}

get_decoding_main_model_residuals_slope_stat_group <- function(cfg, paths, group = NULL) {
  path_input <- paste(paths$source$decoding_main_model_residuals_slope_mean, group, sep = "_")
  path_output <- paste(paths$source$decoding_main_model_residuals_slope_stat, group, sep = "_")
  dt_input <- load_data(path_input)
  ttest_cfg <- list(
    lhs = "value",
    rhs = "1",
    adjust_method = "fdr",
    paired = FALSE,
    mu = 0,
    alternative = "two.sided"
  )
  dt_output <- dt_input %>%
    melt(id.vars = c("id", "roi", "group", "model_name", "graph", "interval_tr"), measure.vars = c("mean_slope")) %>%
    .[model_name == "Stimulus", ] %>%
    # remove subgroups with n = 1:
    .[, by = .(roi, group, model_name, graph, interval_tr), group_id := rleid(id)] %>%
    .[, by = .(group), num_subs := length(unique(group_id))] %>%
    .[num_subs > 1, ] %>%
    .[, by = .(group, roi, model_name, graph, interval_tr, variable), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg) %>%
    setorder(., group, roi, model_name, graph, interval_tr) %>%
    save_data(path_output)
}

get_decoding_main_model_residuals_surprise_cor <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_residuals_slope)
  dt_output <- dt_input %>%
    .[, by = .(id, alpha_group, gamma_group, sequence_detected, roi, graph, model_name, interval_tr), .(
      num_trials = .N,
      cor = list(broom::tidy(cor.test(shannon_surprise, abs(slope), method = "pearson")))
    )] %>%
    verify(num_trials <= cfg$decoding_sequence$max_trials_graph) %>%
    unnest(cor) %>%
    setDT(.) %>%
    get_pvalue_adjust(., list(adjust_method = "fdr")) %>%
    .[model_name == "Stimulus", ] %>%
    .[roi == "visual", ] %>%
    .[graph == "uni", ]
    # .[, by = .(roi, graph, model_name, interval_tr), .(
    #   num_subs = .N,
    #   mean_cor = mean(estimate)
    # )] %>%
    # verify(num_subs == cfg$num_subs)
}

plot_decoding_main_model_residuals_surprise <- function(cfg, paths) {

figure <- ggplot(dt_output, aes(x = as.factor(interval_tr), y = as.numeric(estimate))) +
  # annotate("rect", xmin = 1, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgray") +
  # annotate("rect", xmin = 4.5, xmax = 8, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "darkgray") +
  geom_hline(yintercept = 0, color = "gray") +
  stat_summary(aes(group = as.factor(model_name), fill = as.factor(model_name)),
               geom = "ribbon", fun.data = "mean_se", color = NA, alpha = 0.3) +
  stat_summary(aes(group = as.factor(model_name), color = as.factor(model_name)),
               geom = "line", fun = "mean") +
  stat_summary(aes(group = as.factor(model_name), color = as.factor(model_name)),
               geom = "point", fun = "mean") +
  ylab("Correlation between Shannon surprise and residuals slope") +
  xlab("Time from inter-trial interval onset") +
  # facet_grid(vars(roi), vars(graph), scales = "free_y") +
  facet_wrap(~ gamma_group) + 
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
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

}

get_decoding_main_model_residuals_rt_cor <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_residuals_slope)
  dt_output <- dt_input %>%
    .[, by = .(id, roi, alpha_group, gamma_group, rating_group, knowledge_group, sequence_detected, model_name, graph, trial_index, mean_rt_prev, mean_rt_next), .(
      num_trs = .N,
      mean_abs_slope = mean(abs(slope))
    )] %>%
    verify(num_trs == cfg$decoding_sequence$num_trs) %>%
    melt(measure.vars = c("mean_rt_prev", "mean_rt_next"), variable.name = "rt_type", value.name = "mean_rt") %>%
    .[, by = .(id, roi, alpha_group, gamma_group, rating_group, knowledge_group, sequence_detected, model_name, graph, rt_type), .(
      num_trials = .N,
      mean_rt = mean(mean_rt),
      mean_abs_slope = mean(mean_abs_slope)
    )] %>%
    # .[, by = .(id, alpha_group, gamma_group, sequence_detected, roi, graph, model_name, rt_type), .(
    #   num_trials = .N,
    #   cor = list(broom::tidy(cor.test(mean_rt, mean_abs_slope, method = "pearson")))
    # )] %>%
    # verify(num_trials <= cfg$decoding_sequence$max_trials_graph) %>%
    # unnest(cor) %>%
    # setDT(.) %>%
    # get_pvalue_adjust(., list(adjust_method = "fdr")) %>%
    .[model_name == "Stimulus", ] %>%
    .[roi == "visual", ] %>%
    .[graph == "uni", ]
  # .[, by = .(roi, graph, model_name, interval_tr), .(
  #   num_subs = .N,
  #   mean_cor = mean(estimate)
  # )] %>%
  # verify(num_subs == cfg$num_subs)
}

plot_decoding_main_model_residuals_rt_cor <- function(cfg, paths) {
  
  figure <- ggplot(dt_output, aes(x = as.numeric(mean_abs_slope), y = as.numeric(mean_rt))) +
    geom_point() +
    geom_smooth(method = "lm") +
    # geom_text(data = dt2, aes(y = Inf, x = 0, label = result), vjust = 2, hjust = 0, size = 4) +
    xlab("Mean absolute slope") +
    ylab("Mean response time (log ms)") +
    theme_zoo() +
    facet_wrap(~ rt_type) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    scale_color_manual(name = "Predictor", values = cfg$colors_predictors) +
    # scale_x_continuous(labels = label_fill(seq(0, 1, 0.25), mod = 2), breaks = seq(0, 1, 0.25)) +
    scale_fill_manual(name = "Predictor", values = cfg$colors_predictors) +
    scale_shape_manual(name = "Predictor", values = cfg$shapes_predictors) +
    ggtitle("Relationship RTs before/after (5 trials)\nand mean absolute slope of residuals") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(legend.position = "none")
  figure
  
}

get_decoding_main_model_prediction <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_input)
  model_formulas <- cfg$decoding_sequence$models$model_formulas
  model_names <- cfg$decoding_sequence$models$model_names
  dt_output <- dt_input %>%
    .[!(node_classifier == node), ] %>%
    .[, by = .(roi, graph, interval_tr), {
      model = lapply(model_formulas, run_lmer, data = .SD, cfg = cfg, tidy = FALSE)
      model_formula = model_formulas
      model_name = model_names
      model_number = seq_len(length(model_formulas))
      prob_pred = lapply(model, predict)
      prob_class = list(prob_class)
      dist_graph = list(dist_graph)
      id = list(id)
      trial_index = list(trial_index)
      N = .N
      list(model_formula, model_name, model_number, prob_pred, prob_class, dist_graph, id, trial_index, N)
    }] %>%
    .[, model_label := paste("Model", model_number)] %>%
    unnest(., c(prob_pred, prob_class, dist_graph, id, trial_index)) %>%
    setDT(.) %>%
    melt(.,
         measure.vars = c("prob_class", "prob_pred"),
         variable.name = "datatype",
         value.name = "prob") %>%
    .[, datatype := dplyr::case_when(
      datatype == "prob_class" ~ "Data",
      datatype == "prob_pred" ~ "Stimulus Model"
    )] %>%
    .[, by = .(id, roi, model_formula, model_name, model_number, interval_tr, graph, dist_graph, datatype), .(
      num_trials = .N,
      mean_prob = mean(prob)
    )] %>%
    verify(num_trials <= cfg$decoding_sequence$max_trials) %>%
    .[, num_trials := NULL] %>%
    save_data(paths$source$decoding_main_model_prediction)
}

get_decoding_main_model_betas <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_input)
  model_formulas <- cfg$decoding_sequence$models$model_formulas
  model_names <- cfg$decoding_sequence$models$model_names
  dt_output <- dt_input %>%
    .[!(node_classifier == node), ] %>%
    .[, by = .(roi, graph, interval_tr), {
      model = lapply(model_formulas, run_lmer, data = .SD, cfg = cfg, tidy = FALSE)
      model_formula = model_formulas
      model_name = model_names
      model_number = seq_len(length(model_formulas))
      random_effects = lapply(model, ranef)
      beta = lapply(model, fixef)
      N = .N
      list(model_formula, model_name, model_number, beta, random_effects, N)
    }] %>%
    verify(.[, by = .(graph), .(uniqueN = length(unique(N)))]$uniqueN == 1) %>%
    .[, model_label := paste("Model", model_number)] %>%
    unnest_wider(., beta, names_sep = "_") %>%
    setnames(old = "beta_(Intercept)", new = "beta_intercept") %>%
    setDT(.) %>%
    .[, random_effects := lapply(random_effects, broom::augment)] %>%
    unnest(random_effects) %>%
    setDT(.) %>%
    .[, grp := NULL] %>%
    setnames(., old = "level", new = "id") %>%
    melt(., measure.vars = patterns("^beta_"), variable.name = "predictor", value.name = "beta") %>%
    .[, predictor := dplyr::case_when(
      stringr::str_detect(predictor, "intercept") ~ "Intercept",
      stringr::str_detect(predictor, "prob_graph") ~ "1-step",
      stringr::str_detect(predictor, "prob_sr") ~ "SR",
      stringr::str_detect(predictor, "prob_stim") ~ "Stimulus"
    )] %>%
    .[, id := factor(as.factor(id), levels = cfg$subjects)] %>%
    setorder(id) %>%
    .[, beta_id := beta + estimate] %>%
    save_data(paths$source$decoding_main_model_betas)
}

get_decoding_main_model_betas_id <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_input)
  model_formulas <- cfg$decoding_sequence$models$suite8
  model_names <- cfg$decoding_sequence$models$model_names
  dt_output <- dt_input %>%
    .[!(node_classifier == node), ] %>%
    # .[, by = .(id, roi, interval_tr), {
    .[, by = .(id, roi, graph, interval_tr), {
      model = lapply(model_formulas, run_glm, data = .SD, cfg = cfg, tidy = TRUE)
      model_formula = model_formulas
      model_name = model_names
      model_number = seq_len(length(model_formulas))
      N = .N
      list(model, model_formula, model_name, model_number, N)
    }] %>%
    unnest(model) %>%
    setDT(.) %>%
    .[, term := dplyr::case_when(
      stringr::str_detect(term, "(Intercept)") ~ "Intercept",
      stringr::str_detect(term, "prob_graph") ~ "1-step",
      stringr::str_detect(term, "prob_sr") ~ "SR",
      stringr::str_detect(term, "prob_stim") ~ "Stimulus"
    )] %>%
    setnames(., old = "term", new = "predictor") %>%
    save_data(paths$source$decoding_main_model_betas_id)
}

get_decoding_main_model_betas_behav <- function(cfg, paths) {
  dt_sr_fits <- load_data(paths$source$behavior_sr_fit_parameter_distribution) %>%
    .[process == "Model Fitting", ] %>%
    .[model_name == "SR + 1-step", ] %>%
    .[, c("id", "variable", "value")] %>%
    verify(.[, by = .(id), num_var := length(unique(variable))]$num_var == 2) %>%
    pivot_wider(id_cols = c("id"), names_from = variable, values_from = value)
  dt_behav_sequence_onestep <- load_data(paths$source$behavior_sequence_onestep_run_glm)
  dt_sr_beta <- load_data(paths$source$decoding_main_model_betas_id) %>%
    .[model_number == 4] %>%
    merge.data.table(x = ., y = dt_sr_fits, by = c("id")) %>%
    merge.data.table(x = ., y = dt_behav_sequence_onestep, by = c("id")) %>%
    .[, estimate_abs := abs(estimate)] %>%
    melt(measure.vars = c("estimate", "estimate_abs"), variable.name = "beta", value.name = "beta_value") %>%
    melt(measure.vars = c("alpha", "gamma"), variable.name = "sr_parameter", value.name = "sr_parameter_value") %>%
    save_data(paths$source$decoding_main_model_betas_behav)
  dt_output <- dt_sr_beta %>%
    # .[, by = .(roi, interval_tr, predictor, variable), .(
    .[, by = .(roi, graph, interval_tr, model_name, predictor, beta, sr_parameter), .(
      num_subs = .N,
      cor = list(broom::tidy(cor.test(beta_value, sr_parameter_value, method = "pearson")))
    )] %>%
    verify(num_subs == cfg$num_subs) %>%
    unnest(cor) %>%
    setDT(.) %>%
    .[predictor == "SR" & beta == "estimate" & roi == "visual", ] %>%
    get_pvalue_adjust(., list(adjust_method = "fdr")) %>%
    setorder(roi, graph, interval_tr) %>%
    save_data(paths$source$decoding_main_model_betas_behav_cor)
    # .[p.value_adjust_round <= 0.05, ] %>%
}

get_decoding_main_model_betas_behav_cor_mean <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_betas_behav)
  dt_output <- dt_input %>%
    # average beta and sr parameter estimates across TRs:
    .[, by = .(id, roi, graph, model_name, predictor, beta, sr_parameter), .(
      num_trs = .N,
      beta_value = mean(beta_value),
      sr_parameter_value = unique(sr_parameter_value),
      slope_diff = unique(slope_diff),
      slope_low = unique(Low),
      slope_high = unique(High)
    )] %>%
    verify(num_trs == cfg$decoding_sequence$num_trs) %>%
    save_data(paths$source$decoding_main_model_betas_behav_cor_mean)
  # dt_sd <- dt_input %>%
  #   .[, by = .(roi, graph, predictor), ":="(
  #     num_subs = .N,
  #     sd_group = sd(mean_estimate, na.rm = TRUE),
  #     mean_group = mean(mean_estimate, na.rm = TRUE)
  #   )] %>%
  #   verify(num_subs == cfg$num_subs) %>%
  #   .[, outlier_cutoff := abs(mean_estimate + 1 * sd_group)] %>%
  #   .[, outlier := mean_estimate >= outlier_cutoff]
}

get_decoding_main_model_betas_behav_cor_mean_stat <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_betas_behav_cor_mean)
  dt_output <- dt_input %>%
    # .[mean_estimate <= 1, ] %>%
    .[, by = .(roi, graph, model_name, predictor, beta, sr_parameter), .(
      num_subs = .N,
      cor = list(broom::tidy(cor.test(beta_value, sr_parameter_value, method = "pearson")))
    )] %>%
    verify(num_subs == cfg$num_subs) %>%
    unnest(cor) %>%
    setDT(.) %>%
    get_pvalue_adjust(., list(adjust_method = "bonferroni")) %>%
    .[, result := sprintf("r = %.2f, p %s", estimate, p.value_round_label)] %>%
    save_data(paths$source$decoding_main_model_betas_behav_cor_mean_stat) %>%
    .[p.value < 0.05, ]
}

get_decoding_main_model_betas_behav_cor_mean_stat_rt <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_betas_behav_cor_mean)
  dt_output <- dt_input %>%
    .[, c("id", "roi", "graph", "model_name", "predictor", "beta", "beta_value", "slope_diff", "slope_low", "slope_high"), with = FALSE] %>%
    unique(.) %>%
    .[, by = .(roi, graph, model_name, predictor, beta), .(
      num_subs = .N,
      cor = list(broom::tidy(cor.test(beta_value, slope_high, method = "pearson")))
    )] %>%
    verify(num_subs == cfg$num_subs) %>%
    unnest(cor) %>%
    setDT(.) %>%
    get_pvalue_adjust(., list(adjust_method = "bonferroni")) %>%
    .[, result := sprintf("r = %.2f, p %s", estimate, p.value_round_label)] %>%
    save_data(paths$source$decoding_main_model_betas_behav_cor_mean_stat_rt) %>%
    .[p.value < 0.05, ]
}

get_decoding_main_model_results_report <- function(cfg, paths) {
  # reduce AICs of model comparison to the ones used in reporting
  dt_input <- load_data(paths$source$decoding_main_model_diff) 
  dt_output <- dt_input %>%
    .[roi == "visual", ] %>%
    .[interval_tr %in% c(2, 5)]
}

get_decoding_main_model_results <- function(cfg, paths, group = NULL) {
  dt_input <- load_data(paths$source$decoding_main_model_input)
  group_name <- paste(group, collapse = "_")
  save_path <- paste(paths$source$decoding_main_model_results, group_name, sep = "_")
  model_formulas <- cfg$decoding_sequence$models$suite6
  model_names <- cfg$decoding_sequence$models$model_names
  dt_output <- dt_input %>%
    .[!(node_classifier == node), ] %>%
    .[, by = c(group, "interval_tr"), {
      model_tidy = lapply(model_formulas, run_lmer, data = .SD, cfg = cfg, tidy = TRUE)
      model = lapply(model_formulas, run_lmer, data = .SD, cfg = cfg, tidy = FALSE)
      model_formula = model_formulas
      model_name = model_names
      model_number = seq_len(length(model_formulas))
      aic = unlist(lapply(model, AIC))
      N = .N
      list(model_tidy, model_formula, model_name, model_number, aic, N)
    }] %>%
    unnest(model_tidy) %>%
    setDT(.) %>%
    .[, model_label := paste("Model", model_number)] %>%
    save_data(save_path)
}

get_decoding_main_model_results_diff <- function(cfg, paths, group = NULL) {
  # calculate the AIC score differences of all models against the baseline model:
  group_name <- paste(group, collapse = "_")
  input_path <- paste(paths$source$decoding_main_model_results, group_name, sep = "_")
  save_path <- paste(paths$source$decoding_main_model_results_diff, group_name, sep = "_")
  dt_input <- load_data(input_path)
  dt_output <- dt_input %>%
    .[, c(group, "interval_tr", "model_name", "model_number", "aic"), with = FALSE] %>%
    unique(.) %>%
    # remember that smaller (i.e., more negative) AIC means better fit!
    .[, by = c(group, "interval_tr"), aic_diff := aic - aic[model_name == "Stimulus"]] %>%
    save_data(save_path)
}

get_decoding_main_model_results_diff_mean <- function(cfg, paths, group = NULL) {
  # calculate the AIC score differences of all models against the baseline model:
  group_name <- paste(group, collapse = "_")
  input_path <- paste(paths$source$decoding_main_model_results_diff, group_name, sep = "_")
  save_path <- paste(paths$source$decoding_main_model_results_diff_mean, group_name, sep = "_")
  dt_input <- load_data(input_path)
  dt_output <- dt_input %>%
    .[, by = c(group, "model_name", "model_number"), .(
      num_trs = .N,
      aic_diff_abs_mean = mean(abs(aic_diff)),
      aic_diff_abs_max = aic_diff[which.max(abs(aic_diff))],
      aic_diff_abs_max_tr = which.max(abs(aic_diff))
    )] %>%
    verify(num_trs == cfg$decoding_sequence$num_trs) %>%
    save_data(save_path)
}

get_decoding_main_model_no_evoked <- function(cfg, paths) {
  # analyze sequentiality only in periods with no stimulus driven activity
  dt_main_stim <- load_data(paths$source$decoding_main_stim_modeled)
  dt_demographics <- load_data(paths$source$demographics) %>%
    .[, c("id", "sequence_detected")]
  dt_input <- dt_main_stim %>%
    merge.data.table(., dt_demographics, by = c("id"))
  dt_output <- dt_input %>%
    .[probability_modeled == 0, ] %>%
    .[!(node == node_classifier),] %>%
    .[, by = .(id, mask_test, graph, interval_tr, dist_combined), .(
      num_trials = .N,
      mean_prob = mean(probability_norm)
    )] %>%
    verify(num_trials <= cfg$decoding_sequence$max_trials) %>%
    .[, num_trials := NULL] %>%
    save_data(paths$source$decoding_main_model_no_evoked)
}

get_decoding_main_model_no_evoked_slope <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_stim_modeled)
  variable <- "probability_norm"
  cor_method <- cfg$decoding_sequence$cor_method
  dt_output <- dt_input %>%
    .[probability_modeled == 0, ] %>%
    .[!(node == node_classifier),] %>%
    # order positions by decreasing probability and calculate step size
    # calculate correlation and slope between position and probability
    # verify that there are five probabilities (one for each class) per volume
    # verify that all correlations range between -1 and 1
    .[, by = .(id, mask_test, trial_index, graph, interval_tr), {
      # order the probabilities in decreasing order (first = highest):
      prob_order_index = order(get(variable), decreasing = TRUE)
      # order the sequential positions by probability:
      pos_order = dist_graph[prob_order_index]
      # order the probabilities:
      prob_order = get(variable)[prob_order_index]
      list(
        # calculate the number of events:
        num_nodes = .N,
        # calculate the mean step size between probability-ordered events:
        step = mean(diff(pos_order)),
        # calculate the mean correlation between positions and their probabilities:
        cor = ifelse(.N <= 2, NA_real_, cor.test(pos_order, prob_order, method = cor_method)$estimate  * (-1)),
        # calculate the slope of a linear regression between position and probabilities:
        slope = coef(lm(prob_order ~ pos_order))[2] * (-1)
        # verify that the number of events matches selection and correlations -1 < r < 1
      )}] %>%
    verify(num_nodes <= cfg$num_nodes) %>% 
    verify(between(cor[!is.na(cor)], -1, 1)) %>%
    setorder(., id, mask_test, trial_index, graph, interval_tr) %>%
    pivot_longer(cols = c("step", "cor", "slope"), names_to = "variable", values_to = "value") %>%
    setDT(.) %>%
    .[, variable := dplyr::case_when(
      variable == "step" ~ "Step",
      variable == "cor" ~ "Correlation",
      variable == "slope" ~ "Slope"
    )] %>%
    .[, variable := factor(as.factor(variable), levels = c("Slope", "Correlation", "Step"))] %>%
    .[, by = .(id, mask_test, graph, interval_tr, variable), .(
      num_trials = .N,
      value = mean(value, na.rm = TRUE)
    )] %>%
    verify(num_trials <= cfg$decoding_sequence$max_trials_graph) %>%
    .[, num_trials := NULL] %>%
    save_data(paths$source$decoding_main_model_no_evoked_slope)
}

get_decoding_main_model_no_evoked_slope_stat <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_no_evoked_slope)
  ttest_cfg <- list(
    lhs = "value",
    rhs = "1",
    adjust_method = "fdr",
    paired = FALSE,
    mu = 0,
    alternative = "greater"
  )
  dt_output <- dt_input %>%
    .[, by = .(mask_test, graph, interval_tr, variable), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg) %>%
    save_data(paths$source$decoding_main_model_no_evoked_slope_stat)
}

get_decoding_main_model_no_evoked_slope_stat_select <- function(cfg, paths) {
  dt_input <- load_data(paths$source$decoding_main_model_no_evoked_slope_stat)
  dt_output <- dt_input %>%
    .[variable == "Slope", ] %>%
    .[p.value_significance != "n.s.", ]
  return(dt_output)
}

get_decoding_main_model_no_evoked_num_class_trials <- function(cfg, paths) {
  # classes: how many trials are there without stimulus driven activity?
  dt_input <- load_data(paths$source$decoding_main_stim_modeled)
  dt_output <- dt_input %>%
    .[, by = .(id, mask_test, graph, interval_tr, node_classifier),
      num_trials := length(unique(trial_index_run))
    ] %>%
    verify(num_trials <= cfg$decoding_sequence$max_trials_graph) %>%
    .[probability_modeled == 0, ] %>%
    .[!(node == node_classifier),] %>%
    .[, by = .(id, mask_test, graph, interval_tr, node_classifier), .(
      num_trials = .N,
      ratio_trials = .N/num_trials
    )] %>%
    save_data(paths$source$decoding_main_no_evoked_num_class_trials)
}

get_decoding_main_model_no_evoked_num_dist_trials <- function(cfg, paths) {
  # node distance: how many trials are there without stimulus driven activity
  dt_input <- load_data(paths$source$decoding_main_stim_modeled)
  dt_output <- dt_input %>%
    # add total number of trials across entire experiment
    group_by(id, run, trial_index) %>%
    mutate(index = cur_group_id()) %>%
    ungroup() %>%
    setDT(.) %>%
    .[, by = .(id, mask_test, graph, interval_tr, dist_combined), num_trials := length(unique(index))] %>%
    verify(num_trials <= cfg$decoding_sequence$max_trials) %>%
    # we remove all data where the modeling of stimulus-evoked activity indicates 0:
    .[probability_modeled == 0, ] %>%
    # we remove data of the current node:
    .[!(node == node_classifier),] %>%
    .[, by = .(id, mask_test, graph, interval_tr, dist_combined), .(
      num_trials = .N,
      ratio_trials = .N/num_trials
    )] %>%
    save_data(paths$source$decoding_main_no_evoked_num_dist_trials)
}

get_decoding_main_model_no_evoked_consc <- function(cfg, paths) {
  dt_main_stim <- load_data(paths$source$decoding_main_stim_modeled)
  dt_demographics <- load_data(paths$source$demographics) %>%
    .[, c("id", "sequence_detected")]
  dt_input <- dt_main_stim %>%
    merge.data.table(., dt_demographics, by = c("id"))
  dt_output <- dt_input %>%
    .[probability_modeled == 0, ] %>%
    .[!(node == node_classifier),] %>%
    .[, by = .(id, mask_test, graph, interval_tr, dist_combined, sequence_detected), .(
      num_trials = .N,
      mean_prob = mean(probability_norm * 100)
    )] %>%
    verify(num_trials <= cfg$decoding_sequence$max_trials) %>%
    .[, num_trials := NULL] %>%
    save_data(paths$source$decoding_main_no_evoked)
}
