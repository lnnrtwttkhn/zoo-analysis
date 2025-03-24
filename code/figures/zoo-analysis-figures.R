if (!requireNamespace("here")) install.packages("here")
source(here::here("code", "figures", "zoo-analysis-figures-behavior.R"))
source(here::here("code", "figures", "zoo-analysis-figures-sr-modeling.R"))
source(here::here("code", "figures", "zoo-analysis-figures-questionnaire.R"))
source(here::here("code", "figures", "zoo-analysis-figures-decoding-recall.R"))
source(here::here("code", "figures", "zoo-analysis-figures-decoding-main.R"))

plot_behavior <- function(cfg, paths) {
  figure <- plot_grid(
    plot_grid(
      plot_behavior_single_sequence_accuracy_mean(cfg, paths),
      plot_behavior_sequence_run_response_time(cfg, paths),
      plot_behavior_sequence_accuracy_onestep(cfg, paths),
      plot_behavior_sequence_response_time_onestep(cfg, paths),
      nrow = 1, ncol = 4, rel_widths = c(0.2, 0.2, 0.3, 0.3), labels = c("a", "b", "c", "d"), vjust = 1
    ),
    plot_grid(
      plot_behavior_sequence_response_time_distance(cfg, paths),
      plot_demographics_seq_detect(cfg, paths),
      plot_demographics_seq_when(cfg, paths),
      plot_questionnaire_prob_ratings(cfg, paths),
      nrow = 1, ncol = 4, rel_widths = c(0.3, 0.2, 0.2, 0.3), labels = c("e", "f", "g", "h")
    ),
    nrow = 2, ncol = 1, rel_heights = c(1, 1)
  )
  save_figure(plot = figure, "behavior", width = 9, height = 6)
  return(figure)
}

plot_behavior_run <- function(cfg, paths) {
  figure <- plot_grid(
    plot_behavior_single_run_accuracy(cfg, paths),
    plot_behavior_sequence_run_accuracy(cfg, paths),
    nrow = 1, ncol = 2, labels = c("a", "b")
  )
  save_figure(plot = figure, "behavior_run", width = 7, height = 4)
  return(figure)
}

plot_sr_modeling <- function(cfg, paths) {
  figure <- plot_grid(
    plot_grid(
      # plot_sr_grid_seq_lme(cfg, paths),
      plot_behavior_sr_fit_model_comparison_sum_aic(cfg, paths),
      plot_behavior_sr_fit_parameter_mean(cfg, paths),
      plot_behavior_sr_fit_suprise_effect(cfg, paths),
      # nrow = 1, ncol = 4, labels = c("a", "b", "c", "d"), rel_widths = c(0.3, 0.25, 0.15, 0.3)
      nrow = 1, ncol = 3, labels = c("a", "b", "c"), rel_widths = c(0.25, 0.15, 0.3)
    ),
    # plot_grid(
    #   plot_behavior_sr_fit_parameter_order(cfg, paths),
    #   plot_behavior_sr_fit_parameter_conscious(cfg, paths),
    #   nrow = 1, ncol = 2, labels = c("e", "f")
    # ),
    plot_grid(
      plot_sr_matrices_select(cfg, paths),
      nrow = 1, ncol = 1, labels = c("d")
    ),
    nrow = 2, ncol = 1, rel_heights = c(0.45, 0.55)
  )
  # save_figure(plot = figure, "sr_modeling", width = 9, height = 10)
  save_figure(plot = figure, "sr_modeling", width = 7, height = 6.5)
  return(figure)
}

plot_sr_modeling_group_splits <- function(cfg, paths) {
  figure <- plot_grid(
    plot_behavior_sr_fit_parameter_order(cfg, paths),
    plot_behavior_sr_fit_parameter_conscious(cfg, paths),
    nrow = 1, ncol = 2, labels = c("a", "b")
  )
  save_figure(plot = figure, "sr_modeling_group_splits", width = 7, height = 3)
}

plot_sr_modeling_supplement <- function(cfg, paths) {
  figure <- plot_grid(
    plot_sr_grid_seq_graph_lme(cfg, paths),
    plot_sr_grid_seq_graph_order_lme(cfg, paths),
    nrow = 2, ncol = 1, rel_heights = c(1/3, 2/3), labels = c("a", "b")
  )
  save_figure(plot = figure, "sr_modeling_supplement", width = 5, height = 7)
  return(figure)
}

plot_sr_modeling_parameter_recovery <- function(cfg, paths) {
  figure <- plot_grid(
    plot_behavior_sr_fit_parameter_recovery_corr(cfg, paths),
    plot_behavior_sr_fit_parameter_recovery(cfg, paths),
    plot_behavior_sequence_response_time_onestep_run(cfg, paths),
    plot_behavior_sr_fit_response_time_onestep_diff(cfg, paths),
    nrow = 2, ncol = 2, rel_widths = c(0.5, 0.5), labels = c("a", "b", "c", "d")
  )
  save_figure(plot = figure, "sr_modeling_parameter_recovery", width = 12, height = 8)
  return(figure)
}

plot_sr_modeling_sr_onestep <- function(cfg, paths) {
  figure <- plot_grid(
    plot_behavior_sr_fit_model_comparison_sum_aic(cfg, paths),
    plot_behavior_sr_fit_suprise_effect(cfg, paths),
    nrow = 1, ncol = 2, rel_widths = c(0.4, 0.6), labels = letters[1:2]
  )
  save_figure(plot = figure, "sr_modeling_sr_onestep", width = 8, height = 5)
  return(figure)
}

plot_sr_fit_check <- function(cfg, paths){
  figure <- plot_grid(
    plot_behavior_sr_fit_starting_values(cfg, paths),
    plot_behavior_sr_fit_parameter_dispersion(cfg, paths),
    plot_behavior_sr_fit_parameter_distribution(cfg, paths),
    plot_behavior_sr_fit_parameter_distribution_mean(cfg, paths),
    nrow = 2, ncol = 2, labels = c("a", "b", "c", "d")
  )
  save_figure(plot = figure, "sr_modeling_checks", width = 8, height = 6)
  return(figure)
}

plot_behavior_graph_run = function() {
  panel_a = plot_behavior_main_graph_run_response_time()
  panel_b = plot_behavior_main_graph_run_accuracy()
  figure = plot_grid(panel_a, panel_b, nrow = 2, ncol = 1, labels = c("a", "b"))
  return(figure)
}

plot_decoding_current <- function(cfg, paths) {
  title_upper <- ggdraw() + 
    draw_label("Single trials", hjust = 0.5, fontface = "bold")
  title_lower <- ggdraw() + 
    draw_label("Sequence trials", hjust = 0.5, fontface = "bold")
  panel_a <- plot_decoding_single_accuracy_mean(cfg, paths)
  panel_b <- plot_decoding_single_interval(cfg, paths)
  panel_c <- plot_decoding_main_current_mean(cfg, paths) +
    theme(legend.position = "none")
  panel_d <- plot_decoding_main_current_interval(cfg, paths) +
    theme(legend.position = "none")
  figure <- plot_grid(
    title_upper,
    plot_grid(panel_a, panel_b, rel_widths = c(2, 5),
              labels = c("a", "b"), ncol = 2, nrow = 1, vjust = 0),
    title_lower,
    plot_grid(panel_c, panel_d, rel_widths = c(3, 4),
              labels = c("c", "d"), ncol = 2, nrow = 1),
    ncol = 1, nrow = 4, rel_heights = c(0.1, 1, 0.1, 1)
  )
  save_figure(figure, "decoding_current", width = 6, height = 6)
  return(figure)
}

plot_decoding_main_model <- function(cfg, paths, roi_input, graph_input) {
  figure <- plot_grid(
    ggdraw() + draw_label(sprintf("%s ROI", tools::toTitleCase(roi_input)), hjust = 0.5, fontface = "bold"),
    plot_grid(plot_decoding_main_model_prediction(cfg, paths, roi_input, graph_input),
              labels = c("a"), ncol = 1, nrow = 1),
    plot_grid(plot_decoding_main_model_results_stim(cfg, paths, roi_input),
              plot_decoding_main_model_results_diff(cfg, paths, group = c("roi"), roi_input = roi_input) +
                theme(legend.position = "none") +
                theme(legend.position = c(0.7, 0.85)),
              labels = c("b", "c"), ncol = 2, nrow = 1),
    plot_grid(plot_decoding_main_model_residuals(cfg, paths, roi_input, graph_input) + theme(legend.position = "none"),
              plot_decoding_main_model_residuals_slope(cfg, paths, roi_input, graph_input),
              labels = c("d", "e"), ncol = 2, nrow = 1),
    plot_grid(plot_decoding_main_model_no_evoked_prob(cfg, paths, roi_input, graph_input) + theme(legend.position = "none"),
              plot_decoding_main_model_no_evoked_slope(cfg, paths, roi_input, graph_input),
              labels = c("f", "g"), ncol = 2, nrow = 1, vjust = 0.5),
    ncol = 1, nrow = 5, rel_heights = c(0.1, 1, 1.1, 1, 1)
  )
  filename = sprintf("decoding_main_model_%s_%s", roi_input, graph_input)
  save_figure(plot = figure, filename, width = 7, height = 10)
  return(figure)
}

plot_decoding_main_model_no_evoked <- function(cfg, paths) {
  # supplementary figure: number of trials after removing evoked trials
  figure <- plot_grid(
    plot_grid(
      plot_decoding_main_model_no_evoked_num_class_trials(cfg, paths),
      plot_decoding_main_model_no_evoked_num_dist_trials(cfg, paths),
      labels = c("a", "b"), ncol = 2, nrow = 1
    ),
    ncol = 1, nrow = 1
  )
  save_figure(plot = figure, "decoding_main_no_evoked", width = 8.5, height = 4)
  return(figure)
}

plot_decoding_main_residuals_slope_alpha_gamma <- function(cfg, paths, roi_input, graph_input) {
  figure <- plot_grid(
    plot_grid(
      plot_grid(
        ggdraw() + draw_label(sprintf("%s", cfg$alpha_utf), hjust = 0.5, fontface = "bold"),
        plot_decoding_main_model_residuals(cfg, paths, roi_input = "visual", graph_input = "uni", group = "alpha_group"),
        plot_decoding_main_model_residuals_slope(cfg, paths, roi_input = "visual", graph_input = "uni", group = "alpha_group"),
        labels = c("", "a", "c"), ncol = 1, nrow = 3, rel_heights = c(0.1, 1, 1)
      ),
      plot_grid(
        ggdraw() + draw_label(sprintf("%s", cfg$gamma_utf), hjust = 0.5, fontface = "bold"),
        plot_decoding_main_model_residuals(cfg, paths, roi_input = "visual", graph_input = "uni", group = "gamma_group"),
        plot_decoding_main_model_residuals_slope(cfg, paths, roi_input = "visual", graph_input = "uni", group = "gamma_group"),
        labels = c("", "b", "d"), ncol = 1, nrow = 3, rel_heights = c(0.1, 1, 1)
      ), nrow = 1, ncol = 2),
    plot_grid(
      plot_decoding_main_model_betas_behav_cor_mean(cfg, paths, roi_input = roi_input, graph_input = graph_input, predictor_input = "SR"),
      plot_decoding_main_model_betas_behav_cor_mean(cfg, paths, roi_input = roi_input, graph_input = graph_input, predictor_input = "1-step"),
      labels = c("e", "f"), ncol = 2, nrow = 1
    ), ncol = 1, nrow = 2, rel_heights = c(2/3, 1/3)
  )
  save_figure(plot = figure, "decoding_main_residuals_slope_alpha_gamma", width = 11, height = 10)
  return(figure)
}

plot_decoding_main_residuals_slope_surprise <- function(cfg, paths, roi_input, graph_input) {
  figure <- plot_grid(
    plot_decoding_main_model_residuals(cfg, paths, roi_input = "visual", graph_input = "uni", group = "surprise_group"),
    plot_decoding_main_model_residuals_slope(cfg, paths, roi_input = "visual", graph_input = "uni", group = "surprise_group"),
    nrow = 2, ncol = 1, labels = letters[1:2]
  )
  save_figure(plot = figure, "decoding_main_residuals_slope_surprise", width = 6, height = 7)
  return(figure)
}

plot_decoding_main_residuals_slope_awareness <- function(cfg, paths, roi_input, graph_input) {
  figure <- plot_grid(
    ggdraw() + draw_label("Classification based on explicit binary report of sequence awarenes", hjust = 0.5, fontface = "bold"),
    plot_grid(
      plot_decoding_main_model_residuals(cfg, paths, roi_input = "visual", graph_input = "uni", group = "sequence_detected"),
      plot_decoding_main_model_residuals_slope(cfg, paths, roi_input = "visual", graph_input = "uni", group = "sequence_detected"),
      labels = c("a", "b")
    ),
    ggdraw() + draw_label("Classification based on ratings of pairwise transition probabilities", hjust = 0.5, fontface = "bold"),
    plot_grid(
      plot_decoding_main_model_residuals(cfg, paths, roi_input = "visual", graph_input = "uni", group = "rating_group"),
      plot_decoding_main_model_residuals_slope(cfg, paths, roi_input = "visual", graph_input = "uni", group = "rating_group"),
      labels = c("c", "d")
    ),
    ggdraw() + draw_label("Classification based on binary report and transition probability ratings", hjust = 0.5, fontface = "bold"),
    plot_grid(
      plot_decoding_main_model_residuals(cfg, paths, roi_input = "visual", graph_input = "uni", group = "knowledge_group"),
      plot_decoding_main_model_residuals_slope(cfg, paths, roi_input = "visual", graph_input = "uni", group = "knowledge_group"),
      labels = c("e", "f")
    ),
    nrow = 6, ncol = 1, rel_heights = c(0.05, 0.45, 0.05, 0.45, 0.05, 0.45)
  )
  save_figure(plot = figure, "decoding_main_residuals_slope_awareness", width = 10, height = 10)
  return(figure)
}

plot_brain_behavior <- function(cfg, paths, roi_input) {
  figure <- plot_grid(
    plot_grid(
      plot_decoding_main_model_results_run_diff(cfg, paths, roi_input),
      plot_decoding_main_model_betas_behav_cor_mean(cfg, paths),
      plot_decoding_main_model_betas_behav_cor(cfg, paths),
      ncol = 3, nrow = 1, rel_widths = c(1/3, 1/3, 1/3), labels = letters[1:3]
    ), plot_grid(
      plot_decoding_main_model_residuals_consciousness(cfg, paths, roi_input),
      plot_decoding_main_model_residuals_slope_consciousness(cfg, paths, roi_input),
      ncol = 2, nrow = 1, rel_widths  = c(1/2, 1/2), labels = letters[4:5]
    ), ncol = 1, nrow = 2, rel_heights = c(1/2, 1/2)
  )
  filename <- sprintf("brain_behavior_%s", roi_input)
  save_figure(plot = figure, filename = filename, width = 11, height = 7)
  return(figure)
}

figure = plot_decoding_main_class_probabilities()
save_figure(plot = figure, "decoding_main_class_probabilities", width = 6, height = 4)

figure = plot_decoding_main_graph_probablities()
save_figure(plot = figure, "decoding_main_graph_probabilities", width = 6, height = 4)

plot_decoding_interval_probabilities = function() {
  # row 1:
  panel_a = plot_decoding_main_class_probabilities()
  # row 2:
  panel_b = plot_decoding_main_graph_probablities_distance()
  panel_c = plot_decoding_main_graph_probablities() + theme(legend.position = "none")
  legend_bc = get_legend(panel_b)
  panel_b = panel_b + theme(legend.position = "none")
  # row 3:
  panel_d = plot_decoding_main_probabilities_sequence_distribution()
  panel_e = plot_decoding_main_probabilities_sequence_correlation()
  # row 4:
  panel_f = plot_decoding_main_slope_seq_prob_interval() + theme(legend.position = "none")
  panel_g = plot_decoding_main_slope_seq_prob_phase()
  legend_fg = get_legend(panel_g)
  panel_g = panel_g + theme(legend.position = "none")
  figure = plot_grid(
    plot_grid(panel_a, labels = c("a"), ncol = 1, nrow = 1),
    plot_grid(panel_b, panel_c, labels = c("b", "c"), ncol = 2, nrow = 1),
    plot_grid(legend_bc, ncol = 1, nrow = 1),
    plot_grid(panel_d, panel_e, labels = c("d", "e"), ncol = 2, nrow = 1),
    plot_grid(panel_f, panel_g, labels = c("f", "g"), ncol = 2, nrow = 1),
    plot_grid(legend_fg, ncol = 1, nrow = 1),
    ncol = 1, nrow = 6, rel_heights = c(0.9, 1, 0.1, 1, 1, 0.175)
  )
  return(figure)
}


figure = plot_grid(
  plot_grid(panel_b, panel_c, ncol = 1, nrow = 2),
  plot_grid(legend_bc, ncol = 1, nrow = 1),
  ncol = 1, nrow = 2, rel_heights = c(1, 0.1)
)
save_figure(plot = figure, "decoding_main_graph_distance_occipito", width = 4, height = 5)

figure = plot_decoding_interval_probabilities()
save_figure(plot = figure, "decoding_main_graph_all", width = 8, height = 13)

figure = plot_decoding_main_slope_seq_prob_phase()
save_figure(plot = figure, filename = "decoding_main_slope_seq_prob_phase", width = 6, height = 4)

plot_decoding_graph_model_comparison = function() {
  panel_a = plot_decoding_main_graph_prob_model_comparison()
  panel_b = plot_decoding_main_graph_prob_aic_table()
  figure = plot_grid(panel_a, panel_b, labels = "auto", ncol = 2, nrow = 1,
                     rel_widths = c(1, 0.7))
  return(figure)
}

figure = plot_decoding_graph_model_comparison()
save_figure(plot = figure, filename = "decoding_main_graph_prob_model_comparison", width = 7, height = 4)
