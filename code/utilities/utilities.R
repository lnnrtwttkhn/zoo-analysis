find_root <- function() {
  if ( basename(here::here()) == "zoo" ) {
    path_root <- here::here("zoo-analysis")
  } else {
    path_root <- here::here()
  }
  return(path_root)
}

load_packages <- function() {
  library("here")
  library("data.table")
  library("magrittr")
  library("assertr")
  library("tidyr")
  library("dplyr")
  library("purrr")
  library("ggplot2")
  library("ggbeeswarm")
  library("lemon")
  library("broom")
  library("rstatix")
  library("cowplot")
  library("lmerTest")
  library("broom.mixed")
  library("tibble")
  library("cowplot")
}

load_config <- function() {
  cfg <- list()
  # study parameters:
  cfg$sub_exclude = c("sub-08", "sub-09", "sub-13", "sub-14", "sub-17")
  cfg$subjects = sprintf("sub-%02d", seq(1, 44))
  cfg$num_subs = length(cfg$subjects) - length(cfg$sub_exclude)
  cfg$tr <- 1.25
  cfg$num_nodes <- 6
  cfg$num_graphs <- 2
  cfg$nodes_letters <- LETTERS[1:cfg$num_nodes]
  cfg$lcctrl <- lme4::lmerControl(
    optimizer = c('bobyqa'),
    optCtrl = list(maxfun = 100000),
    calc.derivs = FALSE
  )
  cfg$sub_exclude = c("sub-08", "sub-09", "sub-13", "sub-14", "sub-17")
  cfg$subjects = sprintf("sub-%02d", seq(1, 44))
  cfg$num_subs = length(cfg$subjects) - length(cfg$sub_exclude)
  cfg$event_levels <- c("fixation", "stimulus", "sri", "response", "feedback", "iti")
  cfg$key_levels <- c("w", "n", "d", "z", "g", "r", "n/a")
  cfg$finger_levels <- c("index", "middle", "ring", "n/a")
  cfg$hand_levels <- c("left", "right", "n/a")
  cfg$condition_levels <- c("Training", "Single", "Sequence")
  cfg$graph_levels <- c("uni", "bi", "flat")
  cfg$alpha_utf <- "\u03B1"
  cfg$gamma_utf <- "\u0263"
  # set plotting colors:
  cfg$colors_probability = hcl.colors(4, "Dark Mint")
  cfg$colors_class <- rev(hcl.colors(6, "Zissou 1"))
  cfg$colors_dist <- hcl.colors(5, "Viridis")
  cfg$colors_graph <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")[c(6,7)]
  cfg$colors_sr <- hcl.colors(20, "Inferno")
  cfg$colors_conscious <- hcl.colors(5, "Plasma")[c(1, 4)]
  cfg$colors_decoding_current <- rev(hcl.colors(5, "Inferno")[c(1,3)])
  # configuration parameters for questionnaire data:
  cfg$questionnaire$num_trials <- 30
  # configuration parameters for sequence trial behavioral data:
  cfg$sequence$num_runs <- 5
  cfg$sequence$num_trials_run <- 240
  # configuration parameters for resting-state decoding data:
  cfg$rest$num_trs <- c(233, 137)
  # configuration parameters for decoding data on single trials:
  cfg$single$num_runs <- 9
  cfg$single$max_trials_run <- 60
  cfg$single$max_trials <- cfg$single$num_runs * cfg$single$max_trials_run
  cfg$single$num_trs <- 15
  # configuration parameters for single-trial interval decoding data:
  cfg$decoding_single_interval$num_trs <- 15
  cfg$decoding_single_interval$max_trials_run <- 80
  # configuration parameters for decoding data on sequence trials:
  cfg$decoding_sequence$num_trs <- 8
  cfg$decoding_sequence$num_prev <- 9
  cfg$decoding_sequence$num_next <- 10
  cfg$decoding_sequence$max_stim_evoked <- cfg$decoding_sequence$num_prev + 1 + cfg$decoding_sequence$num_next
  cfg$decoding_sequence$max_trials <- 120
  cfg$decoding_sequence$max_trials_graph <- cfg$decoding_sequence$max_trials / cfg$num_graphs
  cfg$decoding_sequence_interval$max_trials_run <- 24
  cfg$decoding_sequence_interval$max_trials_run_node <- 4
  cfg$decoding_sequence$cor_method <- "pearson"
  # parameters for modeling of the sine-based response function:
  cfg$sine_params$names <- c("frequency", "amplitude", "shift", "baseline")
  cfg$sine_params$default_params <- c(0.2, 0.6, 0, 0.1)
  cfg$sine_params$lower_bounds <- c(0.01, 0.1, 0, 0)
  cfg$sine_params$upper_bounds <- c(0.5, 1, 8, 0.3)
  cfg$sine_params$num_trs <- 10
  cfg$sine_params$time <- seq(1, cfg$sine_params$num_trs, 1) - 1
  cfg$sine_params$time_eval = seq(1, cfg$sine_params$num_trs, 0.1) - 1
  cfg$sine_params$opts <- list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1.0e-8, "maxeval" = 1.0e+5)
  # model configuration for decoding LME models:
  cfg$decoding_sequence$models <- list()
  cfg$decoding_sequence$models$suite1 <- c(
    "prob_class ~ prob_stim + (1 | id)",
    "prob_class ~ prob_stim + prob_graph + (1 | id)",
    "prob_class ~ prob_stim + dist_graph + (1 | id)",
    "prob_class ~ prob_stim + prob_graph + dist_graph + (1 | id)",
    "prob_class ~ prob_stim + prob_sr + (1 | id)",
    "prob_class ~ prob_stim + prob_sr + prob_graph + (1 | id)"
  )
  cfg$decoding_sequence$models$suite2 <- c(
    "prob_class_scale ~ prob_stim_scale + (1 | id)",
    "prob_class_scale ~ prob_stim_scale + prob_graph + (1 | id)",
    "prob_class_scale ~ prob_stim_scale + dist_graph + (1 | id)",
    "prob_class_scale ~ prob_stim_scale + prob_graph + dist_graph + (1 | id)",
    "prob_class_scale ~ prob_stim_scale + prob_sr_scale + (1 | id)",
    "prob_class_scale ~ prob_stim_scale + prob_sr_scale + prob_graph + (1 | id)"
  )
  cfg$decoding_sequence$models$suite3 <- c(
    "prob_class_scale ~ prob_stim_scale + (1 | id)",
    "prob_class_scale ~ prob_stim_scale + prob_graph + sequence_detected + (1 | id)",
    "prob_class_scale ~ prob_stim_scale + dist_graph + sequence_detected + (1 | id)",
    "prob_class_scale ~ prob_stim_scale + prob_graph + sequence_detected + dist_graph + (1 | id)",
    "prob_class_scale ~ prob_stim_scale + prob_sr_scale + sequence_detected + (1 | id)",
    "prob_class_scale ~ prob_stim_scale + prob_sr_scale + prob_graph + sequence_detected + (1 | id)"
  )
  cfg$decoding_sequence$models$suite4 <- c(
    "logit(prob_class) ~ prob_stim_norm + (1 | id)",
    "logit(prob_class) ~ prob_stim_norm + prob_graph + (1 | id)",
    "logit(prob_class) ~ prob_stim_norm + prob_sr + (1 | id)",
    "logit(prob_class) ~ prob_stim_norm + prob_sr + prob_graph + (1 | id)"
  )
  cfg$decoding_sequence$models$suite5 <- c(
    "prob_class_scale ~ prob_stim_scale + (1 | id)",
    "prob_class_scale ~ prob_stim_scale + prob_graph + (1 | id)",
    "prob_class_scale ~ prob_stim_scale + prob_sr_scale + (1 | id)",
    "prob_class_scale ~ prob_stim_scale + prob_sr_scale + prob_graph + (1 | id)"
  )
  cfg$decoding_sequence$models$suite6 <- c(
    "prob_class ~ prob_stim + (1 | id)",
    "prob_class ~ prob_stim + prob_graph + (1 | id)",
    "prob_class ~ prob_stim + prob_sr + (1 | id)",
    "prob_class ~ prob_stim + prob_sr + prob_graph + (1 | id)"
  )
  cfg$decoding_sequence$models$suite7 <- c(
    # "prob_class ~ prob_stim + (prob_stim | id)",
    # "prob_class ~ prob_stim + prob_graph + (prob_stim + prob_graph | id)",
    # "prob_class ~ prob_stim + prob_sr + (prob_stim + prob_sr | id)",
    "prob_class ~ prob_stim + prob_sr + prob_graph + (prob_sr | id)"
  )
  cfg$decoding_sequence$models$suite8 <- c(
    "prob_class ~ prob_stim",
    "prob_class ~ prob_stim + prob_graph",
    "prob_class ~ prob_stim + prob_sr",
    "prob_class ~ prob_stim + prob_sr + prob_graph"
  )
  cfg$decoding_sequence$models$model_names <- c(
    "Stimulus",
    "1-step",
    "SR (multi-step)",
    "SR + 1-step"
  )
  cfg$decoding_sequence$models$model_formulas <- cfg$decoding_sequence$models$suite6
  return(cfg)
}

create_paths <- function() {
  path_root <- find_root()
  datetime <- strftime(Sys.time(), format = "%y-%m-%d_%H-%M-%S")
  paths <- list()
  paths$code <- file.path(path_root, "code")
  paths$input <- file.path(path_root, "input")
  paths$output <- file.path(path_root, "output")
  paths$input_behavior <- file.path(paths$input, "bids", "*", "*", "func", "*events")
  paths$input_demographics <- file.path(paths$input, "bids", "participants")
  paths$input_questionnaire <- file.path(paths$input, "bids", "*", "ses-02", "beh", "*beh.tsv")
  paths$input_sr_modeling <- file.path(path_root, "input", "sr-modeling", "modeling", "sub-*-sr.csv")
  paths$input_sr_base_modeling <- file.path(path_root, "input", "sr-modeling", "modeling", "sub-*-sr_base.csv")
  paths$input_mri_rest <- file.path(paths$input, "decoding", "sub-*", "decoding", "*scheme-7*_time_shift-4*decoding*")
  paths$input_mri_single_peak <- c(
    file.path(paths$input, "decoding", "sub-*", "decoding", "*scheme-0*_time_shift-4*decoding*"),
    file.path(paths$input, "decoding", "sub-*", "decoding", "*scheme-2*_time_shift-4*decoding*"),
    file.path(paths$input, "decoding", "sub-*", "decoding", "*scheme-4*_time_shift-4*decoding*"),
    file.path(paths$input, "decoding", "sub-*", "decoding", "*scheme-5*_time_shift-4*decoding*")
  )
  paths$input_mri_single_interval <- c(
    file.path(path_root, "input", "decoding", "sub-*", "decoding", "*mask-vis_masking-anatomical_scheme-1*_time_shift-4*"),
    file.path(path_root, "input", "decoding", "sub-*", "decoding", "*mask-mot_masking-anatomical_scheme-3*_time_shift-4*")
  )
  paths$input_mri_sequence <- c(
    file.path(paths$input, "decoding", "sub-*", "decoding", "*scheme-6*_time_shift-4*decoding*"),
    file.path(paths$input, "decoding", "sub-*", "decoding", "*scheme-8*_time_shift-4*decoding*")
  )
  paths$graphs <- file.path(paths$code, "utilities", "graphs.yml")
  # outputs:
  paths$figures <- file.path(paths$output, "figures")
  # source data:
  paths$sourcedata <- file.path(paths$output, "sourcedata")
  source_path <- file.path(paths$sourcedata, "zoo-sourcedata-%s")
  paths$source <- list()
  paths$source$behavior_task <- sprintf(source_path, "behavior_task")
  # paths: analysis of demographics data:
  paths$source$demographics <- sprintf(source_path, "demographics")
  # paths: analysis of behavioral data from sequence trials:
  paths$source$behavior_sequence_run <- sprintf(source_path, "behavior_sequence_run")
  paths$source$behavior_sequence_run_stat <- sprintf(source_path, "behavior_sequence_run_stat")
  paths$source$behavior_sequence_run_glm <- sprintf(source_path, "behavior_sequence_run_glm")
  paths$source$behavior_sequence_halfrun_glm <- sprintf(source_path, "behavior_sequence_halfrun_glm")
  paths$source$behavior_sequence_onestep <- sprintf(source_path, "behavior_sequence_onestep")
  paths$source$behavior_sequence_onestep_stat <- sprintf(source_path, "behavior_sequence_onestep_stat")
  paths$source$behavior_sequence_onestep_run <- sprintf(source_path, "behavior_sequence_onestep_run")
  paths$source$behavior_sequence_onestep_run_glm <- sprintf(source_path, "behavior_sequence_onestep_run_glm")
  paths$source$behavior_sequence_graph <- sprintf(source_path, "behavior_sequence_graph")
  paths$source$behavior_sequence_graph_run <- sprintf(source_path, "behavior_sequence_graph_run")
  paths$source$behavior_sequence_previous <- sprintf(source_path, "behavior_sequence_previous")
  paths$source$behavior_sequence_previous_dist <- sprintf(source_path, "behavior_sequence_previous_dist")
  # paths: analysis of questionnaire data:
  paths$source$questionnaire <- sprintf(source_path, "questionnaire")
  paths$source$questionnaire_seq_detect <- sprintf(source_path, "questionnaire_seq_detect")
  paths$source$questionnaire_seq_detect_order <- sprintf(source_path, "questionnaire_seq_detect_order")
  paths$source$questionnaire_seq_when <- sprintf(source_path, "questionnaire_seq_when")
  paths$source$questionnaire_seq_when_order <- sprintf(source_path, "questionnaire_seq_when_order")  
  paths$source$questionnaire_prob_ratings <- sprintf(source_path, "questionnaire_prob_ratings")
  paths$source$questionnaire_prob_ratings_accuracy <- sprintf(source_path, "questionnaire_prob_ratings_accuracy")
  paths$source$questionnaire_prob_ratings_accuracy_stat <- sprintf(source_path, "questionnaire_prob_ratings_accuracy_stat")
  paths$source$questionnaire_prob_ratings_onestep <- sprintf(source_path, "questionnaire_prob_ratings_onestep")
  paths$source$questionnaire_prob_ratings_correlation <- sprintf(source_path, "questionnaire_prob_ratings_correlation")
  paths$source$questionnaire_prob_ratings_correlation_stat <- sprintf(source_path, "questionnaire_prob_ratings_correlation_stat")
  # paths: analysis of behavioral data from successor representation modeling (grid search):
  paths$source$behavior_sr_grid <- sprintf(source_path, "behavior_sr_grid")
  paths$source$behavior_sr_grid_seq <- sprintf(source_path, "behavior_sr_grid_seq")
  paths$source$behavior_sr_grid_seq_lme <- sprintf(source_path, "behavior_sr_grid_seq_lme")
  paths$source$behavior_sr_grid_seq_graph_lme <- sprintf(source_path, "behavior_sr_grid_seq_graph_lme")
  paths$source$behavior_sr_grid_seq_graph_order_lme <- sprintf(source_path, "behavior_sr_grid_seq_graph_order_lme")
  paths$source$behavior_sr_grid_seq_block_lme <- sprintf(source_path, "behavior_sr_grid_seq_block_lme")
  paths$source$behavior_sr_grid_seq_mean <- sprintf(source_path, "behavior_sr_grid_seq_mean")
  paths$source$behavior_sr_grid_seq_mean_lme <- sprintf(source_path, "behavior_sr_seq_mean_lme")
  # paths: analysis of behavioral data from successor representation model fitting:
  paths$source$behavior_sr_fit_parameters <- sprintf(source_path, "behavior_sr_fit_parameters")
  paths$source$behavior_sr_fit_starting_values <- sprintf(source_path, "behavior_sr_fit_starting_values")
  paths$source$behavior_sr_fit_parameter_dispersion <- sprintf(source_path, "behavior_sr_fit_parameter_dispersion")
  paths$source$behavior_sr_fit_parameter_distribution <- sprintf(source_path, "behavior_sr_fit_parameter_distribution")
  paths$source$behavior_sr_fit_model_comparison <- sprintf(source_path, "behavior_sr_fit_model_comparison")
  paths$source$behavior_sr_fit_model_comparison_stat <- sprintf(source_path, "behavior_sr_fit_model_comparison_stat")
  paths$source$behavior_sr_fit_parameter_conscious <- sprintf(source_path, "behavior_sr_fit_parameter_conscious")
  paths$source$behavior_sr_fit_parameter_order <- sprintf(source_path, "behavior_sr_fit_parameter_order")
  paths$source$behavior_sr_fit_suprise_effect <- sprintf(source_path, "behavior_sr_fit_suprise_effect")
  paths$source$behavior_sr_fit_sr_matrices <- sprintf(source_path, "behavior_sr_fit_sr_matrices")
  paths$source$behavior_sr_fit_sr_matrices_plot <- sprintf(source_path, "behavior_sr_fit_sr_matrices_plot")
  paths$source$behavior_sr_fit_sr_matrices
  paths$source$behavior_sr_fit_response_time_alpha <- sprintf(source_path, "behavior_sr_fit_response_time_alpha")
  paths$source$behavior_sr_fit_response_time_alpha_stat <- sprintf(source_path, "behavior_sr_fit_response_time_alpha_stat")
  paths$source$behavior_sr_fit_response_time_glm <- sprintf(source_path, "behavior_sr_fit_response_time_glm")
  paths$source$behavior_sr_fit_response_time_onestep <- sprintf(source_path, "behavior_sr_fit_response_time_onestep")
  paths$source$behavior_sr_fit_response_time_onestep_run <- sprintf(source_path, "behavior_sr_fit_response_time_onestep_run")
  paths$source$behavior_sr_fit_response_time_onestep_diff <- sprintf(source_path, "behavior_sr_fit_response_time_onestep_diff")
  paths$source$behavior_sr_fit_response_time_onestep_run_stat <- sprintf(source_path, "behavior_sr_fit_response_time_onestep_run_stat")
  paths$source$behavior_sr_fit_parameter_recovery  <- sprintf(source_path, "behavior_sr_fit_parameter_recovery")
  paths$source$behavior_sr_fit_parameter_recovery_corr <- sprintf(source_path, "behavior_sr_fit_parameter_recovery_corr")
  paths$source$behavior_sr_fit_parameter_recovery_corr_stat <- sprintf(source_path, "behavior_sr_fit_parameter_recovery_corr_stat")
  
  paths$decoding_rest <- sprintf(source_path, "decoding-rest")
  # source data for decoding on single trials (peaks):
  paths$source$decoding_single_peak <- sprintf(source_path, "decoding_single_peak")
  paths$source$decoding_single_peak_accuracy <- sprintf(source_path, "decoding_single_peak_accuracy")
  paths$source$decoding_single_peak_accuracy_mean <- sprintf(source_path, "decoding_single_peak_accuracy_mean")
  paths$source$decoding_single_peak_accuracy_run <- sprintf(source_path, "decoding_single_peak_accuracy_run")
  # source data for decoding on single trials (interval):
  paths$source$decoding_single_interval <- sprintf(source_path, "decoding_single_interval")
  paths$source$decoding_single_interval_trial <- sprintf(source_path, "decoding_recall_interval_trial")
  paths$source$decoding_single_interval_trial_mean <- sprintf(source_path, "decoding_recall_interval_trial_mean")
  paths$source$decoding_single_interval_node <- sprintf(source_path, "decoding_recall_interval_node")
  paths$source$decoding_single_interval_node_all <- sprintf(source_path, "decoding_recall_interval_node_all")
  paths$source$decoding_single_interval_sine_fit <- sprintf(source_path, "decoding_recall_interval_sine_fit")
  paths$source$decoding_single_interval_sine_fit_mean <- sprintf(source_path, "decoding_recall_interval_sine_fit_mean")
  paths$source$decoding_single_interval_sine_fit_mean_eval <- sprintf(source_path, "decoding_recall_interval_sine_fit_mean_eval")
  paths$source$decoding_single_interval_sine_fit_eval <- sprintf(source_path, "decoding_recall_interval_sine_fit_eval")
  paths$source$decoding_single_interval_sine_fit_eval_mean <- sprintf(source_path, "decoding_recall_interval_sine_fit_eval_mean")
  paths$source$decoding_single_interval_sine_fit_sub <- sprintf(source_path, "decoding_recall_interval_sine_fit_sub")
  # source data for decoding on sequence trials:
  paths$source$decoding_main <- sprintf(source_path, "decoding_main")
  paths$source$decoding_main_current_mean <- sprintf(source_path, "decoding_main_current_mean")
  paths$source$decoding_main_current_stat <- sprintf(source_path, "decoding_main_current_stat")
  paths$source$decoding_main_current_interval <- sprintf(source_path, "decoding_main_current_interval")
  paths$source$decoding_main_sine <- sprintf(source_path, "decoding_main_sine")
  paths$source$decoding_main_sine_mean <- sprintf(source_path, "decoding_main_sine_mean")
  paths$source$decoding_main_seq_prev <- sprintf(source_path, "decoding_main_seq_prev")
  paths$source$decoding_main_stim_modeled <- sprintf(source_path, "decoding_main_stim_modeled")
  paths$source$decoding_main_model_input <- sprintf(source_path, "decoding_main_model_input")
  paths$source$decoding_main_model_raw_prob <- sprintf(source_path, "decoding_main_model_raw_prob")
  paths$source$decoding_main_model_results <- sprintf(source_path, "decoding_main_model_results")
  paths$source$decoding_main_model_results_run <- sprintf(source_path, "decoding_main_model_results_run")
  paths$source$decoding_main_model_run_trs <- sprintf(source_path, "decoding_main_model_results_trs")
  paths$source$decoding_main_model_residuals <- sprintf(source_path, "decoding_main_model_residuals")
  paths$source$decoding_main_model_betas <- sprintf(source_path, "decoding_main_model_betas")
  paths$source$decoding_main_model_betas_id <- sprintf(source_path, "decoding_main_model_betas_id")
  paths$source$decoding_main_model_betas_behav <- sprintf(source_path, "decoding_main_model_betas_behav")
  paths$source$decoding_main_model_betas_behav_cor <- sprintf(source_path, "decoding_main_model_betas_behav_cor")
  paths$source$decoding_main_model_betas_behav_cor_mean <- sprintf(source_path, "decoding_main_model_betas_behav_cor_mean")
  paths$source$decoding_main_model_prediction <- sprintf(source_path, "decoding_main_model_prediction")
  paths$source$decoding_main_model_residuals_mean <- sprintf(source_path, "decoding_main_model_residuals_mean")
  paths$source$decoding_main_model_residuals_slope <- sprintf(source_path, "decoding_main_model_residuals_slope")
  paths$source$decoding_main_model_residuals_slope_stat <- sprintf(source_path, "decoding_main_model_residuals_slope_stat")
  paths$source$decoding_main_model_comp <- sprintf(source_path, "decoding_main_model_comp")
  paths$source$decoding_main_model_diff_run <- sprintf(source_path, "decoding_main_model_diff_run")
  paths$source$decoding_main_model_no_evoked <- sprintf(source_path, "decoding_main_model_no_evoked")
  paths$source$decoding_main_model_no_evoked_phase <- sprintf(source_path, "decoding_main_model_no_evoked_phase")
  paths$source$decoding_main_model_no_evoked_late_trs <- sprintf(source_path, "decoding_main_model_no_evoked_late_trs")
  paths$source$decoding_main_model_no_evoked_num_class_trials <- sprintf(source_path, "decoding_main_model_no_evoked_num_class_trials")
  paths$source$decoding_main_model_no_evoked_num_dist_trials <- sprintf(source_path, "decoding_main_model_no_evoked_num_dist_trials")
  paths$source$decoding_main_model_no_evoked_slope <- sprintf(source_path, "decoding_main_model_no_evoked_slope")
  paths$source$decoding_main_model_no_evoked_slope_stat <- sprintf(source_path, "decoding_main_model_no_evoked_slope_stat")
  return(paths)
}

check_ci <- function() {
  status_report(text = Sys.getenv("CI"))
  return_code <- Sys.getenv("CI")
  if (return_code == "true") {
    system2("echo", args = c("Running in CI environment"))
  }
  return(return_code)
}

check_tardis <- function() {
  return_code = FALSE
  nodename = Sys.info()['nodename']
  if (nodename == "master") {
    system2("echo", args = c("Running on MPIB cluster Tardis"))
    return_code = TRUE
  } else {
    status_report("NOT running on MPIB cluster Tardis")
  }
  return(return_code)
}

check_singularity <- function() {
  return_code = FALSE
  path_container = Sys.getenv("SINGULARITY_CONTAINER")
  if (path_container == "") {
    status_report("NOT running inside Singularity container")
  } else if (grepl(".singularity/zoo_latest.sif", path_container, fixed = TRUE)) {
    status_report("Running inside Singularity container")
    return_code = TRUE
  }
  return(return_code)
}

check_datalad <- function() {
  return_code <- system2("datalad", args = c("--version"))
  if (return_code != 0) {
    warning("Please check if datalad is installed!")
  }
  return(return_code)
}

get_data <- function(paths) {
  if (check_ci() == "true") {
    return(NULL)
  }
  if (check_singularity() == TRUE) {
    return(NULL)
  }
  if (check_datalad() != 0) {
    return(NULL)
  }
  status_report(text = "retrieving data ...")
  # datalad get commands are only needed locally, not in CI
  for (path in paths) {
    if (stringr::str_detect(string = path, pattern = "input")) {
      if (check_tardis() == TRUE) {
        setwd(path_root)
        path = stringr::str_replace(path, paste0(here::here(), "/"), "")
      }
      status_report(text = paste("retrieving", path, "..."))
      system2("datalad", args = c("get", "--jobs", 8, path), stdout = "", stderr = "")
      status_report(text = "data retrieval was successful!")
      status_report(text = "------------------------------")
    }
  }
}

load_data <- function(paths_input) {
  if (all(tools::file_ext(paths_input) == "")) {
    paths_input <- Sys.glob(paste0(paths_input, ".*"))
  } else {
    paths_input <- Sys.glob(paste0(paths_input, "*"))
  }
  rds_files <- filter_paths(paths = paths_input, pattern = ".rds")
  tsv_files <- filter_paths(paths = paths_input, pattern = ".tsv")
  csv_files <- filter_paths(paths = paths_input, pattern = ".csv")
  if (length(rds_files) > 0) {
    data <- data.table::rbindlist(lapply(rds_files, readRDS), fill = TRUE)
    file_type = ".rds"
  } else if (length(tsv_files) > 0) {
    data <- data.table::rbindlist(lapply(Sys.glob(tsv_files), data.table::fread), fill = TRUE)
    file_type = ".tsv"
  } else if (length(csv_files) > 0) {
    data <- data.table::rbindlist(lapply(Sys.glob(csv_files), data.table::fread), fill = TRUE)
    file_type = ".csv"
  }
  message <- sprintf("successfully imported data from %s", file_type)
  status_report(text = message)
  return(data)
}

save_data <- function(df, path) {
  if (is.data.frame(df) == FALSE) {
    stop("input is not a data.frame!")
  }
  if (is.null(path)) {
    stop("no path provided!")
  }
  data.table::fwrite(setDT(df), paste0(path, ".tsv"), row.names = FALSE, sep = "\t")
  saveRDS(df, paste0(path, ".rds"))
  message <- sprintf("successfully exported %s", path)
  status_report(text = message)
  return(df)
}

save_figure <- function(plot, filename, width, height) {
  ggsave(filename = paste0("zoo_figure_", filename, ".pdf"),
         plot = plot, device = cairo_pdf, path = paths$figures,
         scale = 1, dpi = "retina", width = width, height = height)
  ggsave(filename = paste0("zoo_figure_", filename, ".png"),
         plot = plot, device = "png", path = paths$figures,
         scale = 1, dpi = "retina", width = width, height = height)
  return(plot)
}

filter_paths <- function(paths, pattern) {
  paths_filtered <- paths[unlist(lapply(paths, function(x) grepl(pattern, x)))]
  return(paths_filtered)
}

status_report <- function(text) {
  stopifnot(is.character(text))
  system2("echo", args = c(text))
}

load_graphs <- function(paths) {
  graphs <- yaml::read_yaml(file = paths$graphs) %>%
    plyr::ldply(data.frame, .id = "node_previous") %>%
    gather(key, value, -node_previous) %>%
    separate(key, into = c("node", "var"), extra = "merge") %>%
    spread(var, value) %>%
    setDT(.) %>%
    .[, node_previous := as.factor(as.character(node_previous))] %>%
    .[, node := as.factor(as.character(node))] %>%
    .[, dist_bi := as.numeric(dist_bi)] %>%
    .[, dist_flat := as.numeric(dist_flat)] %>%
    .[, dist_uni := as.numeric(dist_uni)] %>%
    .[, prob_bi := as.numeric(prob_bi)] %>%
    .[, prob_flat := as.numeric(prob_flat)] %>%
    .[, prob_uni := as.numeric(prob_uni)]
  return(graphs)
}

label_fill <- function(original, offset = 0, mod = 2, fill = "") {
  # this function can be used to generate axis labels that omit e.g.,
  # every second label. Solution was taken from [here](https://bit.ly/2VycSy0).
  ii <- as.logical((seq_len(length(original)) - 1 + offset) %% mod)
  original[ii] <- fill
  return(original)
}

theme_zoo <- function() {
  theme_font <- "Helvetica"
  theme_color <- "black"
  size_factor <- 1.1
  theme_out <- theme() +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(panel.border = element_blank()) +
    theme(axis.title = element_text(family = theme_font, color = theme_color)) +
    theme(axis.text = element_text(family = theme_font, color = theme_color)) +
    theme(axis.ticks = element_line(color = theme_color)) +
    theme(axis.line = element_line(color = theme_color)) +
    theme(strip.text = element_text(margin = margin(b = 3, t = 3, r = 3, l = 3))) +
    theme(legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    theme(axis.text = element_text(size = rel(size_factor))) +
    theme(axis.title = element_text(size = rel(size_factor))) +
    theme(legend.text = element_text(size = rel(size_factor))) +
    theme(legend.title = element_text(size = rel(size_factor))) +
    theme(strip.text = element_text(size = rel(size_factor)))
  return(theme_out)
}

get_ttest <- function(dt_input, ttest_cfg) {
  if (ttest_cfg$paired == TRUE) {
    pair_factors <- unique(droplevels(dt_input[[ttest_cfg$rhs]]))
    stopifnot(length(pair_factors) == 2)
    lhs <- as.numeric(dt_input[[ttest_cfg$lhs]])
    rhs <- dt_input[[ttest_cfg$rhs]]
    ttest <- broom::tidy(stats::t.test(
      lhs[rhs == pair_factors[1]],
      lhs[rhs == pair_factors[2]],
      paired = TRUE,
      alternative = ttest_cfg$alternative
    ))
  } else if (ttest_cfg$paired == FALSE) {
    ttest <- broom::tidy(stats::t.test(
      formula = as.formula(sprintf("%s ~ %s", ttest_cfg$lhs, ttest_cfg$rhs)),
      data = droplevels(dt_input),
      mu = ttest_cfg$mu,
      alternative = ttest_cfg$alternative
    ))
  }
  cohensd <- rstatix::cohens_d(
    data = droplevels(dt_input),
    formula = as.formula(sprintf("%s ~ %s", ttest_cfg$lhs, ttest_cfg$rhs)),
    paired = ttest_cfg$paired,
    mu = ttest_cfg$mu
  )
  mean_value <- mean(dt_input$value)
  std_value <- round(sd(dt_input$value), 2)
  dt_output <- cbind(ttest, cohensd, mean_value, std_value)
  return(dt_output)
}

get_pvalue_adjust <- function(dt_input, ttest_cfg = NA) {
  dt_output <- dt_input %>%
    setDT(.) %>%
    .[, num_tests := .N] %>%
    .[, estimate_round := round(estimate, 2)] %>%
    .[, statistic_round := round(statistic, 2)] %>%
    .[, conf.low_round := round(conf.low, 2)] %>%
    .[, conf.low_latex := ifelse(conf.low == -Inf, "-\\infty", as.character(conf.low_round))] %>%
    .[, conf.high_round := round(conf.high, 2)] %>%
    .[, conf.high_latex := ifelse(conf.high == Inf, "+\\infty", as.character(conf.high_round))] %>%
    .[, effsize_round := tryCatch(round(effsize, 2), error=function(err) NA)] %>%
    .[, p.value_round := round(p.value, 2)] %>%
    .[, p.value_round_label := format_pvalue(p.value_round)] %>%
    .[, p.value_significance := ifelse(p.value < 0.05, "*", "n.s.")] %>%
    .[, p.value_adjust := stats::p.adjust(p.value, method = ttest_cfg$adjust_method, n = .N)] %>%
    .[, p.value_adjust_round := as.numeric(round(p.value_adjust, 2))] %>%
    .[, p.value_adjust_round_label := format_pvalue(p.value_adjust_round)] %>%
    .[, p.value_adjust_significance := ifelse(p.value_adjust < 0.05, "*", "n.s.")] %>%
    .[, adjust_method := ttest_cfg$adjust_method] %>%
    .[, adjust_method := dplyr::case_when(
      adjust_method == "fdr" ~ "FDR",
      adjust_method == "bonferroni" ~ "Bonferroni"
    )] %>%
    .[, alternative := dplyr::case_when(
      alternative == "two.sided" ~ "two-sided"
    )] %>%
    # adjust latex reporting depending on the type of test:
    .[grepl("correlation", method), report_latex := paste(
      sprintf("$r(%d) = %.2f$,", parameter, estimate),
      sprintf("$p %s$,", p.value_adjust_round_label),
      sprintf("%s %s,", alternative, method),
      sprintf("$p$-values %s-corrected", adjust_method)
    )] %>%
    .[grepl("t-test", method), report_latex := paste(
      sprintf("$M = %.2f$,", estimate),
      sprintf("$SD = %.2f$,", std_value),
      sprintf("$t_{%d} = %.2f$,", parameter, statistic),
      sprintf("CI [$%s$, $%s$],", conf.low_latex, conf.high_latex),
      sprintf("$p %s$", p.value_adjust_round_label),
      sprintf("$d = %.2f,$", effsize),
      sprintf("%s %s,", alternative, method),
      sprintf("$p$-values %s-corrected", adjust_method)
    )]
  return(dt_output)
}

format_pvalue <- function(pvalue, add_p = FALSE) {
  pvalue_format <- format.pval(
    pvalue,
    digits = 1,
    eps = 0.001,
    nsmall = 2,
    scientific = FALSE,
    na.form = "NA")
  pvalue_format <- ifelse(pvalue_format == "<0.001", "< 0.001", paste("=", pvalue_format))
  if (add_p == TRUE) {
    pvalue_format <- paste("p", pvalue_format)
  }
  return(pvalue_format)
}

run_glm <- function(formula, data, cfg, tidy = TRUE) {
  model = stats::glm(
    formula = as.formula(formula),
    data = data,
    subset = NULL,
    weights = NULL,
    na.action = na.omit,
    offset = NULL
  )
  # return a tidy model if TRUE
  if (tidy == TRUE) {
    model = broom::tidy(model)
  }
  return(model)
}

get_lme <- function(formulas, data, cfg) {
  models_output <- lapply(seq_along(formulas), function(i) {
    model <- lmerTest::lmer(
      formula = as.formula(formulas[[i]]),
      control = cfg$lcctrl,
      data = data,
      subset = NULL,
      weights = NULL,
      na.action = na.omit,
      offset = NULL,
      REML = TRUE
    )
    tidy_output <- broom::tidy(stats::anova(model)) %>%
      setDT(.) %>%
      .[, p.value_round := round(p.value, 2)] %>%
      .[, p.value_round_label := format_pvalue(p.value_round)] %>%
      .[, p.value_significance := ifelse(p.value < 0.05, "*", "n.s.")] %>%
      .[, latex := sprintf(
        fmt = "$F_{%#.2f, %#.2f} = %#.2f$, $p %s$",
        round(NumDF, 2), round(DenDF, 2), round(statistic, 2), p.value_round_label
      )]
    tidy_output$model_formula <- formulas[[i]]
    tidy_output$model_number <- i
    return(tidy_output)
  })
  results_df <- bind_rows(models_output)
  return(results_df)
}

run_lmer <- function(formula, data, cfg, tidy = TRUE) {
  model <- lmerTest::lmer(
    formula = as.formula(formula),
    data = data,
    subset = NULL,
    weights = NULL,
    REML = FALSE,
    na.action = na.omit,
    offset = NULL,
    control = cfg$lcctrl
  )
  # return a tidy model if TRUE
  if (tidy == TRUE) {
    model <- broom::tidy(model)
  }
  return(model)
}

calc_bits = function(probability) {
  bits = -log(probability, base = 2)
  return(bits)
}

sr_fun <- function(node_previous, node, alpha, gamma, fig = FALSE){
  num_nodes = 6
  node_letters = LETTERS[1:num_nodes]
  num_transitions = length(node_previous)
  counter = num_transitions - 1
  # pre-allocate an empty vector to hold the bits:
  bits = rep(NA, counter)
  # pre-allocate the successor matrix with baseline expectation
  # baseline expectation could also be zero
  expectation = 1 / num_nodes ^ 2
  sr = matrix(expectation, num_nodes, num_nodes)
  # add letters to the successor matrix:
  colnames(sr) = rownames(sr) = LETTERS[1:6]
  # loop through all trials (transitions):
  for (i in 2:(counter + 1)) {
    # determine the previous node and the current node:
    node_x = which(node_previous[i] == node_letters)
    node_y = which(node[i] == node_letters)
    # normalize the successor matrix to express it in probabilities:
    sr_norm = sr / matrix(rowSums(sr), num_nodes, num_nodes)
    probability = sr_norm[node_x, node_y]
    bits[i - 1] = calc_bits(probability = probability)
    # update the successor representation:
    occupancy = rep(0, num_nodes)
    occupancy[node_y] = 1
    sr[node_x,] = sr[node_x,] + alpha * (occupancy + gamma * sr[node_y,] - sr[node_x,])
    if (fig == TRUE) {
      dev.set(dev.prev())
      image(sr, main = i, zlim = c(0, 1))
      Sys.sleep(0.005)
    }
  }
  bits = c(NA, bits)
  return(bits)
}

sr_mat_fun = function(node_previous, node, alpha, gamma){
  num_nodes = 6
  node_letters = LETTERS[1:num_nodes]
  num_transitions = length(node_previous)
  counter = num_transitions - 1
  # pre-allocate an empty vector to hold the bits:
  bits = rep(NA, counter)
  # pre-allocate an empty vector to hold the SR matrix
  sr_mat = list()
  # pre-allocate the successor matrix with baseline expectation
  # baseline expectation could also be zero
  expectation = 1 / num_nodes ^ 2
  sr = matrix(expectation, num_nodes, num_nodes)
  # add letters to the successor matrix:
  colnames(sr) = rownames(sr) = LETTERS[1:6]
  # loop through all trials (transitions):
  for (i in 2:(counter + 1)) {
    # determine the previous node and the current node:
    node_x = which(node_previous[i] == node_letters)
    node_y = which(node[i] == node_letters)
    # normalize the successor matrix to express it in probabilities:
    sr_norm = sr / matrix(rowSums(sr), num_nodes, num_nodes)
    sr_tmp = as.data.frame(sr_norm, row.names = TRUE)
    sr_tmp = rownames_to_column(sr_tmp, "previous")
    sr_mat[[i - 1]] <- list(sr_tmp)
    probability = sr_norm[node_x, node_y]
    bits[i - 1] = calc_bits(probability = probability)
    # update the successor representation:
    occupancy = rep(0, num_nodes)
    occupancy[node_y] = 1
    sr[node_x,] = sr[node_x,] + alpha * (occupancy + gamma * sr[node_y,] - sr[node_x,])
  }
  sr_mat = c(list(sr_mat[[1]]), sr_mat)
  bits = c(NA, bits)
  return(list(sr_mat))
}

label_fill <- function(original, offset = 0, mod = 2, fill = "") {
  # this function can be used to generate axis labels that omit e.g.,
  # every second label. Solution was taken from [here](https://bit.ly/2VycSy0).
  ii <- as.logical((seq_len(length(original)) - 1 + offset) %% mod)
  original[ii] <- fill
  return(original)
}

codeblock = function(text_list) {
  codeblock = paste(c(
    paste0("```"),
    text_list,
    "```"
  ), collapse = "\n")
  return(codeblock)
}

report_lme_stats <- function(num_df, den_df, f_value, p_value) {
  latex <- sprintf(
    fmt = "$F_{%#.2f, %#.2f} = %#.2f$, $p %s$",
    round(num_df, 2), round(den_df, 2), round(f_value, 2), format_pvalue(p_value)
  )
  markdown <- sprintf(
    fmt = "*F*<sup>%#.2f, %#.2f</sup> = %#.2f, *p* %s",
    round(num_df, 2), round(den_df, 2), round(f_value, 2), format_pvalue(p_value)
  )
  return(list(latex = latex, markdown = markdown))
}

run_lme <- function(lme_formula, lme_data) {
  model <- lmerTest::lmer(
    formula = as.formula(lme_formula), 
    data = lme_data,
    control = cfg$lcctrl,
    subset = NULL,
    weights = NULL,
    na.action = na.omit,
    offset = NULL,
    REML = TRUE
  )
  # summary(model)
  model_stat <- broom::tidy(stats::anova(model))
  report_lme_model <- report_lme_stats(
    num_df = model_stat$NumDF,
    den_df = model_stat$DenDF,
    f_value = model_stat$statistic,
    p_value = model_stat$p.value
  )
  cat(codeblock(text_list = report_lme_model$latex))
}
  
sine_truncated <- function(params, time) {
  if (!is.list(params)) {
    params <- as.list(params)
    names(params) = c("frequency", "amplitude", "shift", "baseline")
  }
  y <- params$amplitude/2 * sin(2*pi*params$frequency*time - 2*pi*params$frequency*params$shift - 0.5*pi) + params$baseline + params$amplitude/2
  # flatten response function after one cycle:
  y[time < (params$shift)] <- params$baseline
  y[time > (params$shift + 1/params$frequency)] <- params$baseline  
  return(y) 
}

sine_truncated_eval <- function(params, time, data) {
  y <- sine_truncated(params, time)
  SSE <- sum((data - y)^2)
  return(SSE)
}

get_class_dist <- function(trial_run, onset, node) {
  num_letters <- 6
  dist_time <- matrix(NA, length(node), num_letters, dimnames = list(node, LETTERS[1:num_letters]))
  dist_trial <- matrix(NA, length(node), num_letters, dimnames = list(node, LETTERS[1:num_letters]))
  for (i in seq_along(node)) {
    element <- node[i]
    prev_seq <- node[1:i]
    for (j in seq(num_letters)) {
      letter_pos <- which(prev_seq == LETTERS[j])
      if (length(letter_pos) != 0) {
        dist_trial[i, j] <- max(letter_pos - i)
        # dist_time[i, j] <- onset[i] - onset[max(letter_pos)]
        dist_time[i, j] <- onset[max(letter_pos)]
      }
    }
  }
  dt_trial <- data.table(node = rownames(dist_trial), dist_trial) %>%
    .[, trial_run := trial_run] %>%
    melt(id.vars = c("trial_run", "node"), variable.name = "class", value.name = "class_dist_trial")
  dt_time <- data.table(node = rownames(dist_time), dist_time) %>%
    .[, trial_run := trial_run] %>%
    melt(id.vars = c("trial_run", "node"), variable.name = "class", value.name = "class_dist_onset")
  dt <- merge.data.table(x = dt_trial, y = dt_time, by = c("trial_run", "node", "class")) %>%
    setorder(trial_run, node, class)
  return(dt)
}

