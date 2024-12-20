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
  cfg$event_levels <- c("fixation", "stimulus", "sri", "response", "feedback", "iti")
  cfg$key_levels <- c("w", "n", "d", "z", "g", "r", "n/a")
  cfg$finger_levels <- c("index", "middle", "ring", "n/a")
  cfg$hand_levels <- c("left", "right", "n/a")
  cfg$condition_levels <- c("Training", "Single", "Sequence")
  cfg$graph_levels <- c("uni", "bi", "flat")
  # configuration parameters for resting-state decoding data:
  cfg$rest$num_trs <- c(233, 137)
  # configuration parameters for single-trial interval decoding data:
  cfg$decoding_single_interval$num_trs <- 15
  cfg$decoding_single_interval$max_trials_run <- 80
  # parameters for modeling of the sine-based response function:
  cfg$sine_params$names <- c("frequency", "amplitude", "shift", "baseline")
  cfg$sine_params$default_params <- c(0.2, 0.6, 0, 0.1)
  cfg$sine_params$lower_bounds <- c(0.01, 0.1, 0, 0)
  cfg$sine_params$upper_bounds <- c(0.5, 1, 8, 0.3)
  cfg$sine_params$num_trs <- 10
  cfg$sine_params$time <- seq(1, cfg$sine_params$num_trs, 1) - 1
  cfg$sine_params$time_eval = seq(1, cfg$sine_params$num_trs, 0.1) - 1
  cfg$sine_params$opts <- list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1.0e-8, "maxeval" = 1.0e+5)
  # colors for plotting:
  cfg$colors$class <- rev(hcl.colors(6, "Zissou 1"))
  
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
  paths$input_mri_rest <- file.path(paths$input, "decoding", "sub-*", "decoding", "*scheme-7*_time_shift-4*decoding*")
  paths$input_mri_single_interval <- c(
    file.path(path_root, "input", "decoding", "sub-*", "decoding", "*mask-vis_masking-anatomical_scheme-1*_time_shift-4*"),
    file.path(path_root, "input", "decoding", "sub-*", "decoding", "*mask-mot_masking-anatomical_scheme-3*_time_shift-4*")
  )
  paths$graphs <- file.path(paths$code, "utilities", "graphs.yml")
  # outputs:
  paths$figures <- file.path(paths$output, "figures")
  # source data:
  paths$sourcedata <- file.path(paths$output, "sourcedata")
  source_path <- file.path(paths$sourcedata, "zoo-sourcedata-%s")
  paths$behav_task <- sprintf(source_path, "behavior-task")
  paths$decoding_rest <- sprintf(source_path, "decoding-rest")
  paths$source <- list()
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
    data <- data.table::rbindlist(lapply(rds_files, readRDS))
    file_type = ".rds"
  } else if (length(tsv_files) > 0) {
    data <- data.table::rbindlist(lapply(Sys.glob(tsv_files), data.table::fread))
    file_type = ".tsv"
  } else if (length(csv_files) > 0) {
    data <- data.table::rbindlist(lapply(Sys.glob(csv_files), data.table::fread))
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

save_figure <- function(plot, filename, width, height) {
  ggsave(filename = paste0("zoo_figure_", filename, ".pdf"),
         plot = plot, device = cairo_pdf, path = paths$figures,
         scale = 1, dpi = "retina", width = width, height = height)
  ggsave(filename = paste0("zoo_figure_", filename, ".png"),
         plot = plot, device = "png", path = paths$figures,
         scale = 1, dpi = "retina", width = width, height = height)
  return(plot)
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
