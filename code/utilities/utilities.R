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
}

load_config <- function() {
  cfg <- list()
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
  paths$graphs <- file.path(paths$code, "utilities", "graphs.yml")
  # source data:
  paths$sourcedata <- file.path(paths$output, "sourcedata")
  source_path <- file.path(paths$sourcedata, "zoo-sourcedata-%s")
  paths$behav_task <- sprintf(source_path, "behavior-task")
  paths$decoding_rest <- sprintf(source_path, "decoding-rest")
  return(paths)
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

