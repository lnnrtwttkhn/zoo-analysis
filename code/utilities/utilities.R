find_root <- function() {
  if ( basename(here::here()) == "zoo" ) {
    path_root <- here::here("zoo-analysis")
  } else {
    path_root <- here::here()
    # root path should be an empty string when it's .Platform$file.sep
    # relevant for high-performance computing environments
    if (path_root == .Platform$file.sep) {
      path_root = ""
    }
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
  library("optparse")
  library("nloptr")
  library("lme4")
  library("comprehenr")
  library("combinat")
  library("lomb")
  library("forcats")
  library("tibble")
  library("ggnewscale")
}

load_config <- function() {
  cfg <- list()
  cfg$tr <- 1.25
  cfg$num_nodes <- 6
  cfg$num_graphs <- 2
  cfg$nodes_letters <- LETTERS[1:cfg$num_nodes]
  cfg$sub_exclude = c("sub-08", "sub-09", "sub-13", "sub-14", "sub-17")
  cfg$subjects = sprintf("sub-%02d", seq(1, 44))
  cfg$num_subs = length(cfg$subjects) - length(cfg$sub_exclude)
  cfg$lcctrl <- lme4::lmerControl(
    optimizer = c('bobyqa'),
    optCtrl = list(maxfun = 100000),
    calc.derivs = FALSE
  )
  cfg$hpc <- list(
    "partition" = "quick",
    "time" = "0:30:00",
    "memory" = "1500MB",
    "cpus" = "1"
  )
  cfg$event_levels <- c("fixation", "stimulus", "sri", "response", "feedback", "iti")
  cfg$key_levels <- c("w", "n", "d", "z", "g", "r", "n/a")
  cfg$finger_levels <- c("index", "middle", "ring", "n/a")
  cfg$hand_levels <- c("left", "right", "n/a")
  cfg$condition_levels <- c("Training", "Single", "Sequence")
  cfg$graph_levels <- c("uni", "bi", "flat")
  # configuration parameters for resting-state decoding data:
  cfg$rest$num_trs <- c(233, 137)
  cfg$rest$num_seq <- length(filter_sequences(combinat::permn(1:cfg$num_nodes, sort = TRUE)))
  cfg$rest$freq_smoothing_kernel <- 0.015
  cfg$rest$freq_baseline_run <- "ses-01_run-1"
  return(cfg)
}

create_paths <- function() {
  path_root <- find_root()
  datetime <- strftime(Sys.time(), format = "%y-%m-%d_%H-%M")
  paths <- list()
  paths$code <- file.path(path_root, "code")
  paths$container <- file.path(path_root, "zoo-analysis_latest.sif")
  paths$input <- file.path(path_root, "input")
  paths$output <- file.path(path_root, "output")
  paths$input_behavior <- file.path(paths$input, "bids", "*", "*", "func", "*events")
  paths$input_mri_decoding <- file.path(paths$input, "decoding", "sub-*", "decoding", "*scheme*_time_shift-4*decoding*")
  paths$input_mri_rest <- file.path(paths$input, "decoding", "sub-*", "decoding", "*scheme-7*_time_shift-4*decoding*")
  paths$input_sr_modeling <- file.path(paths$input, "sr-modeling", "modeling", "sub-*-sr.csv")
  paths$input_sr_base_modeling <- file.path(paths$input, "sr-modeling", "modeling", "sub-*-sr_base.csv")
  paths$graphs <- file.path(paths$code, "utilities", "graphs.yml")
  # source data:
  
  paths$slopes <- file.path(paths$output, "slopes")
  paths$slopes_rest <- file.path(paths$output, "slopes", "rest", "*tsv") 
  paths$logs <- file.path(paths$output, "logs", datetime)
  paths$logs_latest <- file.path(paths$output, "logs", "latest")
  paths$figures <- file.path(paths$output, "figures")
  # paths to source data:
  paths$sourcedata <- file.path(paths$output, "sourcedata")
  source_path <- file.path(paths$sourcedata, "zoo-sourcedata-%s")
  # paths to source data of the behavioral analyses:
  paths$behav_task <- sprintf(source_path, "behavior-task")
  paths$behav_sr_params <- sprintf(source_path, "behavior-sr-params")
  paths$behav_sr_mat <- sprintf(source_path, "behavior-sr-mat")
  paths$behav_sr_mat_rest <- sprintf(source_path, "behavior-sr-mat-rest")
  # paths to source data of the resting-state decoding analyses:
  paths$decoding_rest <- sprintf(source_path, "decoding-rest")
  paths$decoding_rest_between_tr <- sprintf(source_path, "decoding-rest-between-tr")
  paths$decoding_rest_between_tr_sr <- sprintf(source_path, "decoding-rest-between-tr-sr")
  paths$decoding_rest_between_tr_sr_cor <- sprintf(source_path, "decoding-rest-between-tr-sr-cor")
  paths$decoding_rest_std <- sprintf(source_path, "decoding-rest-std")
  paths$decoding_rest_slopes <- sprintf(source_path, "decoding-rest-slopes")
  paths$decoding_rest_slopes_session <- sprintf(source_path, "decoding-rest-slopes-session")
  paths$decoding_rest_slopes_session_stat <- sprintf(source_path, "decoding-rest-slopes-session_stat")
  paths$decoding_rest_slopes_mean <- sprintf(source_path, "decoding-rest-slopes-mean")
  paths$decoding_rest_slopes_mean_diff <- sprintf(source_path, "decoding-rest-slopes-mean_diff")
  paths$decoding_rest_slopes_true <- sprintf(source_path, "decoding-rest-slopes-true")
  paths$decoding_rest_slopes_true_time <- sprintf(source_path, "decoding-rest-slopes-true_time")
  paths$decoding_rest_slopes_true_mean <- sprintf(source_path, "decoding-rest-slopes-true_mean")
  paths$decoding_rest_slopes_max <- sprintf(source_path, "decoding-rest-slopes-max")
  paths$decoding_rest_slopes_max_diff <- sprintf(source_path, "decoding-rest-slopes-max-diff")
  paths$decoding_rest_freq_spec <- sprintf(source_path, "decoding-rest-freq-spec")
  paths$decoding_rest_freq_spec_power <- sprintf(source_path, "decoding-rest-freq-spec-power")
  paths$decoding_rest_freq_spec_power_mean <- sprintf(source_path, "decoding-rest-freq_spec-power-mean")
  paths$decoding_rest_freq_spec_power_mean_cond <- sprintf(source_path, "decoding-rest-freq_spec-power-mean-cond")
  paths$decoding_rest_freq_expect <- sprintf(source_path, "decoding-rest-freq-expect")
  paths$decoding_rest_freq_spec_power_expect <- sprintf(source_path, "decoding-rest-freq-spec-power-expect")
  paths$decoding_rest_freq_spec_power_expect_cond <- sprintf(source_path, "decoding-rest-freq-spec-power-expect-cond")
  paths$decoding_rest_freq_spec_power_expect_cond_stat <- sprintf(source_path, "decoding-rest-freq-spec-power-expect-cond-stat")
  paths$decoding_rest_slopes_sr <- sprintf(source_path, "decoding-rest-slopes-sr")
  paths$decoding_sd_sr_prob <- sprintf(source_path, "decoding-rest-sd-sr-prob")
  paths$decoding_rest_slopes_sr_mean <- sprintf(source_path, "decoding-rest-slopes-sr-mean")
  paths$decoding_rest_slopes_sr_mean_phase <- sprintf(source_path, "decoding-rest-slopes-sr-mean-phase")
  paths$decoding_rest_slopes_sr_gamma_corr <- sprintf(source_path, "decoding-rest-slopes-sr-gamma-corr")
  paths$decoding_rest_surprise_prob <- sprintf(source_path, "decoding-rest-surprise-prob")
  for (path in c(paths$output, paths$figures, paths$sourcedata, paths$slopes, paths$logs)) {
    create_dir(path)
  }
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

create_dir <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
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

save_figure <- function(plot, filename, width, height) {
  paths <- create_paths()
  ggsave(filename = paste0("zoo-figure-", filename, ".pdf"),
         plot = plot, device = cairo_pdf, path = paths$figures,
         scale = 1, dpi = "retina", width = width, height = height)
  ggsave(filename = paste0("zoo-figure-", filename, ".png"),
         plot = plot, device = "png", path = paths$figures,
         scale = 1, dpi = "retina", width = width, height = height)
  return(plot)
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

get_input <- function() {
  option_list <- list(
    make_option(c("-s", "--subject"),
                help = "participant label (integer, no zero-padding, no 'sub-' prefix)",
                default = 1, 
                type = "integer",
                metavar = "integer"),
    make_option(c("-c", "--classification"),
                help = "classification type [default = %default]",
                default = "ensemble",
                type = "character",
                metavar = "character"),
    make_option(c("-m", "--mask_test"),
                help = "anatomical mask of the test set [default = %default] (options: [hippocampus, occipito-temporal, motor, medial-temporal])",
                default = "occipito-temporal",
                type = "character",
                metavar = "character"),
    make_option(c("-o", "--train_set"),
                help = "onset of the training set [default = %default] (options: [stimulus, response])",
                default = "stimulus", 
                type = "character",
                metavar = "character"),
    make_option(c("-r", "--run"),
                help = "run [default = %default] (options: 1 to 5)",
                default = 1,
                type = "integer",
                metavar = "integer"),
    make_option(c("-t", "--trial_index"),
                help = "run [default = %default] (options: 1 to 144)",
                default = 1,
                type = "integer",
                metavar = "integer"),
    make_option(c("-i", "--interval_tr"),
                help = "interval_tr [default = %default] (options: 1 to 8)",
                default = 1,
                type = "integer",
                metavar = "integer"),
    make_option(c("-d", "--data"),
                help = "data [default = %default] (options: 'main' or 'rest')",
                default = "rest",
                type = "character",
                metavar = "character")
  )
  
  opt_parser <- OptionParser(option_list =  option_list);
  opt <- parse_args(opt_parser)
  
  if (any(unlist(lapply(opt, function(x) is.null(x[[1]]))))) {
    print_help(opt_parser)
    stop("At least one argument must be supplied (input file).n", call. = FALSE)
  }
  
  # reformat some of the inputs:
  opt$subject <- paste0("sub-", sprintf("%02d", as.integer(opt$subject)))
  opt$run <- paste0("run-", sprintf("%02d", as.integer(opt$run)))
  
  return(opt)
}

filter_sequences <- function(sequence_list) {
  sequences_filtered <- comprehenr::to_list(for (x in sequence_list) if (x[length(x)] > x[1]) x)
  base::stopifnot(length(sequences_filtered) == length(sequence_list) / 2)
  return(sequences_filtered)
}

reverse_number <- function(number) {
  number_rev <- unlist(lapply(base::strsplit(as.character(number), ""), function(x) as.numeric(paste(rev(x), collapse = ""))))
  return(number_rev)
}

reverse_string <- function(string) {
  string_rev <- unlist(lapply(base::strsplit(string, NULL), function(x) paste(rev(x), collapse = "")))
  return(string_rev)
}

seq_direction = function(number) {
  differences = lapply(base::strsplit(as.character(number), ""), function(x) diff(as.numeric(x)))
  get_direction = function(numbers) {
    if (all(numbers == -1) | sum(numbers == 5) == 1) {
      direction = "counterclockwise"
    } else if (all(numbers == 1) | sum(numbers == -5) == 1) {
      direction = "clockwise"
    }
  }
  directions = unlist(lapply(differences, function(x) get_direction(x)))
  return(directions)
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
    pvalue_format = paste("p", pvalue_format)
  }
  return(pvalue_format)
}

get_ttest <- function(dt_input, ttest_cfg) {
  ttest <- broom::tidy(stats::t.test(
    formula = as.formula(ttest_cfg$formula),
    data = droplevels(dt_input),
    mu = ttest_cfg$mu,
    paired = ttest_cfg$paired,
    alternative = ttest_cfg$alternative
  ))
  cohensd <- rstatix::cohens_d(
    data = droplevels(dt_input),
    formula = as.formula(ttest_cfg$formula),
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
    .[, conf.high_round := round(conf.high, 2)] %>%
    .[, effsize_round := tryCatch(round(effsize, 2), error=function(err) NA)] %>%
    .[, p.value_round := round(p.value, 2)] %>%
    .[, p.value_round_label := format_pvalue(p.value_round)] %>%
    .[, p.value_significance := ifelse(p.value < 0.05, "*", "n.s.")] %>%
    .[, p.value_adjust := stats::p.adjust(p.value, method = ttest_cfg$adjust_method, n = .N)] %>%
    .[, p.value_adjust_round := as.numeric(round(p.value_adjust, 2))] %>%
    .[, p.value_adjust_round_label := format_pvalue(p.value_adjust_round)] %>%
    .[, p.value_adjust_significance := ifelse(p.value_adjust < 0.05, "*", "n.s.")] %>%
    .[, adjust_method := ttest_cfg$adjust_method]
  return(dt_output)
}

random_subsequence <- function(sequence, sublength) {
  # given a "sequence" calculate a random continuous subsequence of a certain "sublength"
  possible_start <- seq(length(sequence) - sublength + 1)
  start_index <- sample(possible_start, 1)
  subsequence <- sequence[start_index:(start_index + sublength - 1)]
  stopifnot(length(subsequence) == sublength)
  return(subsequence)
}

label_fill <- function(original, offset = 0, mod = 2, fill = "") {
  # this function can be used to generate axis labels that omit e.g.,
  # every second label. Solution was taken from [here](https://bit.ly/2VycSy0).
  ii <- as.logical((seq_len(length(original)) - 1 + offset) %% mod)
  original[ii] <- fill
  return(original)
}

calc_bits <- function(probability) {
  bits <- -log(probability, base = 2)
  return(bits)
}

sr_mat_fun <- function(dt, cfg){
  dt$sr_mat <- rep(NA, nrow(dt))
  bits <- rep(NA, nrow(dt) - 1)
  gamma <- unique(dt$gamma)
  alpha <- unique(dt$alpha)
  # pre-allocate the successor matrix with baseline expectation
  # baseline expectation could also be zero
  expectation <- 1 / cfg$num_nodes ^ 2
  sr <- matrix(expectation, cfg$num_nodes, cfg$num_nodes)
  # add letters to the successor matrix:
  colnames(sr) <- rownames(sr) <- cfg$nodes_letters
  # loop through all trials (transitions):
  for (i in nrow(dt) + 1) {
    # determine the previous node and the current node:
    node_x <- which(dt$node_previous[i] == cfg$nodes_letters)
    node_y <- which(dt$node[i] == cfg$nodes_letters)
    # normalize the successor matrix to express it in probabilities:
    sr_norm <- sr / matrix(rowSums(sr), cfg$num_nodes, cfg$num_nodes)
    dt$sr_mat[i - 1] <- list(rownames_to_column(as.data.frame(sr_norm, row.names = TRUE), "previous"))
    probability <- sr_norm[node_x, node_y]
    bits[i] <- calc_bits(probability = probability)
    # update the successor representation:
    occupancy <- rep(0, cfg$num_nodes)
    occupancy[node_y] <- 1
    successor_prediction_error <- gamma * sr[node_y,] - sr[node_x,]
    sr[node_x,] <- sr[node_x,] + alpha * (occupancy + successor_prediction_error)
  }
  dt$bits <- c(NA, bits)
  return(list(dt = list(dt)))
}

