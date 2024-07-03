library("here")
source(here::here("code", "utilities", "setup.R"))
opt <- get_input()
if (opt$data == "main") {
  dt_all = data_load %>%
    prep_decoding_main(.)
} else if (opt$data == "rest") {
  dt_all <- prepare_data_mri_rest(cfg, paths)
}

dt_select <- dt_all %>%
  .[id == opt$subject, ] %>%
  .[classification == "ensemble", ] %>%
  .[train_set == opt$train_set, ] %>%
  .[mask_test == opt$mask_test, ]

get_slopes <- function(dt_prob, sequences, data_type = NULL) {
  if (data_type == "main") {
    grouping_variables = c("id", "classification", "mask_test", "train_set", "session", "run", "graph", "accuracy", "trial_accuracy", "node_previous", "trial_run", "trial_index", "interval_tr")
  } else if (data_type == "rest") {
    grouping_variables = c("id", "classification", "mask_test", "train_set", "session", "run", "node_previous", "trial_index", "interval_tr")
  }
  dt_slope <- dt_prob %>%
    .[, by = grouping_variables, .(
      slope = unlist(lapply(sequences, function(x) coef(lm(probability ~ x))[2] * (-1))),
      slope_norm = unlist(lapply(sequences, function(x) coef(lm(probability_norm ~ x))[2] * (-1))),
      kendall = unlist(lapply(sequences, function(x) cor.test(x, probability, method = "kendall")$estimate * (-1))),
      kendall_norm = unlist(lapply(sequences, function(x) cor.test(x, probability_norm, method = "kendall")$estimate * (-1))),
      pearson = unlist(lapply(sequences, function(x) cor.test(x, probability, method = "pearson")$estimate * (-1))),
      pearson_norm = unlist(lapply(sequences, function(x) cor.test(x, probability_norm, method = "pearson")$estimate * (-1))),
      seq_numbers = unlist(lapply(sequences, function(x) paste(x, collapse = ""))),
      seq_letters = unlist(lapply(sequences, function(x) paste(node[x], collapse = ""))),
      seq_start = unlist(lapply(sequences, function(x) node[x][1])),
      seq_diff_mean = unlist(lapply(sequences, function(x) mean(diff(x)))),
      seq_diff_sd = unlist(lapply(sequences, function(x) sd(diff(x)))),
      seq_length = unlist(lapply(sequences, function(x) length(x))),
      seq_graph = unlist(lapply(sequences, function(x) as.numeric(all(abs(diff(x)) %in% c(1, 5)))))
    )] %>%
    # add a counter for all sequential combinations:
    .[, by = grouping_variables,
      "seq_id" := seq(1, .N)
    ] %>%
    verify(length(sequences) == length(unique(seq_id)))
  return(dt_slope)
}

dt_list <- list()
for (num in seq(cfg$num_nodes, 5)) {
  sequences <- filter_sequences(combinat::permn(1:num, sort = TRUE))
  if (opt$data == "main" & num == 5) {
    # remove current node
    dt_prob = dt_prob %>%
      .[node_previous != node,]
  } else if (opt$data == "rest" & num == 5) {
    next
  }
  dt_slope <- get_slopes(dt_select, sequences, data_type = opt$data)
  dt_list <- append(dt_list, list(dt_slope))
}
dt_out <- rbindlist(dt_list)

filename <- paste(unlist(lapply(seq(1:4), function(x) paste0(names(opt[x]), "-", opt[x]))), collapse = "_")
filename <- paste0(filename, sprintf("_task-%s_slopes.tsv", opt$data))
create_dir(file.path(paths$slopes, opt$data))
path_out <- file.path(paths$slopes, opt$data, filename)
data.table::fwrite(dt_out, path_out, sep = "\t")
status_report("done")
