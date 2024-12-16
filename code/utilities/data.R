prepare_data_behavior <- function(cfg, paths) {
  dt_input <- load_data(paths$input_behavior)
  # dt_demographics <- load_data(paths$source$demographics) %>%
  #   .[!(id %in% cfg$sub_exclude), ]
  dt_output <- dt_input %>%
    .[, index := NULL] %>%
    .[, event_type := factor(as.factor(event_type), levels = cfg$event_levels)] %>%
    .[, onset := as.numeric(onset)] %>%
    .[, duration := as.numeric(duration)] %>%
    .[, duration_true := as.numeric(duration_true)] %>%
    .[, duration_true := NULL] %>%
    .[, duration_diff := as.numeric(duration_diff)] %>%
    .[, duration_diff := NULL] %>%
    .[, accuracy := as.numeric(accuracy)] %>%
    assertr::assert(in_set(0, 1), accuracy) %>%
    .[, stim_file := as.factor(stim_file)] %>%
    .[, stim_name := as.factor(stim_name)] %>%
    .[, stim_name := NULL] %>%
    .[, event_limit := as.numeric(event_limit)] %>%
    .[, id := as.factor(id)] %>%
    .[, study_type := factor(as.factor(study_type), levels = c("MRI"))] %>%
    .[, study_type := NULL] %>%
    .[, study_name := factor(as.factor(study_name), levels = c("zoo"))] %>%
    .[, study_name := NULL] %>%
    .[, session := factor(as.factor(session), levels = c("ses-01", "ses-02"))] %>%
    .[, condition := dplyr::case_when(
      condition == "training" ~ "Training",
      condition == "recall" ~ "Single",
      condition == "main" ~ "Sequence"
    )] %>%
    .[, condition := factor(as.factor(condition), levels = cfg$condition_levels)] %>%
    .[, run := as.factor(run)] %>%
    .[, graph := as.factor(graph)] %>%
    verify(!is.na(graph)) %>%
    .[, graph_condition := as.factor(graph_condition)] %>%
    verify(!is.na(graph_condition)) %>%
    .[, graph_condition := NULL, ] %>%
    .[, node := factor(as.factor(node), levels = cfg$nodes_letters)] %>%
    .[, trial_run := as.numeric(trial_run)] %>%
    .[, num_trials_run := as.numeric(num_trials_run)] %>%
    .[, num_trials_run := NULL] %>%
    .[, key_correct := factor(as.factor(as.character(key_correct)), levels = cfg$key_levels)] %>%
    .[, hand_correct := factor(as.factor(as.character(hand_correct)), levels = cfg$hand_levels)] %>%
    .[, finger_correct := factor(as.factor(as.character(finger_correct)), levels = cfg$finger_levels)] %>%
    .[, trial_accuracy := as.numeric(trial_accuracy)] %>%
    assertr::assert(in_set(0, 1), trial_accuracy) %>%
    .[, psychopy_version := NULL] %>%
    .[, key_pressed := factor(as.factor(as.character(key_pressed)), levels = cfg$key_levels)] %>%
    .[, hand_pressed := ifelse(is.na(hand_pressed), "n/a", hand_pressed)] %>%
    .[, hand_pressed := factor(as.factor(as.character(hand_pressed)), levels = cfg$hand_levels)] %>%
    .[, finger_pressed := ifelse(is.na(finger_pressed), "n/a", finger_pressed)] %>%
    .[, finger_pressed := factor(as.factor(as.character(finger_pressed)), levels = cfg$finger_levels)] %>%
    .[, response_time := ifelse(response_time == "n/a", NA, response_time)] %>%
    .[, response_time := as.numeric(response_time)] %>%
    # check if response times are below the event / response time limit:
    verify(response_time[!is.na(response_time) & event_type == "response"] < 
             event_limit[!is.na(response_time) & event_type == "response"]) %>%
    verify(response_time[!is.na(response_time) & event_type == "response"] > 0) %>%
    .[, event_limit := NULL] %>%
    # check if accuracy if correctly specified (key_pressed == key_correct):
    verify(as.numeric(key_correct[!is.na(key_pressed) & event_type == "response"] == key_pressed[
      !is.na(key_pressed) & event_type == "response"]) == accuracy[!is.na(key_pressed) & event_type == "response"]) %>%
    # add additional variables:
    .[, key_hand_correct := as.factor(paste0(key_correct, "\n(", hand_correct, ")"))] %>%
    .[, key_hand_pressed := as.factor(paste0(key_pressed, "\n(", hand_pressed, ")"))] %>%
    .[, log_response_time := log(response_time)] %>%
    .[, press_correct := paste(key_correct, paste0("(", hand_correct), finger_correct, "finger)")] %>%
    # add the previous node:
    plyr::join(
      x = .,
      y = .[event_type == "response"] %>% .[, by = .(id, session, run, condition), node_previous := lag(node)],
      by = colnames(.)[!stringr::str_detect(colnames(.), "dist|prob")]
    ) %>%
    .[, by = .(id, session, run, condition, trial_run), node_previous := node_previous[event_type == "response"]] %>%
    # add the next node:
    plyr::join(
      x = .,
      y = .[event_type == "response"] %>% .[, by = .(id, session, run, condition), node_next := dplyr::lead(node)],
      by = colnames(.)[!stringr::str_detect(colnames(.), "dist|prob")]
    ) %>%
    .[, by = .(id, session, run, condition, trial_run), node_next := node_next[event_type == "response"]] %>%
    plyr::join(., graphs, by = c("node_previous", "node")) %>%
    .[, graph := factor(as.factor(graph), levels = cfg$graph_levels)] %>%
    setDT(.) %>%
    .[, run_cond := paste0(run, " (", graph, ")")] %>%
    .[, dist_dir_label := dplyr::case_when(
      dist_uni == 1 ~ "uni: 1\n bi: 1",
      dist_uni == 2 ~ "     2\n     2",
      dist_uni == 3 ~ "     3\n     3",
      dist_uni == 4 ~ "     4\n    -2",
      dist_uni == 5 ~ "     5\n    -1"
    )] %>%
    .[, dist_dir_label := factor(as.factor(dist_dir_label), levels = c(
      "uni: 1\n bi: 1", "     2\n     2", "     3\n     3", "     4\n    -2", "     5\n    -1"))] %>%
    .[, by = .(id), order := factor(
      as.factor(ifelse(any(run_cond == "run-01 (uni)"), "uni - bi", "bi - uni")),
      levels = c("uni - bi", "bi - uni"))] %>%
    .[, dist_current := dplyr::case_when(
      graph == "flat" ~ dist_flat,
      graph == "uni" ~ dist_uni,
      graph == "bi" ~ dist_bi
    )] %>%
    .[, prob_current := dplyr::case_when(
      graph == "flat" ~ prob_flat,
      graph == "uni" ~ prob_uni,
      graph == "bi" ~ prob_bi
    )] %>%
    .[, onestep := dplyr::case_when(
      prob_uni == 0.7 & graph == "uni" ~ "High\n(0.7)",
      prob_uni == 0.1 & graph == "uni" ~ "Low\n(0.1)",
      prob_bi == 0.35 & graph == "bi" ~ "High\n(0.35)",
      prob_bi == 0.1 & graph == "bi" ~ "Low\n(0.1)"
    )] %>%
    .[, onestep := factor(as.factor(onestep), levels = c("Low\n(0.1)", "High\n(0.35)", "High\n(0.7)"))] %>%
    # merge.data.table(x = ., y = dt_demographics, by = c("id", "order")) %>%
    setcolorder(., c(
      "id", "session", "condition", "run", "trial_run", "event_type",
      "node", "node_previous", "node_next",
      "graph", "accuracy", "response_time", "trial_accuracy",
      "key_correct", "finger_correct", "hand_correct",
      "key_pressed", "finger_pressed", "hand_pressed"
    )) %>%
    setorder(., id, session, condition, run, trial_run) %>%
    save_data(., paths$behav_task)
}

prepare_data_mri_rest <- function(cfg, paths) {
  dt_input <- load_data(paths$input_mri_rest)
  dt_output <- dt_input %>%
    .[classification == "ensemble", ] %>%
    .[class != "other", ] %>%
    setnames(old = "sub", new = "id") %>%
    setnames(old = "ses", new = "session") %>%
    .[, node := NULL] %>%
    setnames(old = "class", new = "node") %>%
    setnames(old = "onset_interval", new = "interval_tr") %>%
    .[, by = .(id, classification, mask_test, train_set, session, run, node), ":="(
      num_trs = length(unique(interval_tr)),
      probability_norm = probability / sum(probability)
    )] %>%
    verify(num_trs %in% cfg$rest$num_trs) %>%
    .[, num_trs := NULL] %>%
    verify(.[, by = .(id, classification, mask_test, train_set, session, run, trial_run, interval_tr), .(
      num_classes = .N
    )]$num_classes == cfg$num_nodes) %>%
    .[, train_set := ifelse(stringr::str_detect(train_set, "response"), "response", "stimulus")] %>%
    .[, mask_test := dplyr::case_when(
      mask_test == "hpc" ~ "hippocampus",
      mask_test == "vis" ~ "occipito-temporal",
      mask_test == "mot" ~ "motor",
      mask_test == "mtl" ~ "medial-temporal"
    )] %>%
    group_split(id, classification, train_set, mask_test, session, run) %>%
    map_df(~.x %>% group_by(trial_run) %>% mutate(trial_index = cur_group_id())) %>%
    setDT(.) %>%
    .[, node_previous := NA] %>%
    setorder(id, classification, train_set, mask_test, session, run, trial_index, interval_tr) %>%
    save_data(., paths$decoding_rest)
}

prepare_data_decoding <- function(dt_input) {
  dt_output <- dt_input %>%
    .[, index := NULL] %>%
    .[, event_type := NULL] %>%
    .[, onset := as.numeric(onset)] %>%
    .[, duration := as.numeric(duration)] %>%
    .[, duration := NULL] %>%
    .[, duration_true := as.numeric(duration_true)] %>%
    .[, duration_true := NULL] %>%
    .[, duration_diff := as.numeric(duration_diff)] %>%
    .[, duration_diff := NULL] %>%
    .[, accuracy := as.numeric(accuracy)] %>%
    assertr::assert(in_set(0, 1), accuracy) %>%
    .[, stim_file := as.factor(as.character(stim_file))] %>%
    .[, stim_file := NULL] %>%
    .[, stim_name := as.factor(as.character(stim_name))] %>%
    .[, stim_name := NULL] %>%
    .[, event_limit := NULL] %>%
    .[, id := as.factor(id)] %>%
    .[, study_type := factor(as.factor(study_type), levels = c("MRI"))] %>%
    .[, study_type := NULL] %>%
    .[, study_name := factor(as.factor(study_name), levels = c("zoo"))] %>%
    .[, study_name := NULL] %>%
    .[, session := as.factor(as.character(session))] %>%
    .[, condition := dplyr::case_when(
      condition == "training" ~ "Training",
      condition == "recall" ~ "Single",
      condition == "main" ~ "Sequence"
    )] %>%
    .[, condition := factor(as.factor(condition), levels = c(
      "Training", "Single", "Sequence"))] %>%
    .[, run := as.factor(run)] %>%
    .[, graph := as.factor(graph)] %>%
    .[, graph := factor(as.factor(graph), levels = c("uni", "bi"))] %>%
    .[, graph_condition := as.factor(graph_condition)] %>%
    .[, graph_condition := NULL] %>%
    .[, node := factor(as.factor(node), levels = cfg$nodes_letters)] %>%
    .[, trial_run := as.numeric(trial_run)] %>%
    .[, num_trials_run := as.numeric(num_trials_run)] %>%
    .[, num_trials_run := NULL] %>%
    .[, key_correct := factor(as.factor(as.character(key_correct)), levels = c(
      "w", "n", "d", "z", "g", "r", "n/a"))] %>%
    .[, hand_correct := factor(as.factor(as.character(hand_correct)), levels = c(
      "left", "right", "n/a"))] %>%
    .[, finger_correct := factor(as.factor(as.character(finger_correct)), levels = c(
      "index", "middle", "ring", "n/a"))] %>%
    .[, trial_accuracy := as.numeric(trial_accuracy)] %>%
    assertr::assert(in_set(0, 1), trial_accuracy) %>%
    .[, psychopy_version := NULL] %>%
    .[, key_pressed := ifelse(is.na(key_pressed), "n/a", key_pressed)] %>%
    .[, key_pressed := factor(as.factor(as.character(key_pressed)), levels = c(
      "w", "n", "d", "z", "g", "r", "n/a"))] %>%
    .[, hand_pressed := ifelse(is.na(hand_pressed), "n/a", hand_pressed)] %>%
    .[, hand_pressed := factor(as.factor(as.character(hand_pressed)), levels = c(
      "left", "right", "n/a"))] %>%
    .[, finger_pressed := ifelse(is.na(finger_pressed), "n/a", hand_pressed)] %>%
    .[, finger_pressed := factor(as.factor(as.character(finger_pressed)), levels = c(
      "index", "middle", "ring", "n/a"))] %>%
    .[, response_time := ifelse(response_time == "n/a", NA, response_time)] %>%
    .[, response_time := as.numeric(response_time)] %>%
    .[, onset_shifted := as.numeric(onset_shifted)] %>%
    .[, onset_shifted := NULL] %>%
    .[, onset_shifted_tr := as.numeric(onset_shifted_tr)] %>%
    .[, onset_shifted_tr := NULL] %>%
    .[, onset_tr := as.numeric(onset_tr)] %>%
    .[, onset_shifted_tr_round := as.numeric(onset_shifted_tr_round)] %>%
    .[, onset_shifted_tr_round := NULL] %>%
    .[, onset_tr_round := as.numeric(onset_tr_round)] %>%
    .[, onset_tr_diff := NULL] %>%
    .[, onset_shifted_tr_diff := NULL] %>%
    .[, onset_interval_tr := as.numeric(onset_interval)] %>%
    .[, onset_interval := as.numeric(onset + cfg$tr * (interval_tr - 1))] %>%
    .[, interval_tr := as.numeric(interval_tr)] %>%
    .[, pred_label := as.factor(as.character(pred_label))] %>%
    .[, classifier := as.factor(classifier)] %>%
    .[, classification := NULL] %>%
    .[, train_set := ifelse(stringr::str_detect(train_set, "response"), "response onset", train_set)] %>%
    .[, train_set := ifelse(stringr::str_detect(train_set, "stimulus"), "stimulus onset", train_set)] %>%
    .[(train_set == "response onset" & mask_test == "mot") |
        (train_set == "stimulus onset" & mask_test == "vis"), ] %>%
    .[, train_set := NULL] %>%
    .[, test_set := ifelse(stringr::str_detect(test_set, "response"), "response onset", test_set)] %>%
    .[, test_set := ifelse(stringr::str_detect(test_set, "stimulus"), "stimulus onset", test_set)] %>%
    # .[(test_set == "response onset" & mask_test == "mot") |
    #     (test_set == "stimulus onset" & mask_test == "vis"), ] %>%
    # verify(.[, .(num_test_set = length(unique(test_set)))]$num_test_set == 2) %>%
    .[, test_set := NULL] %>%
    .[, ses_test := NULL] %>%
    .[, mask_test := dplyr::case_when(
      mask_test == "hpc" ~ "hippocampus",
      mask_test == "vis" ~ "visual",
      mask_test == "mot" ~ "motor",
      mask_test == "mtl" ~ "medial-temporal"
    )] %>%
    .[, mask_test := factor(as.factor(mask_test), levels = c(
      "visual", "motor", "hippocampus", "medial-temporal"))] %>%
    .[, masking_test := NULL] %>%
    .[, tr_rounding := NULL] %>%
    .[, tr_interval := NULL] %>%
    .[, time_selection := NULL] %>%
    .[, time_shift := NULL] %>%
    .[, tmap_threshold := NULL] %>%
    .[, pred_acc := as.numeric(pred_acc)] %>%
    assertr::assert(in_set(0, 1), pred_acc) %>%
    .[, class := factor(as.factor(as.character(class)), levels = cfg$nodes_letters)] %>%
    .[, probability := as.numeric(probability)] %>%
    # probabilities are normalized for each class within a trial to sum up to 1
    .[, by = .(id, mask_test, run, trial_run, node, class), ":="(
      probability_norm = probability / sum(probability)
    )] %>%
    group_split(id, mask_test) %>%
    purrr::map_df(~.x %>% group_by(run, trial_run) %>% mutate(trial_index = cur_group_id())) %>%
    ungroup(.) %>%
    group_split(id, mask_test, run) %>%
    purrr::map_df(~.x %>% group_by(trial_run) %>% mutate(trial_index_run = cur_group_id())) %>%
    setDT(.) %>%
    .[, run_index := as.numeric(stringr::str_sub(run, -1))] %>%
    setcolorder(., c(
      "id", "mask_test", "session", "condition", "run", "trial_run", "interval_tr", "classifier", "class"
    )) %>%
    setorder(., id, mask_test, session, condition, run, trial_run, interval_tr, classifier, class)
  return(dt_output)
}

prepare_data_decoding_single_interval <- function(cfg, paths) {
  get_data(paths$input_mri_single_interval)
  dt_input <- load_data(paths$input_mri_single_interval)
  num_trial_trs <- cfg$decoding_single_interval$num_trs
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude), ] %>%
    .[classification == "ensemble", ] %>%
    .[class != "other", ] %>%
    .[stringr::str_detect(test_set, "decode-interval"), ] %>%
    prepare_data_decoding(.) %>%
    .[interval_tr %in% seq(num_trial_trs), ] %>%
    verify(interval_tr %in% seq(1, num_trial_trs)) %>%
    verify(.[, by = .(id, mask_test, run, trial_run, node, class), .(
      num_unique_trs = length(unique(interval_tr))
    )]$num_unique_trs == num_trial_trs) %>%
    save_data(paths$source$decoding_single_interval)
  return(dt_output)
}

