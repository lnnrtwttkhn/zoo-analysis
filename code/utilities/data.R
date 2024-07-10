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
  # standard ordering is A B C D E F
  dt_output <- dt_input %>%
    .[classification == "ensemble", ] %>%
    .[class != "other", ] %>%
    setnames(old = "sub", new = "id") %>%
    setnames(old = "ses", new = "session") %>%
    .[, node := NULL] %>%
    setnames(old = "class", new = "node") %>%
    setnames(old = "onset_interval", new = "interval_tr") %>%
    setorder(id, classification, train_set, mask_test, run, trial_run, interval_tr, node) %>%
    .[, interval_tr := interval_tr + 1] %>%
    .[, by = .(id, classification, mask_test, train_set, session, run, node), ":="(
      num_trs = length(unique(interval_tr)),
      probability_norm = probability / sum(probability)
    )] %>%
    verify(num_trs %in% cfg$rest$num_trs) %>%
    .[, num_trs := NULL] %>%
    # check if there is the correct number of classes per TR:
    verify(.[, by = .(id, classification, mask_test, train_set, session, run, trial_run, interval_tr), .(
      num_classes = .N
    )]$num_classes == cfg$num_nodes) %>%
    .[, by = .(id, classification, train_set, mask_test, session, run, trial_run, interval_tr), ":="(
      node_index = rleid(node)
    )] %>%
    verify(node_index %in% seq(1, cfg$num_nodes)) %>%
    verify(.[, by = .(node), .(
      num_indices = length(unique(node_index))
    )]$num_indices == 1) %>%
    setorder(id, classification, train_set, mask_test, session, run, trial_run, interval_tr, node, node_index) %>%
    # check if order of letters matches order of indices:
    verify(.[, by = .(id, classification, mask_test, train_set, session, run, trial_run, interval_tr), .(
      check = stringr::str_order(node) == node_index
    )]$check == TRUE) %>%
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
  return(dt_output)
}
