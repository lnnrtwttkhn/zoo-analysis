get_questionnaire_seq_detect <- function(cfg, paths) {
  dt_input <- load_data(paths$source$demographics)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude), ] %>%
    .[, by = .(sequence_detected), .(number = .N, proportion = .N/nrow(.))] %>%
    save_data(paths$source$questionnaire_seq_detect)
}

plot_questionnaire_seq_detect <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire_seq_detect)
  figure <- ggplot(data = dt_input, aes(x = sequence_detected,  y = number)) +
    geom_bar(stat = "identity", width = 0.3) +
    geom_text(aes(label = number, y = number + 2)) +
    ggtitle('"Did you notice any\nsequential structure?"') +
    ylab("Number of participants") +
    theme_zoo() +
    theme(axis.line.x = element_blank()) +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(axis.text.x = element_text(colour = "black")) +
    coord_capped_cart(left = "both", expand = TRUE) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = rel(0.85))) +
    ylim(c(0, 25))
  return(figure)
}

get_questionnaire_seq_detect_order <- function(cfg, paths) {
  dt_input <- load_data(paths$source$demographics)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude), ] %>%
    .[, by = .(order, sequence_detected), .(number = .N, proportion = .N/nrow(.))] %>%
    save_data(paths$source$questionnaire_seq_detect_order)
}

plot_questionnaire_seq_detect_order <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire_seq_detect_order)
  figure = ggplot(data = dt_input, aes(x = sequence_detected, y = number)) +
    facet_wrap(~ order) +
    geom_bar(stat = "identity", width = 0.3) +
    geom_text(aes(label = number, y = number + 2)) +
    ggtitle('"Did you notice any\nsequential structure?"') +
    ylab("Number of participants") +
    theme_zoo() +
    theme(axis.line.x = element_blank()) +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(axis.text.x = element_text(colour = "black")) +
    coord_capped_cart(left = "both", expand = TRUE) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = rel(0.85))) +
    ylim(c(0, 25))
  return(figure)
}

get_questionnaire_seq_when <- function(cfg, paths) {
  dt_input <- load_data(paths$source$demographics)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude), ] %>%
    .[sequence_detected == "yes", ] %>%
    .[, by = .(when_detected), .(number = .N)] %>%
    # TODO: fix this hack:
    rbind(., data.table(when_detected = "4", number = 0)) %>%
    save_data(paths$source$questionnaire_seq_when)
}

plot_questionnaire_seq_when <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire_seq_when)
  figure <- ggplot(data = dt_input, aes(x = when_detected,  y = number)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(label = number, y = number + 1)) +
    ggtitle('"When did you notice\nsequential structure?"') +
    ylab("Number of participants") +
    xlab("Run (Session 2)") +
    theme_zoo() +
    theme(axis.title.x = element_text(colour = "black")) +
    theme(axis.text.x = element_text(colour = "black")) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = rel(0.85))) +
    ylim(c(0, 15))
  return(figure)
}

get_questionnaire_seq_when_order <- function(cfg, paths) {
  dt_input <- load_data(paths$source$demographics)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude), ] %>%
    .[sequence_detected == "yes", ] %>%
    .[, by = .(order, when_detected), .(number = .N)] %>%
    save_data(paths$source$questionnaire_seq_when_order)
}

plot_questionnaire_seq_when_order <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire_seq_when_order)
  figure = ggplot(data = dt_input, aes(x = when_detected,  y = number)) +
    facet_wrap(~ order) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(label = number, y = number + 1)) +
    ggtitle('"When did you notice\nsequential structure?"') +
    ylab("Number of participants") +
    xlab("Run (in Session 2)") +
    theme_zoo() +
    theme(axis.title.x = element_text(colour = "black")) +
    theme(axis.text.x = element_text(colour = "black")) +
    coord_capped_cart(left = "both", bottom = "both", expand = TRUE) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = rel(0.85))) +
    ylim(c(0, 15))
  return(figure)
}

get_questionnaire_duration <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire)
  dt_output <- dt_input %>%
    .[, by = .(id), .(
      duration = (max(onset) + response_time)/60
    )]
}

get_questionnaire_prob_ratings <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude)] %>%
    .[, by = .(id, order, prob_uni, dist_uni, prob_bi, dist_bi, sequence_detected), .(
      num_transitions = .N,
      mean_probability = mean(probability_rating),
      mean_log_response_time = mean(log_response_time),
      mean_prob_uni_diff = mean(prob_uni_diff),
      mean_prob_bi_diff = mean(prob_bi_diff)
    )] %>%
    .[, dist_dir_label := dplyr::case_when(
      dist_uni == 1 ~ "uni: 1\n bi: 1",
      dist_uni == 2 ~ "     2\n     2",
      dist_uni == 3 ~ "     3\n     3",
      dist_uni == 4 ~ "     4\n    -2",
      dist_uni == 5 ~ "     5\n    -1"
    )] %>%
    .[, dist_dir_label := factor(as.factor(dist_dir_label), levels = c(
      "uni: 1\n bi: 1", "     2\n     2", "     3\n     3", "     4\n    -2", "     5\n    -1"))] %>%
    .[, prob_label := paste0(prob_uni, "\n", prob_bi)] %>%
    # TODO: FILTER FOR CORRECT TRIALS:
    .[!(is.na(dist_uni)), ] %>%
    save_data(paths$source$questionnaire_prob_ratings)
}

plot_questionnaire_prob_ratings <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire_prob_ratings)
  figure <- ggplot(data = dt_input, aes(x = dist_dir_label, y = mean_probability)) +
    facet_wrap(~ order, scales = "free_x") +
    geom_boxplot(aes(fill = order), outlier.colour = NA, outlier.shape = NA, width = 0.3) +
    geom_beeswarm(aes(group = id, fill = order), pch = 21, color = "black", alpha = 0.3) +
    stat_summary(aes(group = 1, fill = order), geom = "ribbon", fun.data = "mean_se", alpha = 0.7) +
    stat_summary(geom = "errorbar", fun.data = "mean_se", color = "black", width = 0) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(aes(fill = order), geom = "point", fun = "mean", color = "black", pch = 23) +
    theme_zoo() +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    ylim(c(0, 1)) +
    scale_color_manual(values = c("gray", "gray"), name = "Graph") +
    scale_fill_manual(values = c("gray", "gray"), name = "Graph") +
    ylab("Probability rating") +
    xlab("Node distance") +
    theme(legend.position = "none") +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both") +
    theme(axis.text.x = element_text(hjust = 0.85)) +
    theme(panel.spacing = unit(2, "lines"))
  return(figure)
}

plot_questionnaire_prob_ratings_seq_detect <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire_prob_ratings)
  figure <- ggplot(data = dt_input, aes(x = dist_dir_label, y = mean_probability)) +
    facet_wrap(~ sequence_detected, scales = "free_x") +
    geom_boxplot(aes(fill = sequence_detected), outlier.colour = NA, outlier.shape = NA, width = 0.3) +
    geom_beeswarm(aes(group = id, fill = sequence_detected), pch = 21, color = "black", alpha = 0.3) +
    stat_summary(aes(group = 1, fill = sequence_detected), geom = "ribbon", fun.data = "mean_se", alpha = 0.7) +
    stat_summary(geom = "errorbar", fun.data = "mean_se", color = "black", width = 0) +
    stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(aes(fill = sequence_detected), geom = "point", fun = "mean", color = "black", pch = 23) +
    theme_zoo() +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    ylim(c(0, 1)) +
    scale_color_manual(values = c("gray", "gray"), name = "Graph") +
    scale_fill_manual(values = c("gray", "gray"), name = "Graph") +
    ylab("Probability rating") +
    xlab("Node distance") +
    theme(legend.position = "none") +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both") +
    theme(axis.text.x = element_text(hjust = 0.85)) +
    theme(panel.spacing = unit(2, "lines"))
  return(figure)
}

plot_questionnaire_prob_ratings_seq_detect_overlay <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire_prob_ratings) %>%
    melt(., id.vars = c("id", "sequence_detected", "dist_dir_label"), measure.vars = c("mean_prob_bi_diff", "mean_prob_uni_diff"),
         variable.name = "graph", value.name = "mean_prob") %>%
    .[, graph := ifelse(graph == "mean_prob_bi_diff", "bi", "uni")]
  
  figure <- ggplot(data = dt_input, aes(x = dist_dir_label, y = mean_prob)) +
  # figure <- ggplot(data = dt_input, aes(x = dist_dir_label, y = mean_probability)) +
    facet_wrap(~ graph, scales = "free_x") +
    geom_hline(yintercept = 0, color = "gray") +
    geom_point(aes(group = id, fill = sequence_detected), pch = 21, color = "black", alpha = 0.3,
               position = position_jitterdodge(dodge.width = 0.5)) +
    geom_boxplot(aes(fill = sequence_detected), outlier.colour = NA, outlier.shape = NA, width = 0.3,
                 position = position_dodge(width = 0.5)) +
    # stat_summary(aes(group = sequence_detected, fill = sequence_detected), geom = "ribbon", fun.data = "mean_se", alpha = 0.7) +
    stat_summary(aes(group = sequence_detected), geom = "errorbar", fun.data = "mean_se", color = "black", width = 0,
                 position = position_dodge(width = 0.5)) +
    # stat_summary(aes(fill = sequence_detected), geom = "line", fun = "mean") +
    stat_summary(aes(fill = sequence_detected), geom = "point", fun = "mean", color = "black", pch = 23,
                 position = position_dodge(width = 0.5)) +
    theme_zoo() +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    ylim(c(-1, 1)) +
    ylab("Probability rating (deviation from true score)") +
    xlab("Node distance") +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both") +
    theme(axis.text.x = element_text(hjust = 0.85)) +
    theme(panel.spacing = unit(2, "lines"))
  figure
  return(figure)
}

get_questionnaire_prob_ratings_accuracy <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire_prob_ratings)
  ttest_cfg <- list(
    formula = "value ~ sequence_detected",
    adjust_method =  "bonferroni",
    paired = FALSE,
    mu = 0,
    alternative = "two.sided"
  )
  dt_output <- dt_input %>%
    .[, by = .(id, order, sequence_detected), .(
      num_dist_levels = .N,
      mean_prob_uni_diff = mean(mean_prob_uni_diff),
      mean_prob_bi_diff = mean(mean_prob_uni_diff)
    )] %>%
    verify(num_dist_levels == cfg$num_nodes - 1) %>%
    # .[, value := mean_prob_uni_diff] %>%
    # .[, .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    # unnest(ttest) %>%
    # get_pvalue_adjust(., ttest_cfg) %>%
    save_data(paths$source$questionnaire_prob_ratings_accuracy)
}

get_questionnaire_prob_ratings_accuracy_stat <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire_prob_ratings_accuracy)
  ttest_cfg <- list(
    formula = "value ~ sequence_detected",
    adjust_method =  "bonferroni",
    paired = FALSE,
    mu = 0,
    alternative = "two.sided"
  )
  dt_output <- dt_input %>%
    .[, value := mean_prob_uni_diff] %>%
    .[, .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg) %>%
    save_data(paths$source$questionnaire_prob_ratings_accuracy_stat)
}

plot_questionnaire_prob_ratings_accuracy <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire_prob_ratings_accuracy)
  figure <- ggplot(data = dt_input, aes(x = sequence_detected, y = mean_prob_uni_diff)) +
    stat_summary(aes(fill = sequence_detected), geom = "bar", fun = "mean") +
    stat_summary(aes(group = sequence_detected), geom = "errorbar", fun.data = "mean_se", color = "black", width = 0,
                 position = position_dodge(width = 0.5)) +
    theme_zoo() +
    theme(axis.line.x = element_blank()) +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(axis.text.x = element_text(colour = "black")) +
    coord_capped_cart(left = "both", expand = TRUE) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = rel(0.85)))
  return(figure)
}

get_questionnaire_prob_ratings_onestep <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude)] %>%
    .[, by = .(id, order, sequence_detected, prob_uni), .(
      num = .N,
      mean_prob_uni_diff = mean(prob_uni_diff)
    )] %>%
    save_data(paths$source$questionnaire_prob_ratings_onestep)
}

plot_questionnaire_prob_ratings_onestep <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire_prob_ratings_onestep)
  figure <- ggplot(data = dt_input, aes(x = as.factor(prob_uni), y = mean_prob_uni_diff)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_point(aes(group = id, fill = sequence_detected), pch = 21, color = "black", alpha = 0.3,
               position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
    geom_boxplot(aes(fill = sequence_detected), outlier.colour = NA, outlier.shape = NA, width = 0.3,
                 position = position_dodge(width = 0.5)) +
    # stat_summary(aes(group = sequence_detected, fill = sequence_detected), geom = "ribbon", fun.data = "mean_se", alpha = 0.7) +
    stat_summary(aes(group = sequence_detected), geom = "errorbar", fun.data = "mean_se", color = "black", width = 0,
                 position = position_dodge(width = 0.5)) +
    # stat_summary(aes(fill = sequence_detected), geom = "line", fun = "mean") +
    stat_summary(aes(fill = sequence_detected), geom = "point", fun = "mean", color = "black", pch = 23,
                 position = position_dodge(width = 0.5)) +
    theme_zoo() +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    ylim(c(-1, 1)) +
    ylab("Probability rating (deviation from true score)") +
    xlab("Node distance") +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both") +
    theme(axis.text.x = element_text(hjust = 0.85)) +
    theme(panel.spacing = unit(2, "lines"))
  return(figure)
}

get_questionnaire_prob_ratings_correlation <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude)] %>%
    group_by(id, sequence_detected) %>%
    do(cbind(
      broom::tidy(cor.test(.$prob_bi, .$probability_rating, method = "kendall")),
      data.table(num_data_points = nrow(.)))) %>%
    setDT(.) %>%
    verify(num_data_points == cfg$num_nodes * (cfg$num_nodes - 1)) %>%
    save_data(paths$source$questionnaire_prob_ratings_correlation)
}

get_questionnaire_prob_ratings_correlation_stat <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire_prob_ratings_correlation)
  dt_output <- dt_input %>%
    .[, value := estimate] %>%
    .[, .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg) %>%
    save_data(paths$source$questionnaire_prob_ratings_correlation_stat)
}

plot_questionnaire_prob_ratings_correlation <- function(cfg, paths) {
  dt_input <- load_data(paths$source$questionnaire_prob_ratings_correlation)
  figure <- ggplot(data = dt_input, aes(x = sequence_detected, y = estimate)) +
    # facet_wrap(~ order, scales = "free_x") +
    geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
    geom_boxplot(outlier.colour = NA, outlier.shape = NA, width = 0.3) +
    geom_beeswarm(aes(group = id), pch = 21, color = "black", alpha = 0.3) +
    # stat_summary(aes(group = 1, fill = order), geom = "ribbon", fun.data = "mean_se", alpha = 0.7) +
    stat_summary(geom = "errorbar", fun.data = "mean_se", color = "black", width = 0) +
    # stat_summary(aes(group = 1), geom = "line", fun = "mean") +
    stat_summary(geom = "point", fun = "mean", color = "black", pch = 23) +
    theme_zoo() +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    # ylim(c(0, 1)) +
    scale_color_manual(values = c("gray", "gray"), name = "Graph") +
    scale_fill_manual(values = c("gray", "gray"), name = "Graph") +
    ylab("Correlation (Kendall's tau)") +
    xlab("Sequence detected") +
    theme(legend.position = "none") +
    coord_capped_cart(expand = TRUE, bottom = "both", left = "both") +
    theme(axis.text.x = element_text(hjust = 0.85)) +
    theme(panel.spacing = unit(2, "lines"))
  return(figure)
}

plot_questionnaire_all <- function(cfg, paths) {
  figure <- plot_grid(
    plot_grid(
      plot_questionnaire_seq_detect(cfg, paths),
      plot_questionnaire_seq_when(cfg, paths),
      plot_questionnaire_seq_detect_order(cfg, paths),
      plot_questionnaire_seq_when_order(cfg, paths),
      nrow = 1, ncol = 4, rel_widths = c(0.2, 0.2, 0.3, 0.3), labels = c("a", "b", "c", "d"), vjust = 1
    ),
    plot_grid(
      plot_questionnaire_prob_ratings(cfg, paths),
      plot_questionnaire_prob_ratings_seq_detect(cfg, paths),
      nrow = 1, ncol = 2, rel_widths = c(0.5, 0.5), labels = c("e", "f")
    ),
    nrow = 2, ncol = 1, rel_heights = c(1, 1)
  )
  save_figure(plot = figure, "questionnaire", width = 9, height = 7)
  return(figure)
}
