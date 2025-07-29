get_behavior_single_run <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_task)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude), ] %>%
    .[event_type == "response", ] %>%
    .[condition == "Single", ] %>%
    .[, by = .(id, session, condition, run), .(
      num_trials = .N,
      mean_log_response_time = mean(log_response_time[accuracy == 1], na.rm = TRUE),
      mean_response_time = mean(response_time[accuracy == 1], na.rm = TRUE),
      mean_accuracy = mean(accuracy) * 100
    )] %>%
    verify(num_trials == cfg$single$num_trials_run) %>%
    .[, num_trials := NULL] %>%
    .[, by = .(id), run_counter := as.numeric(rleid(run))] %>%
    verify(length(unique(run_counter)) == cfg$single$num_runs) %>%
    verify(unique(run_counter) %in% seq(cfg$single$num_runs)) %>%
    setorder(id, session, run, run_counter) %>%
    save_data(paths$source$behavior_single_run) 
}

get_behavior_single_run_stat <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_single_run)
  ttest_cfg <- list(
    formula = "value ~ 1",
    adjust_method = "bonferroni",
    paired = FALSE,
    mu = 100/cfg$num_nodes,
    alternative = "greater"
  )
  dt_output <- dt_input %>%
    melt(id.vars = c("id", "session", "condition", "run"), measure.vars = c("mean_accuracy", "mean_log_response_time")) %>%
    .[, by = .(run, variable), .(ttest = list(get_ttest(.SD, ttest_cfg)))] %>%
    unnest(ttest) %>%
    get_pvalue_adjust(., ttest_cfg) %>%
    save_data(paths$source$behavior_single_run_stat)
  return(dt_output)
}

get_behavior_single_lme_all <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_single)
  model <- lmerTest::lmer(
    formula = mean_accuracy ~ run + (1 | id), control = cfg$lcctrl,
    data = dt_input,
    subset = NULL,
    weights = NULL,
    na.action = na.omit,
    offset = NULL,
    REML = TRUE
  )
  summary(model)
  model1_stat <- broom::tidy(stats::anova(model))
  report_lme_model1 = report_lme_stats(
    num_df = model1_stat$NumDF,
    den_df = model1_stat$DenDF,
    f_value = model1_stat$statistic,
    p_value = model1_stat$p.value
  )
}

get_behavior_single_lme_s1 <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behavior_single)
  model <- lmerTest::lmer(
    formula = mean_accuracy ~ run_counter + (1 + run_counter | id), control = cfg$lcctrl,
    data = dt_input %>% .[run_counter != 1, ],
    subset = NULL,
    weights = NULL,
    na.action = na.omit,
    offset = NULL,
    REML = TRUE
  )
  summary(model)
  car::Anova(model)
  model2_stat = broom::tidy(stats::anova(model))
  report_lme_model2 = report_lme_stats(
    num_df = model2_stat$NumDF,
    den_df = model2_stat$DenDF,
    f_value = model2_stat$statistic,
    p_value = model2_stat$p.value
  )
}

get_behavior_single_stat_report <- function(cfg, paths) {
  dt1 <- load_data(paths$source$behavior_single_stat)
  cat(codeblock(c(
    sprintf("Behavioral performance in runs of the recall task was high and surpassed the chance-level by far (%s).", report_t_stats(dt_input, scope = "min")$latex),
    "In order to examine whether behavioral accuracy on recall trials was stable across all task runs of the study, we conducted a \\gls{LME} model that included all nine task runs of the recall condition as the fixed effects of interest as well as random intercepts and slopes for each participant.",
    sprintf("The results showed a significant main effect of task run indicating that behavioral accuracy increased across task runs, %s.", report_lme_model1$latex),
    "In order to further characterize the origin of this effect, we also conceived a reduced model that did not include the data from the first task run.",
    sprintf("In this reduced model, the main effect of task run was not significant, %s, indicating that increments in behavioral accuracy mainly stemmed from an increase in learning performance from the first to the second run and that performance stabilized from the second run onwards.", report_lme_model2$latex)
  )))
}

get_behavior_single_sequence <- function(cfg, paths) {
  dt_input <- load_data(paths$source$behav_task)
  dt_output <- dt_input %>%
    .[!(id %in% cfg$sub_exclude), ] %>%
    .[event_type == "response", ] %>%
    .[condition %in% c("Single", "Sequence"), ] %>%
    .[, by = .(id, condition), .(
      num_trials = .N,
      mean_log_response_time = mean(log_response_time[accuracy == 1], na.rm = TRUE),
      mean_response_time = mean(response_time[accuracy == 1], na.rm = TRUE),
      mean_accuracy = mean(accuracy) * 100
    )] %>%
    save_data(paths$source$behavior_single_sequence)
}
