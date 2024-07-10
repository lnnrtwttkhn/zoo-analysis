if (!requireNamespace("here")) install.packages("here")
source(here::here("code", "utilities", "setup.R"))
# task_type = c("main", "rest")
task_type <- c("rest")
job_script <- here::here("code", "decoding", "zoo-analysis-decoding-slope.R")
job_path <- here::here("job")

for (c_task in task_type) {
  for (c_sub in seq(1, 44)) {
    #   for (c_class in cfg$classification) {
    #     for (c_mask in cfg$mask_test) {
    #       for (c_train in cfg$train_set) {
    for (c_mask in c("occipito-temporal", "motor")) {
      if (c_mask == "occipito-temporal") {
        c_train = "stimulus"
      } else if (c_mask == "motor") {
        c_train = "response"
      }
      #         for (c_run in seq(1, 5)) {
      #           for (c_trial in seq(1, 24)) {
      #             for (c_tr in seq(1, 8)) {
      job_file <- file(job_path)
      output_part <- stringr::str_replace(paths$slopes, paste0(here::here(), "/"), "")
      code_part <- stringr::str_replace(job_script, paste0(here::here(), "/"), "")
      singularity_command <- paste(
        "apptainer exec --cleanenv --contain",
        sprintf("-B %s:/input:ro", paths$input),
        sprintf("-B %s:/code:ro", paths$code),
        sprintf("-B %s:/%s:rw", paths$slopes, output_part),
        paths$container
      )
      main_command <- paste(
        sprintf("Rscript --vanilla %s", code_part),
        sprintf("--subject %d", c_sub),
        # sprintf("--classification %s", c_class),
        sprintf("--mask_test %s", c_mask),
        sprintf("--train_set %s", c_train),
        # sprintf("--run %d", c_run),
        # sprintf("--trial_index %d", c_trial),
        # sprintf("--interval_tr %d", c_tr)
        sprintf("--data %s", c_task)
      )
      filename <- paste(c(
        sprintf("sub-%02d", c_sub),
        # sprintf("classification-%s", c_class),
        sprintf("mask-%s", c_mask),
        sprintf("train-%s", c_train),
        # sprintf("run-%02d", c_run),
        # sprintf("trial-%d", c_trial),
        # sprintf("tr-%d", c_tr)
        sprintf("task-%s", c_task)
      ), collapse = "_")
      path_logfile <- file.path(paths$logs, paste0(filename, "-%j.out"))
      job_info <- c(
        "#!/bin/bash",
        sprintf("#SBATCH --job-name zoo-analysis-slopes-%s", filename),
        sprintf("#SBATCH --partition %s", cfg$hpc$partition),
        sprintf("#SBATCH --time %s", cfg$hpc$time),
        sprintf("#SBATCH --mem %s", cfg$hpc$memory),
        sprintf("#SBATCH --cpus-per-task %s", cfg$hpc$cpus),
        sprintf("#SBATCH --output %s", path_logfile),
        "#SBATCH --mail-type NONE",
        paste(singularity_command, main_command)
      )
      writeLines(job_info, job_file)
      close(job_file)
      status_report(sprintf("write job for %s:", filename))
      system2(command = "sbatch", args = c(job_path))
      #           }
      #         }
      #       }
      #     }
      #   }
    }
  }
}
