for(cycle in c("2024")) {
  params$cycle <- cycle
  source(here::here(paths$r, "003_get_report_cycle_paths.R"))

  for(target_group in c("teacher", "student")) {
    params$target_group <- target_group
    # source(here::here(paths$r, paste0("200_prep_data_for_", params$cycle, "_", params$target_group, ".R")))
    # source(here::here(paths$r, paste0("900_draft_reports_", params$cycle, "_", params$target_group, ".R")))
  }
}

