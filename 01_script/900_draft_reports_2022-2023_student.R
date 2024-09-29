library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
conflicted::conflicts_prefer(dplyr::filter, dplyr::lag, .quiet = TRUE)

# Read in survey_data from disk - to ease caching
survey_data[[params$cycle]][[params$target_group]] <-
  saros.base::ex_survey

### Chapter overview
chapter_overview[[params$cycle]][[params$target_group]] <-
  saros.base::ex_survey_ch_overview

############################################################
## config_report.R

config_macro[[params$cycle]][[params$target_group]] <-
  saros.utils::read_default_draft_report_args(path = paths$nifu_global_draft_report_settings)

config_macro[[params$cycle]][[params$target_group]]$title <- paste0(params$cycle, " - ", params$target_group)
config_macro[[params$cycle]][[params$target_group]]$path <- fs::path(paths$site, "Rapporter", params$cycle, params$target_group)

# tryCatch(fs::dir_delete(fs::path(paths$site,
#                                  "Rapporter", params$cycle, "Elever")), error=function(e) cli::cli_warn(e))

chapter_structure <-
  saros.base::refine_chapter_overview(
    chapter_overview = chapter_overview[[params$cycle]][[params$target_group]],
    data = survey_data[[params$cycle]][[params$target_group]],
    chunk_templates =
      saros.base::get_chunk_template_defaults() |>
        dplyr::filter(!(.template_name %in% c("cat_table_html", "sigtest_table_html"))),
    always_show_bi_for_indep = c("x1_sex", "x3_nationality"),
    hide_bi_entry_if_sig_above = .05
  )

saros.base::draft_report(
  chapter_structure = chapter_structure,
  data = survey_data[[params$cycle]][[params$target_group]],
  !!!config_macro[[params$cycle]][[params$target_group]]
)


## Mesos-rapporter
config_mesos[[params$cycle]][[params$target_group]] <- config_macro[[params$cycle]][[params$target_group]]
config_mesos[[params$cycle]][[params$target_group]]$title <- ""
saros.base::draft_report(
  chapter_structure = chapter_structure,
  data = survey_data[[params$cycle]][[params$target_group]],
  !!!config_mesos[[params$cycle]][[params$target_group]]
)

saros.base::draft_report(
  data =
    survey_data[[params$cycle]][[params$target_group]] |>
      dplyr::filter(dplyr::n() > 10, .by = tidyselect::all_of(params$mesos_group)) |>
      labelled::set_variable_labels(.labels = setNames(
        labelled::var_labels(survey_data[[params$cycle]][[params$target_group]][[params$mesos_group]]),
        params$mesos_group
      )),
  chapter_overview =
    chapter_overview[[params$cycle]][[params$target_group]] %>%
      dplyr::mutate(indep = NULL),
  sort_by = NULL,
  !!!config_mesos[[params$cycle]][[params$target_group]],
  path = fs::path(paths$site, "Rapporter", params$cycle, params$target_group, params$mesos_group)
)
