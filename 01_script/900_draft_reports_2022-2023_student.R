library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
conflicted::conflicts_prefer(dplyr::filter, dplyr::lag, .quiet = TRUE)

# Read in survey_data from disk - to ease caching
survey_data$student[[params$cycle]] <-
  qs::qread(file = here::here(paths$data$saros_ready$student[[params$cycle]]),
            strict = TRUE, nthreads = 4)

### Chapter overview
chapter_overview$student[[params$cycle]] <-
  tibble::tibble(chapter = c("Kildekritikk", 
                             "Argumentasjon", 
                             "Ulv, ulv, ulv", 
                             "Observasjon og forklaring", 
                             "Meninger", 
                             "Tillit til forskere", 
                             "Sosiale medier",
                             "Tillit til kilder",
                             "Samtaler i hjemmet"),
                 dep = c("matches('^q_[2-9]_|^q_1[0-7]_')",  
                         "matches('^q_1[7-9]_|^q_2[0-9]_|^q_30_')", 
                         "matches('^u_')", 
                         "matches('^q_3[1-9]')", 
                         "matches('^m_')", 
                         "matches('^f_')", 
                         "matches('^so_')", 
                         "matches('^k_')",
                         "matches('^s_')"),
                 indep = c("grade_cyc3, treated"))

############################################################
## config_report.R

config_macro$student[[params$cycle]] <- 
  saros::read_default_draft_report_args(
    path = fs::path(paths$resources, "YAML", "_report_generation_setup.yaml")
  )

config_macro$student[[params$cycle]]$title <- paste0(params$cycle, " - Elever") 

# tryCatch(fs::dir_delete(fs::path(paths$site,
#                                  "Rapporter", params$cycle, "Elever")), error=function(e) cli::cli_warn(e))

# saros::draft_report(data = survey_data$student[[params$cycle]],
#                     chapter_overview = chapter_overview$student[[params$cycle]],
#                     !!!config_macro$student[[params$cycle]],
#                     path = fs::path(paths$site, "Rapporter", params$cycle, "Elever"))


## Mesos-rapporter
config_mesos$student[[params$cycle]] <- config_macro$student[[params$cycle]]
config_mesos$student[[params$cycle]]$title <- ""
config_mesos$student[[params$cycle]]$mesos_report <- TRUE
config_mesos$student[[params$cycle]]$sort_by <- ".variable_name"
config_mesos$student[[params$cycle]]$element_names <- c("uni_cat_prop_plot")
config_mesos$student[[params$cycle]]$mesos_var <- "school"


saros::draft_report(
  data =
    survey_data$student[[params$cycle]] %>%
    dplyr::group_by(.data[["school"]]) %>%
    dplyr::filter(dplyr::n()>10) %>%
    dplyr::ungroup() %>%
    labelled::set_variable_labels(school = "Skole"),
  chapter_overview =
    chapter_overview$student[[params$cycle]] %>%
    dplyr::mutate(indep = NULL),
  sort_by = NULL,
  !!!config_mesos$student[[params$cycle]],
  path = fs::path(paths$site, "Rapporter", params$cycle, "Elever", "Skoler"))

### PISVEEP: Manuelt arbeid! ###
