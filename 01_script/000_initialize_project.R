here::i_am("01_script/000_initialize_project.R")

params <- list(project_title_abbreviation = "EXAMPLE",
               mesos_var = "f_uni")


## Named list with file and folder paths
paths <- list()
paths$data <- list()
# paths$data$population <- list()
# paths$data$sample <- list()
paths$data$survey <- list()
paths$data$saros_ready <- list()
paths$chapter_overview <- list()

paths$r <- here::here("01_script")
paths$resources <-
  here::here("02_resources")
# paths$drafts_produced <-
#   here::here("03_draft_generations")
# paths$drafts_completed <-
#   here::here("99_completed_drafts")


paths$site <-
  fs::path(Sys.getenv("USERPROFILE"), "Saros", params$project_title_abbreviation)
# paths$site_drafts_completed <-
#   fs::path(paths$site)
# paths$site_resources <-
#   fs::path(paths$site, "02_resources")

# paths$saros <-
#   here::here()

paths$nifu_global_draft_report_settings <- fs::path(Sys.getenv("USERPROFILE"), "NIFU", "Metode - General", "SAROS-core", "shared resources", "_draft_report_settings.yaml")
paths$nifu_global_refine_chapter_overview_settings <- fs::path(Sys.getenv("USERPROFILE"), "NIFU", "Metode - General", "SAROS-core", "shared resources", "_refine_chapter_overview_settings.yaml")

paths$map$fylker <- fs::path("..", "..", "Metode - General", "SAROS-core", "shared resources", "maps", "fylker2021.json")
paths$map$kommuner <- fs::path("..", "..", "Metode - General", "SAROS-core", "shared resources", "maps", "kommuner2021.json")



# df_labels <- list()
survey_data <- list()
chapter_overview <- list()
config_macro <- list()
config_mesos <- list()
output_files <- list()

