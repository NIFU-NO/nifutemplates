paths$data$survey[[params$cycle]]$student <-
  here::here("..", "..", "Survey - elever", paste0(params$cycle, " - Data"), "raw_cyc3.rds")

paths$data$saros_ready[[params$cycle]]$student <-
  here::here("..", "..", "Survey - elever", paste0(params$cycle, " - Data"), "stu_sarosready.qs")
fs::dir_create(fs::path_dir(paths$data$saros_ready[[params$cycle]]$student))

paths$chapter_overview[[params$cycle]]$student <-
  here::here("..", "..", "Survey - elever", paste0(params$cycle, " - Data"), "stu_chapter_overview.xlsx")

paths$labels[[params$cycle]]$student <-
  here::here("labels.xlsx")
