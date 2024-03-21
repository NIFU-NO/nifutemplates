library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
conflicted::conflicts_prefer(dplyr::filter, dplyr::lag, .quiet = TRUE)

df_labels[[params$cycle]]$student <-
  here::here(paths$labels[[params$cycle]]$student) %>%
  readxl::read_excel() %>%
  dplyr::mutate(nr = stringr::str_replace_all(nr, pattern = " - ", replacement = ".")) %>%
  tidyr::unite(col = "label", c(prefix, suffix), sep = " - ", na.rm = TRUE) %>%
  dplyr::select(-nr)


survey_data[[params$cycle]]$student <-
  paths$data$survey[[params$cycle]]$student %>%
  readRDS() %>% #labelled::lookfor(details = F)
  labelled::unlabelled()


# labelled::look_for(survey_data[[params$cycle]]$student, details = T) %>% View()

qs::qsave(x = survey_data[[params$cycle]]$student,
          file = here::here(paths$data$saros_ready[[params$cycle]]$student))
