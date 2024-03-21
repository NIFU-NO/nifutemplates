## Post-PISVEEP (rendering of website)

# Kompiler nettstedet og all output. Alternativ 1: Lagre lokalt i en midlertidig (temp)-mappe.
# Fordeler:
#   - Viktigst: UNNGÅR SHAREPOINT/ONEDRIVE BEGRENSNING PÅ LENGDE PÅ FILBANER.
#   - Nettstedets filer (inkl Word-versjon, PDF-versjon, etc) er i seg selv ikke viktige - de kan kompileres på nytt. Trenger derfor ikke å lagres i skyen/Sharepoint
#   - Filene kan til sammen utgjøre mange MB og de trengs ikke etter at de er lagt ut på serveren.
# Ulemper:
#   - Litt vanskeligere å finne disse filene (men se nedenfor for funksjoner/skript)

saros::prepare_safe_render(
  remote_basepath = "/home/nifuno/domains/stephan/crithise.nifu.no/public_html/",
  from_folders = c(file.path(paths$drafts_completed, "Main"),
                   file.path(paths$drafts_completed, "Rapporter"),
                   file.path(paths$resources, "_images"),
                   file.path(paths$resources, "_extensions")),
  from_files = c(file.path(paths$resources, "YAML", c("_quarto.yaml",
                                                      "_nifu_global.yaml")),
                 file.path(paths$drafts_completed, c("index.qmd",
                                                     "index.qmd")),
                 file.path(paths$resources, "CSS", c("styles.css",
                                                     "styles.scss")),
                 file.path(paths$resources, "CitationStyles", "nifu.csl")),
  local_basepath = paths$drafts_completed,
  site = paths$site,
  rel_path_base_to_parent_of_user_restricted_folder = file.path("Rapporter", params$cycle, "Larere", "Mesos"),
  overwrite = TRUE,
  prompt=FALSE)



withr::with_envvar(new = c(LC_ALL="C"),
                   action = "replace",
                   code = system.time(
                     quarto::quarto_render(
                       input = paths$site,
                       as_job = FALSE,
                       output_format = c("all")
                     )))



c("2022-2023") |>
  rlang::set_names() |>
  purrr::map(.f = function(cycle) {
    c("student", 
      "teacher") |>
      rlang::set_names() |>
      purrr::map(.f = function(response_group) {
        
        if(!is.null(survey_data[[cycle]][[response_group]])) {
          
          survey_data[[cycle]][[response_group]] |>
            dplyr::rename(username = tidyselect::any_of(c("school", "skole"))) %>% 
            dplyr::distinct(username) |>
            dplyr::filter(!is.na(.data$username)) |>
            dplyr::mutate(folder = saros::filename_sanitizer(.data$username, max_chars = 12),
                          parent_folder_path_rel = fs::path(paste0(.env$cycle, 
                                                                   "_", 
                                                                   if(.env$response_group == "teacher") "Larer" else "Elever"), 
                                                                   "Skoler"))
        } else {
          cli::cli_warn("Nothing found for {cycle} and {response_group}");
          NULL
        }
        
      }) |>
      dplyr::bind_rows(.id = "response_group")
  }) |>
  dplyr::bind_rows(.id = "cycle") |>
  writexl::write_xlsx(fs::path(paths$saros, "_username_folder_matching_df.xlsx"))

saros::setup_access_restrictions(
  remote_basepath = "/home/nifuno/domains/stephan/crithise.nifu.no/public_html/",
  local_basepath = file.path(paths$site, "_site"),
  rel_path_base_to_parent_of_user_restricted_folder =
    file.path("Rapporter", c("2022-2023_Elever", "2022-2023_Larere"), "Skoler"),
  local_main_password_path = file.path("..", "..", "..", "..", "NIFU", "Metode - Sensitivt - Sensitivt", ".main_htpasswd_private"),
  universal_usernames = c("admin", "nifu"),
  username_folder_matching_df = readxl::read_excel(fs::path(paths$saros, "_username_folder_matching_df.xlsx")),
  password_input = "10")

