## Post-PISVEEP (rendering of website)

## Ikke nødvendig her om man lagrer direkte i lokal mappe ##
## Derimot nødvendig dersom man jobber med filene i skylagring ##
## OBS! Denne naive kopiering av filer vil medføre at cache/freeze kan måtte genereres på nytt

# fs::dir_copy(path = file.path(paths$drafts_completed),
#              new_path = file.path(paths$site),
#              overwrite = TRUE)

# Kopierer filer over til lokal midlertidig mappe for rendering av nettsted
fs::file_copy(path =
                c(file.path(paths$resources, "YAML", c("_quarto.yaml",
                                                       "_global.yaml")),
                  file.path(paths$resources, "CSS", c("styles.css",
                                                      "styles.scss")),
                  file.path(paths$resources, "CitationStyles", "bib_style.csl"),
                  file.path(paths$resources, "Rscripts", "general_formatting.R"),
                  file.path(paths$resources, "Rscripts", "general_formatting.R")),
              new_path =
                c(file.path(paths$site, c("_quarto.yaml",
                                          "_global.yaml")),
                  file.path(paths$site, c("styles.css",
                                          "styles.scss")),
                  file.path(paths$site, "bib_style.csl"),
                  file.path(paths$site, "general_formatting.R"),
                  file.path(paths$site, "Rapporter", "Innsamling 2", "general_formatting.R")),
              overwrite = TRUE)

fs::dir_copy(path = file.path(paths$resources, "_images"),
             new_path = file.path(paths$site, "_images"),
             overwrite = TRUE)

withr::with_dir(new = paths$site,
                code = {
                  quarto::quarto_add_extension(extension = "NIFU-NO/nifutypst", no_prompt = TRUE, quiet = TRUE)
                  quarto::quarto_add_extension(extension = "NIFU-NO/nifudocx", no_prompt = TRUE, quiet = TRUE)
                  quarto::quarto_add_extension(extension = "NIFU-NO/rename_duplicate_labels", no_prompt = TRUE, quiet = TRUE)
                  quarto::quarto_add_extension(extension = "NIFU-NO/remove_empty_headings", no_prompt = TRUE, quiet = TRUE)
                })

withr::with_envvar(new = c(LC_ALL="C"),
                   action = "replace",
                   code = system.time(
                     quarto::quarto_render(
                       input = paths$site,
                       as_job = FALSE,
                       output_format = c("all")
                     )))

lapply(unique(survey_data[[params$cycle]][[params_target_group]][[params$mesos_var]]),
       FUN = function(mesos_group) {
         all_files <-
           list.files(path=paths$site, pattern="^[^0]+_")
         all_files[!grepl(x=all_files, pattern="index\\.qmd")] |>
           lapply(FUN=function(f) {

             quarto::quarto_render(input=f, output_format="all",
                                   output_file=paste0(s, "_", f),
                                   execute_params = list(mesos_group = s))
        })
      })
