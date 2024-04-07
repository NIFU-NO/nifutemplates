## Post-PISVEEP (rendering of website)

## Ikke nødvendig her om man agrer direkte i lokal mappe ##
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
                  file.path(paths$resources, "CitationStyles", "bib_style.csl")),
              new_path =
                c(file.path(paths$site, c("_quarto.yaml",
                                          "_global.yaml")),
                  file.path(paths$site, c("styles.css",
                                          "styles.scss")),
                  file.path(paths$site, "bib_style.csl")),
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

