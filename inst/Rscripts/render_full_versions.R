processed_files <- Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES")
processable_files <- processed_files[stringr::str_detect(fs::path_file(processed_files), "^_.+$")]
for(f in processable_files) {
  quarto::quarto_render(input = f, output_format = "all")
}
