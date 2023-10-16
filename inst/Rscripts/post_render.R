### This function will convert images in the docx-chapters to proper mschart ones.
### It defaults to the Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR")
report_path <- fs::path(Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR"),
                        "Rapporter", "Barnehageleder", "2022H")
mesos_groups <- list.dirs(fs::path(report_path, "mesos"),
                          full.names = FALSE, recursive = FALSE)
mesos_paths <- fs::path(report_path, "mesos", mesos_groups)

tryCatch(saros::post_render_docx_img_replacer(path = report_path),
         error = function(e) cli::cli_warn(e))

tryCatch(saros::create__headers_file(site_path = Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR"),
                            mesos_paths = mesos_paths,
                            mesos_usernames = mesos_groups,
                            mesos_passwords = mesos_groups,
                            global_username = "admin",
                            global_password = "arturead"),
         error = function(e) cli::cli_warn(e))
