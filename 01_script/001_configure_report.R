for(cycle in c("2022-2023")) {
  params$cycle <- cycle
  source(here::here(paths$r, "003_get_report_cycle_paths.R"))
  
  for(target_group in c("teacher")) { # Husk å manuelt legge inn bilder for macro-student
    params$target_group <- target_group
    source(here::here(paths$r, paste0("200_prep_data_for_", params$cycle, "_", params$target_group, ".R")))
    # source(here::here(paths$r, paste0("900_draft_reports_", params$cycle, "_", params$target_group, ".R")))
  }
}




## Lage eposter, og eventuelt sende dem ut.
## Sett følgende til TRUE for å faktisk sende ut.
## Bør sjekke at de ikke alt har sendt ut tidligere.
send_emails <- FALSE
sent_to <- list("2022-2023" = c("teacher"))
outlb <- Microsoft365R::get_business_outlook()
response_group <- "student"

for(cycle in c("2022-2023")) {
  if(cycle %in% names(sent_to) && response_group %in% sent_to[[cycle]]) next
  params$cycle <- cycle
  source(here::here(paths$r, "003_get_report_cycle_paths.R"))
  print(c(cycle))
  source(here::here(paths$r, "906_email_credentials.R"))
  if(isTRUE(send_emails)) {
    if(any(duplicated(c(sent_to[[cycle]])))) cli::cli_abort("Duplicates!")
    sent_to[[cycle]] <- c(sent_to[[cycle]])
  }
}
dput(sent_to)
str(eposter)
