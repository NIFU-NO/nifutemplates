################################################################################
######################## Report-specific settings ##############################
################################################################################
here::i_am(path = "")

## Innhent årgang fra mappenavnet til der hvor qmd_filene og data ligger?
cycle <- "2023V" # Alltid på formatet åååå-semesterbokstav. Dersom spesialundersøkelse, legg til f.eks. -L for lærer, etc

cycle_pretty <- # Trekke ut til en egen excel-fil eller eget generate_report skript?
  stringr::str_replace(cycle, 
                     "([[:digit:]]{4})([[:alpha:]]{1})",
                     "\\2\\1") |> 
  stringr::str_replace_all(pattern = "^[[:alpha:]]", 
                           replacement = function(.x) {
                             dplyr::case_when(.x=="H" ~ "høsten ", 
                                                           .x=="V" ~ "våren ",
                                                           .x=="S" ~ "sommeren ",
                                                           .x=="L" ~ "til lærere i ",
                                                           .x=="X" ~ "ekstrasurvey ",
                                                           TRUE ~ .x)
                             })
