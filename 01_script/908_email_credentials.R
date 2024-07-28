

eposter <-
  saros.utils::create_email_credentials(local_basepath = paths$site_drafts_completed,
                                  rel_path_base_to_parent_of_user_restricted_folder =
                                      fs::path("Rapporter", paste0(cycle, c("_Larere", "_Elever")), "Skoler"),
                                  email_data_frame =
                                    readxl::read_excel(here::here("..", "..", "Rekruttering av skoler og utvalg", "Utvalgslister.xlsx"), sheet="Skoler") |>
                                    dplyr::select(username=Skolenavn, email=`Kontaktpersons epost`, Fornavn = `Skoleleder/kontakt-fornavn`) |>
                                    dplyr::mutate(username = stringr::str_replace(username, "s+kolan| skole NÅ: Krokstadøra oppvekstsenter| oppvekstsenter| barne- og ungdoms+k[uo]le| barnesk[ou]le| sk[ou]le", "")) %>%
                                    tidyr::separate_longer_delim(cols = email, delim=";") %>%
                                    tidyr::separate_longer_delim(cols = Fornavn, delim=";") %>%
                                    dplyr::filter(!is.na(email), !is.na(username), username != "Dalabrekka") %>%
                                    dplyr::distinct(username, email, .keep_all = TRUE),
                                  email_col= "email",
                                  username_col = "username",
                                  local_main_password_path = file.path(Sys.getenv("USERPROFILE"), "NIFU", "Metode - Sensitivt - Sensitivt", ".main_htpasswd_private"),
                                  email_body = paste0("
Hei {Fornavn}!

CriThiSE-prosjektets skolespesifikke resultater for elevundersøkelsen 2023 er nå tilgjengelig på https://crithise.nifu.no/Rapporter/2022-2023_Elever/ for de skolene med høy nok antall innsamlede svar og samtykke for elever.
Vi beklager at det tok lang tid før vi fikk dette ut på nettsider. Dette var nybrottsarbeid som vi håper kommer til nytte i andre NIFU-prosjekter med skoler, gjerne litt raskere etter datainnsamling. Håper uansett figurene kan være interessante.

For å få tilgang til sammendrag av svar for {username}, gå til 'Skoler' og bla til rapporten du ønsker. Følgende brukernavn og passord er for hele skolen og du som kontaktperson er ansvarlig for behandling av tilgang lokalt.

  Brukernavn: {username}
  Passord: {password}

For de få av dere som mottok tilsvarende for lærerundersøkelsen så skal det være samme brukernavn og passord.
For mer informasjon, se veiledningen på https://saros.nifu.no/#tips-og-triks-for-brukere. Merk at vi ikke har kapasitet til å tolke, forklare eller tilpasse utover det som er tilgjengeliggjort. Dersom en side eller figur er tom skyldes det blant annet anonymitetshensyn.


På vegne av prosjektgruppen i Critical Thinking in Sustainability Education (CriThiSE),
Stephan Daus
Nordisk institutt for studier av innovasjon, forskning og utdanning (NIFU)
"),
email_subject = "Innloggingsdetaljer for {username} til CriThiSE-prosjektets elevresultater")



if(nrow(eposter)>0) {
  for(i in seq_len(nrow(eposter))) {

      ny_epost <-
        outlb$get_drafts()$
        create_email()$
        set_body(eposter[i, "body"][[1]], content_type = "html")$
        set_recipients(to = eposter[i, "to"][[1]])$
        set_subject(eposter[i, "subject"][[1]])

      if(isTRUE(send_emails)) {
        ny_epost$send()

        cat(paste0("Eposter med passord er utsendt til ", params$cycle, "/", params$response_group, " den ", Sys.time()),
            file = here::here("_email_credentials_log.txt"), append = TRUE)
      }
  }
} else cli::cli_warn("Ingen eposter genereres for {params$cycle}/{params$response_group}")
