# Fixme's -----------------------------------------------------------------

# Hent mrs5-kodebok -------------------------------------------------------

#' Hent inn kodebok
#' 
#' @description
#' Leser inn kodebok på mrs5-struktur fra angitt filsti. 
#' skjemanavn angir hvilket skjema som skal leses inn. Hvis skjemanavn = `NULL`
#' leses alle skjema inn. 
#'
#' @param filsti Plassering av kodebok på disk. Forventer .xlsx-format. 
#' @param skjemanavn Navn på skjema som skal leses inn. Hvis skjemanavn = `NULL`
#' leses alle skjema inn. 
#'
#' @returns
#' Listeobjekt med tre navngitte tibbler; `Kodebok`, `Kategoriske` og `Regler` 
#' som inneholder informasjon om variabler i de innleste skjema. 
#' 
#' @export
#'
#' @examples
#' # Filsti til eksempeldata
#' filsti_eksempel = system.file("extdata", "eksempelkodebok.xlsx", package = "rapwhale")
#' 
#' # Les inn alle skjema 
#' mrs5_hent_kodebok(filsti = filsti_eksempel, skjemanavn = NULL)
#' 
#' # Les inn spesifikt skjema 
#' mrs5_hent_kodebok(filsti = filsti_eksempel, skjemanavn = "Testskjema")
mrs5_hent_kodebok = function(filsti, skjemanavn = NULL, valider = FALSE) {

  # FIXME - Legge til sjekk for lesetilgjengelighet av kb-fil.
  
  # # Sjekke at fil er tilgjengelig for å lese inn
  #   assert_that(file.exists(filsti), 
  #               msg = "Finner ikke fil på angitt filsti")
  #   
  #   assert_that(isOpen(file(filsti, open = "r")), 
  #               msg = "Filen er åpen i et annet program og kan ikke leses inn.")
  
    kb_parsed = mrs5_parse_kodebok(filsti, skjemanavn)
    
    kb_kanonisk = mrs5_lag_kanonisk_kb(kb_parsed)
    
    if(valider) {
      rapwhale:::valider_kanonisk_kodebok(kb_kanonisk)
      }
    
    return(kb_kanonisk)
}    

# Parse rådata -------------------------------------------------------------

#' Les inn kodebok
#'
#' @description
#' Leser inn rådata fra aktuelle `skjemanavn` fra MRS5-kodebok funnet på `filsti`.
#' Kaller nødvendige hjelpefunksjoner for å lese inn de ulike fanene og objektene.
#'
#' @param filsti Tekststreng som angir filsti til kodebok.
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn slik de er
#' navngitt i kodebok. Hvis `skjemanavn = NULL` hentes kodebok for alle skjema inn.

#' @return
#' Listeobjekt som inneholder listene:
#' \itemize{
#'    \item versjonslogg
#'    \item metainfo
#'    \item felter
#'    \item regler
#'    }
#'
#'  Hvert skjema som leses inn får en tibble i hver liste.
#'
#' @export
#'
#' @examples
#' # Filsti til eksempeldata
#' filsti_eksempel = system.file("extdata", "eksempelkodebok.xlsx", package = "rapwhale")
#'
#' # Hente rådata for alle skjema i kodebok
#' kb_register = mrs5_parse_kodebok(filsti = filsti_eksempel, skjemanavn = NULL)
#'
#' # Hente rådata for et enkelt skjema
#' kb_testskjema = mrs5_parse_kodebok(filsti = filsti_eksempel, skjemanavn = "testskjema")
mrs5_parse_kodebok = function(filsti, skjemanavn = NULL) {
  kodebok_raa = list(
    "versjonslogg" = list(),
    "metainfo" = list(),
    "felter" = list(),
    "regler" = list()
  )

  skjemakobling = mrs5_trekk_ut_skjemanavn(filsti, skjemanavn)

  for (skjema in skjemakobling$fanenavn) {
    d_parsed_kodebok = mrs5_parse_kodebok_skjema(filsti = filsti, skjemanavn = skjema)

    # hent skjemanavn for det aktuelle fanenavnet for å navngi tibbles i listene
    skjemanavn_cased = skjemakobling$navn_i_fane[skjemakobling$fanenavn == skjema]

    kodebok_raa[["versjonslogg"]][[skjemanavn_cased]] = d_parsed_kodebok[["versjonslogg"]]
    kodebok_raa[["metainfo"]][[skjemanavn_cased]] = d_parsed_kodebok[["metainfo"]]
    kodebok_raa[["felter"]][[skjemanavn_cased]] = d_parsed_kodebok[["felter"]]
    kodebok_raa[["regler"]][[skjemanavn_cased]] = d_parsed_kodebok[["regler"]]
  }

  return(kodebok_raa)
}

#' Les inn skjema fra kodebok
#'
#' Leser inn og lagrer alle aktuelle faner og objekter for ett `skjemanavn`
#' i MRS5-kodebok funnet på `filsti`.
#' Denne funksjonen er ikke ment å kalles direkte.
#'
#' @param filsti Tekststreng som angir filsti til kodebok.
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn.
#'
#' @return
#' Listeobjekt med tibbles som inneholder rådataversjon av metainfo,
#' versjonslogg, felter og regler for `skjemanavn`.
#'
#' @keywords internal
mrs5_parse_kodebok_skjema = function(filsti, skjemanavn) {
  d_skjema = mrs5_parse_kodebok_skjema_meta(filsti, skjemanavn)
  d_versjonslogg = mrs5_hent_versjonslogg(d_skjema)
  d_metainfo = mrs5_hent_metainfo(d_skjema)

  d_felter = mrs5_parse_kodebok_felter(filsti, skjemanavn)

  d_regler = mrs5_parse_kodebok_regler(filsti, skjemanavn)

  return(
    list(
      "versjonslogg" = d_versjonslogg,
      "metainfo" = d_metainfo,
      "felter" = d_felter,
      "regler" = d_regler
    )
  )
}


#' les inn hovedfane for skjema
#'
#' @description
#' Leser inn hovedfane for `skjemanavn` fra MRS5-kodebok. Dette skjemaet
#' inneholder metadata for det aktuelle skjemaet.
#' Funksjonen er ikke ment å kalles direkte.
#'
#' @param filsti Tekststreng som angir filsti til kodebok.
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn.
#'
#' @return
#' Returnerer en tibble med rådataversjon av generelt-fane for `skjemanavn`
#' fra kodebok.
#'
#' @keywords internal
mrs5_parse_kodebok_skjema_meta = function(filsti, skjemanavn) {
  
  # FIXME - Gjør robust for å fange opp eventuelle endringer i struktur 
  # Assert at alle forventede felter eksisterer i kolonne 1
  d_skjemanavn = suppressMessages(read_xlsx(filsti,
    sheet = skjemanavn,
    col_names = FALSE,
    col_types = "text"
  ))

  return(d_skjemanavn)
}

#' les inn felter for skjema
#'
#' @description
#' Leser inn rådataversjon av 'felter-fane' for `skjemanavn` fra MRS5-kodebok.
#' Denne fanen inneholder informasjon om variablene i det aktuelle `skjemanavn`.
#' Funksjonen er ikke ment å kalles direkte.
#'
#' @param filsti Tekststreng som angir filsti til kodebok.
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn.
#'
#' @return
#' Returnerer en tibble med rådataversjon av felter-fane for `skjemanavn`
#' fra kodebok.
#'
#' @keywords internal
mrs5_parse_kodebok_felter = function(filsti, skjemanavn) {
  d_skjemanavn = suppressMessages(read_xlsx(filsti,
    sheet = paste0(skjemanavn, "-felter"),
    col_names = TRUE,
    col_types = "text"
  )) |>
    add_column("Skjemanavn" = str_remove(skjemanavn, "\\d\\-"), .before = 1)

  return(d_skjemanavn)
}

#' Les inn regler for skjema
#'
#' @description
#' Leser in rådataversjon av 'regler-fane' for `skjemanavn` fra MRS5-kodebok.
#' Denne fanen inneholder infomasjon om validering som gjøres for de ulike
#' variablene ved registrering til registeret.
#' Funksjonen er ikke ment å kalles direkte.
#'
#' @param filsti Tekststreng som angir filsti til kodebok.
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn.
#'
#' @return
#' Returnerer en tibble med rådataversjon av regler-fane for `skjemanavn`
#' fra kodebok.
#'
#' @keywords internal
mrs5_parse_kodebok_regler = function(filsti, skjemanavn) {
  d_skjemanavn = suppressMessages(read_xlsx(filsti,
    sheet = paste0(skjemanavn, "-regler"),
    col_names = TRUE,
    col_types = "text"
  )) |>
    add_column("Skjemanavn" = str_remove(skjemanavn, "\\d\\-"), .before = 1)

  return(d_skjemanavn)
}

# Hjelpefunksjoner for Parse ----------------------------------------------

#' Lag skjemakobling
#'
#' @description
#' Sjekker om `skjemanavn` er en gyldig tekst-vektor hvis oppgitt. Hvis det ikke
#' er gitt en tekstvektor som argument hentes alle skjemanavn fra kodebok
#' funnet på `filsti`.
#'
#' @param filsti Tekststreng som angir filsti til kodebok.
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn.
#'
#' @return
#' Returnerer en tibble med fanenavn fra kodebok og menneskevennlige skjemanavn
#' for de samme fanene.
#'
#' @keywords internal
mrs5_trekk_ut_skjemanavn = function(filsti, skjemanavn) {
  mrs5_kontroller_argumenter(filsti, skjemanavn)

  skjemakobling = mrs5_les_skjemanavn(filsti)

  if (is.null(skjemanavn)) {
    skjema_ut = skjemakobling
  } else {
    (
      skjema_ut = skjemakobling[which(
        skjemakobling$skjemanavn %in% str_to_lower(skjemanavn)
      ), ]
    )
  }
  return(skjema_ut)
}


#' Typekontroll for argumenter til mrs5_parse-funksjoner
#'
#' @description
#' Typekontroll for argumenter som brukes i mrs5-parsefunksjoner.
#' Ikke ment å kalles direkte, men brukes for å kontrollere argumenter
#' og gi fornuftige feilmeldinger hvis det er problemer med argumentene.
#'
#' Sjekker også at skjemanavn som oppgis faktisk finnes i kodebok
#'
#' @param filsti tekststreng som indikerer plassering av kodebok.
#' @param skjemanavn en tekst-vektor med skjemanavn som skal leses inn. Hvis
#' `NULL` leses alle skjema fra kodeboken inn.
#'
#' @return
#' Returnerer TRUE hvis argumentene passerer kontroll. Hvis ikke returneres
#' aktuell feilmelding.
#'
#' @keywords internal
mrs5_kontroller_argumenter = function(filsti, skjemanavn) {
  # kontrollerer filsti
  assert_that(is.string(filsti),
    msg = "Filsti må være en tekststreng"
  )
  assert_that(has_extension(filsti, ext = "xlsx"),
    msg = "Kodebok må være en .xlsx-fil"
  )
  assert_that(is.readable(filsti),
    msg = paste0("Finner ikke kodebok på ", filsti)
  )

  # Kontrollere skjemanavn
  assert_that((is.character(skjemanavn) | is.null(skjemanavn)),
    msg = "Skjemanavn må være NULL eller en tekst-vektor"
  )

  # kontroll at skjemanavn finnes i kodebok
  skjemanavn_kobling = mrs5_les_skjemanavn(filsti)

  skjemanavn_lowercase = str_to_lower(skjemanavn)

  assert_that(
    all(skjemanavn_lowercase %in% skjemanavn_kobling$skjemanavn),
    msg = paste0(
      "Skjemanavn: ",
      str_c("«", skjemanavn_lowercase[which(!skjemanavn_lowercase %in% skjemanavn_kobling$skjemanavn)], "»",
        collapse = ", "
      ), " finnes ikke i kodebok"
    )
  )
}

#' Les inn fanenavn for mrs5-kodebok
#'
#' @description
#' Henter inn fanenavn for en MRS5-kodebok og returnerer et koblingsskjema
#' med fanenavn og menneskevennlig navn for de unike skjema som finnes i kodebok.
#'
#' @param filsti tekststreng som indikerer plassering av kodebok.
#'
#' @return
#' Returnerer en tibble med variablene fanenavn og skjemanavn.
#' Fanenavn inneholder fanenavn slik de er i kodebok,
#' skjemanavn inneholder menneskevennlig versjon som for eksempel
#' "testskjema" for fanenavn "1-Testskjema".
#'
#' @export
#'
#' @examples
#' # Filsti til eksempelkodebok
#' filsti_eksempelkodebok = system.file("extdata", "eksempelkodebok.xlsx", package = "rapwhale")
#'
#' # henter skjemanavn fra kodebok
#' skjemanavn = mrs5_les_skjemanavn(filsti = filsti_eksempelkodebok)
mrs5_les_skjemanavn = function(filsti) {
  navn_fra_kb = tibble(fanenavn = excel_sheets(filsti)) |>
    mutate(skjemanavn = str_remove(str_to_lower(fanenavn),
      pattern = "\\d+\\-"
    ),
    navn_i_fane = str_remove(fanenavn, pattern = "\\d\\-")) |>
    slice(seq(2, length(skjemanavn), by = 3))

  return(navn_fra_kb)
}

#' Henter ut versjonslogg for skjema
#'
#' @description
#' Henter versjonslogg fra generelt-fane for `skjemanavn`.
#' Inndata må være rådataversjon av generelt-fane for `skjemanavn`
#' hentet ut med mrs5_parse_kodebok_skjema_meta.
#'
#' @param parsed_generelt rådataversjon av generelt-fane fra kodebok.
#'
#' @return
#' Returnerer tibble med versjonslogg for `skjemanavn`.
#'
#' @keywords internal
mrs5_hent_versjonslogg = function(parsed_generelt) {
  assert_that(is.data.frame(parsed_generelt))

  skjemanavn = parsed_generelt[[1, 2]]

  # finne NA-rad for å splitte metainfo og versjonslogg
  na_rad = parsed_generelt |>
    mutate(radnr = row_number()) |>
    filter(is.na(...1)) |>
    pull(radnr)

  # eProm-skjema har en ekstra tabell i skjemanavn-fane
  siste_rad = if_else(length(na_rad) == 1,
    nrow(parsed_generelt),
    na_rad[2]
  )

  d_versjonslogg = parsed_generelt[seq(na_rad[1] + 1, siste_rad, 1), ] |>
    row_to_names(row_number = 1) |>
    clean_names() |>
    add_column("Skjemanavn" = skjemanavn, .before = 1)

  return(d_versjonslogg)
}

#' Henter ut metainfo for skjema
#'
#' @description
#' Henter metainfo fra generelt-fane for `skjemanavn`.
#' Inndata må være rådataversjon av generelt-fane for `skjemanavn`
#' hentet ut med mrs5_parse_kodebok_skjema_meta.
#'
#' @param parsed_generelt rådataversjon av generelt-fane fra kodebok.
#'
#' @return
#' Returnerer tibble med metainfo for `skjemanavn`.
#'
#' @keywords internal
mrs5_hent_metainfo = function(parsed_generelt) {
  assert_that(is.data.frame(parsed_generelt))

  # finne NA-rad
  na_rad = parsed_generelt |>
    mutate(radnr = row_number()) |>
    filter(is.na(...1)) |>
    pull(radnr)

  na_rad = na_rad[1]

  d_metainfo = parsed_generelt |>
    select(...1, ...2) |>
    slice_head(n = na_rad - 1) |>
    rename("metavariabel" = ...1, "metaverdi" = ...2) |>
    pivot_wider(
      names_from = "metavariabel",
      values_from = "metaverdi"
    ) |>
    clean_names()

  return(d_metainfo)
}

# Hjelpefunksjoner kanonisk -----------------------------------------------

#' Lag fine kolonnenavn
#' 
#' @description 
#' Hjelpefunksjon som vasker kolonnenavn i tabeller hentet ut fra MRS5-kodebok. 
#'
#' @param d datasett hvor kolonnenavn skal vaskes. 
#'
#' @returns
#' Returnerer samme datasett som er gitt som inndata, med vaskede kolonnenavn. 
#' Beholder æ, ø o å, men tar bort hermetegn og andre symboler. Erstatter 
#' mellomrom med understrek. 
#' @export
#'
#' @examples
#' 
#' data = tibble(Skjemanavn = c("Skjema 1", "Skjema 2"), 
#'              `"Kan slettes"` = c("Ja", "Ja"))
#'              
#' data_forenklet = mrs5_lag_fine_kolonnenavn(data)
mrs5_lag_fine_kolonnenavn = function(d) {
  
  janitor::clean_names(d, replace = c(
    `'` = "",
    `"` = "",
    `%` = "_percent_",
    `#` = "_number_",
    `å` = "å",
    `æ` = "æ",
    `ø` = "ø"
  ))
}


#' Konverter rådata til kanonisk kodebok
#'
#' @description Tar inn kodebok på rådataformat slik det produseres av funksjonen
#' `mrs5_parse_kodebok()`. 
#' Leverer ut kodebok på kanonisk format, det vil si en liste som inneholder 
#' tre tibbles, Kodebok, Kategoriske og Regler. 
#'
#' @param kb_parsed_raa Kodebok på rådataformat produsert av `mrs5_parse_kodebok()`
#'
#' @returns
#' @export
#'
#' @examples
mrs5_lag_kanonisk_kb = function(kb_parsed_raa) {
  
  # FIXME - Legge inn kontroll for endringer i kjernefelt, sånn at ikke alle 
  # obligatoriske blir FALSE hvis de feks endrer til boolske verdier. 
  # FIXME - Legge relevante metadata til kanonisk kb som args. 
  # - Trenger hvertfall opprettet dato fra metainfo
  
  d_alle_versjonslogg = bind_rows(kb_parsed_raa[["versjonslogg"]], .id = "kilde") |> mrs5_lag_fine_kolonnenavn()
  d_alle_metainfo = bind_rows(kb_parsed_raa[["metainfo"]], .id = "kilde") |> mrs5_lag_fine_kolonnenavn()
  d_alle_felter = bind_rows(kb_parsed_raa[["felter"]], .id = "kilde") |> mrs5_lag_fine_kolonnenavn()
  d_alle_regler = bind_rows(kb_parsed_raa[["regler"]], .id = "kilde") |> mrs5_lag_fine_kolonnenavn()
  
  # Trekk ut menneskevennlig skjemanavn 
  skjemakobling = d_alle_metainfo |> 
    select(kilde, skjematypenavn)
  
  Regler = d_alle_regler |> 
    filter(eiertype == "Field") |> 
    select(skjemanavn, eier, forklaring)
  
  Kodebok = d_alle_felter |> 
    filter(is.na(fjernet_fra_og_med_skjemaversjon),
           !is.na(variabelnavn)) |> 
    mutate(
    skjema_id = kilde, 
    skjemanavn = skjemakobling$skjematypenavn[match(kilde, skjemakobling$kilde)],  
    variabel_id = variabelnavn, 
    variabeletikett = visningstekst, 
    hjelpetekst = hjelpetekst, 
    variabeltype = koblingsliste_vartyper$Kanonisk[match(felttype, rapwhale::koblingsliste_vartyper$MRS5)], 
    obligatorisk = kjernefelt == "Ja", 
    desimaler = NA_integer_,  
    regler = paste0(skjemanavn, variabelnavn) %in% unique(paste0(Regler$skjemanavn, Regler$eier))
    ) |> 
    select(skjema_id, skjemanavn, variabel_id, variabeletikett, hjelpetekst, 
           variabeltype, obligatorisk, desimaler, regler)
  
  Kategoriske = Kodebok |> 
    filter(variabeltype == "kategorisk", 
           !is.na(variabel_id)) |>
    distinct(skjema_id, variabel_id) |> 
    left_join(d_alle_felter |> 
                select(variabelnavn, skjemanavn, mulige_verdier), 
              by = c("variabel_id" = "variabelnavn", "skjema_id" = "skjemanavn")) |> 
    tidyr::separate_rows(mulige_verdier, 
                  sep = ",\\s*(?=[-]?\\d+\\s*=)") |> 
    mutate(mulige_verdier = str_remove_all(mulige_verdier, pattern = "[\\n]")) |> 
    tidyr::separate_wider_regex(
      cols = mulige_verdier, 
      patterns = c(verdi = "[-]?\\d+", 
                   "\\s*=\\s*",
                   verditekst = "(?:.*)$")
    ) |> 
    select(skjema_id, variabel_id, verdi, verditekst) |> 
    add_column(manglande = FALSE)
  
  
  
  kodebok_kanonisk = list(
    "Kodebok" = Kodebok, 
    "Kategoriske" = Kategoriske, 
    "Regler" = Regler
  )
  
  return(kodebok_kanonisk)
}
