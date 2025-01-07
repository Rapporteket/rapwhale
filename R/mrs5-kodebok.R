
# Hovedfunksjon ---------------------------------------------------------


# Fixme's -----------------------------------------------------------------

# FIXME - Legge eksempelfiler i inst-mappe for å ha eksempler som virker for 
# funksjoner som leser inn filer. 
# FIXME - Oppdatere docs for funksjonene punktet over gjelder. 
#   - mrs5_hent_kodebok, 
# FIXME - Konverter til kanonisk funksjonalitet 
# FIXME - Fikse import av nødvendige funsjoner og fjerne :: 

# Hent mrs5-kodebok -------------------------------------------------------

#' Hent mrs5-kodebok
#' 
#' @description
#' Henter inn kodebok for registre på MRS5 fra `filsti`. 
#' Kaller på de nødvendige hjelpefunksjonene for å lese data, og 
#' lagre data på kanonisk format. 
#'
#' @param filsti Tekststreng som angir filsti til kodebok.  
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn slik det er 
#' navngitt i kodebok. Hvis `skjemanavn = NULL` hentes kodebok for alle skjema inn. 
#' @return
#' Listeobjekt som inneholder tibblene kb_kodebok, kb_kategoriske og kb_regler. 
#' @export
#'
#' @examples
#' # Hente kodebok for alle skjema og få ut på kanonisk format
#' kb_register = mrs5_hent_kodebok(filst = "/sti//til//kodebokfil", skjemanavn = NULL)
#' 
#' # Hente for et enkelt skjema 
#' kb_innleggelse = mrs5_hent_kodebok(filst = "/sti//til//kodebokfil", skjemanavn = "Innleggelse")
mrs5_hent_kodebok = function(filsti, skjemanavn = NULL) {
  
} 

#' Lag skjemanavn-vektor
#' 
#' @description
#' Sjekker om `skjemanavn` er en gyldig tekst-vektor hvis oppgitt. Hvis det ikke
#' er gitt en tekstvektor som argument hentes alle skjemanavn fra kodebok 
#' funnet på `filsti`. 
#' 
#'
#' @param filsti Plassering av kodebok
#' @param skjemanavn Tekstvektor med skjemanavn som skal hentes ut eller `NULL`. 
#'
#' @return
#' Returnerer tekstvektor med aktuelle skjemanavn. 
#' @export
#'
#' @examples
#' # Lager vektor med skjemanavn
#' mrs5_trekk_ut_skjemanavn(filsti = "filsti/til/fil.xlsx", 
#'                          skjemanavn = NULL)
mrs5_trekk_ut_skjemanavn = function(filsti, skjemanavn) {

  if(is.character(skjemanavn)) {
    skjemanavn_ut = skjemanavn
  }
  
  else if(is.null(skjemanavn)) {
    
    fanenavn = readxl::excel_sheets(filsti)
    fanenavn_unike_skjema = fanenavn[seq(2, length(fanenavn), by = 3)]
    
    skjemanavn_ut = stringr::str_remove(fanenavn_unike_skjema, pattern = "\\d+\\-")
    
  } else {
    stop("skjemanavn må være NULL eller en tekstvektor.")
  }
  
  return(skjemanavn_ut)
}
# Parse rådata -------------------------------------------------------------

#' Leser inn kodebokfil på rådata-format
#' 
#' Kaller nødvendige hjelpefunksjoner for å lese inn de ulike skjema som finnes 
#' i mrs5-kodebok. Tar inn filsti som argument og returnerer en liste med 
#' kanonisk kodebok for alle skjema inkludert i skjemanavn. 
#'
#' @param filsti Tekststreng som angir filsti til kodebok.  
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn slik det er 
#' navngitt i kodebok. Hvis `skjemanavn = NULL` hentes kodebok for alle skjema inn. 
#'
#' @return
#' Listeobjekt med rådataversjon av alle skjema i kodeboken for skjema inkludert 
#' i `skjemanavn`. 
#' 
#' @export
#'
#' @examples
mrs5_parse_kodebok = function(filsti, skjemanavn) { 
  
  # Kaller på hjelpefunksjoner for å lese inn de enkelte delene av kodeboken.  
  
  # mrs5_parse_fanenavn()
  # mrs5_parse_kodebok_skjema()
  # mrs5_parse_kodebok_felter()
  # mrs5_parse_kodebok_regler()
  
  # Returnerer 
  }

}

#' les inn generelt fane for skjema
#' 
#' @description
#' Leser inn hovedfane for `skjemanavn` fra MRS5-kodebok. 
#' 
#' @param filsti Plassering av kodebokfil på disk. 
#' @param skjemanavn Navn på skjema slik det er gitt i kodebok.
#'
#' @return
#' Returnerer en tibble med rådataversjon av generelt-fane for `skjemanavn`
#' fra kodebok. 
#' @export
#'
#' @examples
#' # Les inn rådataversjon av generelt-fane for skjemanavn fra kodebok
#' kb_raa = mrs5_parse_kodebok_skjema(filsti = "path/to/file/, skjemanavn = "skjema")
mrs5_parse_kodebok_skjema = function(filsti, skjemanavn) {
  
  # kontrollerer argumenter
  assertthat::assert_that(assertthat::is.string(filsti),
    msg = "Filsti må være en tekststreng"
  )
  
  stopifnot(file.exists(filsti))
  fanenavn = readxl::excel_sheets(filsti)

  assertthat::assert_that(assertthat::is.string(skjemanavn),
    msg = "skjemanavn må være NULL eller en tekst-vektor"
  )

  skjemanavn_aktuelt = fanenavn[stringr::str_detect(
    fanenavn,
    pattern = paste0(
      skjemanavn,
      "$"
    )
  )]

  if (length(skjemanavn_aktuelt) != 1) {
    stop("Skjemanavn finnes ikke i kodebok") 
  }

  d_skjemanavn = suppressMessages(readxl::read_xlsx(filsti,
    sheet = skjemanavn_aktuelt,
    col_names = FALSE,
    col_types = "text"
  ))

  return(d_skjemanavn)
}

#' les inn felter for skjema fra kodebok
#'
#' @description
#' Leser inn rådataversjon av felter-fane for `skjemanavn` fra MRS5-kodebok. 
#' 
#' @param filsti Plassering av kodebokfil på disk.
#' @param skjemanavn Navn på skjema slik det er gitt i kodebok.
#'
#' @return
#' @export
#'
#' @examples
#' # Les inn rådataversjon av felter-fane for skjemanavn fra kodebok
#' kb_felter_raa = mrs5_parse_kodebok_felter(filsti = "path/to/file/, skjemanavn = "skjema")
mrs5_parse_kodebok_felter = function(filsti, skjemanavn) {
  
  # kontrollerer argumenter
  assertthat::assert_that(assertthat::is.string(filsti),
                          msg = "Filsti må være en tekststreng"
  )
  
  stopifnot(file.exists(filsti))
  fanenavn = readxl::excel_sheets(filsti)
  
  assertthat::assert_that(assertthat::is.string(skjemanavn),
                          msg = "skjemanavn må være NULL eller en tekst-vektor"
  )
  
  skjemanavn_aktuelt = fanenavn[stringr::str_detect(
    fanenavn,
    pattern = paste0(
      skjemanavn,
      "-felter$"
    )
  )]
  
  if (length(skjemanavn_aktuelt) != 1) {
    stop("Skjemanavn finnes ikke i kodebok") 
  }
  
  d_skjemanavn = suppressMessages(readxl::read_xlsx(filsti,
                                                    sheet = skjemanavn_aktuelt,
                                                    col_names = TRUE,
                                                    col_types = "text"
  ))
  
  return(d_skjemanavn)
}

#' Les inn regler fane for skjema
#' 
#' @description
#' Leser in rådataversjon av regler-fane for `skjemanavn` fra MRS5-kodebok. 
#'
#' @param filsti Plassering av kodebokfil på disk.
#' @param skjemanavn Navn på skjema slik det er gitt i kodebok.
#'
#' @return
#' @export
#'
#' @examples
#' #' # Les inn rådataversjon av regler-fane for skjemanavn fra kodebok
#' kb_regler_raa = mrs5_parse_kodebok_regler(filsti = "path/to/file/, skjemanavn = "skjema")
mrs5_parse_kodebok_regler = function(filsti, skjemanavn){
  
  # kontrollerer argumenter
  assertthat::assert_that(assertthat::is.string(filsti),
                          msg = "Filsti må være en tekststreng"
  )
  
  stopifnot(file.exists(filsti))
  fanenavn = readxl::excel_sheets(filsti)
  
  assertthat::assert_that(assertthat::is.string(skjemanavn),
                          msg = "skjemanavn må være NULL eller en tekst-vektor"
  )
  
  skjemanavn_aktuelt = fanenavn[stringr::str_detect(
    fanenavn,
    pattern = paste0(
      skjemanavn,
      "-regler$"
    )
  )]
  
  if (length(skjemanavn_aktuelt) != 1) {
    stop("Skjemanavn finnes ikke i kodebok") 
  }
  
  d_skjemanavn = suppressMessages(readxl::read_xlsx(filsti,
                                                    sheet = skjemanavn_aktuelt,
                                                    col_names = TRUE,
                                                    col_types = "text"
  ))
  
  return(d_skjemanavn)
}

# Hjelpefunksjoner for Parse ----------------------------------------------

#' Kombinere innleste skjema
#' 
#' Tar inn utdata fra parse-funksjonene og samler alle skjema i tibbles.
#' Utdata er en liste på samme format som kanonisk kodebok men med rådata.
#' 
#' @param parsed_generelt utdata fra mrs5_parse_kodebbk_skjema. 
#' @param parsed_felter utdata fra mrs5_parse_kodebok_felter.
#' @param parsed_regler utdata fra mrs5_parse_kodebok_regler. 
#'
#' @return
#' Liste med tibblene kodebok_raa_generelt, kodebok_raa_felter, kodebok_raa_regler. 
#' @export
#'
#' @examples
#' # Kombiner rådata
#' mrs5_kombiner_parsed(parsed_generelt = kb_raa_generelt, parsed_felter = kb_raa_felter, parsed_regler = kb_raa_regler)
mrs5_kombiner_parsed = function(parsed_generelt, parsed_felter, parsed_regler) {
  
}

# Konverter til kanonisk --------------------------------------------------

mrs5_konverter_til_kanonisk = function() {
}

# Hjelpefunksjoner kanonisk -----------------------------------------------


# Hent metadata -----------------------------------------------------------

#' Legger til metadata for kodebok
#'
#' @description
#' Hjelpefunksjon for bruk i konvertering fra rådata til kanonisk kodebok. 
#' Kaller på hjelpefunksjoner for å hente ut metadata for `skjemanavn`
#' fra kodebok. Disse legges til kodebok som en attributt med navn 
#' `versjonslogg` og `metainfo`. 
#' 
#' @param filsti Plassering av kodebokfil på disk.
#' @param skjemanavn Navn på skjema slik det er gitt i kodebok.
#'
#' @return
#' Legger til attributtene `versjonslogg` og `metainfo` til liste med kanonisk 
#' kodebok. 
#' @export
#'
#' @examples
mrs5_hent_metadata = function(filsti, skjemanavn) {
}

#' Henter ut versjonslogg for skjema
#' 
#' @description
#' Henter versjonslogg fra generelt-fane for `skjemanavn`.  
#' 
#'
#' @param filsti Plassering av kodebokfil på disk.
#' @param skjemanavn Navn på skjema slik det er gitt i kodebok.
#'
#' @return
#' Returnerer tibble med versjonslogg for `skjemanavn`. 
#' @export
#'
#' @examples
mrs5_hent_versjonslogg = function(filsti, skjemanavn) {

  }

#' Henter ut metainfo for skjema
#' 
#' @description
#' Henter metainfo fra generelt-fane for `skjemanavn`. 
#' 
#' 
#' @param filsti Plassering av kodebokfil på disk.
#' @param skjemanavn Navn på skjema slik det er gitt i kodebok.
#'
#' @return
#' Returnerer tibble med metainfo for `skjemanavn`. 
#' @export
#'
#' @examples
mrs5_hent_metainfo = function(filsti, skjemanavn) {

  }


# kodebok
mrs5_lag_kanonisk_kb = function() {
  
  # Tar inn rådataversjon fra mrs5_parse_kodebok_felter og returnerer 
  # kb[[kodebok]] på kanonisk format. 
}

###
mrs5_lag_skjema_id = function() {
  # Henter ut skjemanavn og genererer maskinlesbar skjema_id.
}

mrs5_kanonisk_variabeltype = function() {
  # Konverter variabeltype til internt navn. 
  # lage datasett med kolonne for alle kilder + kanonisk og legge i rapwhale?   
}

mrs5_definer_obligatorisk = function(){
  # Definer hvilke variabler som er obligatorisk å fylle ut. 
}

mrs5_identifiser_variabler_med_regler = function(){
  # Definer hvilke variabler som har regler tilknyttet. 
}

# kategoriske 
mrs5_lag_kanonisk_kategorisk = function() {
  
  # Tar inn rådataversjon fra mrs5_parse_kodebok_felter og returnerer 
  # kb[[kategoriske]] på kanonisk format. 
}

###
mrs5_ekspander_kategoriske = function() {
  # Fylle ut kb_kategorisk med alle verdi/verditekst-kombinasjoner. 
}

mrs5_definer_manglende = function() {
  
  # Definere hvilke verdier/verditekster som skal regnes som manglande.
  
}

# regler
mrs5_lag_kanonisk_regler = function() {
  
  # Tar inn rådataversjon fra mrs5_parse_kodebok_regler og returnerer 
  # kb[[regler]] på kanonisk format. 
}

###
mrs5_definer_regeltype = function() {}

mrs5_definer_målvariabel = function() {}

mrs5_trekk_ut_regelverdi = function() {}

mrs5_trekk_ut_feilmelding = function() {}

mrs5_returner_manglende_regler = function() {
  
  # Gir ut en oversikt over regler som ikke kunne konverteres
}

# Validering --------------------------------------------------------------
mrs5_valider_kodebok = function(kodebok_kanonisk) {
  
  # Tar inn kodebok på kanonisk format og kaller på valideringsregler vha 
  # hjelpefunksjoner for å validere de ulike kodebok-seksjonene. 
  
  # Kaller på mrs5_valider_kodebok_kb, kategorisk og regler. 
}

###
mrs5_valider_kodebok_kb = function(kodebok_felter) {
  
  # Valideringsregler for kodebok_felter. 
  
  }

mrs5_valider_kodebok_kategorisk = function(kodebok_kategorisk) {
  
}

mrs5_valider_kodebok_regler = function(kodebok_regler) {
  
}
