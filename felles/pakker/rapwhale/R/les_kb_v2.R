library(tidyverse)

les_kb_oqr_v2 = function(adresse) {
  # Tar inn filplassering for kodebok

  # Har i funksjonen oppgitt kb_spek for det spesifikke register/struktur (her kb_oqr_spek)

  # Kaller på les_kodebok_oqr_base()
  # Kaller på fikse_datatyper()
  # Kaller på kb_oqr_base_til_std()
  # Kaller på legg_til_variabler_kb()
  # Kaller på valider_kodebok()

  # Returnerer fullstendig kodebok for registeret. (Må inkludere varnavn_kilde, varnavn_resultat og vartype slik det er oppgitt i spesifikasjon i les_dd funksjoner)
}

#' Les inn OQR-kodebok
#'
#' les_kb_oqr_base leser inn en base-versjon av OQR-kodebok. Det gjøres ingen
#' validering eller korrektur på kodeboken her. Den leses inn slik den er.
#' Eneste argument er adresse, som angir filplassering for kodebok.
#'
#' Returnerer en OQR-kodebok som inneholder alt av informasjon, men som har
#' en del mangler. Variabeltyper er gjerne feil og tall- og  dato-kolonner
#' kan inneholde tekst-verdier.
#'
#' @param adresse filplassering for kodebok.
les_kb_oqr_base = function(adresse) {

  # Spesifikasjon for OQR-kodebok
  kb_spek_oqr = tibble::tribble(
    ~varnavn_kilde, ~varnavn_resultat, ~vartype,
    "skjemanavn", "skjemanavn", "tekst",
    "navn_i_rapporteket", "navn_i_rapporteket", "tekst",
    "ledetekst", "ledetekst", "tekst",
    "obligatorisk", "obligatorisk", "tekst",
    "type", "type", "tekst",
    "listeverdier", "listeverdier", "tekst",
    "listetekst", "listetekst", "tekst",
    "normalintervall_start_numerisk", "normalintervall_start_numerisk", "tekst", # Kan være 'today' etc
    "normalintervall_slutt_numerisk", "normalintervall_slutt_numerisk", "tekst", # Kan være 'today' etc
    "maksintervall_start_numerisk", "maksintervall_start_numerisk", "tekst", # Kan være 'today' etc
    "maksintervall_slutt_numerisk", "maksintervall_slutt_numerisk", "tekst", # Kan være 'today' etc
    "normalintervall_start_dato", "normalintervall_start_dato", "tekst", # Kan være 'today' etc
    "normalintervall_slutt_dato", "normalintervall_slutt_dato", "tekst", # Kan være 'today' etc
    "maksintervall_start_dato", "maksintervall_start_dato", "tekst", # Kan være 'today' etc
    "maksintervall_slutt_dato", "maksintervall_slutt_dato", "tekst", # Kan være 'today' etc
    "antall_tegn", "antall_tegn", "heltall",
    "lovlige_tegn", "lovlige_tegn", "tekst",
    "desimaler", "desimaler", "heltall",
    "aktiveringsspoersmaal", "aktiveringsspoersmaal", "tekst",
    "underspoersmaal", "underspoersmaal", "tekst",
    "innfoert_dato", "innfoert_dato", "tekst",
    "utfaset_dato", "utfaset_dato", "tekst",
    "tabell", "tabell", "tekst",
    "fysisk_feltnavn", "fysisk_feltnavn", "tekst",
    "kommentar", "kommentar", "tekst",
    "variabel_id", "variabel_id", "tekst",
    "hjelpetekst", "hjelpetekst", "tekst"
  )

  # Leser inn kodebok med angitt spesifikasjon
  d = les_csv_oqr(adresse, spesifikasjon = kb_spek_oqr)

  d
}

# Funksjon for å fikse datatyper

fiks_datatyper = function(kb_base) {
  kb_base = konverter_tekst_til_tall(kb_base)
  kb_base = konverter_tekst_til_dato(kb_base)

  # Håndtering av problematiske variabler.
  til_desimal = c(
    "normalintervall_start_numerisk", "normalintervall_slutt_numerisk",
    "maksintervall_start_numerisk", "maksintervall_slutt_numerisk"
  )
  til_dato = c(
    "normalintervall_start_dato", "normalintervall_slutt_dato",
    "maksintervall_start_dato", "maksintervall_slutt_dato",
    "innfoert_dato", "utfaset_dato"
  )


  # FIXME - lage robust funksjon for konvertering av tekst til numerisk
  # Konvertere tekst-verdier til numerisk verdi.

  # Trekke ut som en egen funksjon.
  # Heller bare hente ut de verdiene jeg trenger.
  # 'eventuelt (-), siffer, evt desimal og andre siffer' - resten til NA
  # str_detect, str_extract - modify_if
  # lage funksjon som sjekker om innholdet ser ut som tall.
  tekst_til_tal = function(x) {
    suppressWarnings(as.numeric(x))
  }

  d = mutate_at(d, til_desimal, tekst_til_tal)

  # apostrofdato til dato.
  # sjekke at format er 'YYYY-MM-DD', ser ut som dato = dato (parse_date, format)

  # Maksintervall_start_dato har formatet "'2020-02-02'", så vi må fjerne unødvendige apostrofer.
  d = mutate_at(
    d, "maksintervall_start_dato",
    ~ str_remove_all(., pattern = "\'")
  )

  # Fjerner tekstverdier som for eksempel 'today' fra datovariabler og gjør de til NA
  # Kun godta 'YYYY-MM-DD' og sette resten til NA
  d = mutate_at(d, til_dato, ~ str_replace_all(.,
    pattern = "^[A-Za-z]+$",
    replacement = NA_character_
  ))

  # Konverterer dato-variabler til datoformat
  d = mutate_at(d, til_dato, ~ readr::parse_date(.,
    format = "'%Y-%m-%d'",
    na = ""
  ))



  # - konverter_tekst_til_tall()
  # - konverter_tekst_til_dato()
  # - Lage en generell funksjon for disse.
  # tar inn en regex for hva som er gyldig,
  # en parse_* funksjon for å tolke de som er gyldig
  # tar inn et ekstra argument til parse_* funksjon (... = double => format, date => format)
}

kb_oqr_base_til_std = function(kodebok, kb_kobling) {
  # Tar inn en kodebok-tibble med riktige variabeltyper.

  # Må gjøre en del validering av inndata
  # I funksjonen har vi oppgitt format for standard kodebok vi vil bruke

  # kb_kobling brukes for å koble diverse kodebokformat til standard format.

  # Kalle på funksjon for å fikse variabelnavn
  # skifte varnavn, endre rekkefølge på rader og kolonner.

  # Må ha definert hva et standard kodebokformat skal være.

  # Tester for å sjekke at alle variabeltyper er kjente fra før

  # Fikser statusvariabler (legge til ekstra rader for hvert nivå (0,1,-1))
  # Fikse obligatorisk
  # Fikse skjemanavn
  # Sorter kb etter skjemarekkefølge

  # Returnerer fullstendig kodebok på standard format
}

legg_til_variabler_kb = function(kb, ekstra_spek) {
  # Funksjon(er) for å håndtere ekstra variabler som skal legges til KB og
  # eventuelt om variabler skal fjernes.
}

valider_kodebok = function(kodebok) {
  # Tar inn kodebok og gjennomfører komplett testing av innhold

  # For listevariabler er det viktig at variabler med flere nivåer har samme verdi-verditekst kombinasjon
  # på tvers av skjema

  # Ta ut som egen funksjon - lage generell + tester
  # Må kunne brukes i flere sammenhenger. eksempel å se om en pasient har
  # flere operasjoner på samme dato, flere 1-års oppfølginger eller lignende.

  # Finner de variablene som har flere ulike listetekster for samme listeverdi
  d_avvik = d %>%
    filter(type == "Listevariabel") %>%
    #    filter(fysisk_feltnavn %in% repeterte) %>%
    group_by(fysisk_feltnavn, listeverdier) %>%
    summarise(antall_feil = n_distinct(listetekst, .keep_all = TRUE)) %>%
    filter(n > 1)

  # Stanser innlesning hvis det finnes avvik
  # Gi ut d_avvik som feilmelding
  # format(d_avvik)
  if (nrow(d_avvik) > 0) {
    stop(error = paste0(
      "Det finnes ",
      nrow(d_avvik),
      " avvik for listeverdi mellom skjema: \n Variabel  : ",
      d_avvik$fysisk_feltnavn, "\n Listeverdi: ",
      d_avvik$listeverdier
    ))
  }

  # Validering av variabler med flere nivå. Se over
  # ikke duplikater av variabelnavn bla.
  # Sjekke rekkefølge
  # se i valideringsfunksjon
  # Sjekker at variabeltyper er blant de aksepterte typene.
  # ++ tester fra kodebok-valider
  # Test for duplikate kategoriske verdier

  # Felles for OQR, MRS osv.

  # Returner kodebok_validert
}
