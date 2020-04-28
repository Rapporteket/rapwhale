library(tidyverse)

les_kb_oqr_v2 = function(adresse) {
  kb_oqr = les_kb_oqr_base(adresse)

  # Variabler som må konverteres til desimal eller dato
  til_desimal = c(
    "normalintervall_start_numerisk", "normalintervall_slutt_numerisk",
    "maksintervall_start_numerisk", "maksintervall_slutt_numerisk"
  )
  til_dato = c(
    "normalintervall_start_dato", "normalintervall_slutt_dato",
    "innfoert_dato", "utfaset_dato"
  )

  # FIXME! - Se JIRA-sak https://issuetracker.helsenord.no/browse/ABN-372 for hvordan dette løses fremover.
  til_dato_apo = c("maksintervall_start_dato", "maksintervall_slutt_dato")

  kb_oqr = mutate_at(kb_oqr,
    til_desimal,
    konverter_tekst,
    regex = "[-]?\\d{1,}[.]?[\\d{1,}]?",
    parse_double
  )
  kb_oqr = mutate_at(kb_oqr,
    til_dato,
    konverter_tekst,
    regex = "\\d{4}\\-\\d{2}\\-\\d{2}",
    parse_date,
    format = "%Y-%m-%d"
  )

  # FIXME! - Denne bør revideres når vi får svar på JIRA-sak (https://issuetracker.helsenord.no/browse/ABN-372)
  # Se også test som dekker dette.
  kb_oqr = mutate_at(kb_oqr,
    til_dato_apo,
    konverter_tekst,
    regex = "\\d{4}\\-\\d{2}\\-\\d{2}",
    parse_date,
    format = "'%Y-%m-%d'"
  )

  kb_std = kb_oqr_base_til_std(kb_oqr)

  # kb_std = kb_oqr_base_til_std(kb_oqr)
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

#' Konverter tekstvariabel til desimal eller dato
#'
#' Funksjonen tar inn en tekst-vektor og konverterer til ønsket format.
#' Må oppgi et regex-uttrykk for å vise hvordan tekst-strengene som skal endres ser ut.
#' Funksjonen ser deretter etter mønsteret i tekst-vektoren og konverterer de det gjelder
#' til ønsket format basert på hvilken parse-funksjon som er oppgitt.
#' Verdier som ikke matcher regex vil endres til NA
#'
#' @param d tekst-vektor som skal konverteres
#' @param regex regex uttrykk for hvilke format det forventes at teksten innehar.
#' @param parse_funksjon en parse_*-funksjon fra readr pakken. Foreløpig kun støtte for parse_double og parse_date.
#' @export
konverter_tekst = function(d, regex, parse_funksjon, ...) {
  stopifnot(is.character(d))
  # Konverterer alle ikke-regex til NA
  d[str_detect(d, pattern = regex, negate = TRUE)] = NA

  # Leser inn resten med valgt parse-funksjon
  d = parse_funksjon(d, ...)

  d
}

kb_oqr_base_til_std = function(kb_oqr) {
  # Tar inn en kodebok-tibble med riktige variabeltyper.

  # Utkast til standardformat for kodebok. Dette må revideres.
  kb_std = kb_oqr %>%
    mutate(
      skjema_id = tabell,
      skjemanavn = skjemanavn,
      variabel_id = str_to_lower(fysisk_feltnavn),
      obligatorisk = str_to_lower(obligatorisk),
      variabeletikett = ledetekst,
      forklaring = hjelpetekst,
      variabeltype = type,
      verdi = listeverdier,
      verditekst = listetekst,
      desimaler = desimaler,
      min = maksintervall_start_numerisk,
      maks = maksintervall_slutt_numerisk,
      min_rimelig = normalintervall_start_numerisk,
      maks_rimelig = normalintervall_slutt_numerisk,
      min_dato = maksintervall_start_dato,
      maks_dato = maksintervall_slutt_dato,
      min_rimelig_dato = normalintervall_start_dato,
      maks_rimelig_dato = normalintervall_slutt_dato,
      kommentar = kommentar,
      kategori = NA_character_,
      innleiing = NA_character_,
      eining = NA_character_,
      unik = NA_character_,
      manglande = NA_character_,
      kommentar_rimeleg = NA_character_,
      utrekningsformel = NA_character_,
      logikk = NA_character_
    )

  # Endre rekkefølge på rader og kolonner.

  # Tester for å sjekke at alle variabeltyper er kjente fra før
  # valider_original() # validering som gjøres på kodebok før konvertering
  # Hva skal inn her?
  kb_stb = utvid_status(kb_std)
  # {
  # Fikser statusvariabler (legge til ekstra rader for hvert nivå (0,1,-1))
  # Fikse obligatorisk
  # Fikse skjemanavn
  # Sorter kb etter skjemarekkefølge
  # }

  # Returnerer fullstendig kodebok på standard format
  kb_std
}

#' Utvid statusvariabler til kategorisk
#'
#' Funksjonen tar statusvariabel fra OQR-register og konverterer den til en
#' kategorisk variabel. I OQR-struktur har statusvariabel kun et nivå, men
#' vi vil ha et nivå for hver listeverdi.
#' Tar inn kodebok og returnerer kodebok med statusvariabler utvidet
#'
#' @param kb_std Kodebok på standardformat
utvid_statusvariabel = function(kb_std) {

  # Sjekker at det ingen tabeller har flere statusvariabler.
  stopifnot(all(kb_std %>%
    group_by(skjema_id) %>%
    tally(variabeltype == "Statusvariabel") %>%
    pull(n) <= 1))

  while (any(kb_std$variabeltype == "Statusvariabel")) {
    # Radnummeret til første (ubehandla) statusvariabel
    ind = which(kb_std$variabeltype == "Statusvariabel")[1]

    # Rada må bytast ut med tre rader, éi for kvar moglege verdi.
    # Dette gjer me først ved å utvida kodeboka med to ekstra,
    # identiske rader rett etter rada. Så set me inn rette verdiar.
    #
    # Gjenta aktuell rad tre gongar (når me gjer det slik,
    # funkar det òg viss rada er første eller siste rad).
    kb_std = kb_std[append(seq_len(nrow(kb_std)),
      values = c(ind, ind),
      after = ind
    ), ]

    # Legg rette verdiar inn i dei tre nye radene
    nyind = c(ind, ind + 1, ind + 2)
    kb_std$verdi[nyind] = -1:1
    kb_std$verditekst[nyind] = c("Opprettet", "Lagret", "Ferdigstilt")
    kb_std$variabeltype[nyind] = "Listevariabel"
  }

  kb_std
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
