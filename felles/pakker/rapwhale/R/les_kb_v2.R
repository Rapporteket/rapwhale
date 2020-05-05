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

  # Kaller på legg_til_variabler_kb()
  # Kaller på valider_kodebok()

  # Returnerer fullstendig kodebok for registeret.
  # (Må inkludere varnavn_kilde, varnavn_resultat og vartype slik det er
  # oppgitt i spesifikasjon i les_dd funksjoner)
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

  # Utkast til standardformat for kodebok.
  # FIXME - bli enige om hva som skal være inkludert i vårt standardformat
  kb_mellom = kb_oqr %>%
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
      min_rimeleg = normalintervall_start_numerisk,
      maks_rimeleg = normalintervall_slutt_numerisk,
      min_dato = maksintervall_start_dato,
      maks_dato = maksintervall_slutt_dato,
      min_rimeleg_dato = normalintervall_start_dato,
      maks_rimeleg_dato = normalintervall_slutt_dato,
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

  # Fjerne duplikate variabler på samme skjema
  # Data fra oppfølging lagres i samme tabell men kommer fra ulike skjema,
  # så variabler for oppfølging 2 og oppfølging 3 er gitt egne rader i samme tabell.
  # Dette må reduseres til å være en variabel. Trekker ut den første.
  kb_mellom = kb_mellom %>%
    distinct(skjema_id, variabel_id, verdi, verditekst, .keep_all = TRUE)
  # FIXME - lage test(er) for å se at dette fungerer riktig.

  kb_mellom = utvid_statusvariabel(kb_mellom)

  kb_std = valider_oqr_kb(kb_mellom)

  # Fikse skjemanavn
  # Sorter rader i kb etter skjemarekkefølge

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
utvid_statusvariabel = function(kb_mellom) {

  # Sjekker at det ingen tabeller har flere statusvariabler.
  stopifnot(all(kb_mellom %>%
    group_by(skjema_id) %>%
    tally(variabeltype == "Statusvariabel") %>%
    pull(n) <= 1))

  while (any(kb_mellom$variabeltype == "Statusvariabel")) {
    # Radnummeret til første (ubehandla) statusvariabel
    ind = which(kb_mellom$variabeltype == "Statusvariabel")[1]

    kb_mellom = kb_mellom[append(seq_len(nrow(kb_mellom)),
      values = c(ind, ind),
      after = ind
    ), ]

    # Legg rette verdiar inn i dei tre nye radene
    nyind = c(ind, ind + 1, ind + 2)
    kb_mellom$verdi[nyind] = -1:1
    kb_mellom$verditekst[nyind] = c("Opprettet", "Lagret", "Ferdigstilt")
    kb_mellom$variabeltype[nyind] = "Listevariabel"
  }

  kb_mellom
}

#' Valider oqr-kodebok
#'
#' Funksjonen gjør enkel validering av kodebok på OQR-format før den konverteres
#' til standardformat.
#' Sjekker at det ikke finnes *nye* variabeltyper i OQR-kodebok.
#' Endrer variabeltype fra OQR til standardnavn. for eksempel "Tekstvariabel" til "tekst"
#' Definerer obligatorisk variabel basert på aktiveringsspørsmål og obligatorisk kolonne
#' for å få riktig verdi for *skjulte* spørsmål som kun vises hvis et annet spørsmål
#' er besvart.
#' Returnerer kodebok på standardformat.
#'
#' @param kb_std kodebok med riktige kolonnenavn, men som ikke er fullstendig konvertert.
valider_oqr_kb = function(kb_mellom) {
  kb_mellom = oqr_til_std_variabeltyper(kb_mellom)
  kb_mellom = sjekk_obligatorisk(kb_mellom)
  kb_std = velg_standardkolonner(kb_mellom)
  kb_std = fiks_skjemanavn(kb_std)

  # Ordne rekkefølge for variabler slik variabler fra samme tabell kommer samlet
  kb_std = kb_std %>%
    arrange(forcats::fct_inorder(skjema_id))

  kb_std
}

oqr_til_std_variabeltyper = function(kb_std) {
  vartype_oqr_standard = tribble(
    ~type_oqr, ~type_standard,
    "Listevariabel", "kategorisk",
    "Tekstvariabel", "tekst",
    "Stor tekstvariabel", "tekst",
    "Avkrysningsboks", "boolsk",
    "Datovariabel", "dato",
    "Skjult variabel", "tekst",
    "Tallvariabel", "numerisk",
    "Tidsvariabel", "kl",
    "TIMESTAMP", "dato_kl"
  )

  # Stopp viss det dukkar opp variabeltypar me ikkje kjenner til
  nye_vartypar = na.omit(setdiff(kb_std$variabeltype, vartype_oqr_standard$type_oqr))
  if (length(nye_vartypar) > 0) {
    stop(
      "Kodeboka har variabeltypar me ikkje støttar / har standardnamn på: ",
      str_c(nye_vartypar, collapse = ", ")
    )
  }

  # Byt ut variabeltype-verdiane med våre standardiserte namn
  kb_std$variabeltype = vartype_oqr_standard$type_standard[
    match(kb_std$variabeltype, vartype_oqr_standard$type_oqr)
  ]

  kb_std
}

sjekk_obligatorisk = function(kb_std) {
  # Kontrollere at obligatoriske, aktiveringsspoersmaal og underspoersmaal ikke er NA
  stopifnot(all(!(is.na(kb_std$obligatorisk) |
    is.na(kb_std$aktiveringsspoersmaal) |
    is.na(kb_std$underspoersmaal))))

  kb_std = kb_std %>%
    mutate(
      obligatorisk =
        as.character(ifelse(aktiveringsspoersmaal == "ja" &
          obligatorisk == "ja",
        yes = "ja", no = "nei"
        ))
    )
}

velg_standardkolonner = function(kb_std) {

  # Fikse rekkefølge for og valg av variabler til kb_std
  std_namn = c(
    "skjema_id", "skjemanavn", "kategori", "innleiing", "variabel_id",
    "variabeletikett", "forklaring", "variabeltype", "eining", "unik",
    "obligatorisk", "verdi", "verditekst", "manglande", "desimaler",
    "min", "maks", "min_rimeleg", "maks_rimeleg", "min_dato", "maks_dato",
    "min_rimeleg_dato", "maks_rimeleg_dato", "kommentar_rimeleg",
    "utrekningsformel", "logikk", "kommentar"
  )

  kb_std = kb_std %>%
    select(!!std_namn)
}

fiks_skjemanavn = function(kb_std) {
  # Ordner skjemanavn til å samsvare med hvilken tabell variablene ligger i.

  # Lag ein omkodingstabell, frå tabell-ID til skjemanamn
  kod_id = unique(kb_std$skjema_id)
  kod_namn = character(length(kod_id)) # Tom vektor til å halda resultatet

  # Sjå på alle observerte kombinasjonar av tabell-ID og skjemanamn (i naturleg rekkjefølgje)
  komb = distinct(tibble(id = kb_std$skjema_id, namn = kb_std$skjemanavn))
  for (i in seq_along(kod_id)) {
    kandidatar = c(komb$namn[komb$id == kod_id[i]], kod_id[i])
    kandidatar = setdiff(kandidatar, kod_namn) # Fjern allereie brukte skjemanamn
    kod_namn[i] = kandidatar[1] # Bruk første *ledige* (vil alltid vera ein, utanom det patologiske tilfelle der skjemanamna er lik skjema-ID-ane, men ikkje med 1-1-samsvar)
  }

  # Bruk omkodingstabellen til gje ut rett namn på alle ID-ane
  kb_std$skjemanavn = kod_namn[match(kb_std$skjema_id, kod_id)]

  kb_std
}

#' Legg til ekstra variabler i kodebok
#'
#' Funksjon for å legge til variabler i kodebok som finnes i datadump, men ikke
#' er med i kodebok.
#' Det er ikke nødvendigvis samme variabler som mangler i alle skjema, så det må
#' kunne spesifiseres per skjema/variabel.
#'
#' @param kb_std kodebok på standardformat
#' @param skjema Hvilket skjema det skal legges variabler til
#' @param variabler Dataramme med variabler som skal legges til skjema. Må inneholde variabel_id, variabeletikett,        variabeltype, unik, obligatorisk og desimalar.
#' @param posisjon Hvor i skjema variablene skal ligge. Mulige valg er .before, .after eller en vektor med indeks av samme lengde som variabler. Default er .after.
legg_til_variabler_kb = function(kb_std, skjema, variabler, posisjon) {

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
