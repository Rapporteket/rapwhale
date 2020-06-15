#' @import dplyr
#' @importFrom tibble tribble tibble
#' @importFrom stringr str_detect str_c str_to_lower
NULL

#' Funksjon for å lese inn kodebok for OQR-register
#'
#' Hovedfunksjon for å lese inn kodebok for OQR-register.
#' Tar inn filplassering for kodebok og argumentet valider som angir om det skal
#' gjøres validering av kodebok ved innlesning.
#'
#' @param adresse en tekststreng for filplassering for kodebok.
#' @param valider TRUE eller FALSE for å velge om kodebok skal valideres ved innlesning.
#'
#' @export
les_kb_oqr_v2 = function(adresse, valider = TRUE) {
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
    regex = "^[-]?\\d+[.]?\\d*$",
    parse_double
  )

  kb_oqr = mutate_at(kb_oqr,
    til_dato,
    konverter_tekst,
    regex = "^\\d{4}-\\d{2}-\\d{2}$",
    parse_date,
    format = "%Y-%m-%d"
  )

  # FIXME! - Denne bør revideres når vi får svar på JIRA-sak (https://issuetracker.helsenord.no/browse/ABN-372)
  # Se også test som dekker dette.
  kb_oqr = mutate_at(kb_oqr,
    til_dato_apo,
    konverter_tekst,
    regex = "^\\d{4}-\\d{2}-\\d{2}$",
    parse_date,
    format = "'%Y-%m-%d'"
  )

  # Endrer "Ja" og "Nei" til lower_case
  kb_oqr = kb_oqr %>%
    mutate(
      aktiveringsspoersmaal = str_to_lower(aktiveringsspoersmaal),
      underspoersmaal = str_to_lower(underspoersmaal)
    )

  kb_std = kb_oqr_base_til_std(kb_oqr)

  # Kaller på legg_til_variabler_kb()

  if (valider) {
    valider_kodebok(kb_std)
  }

  kb_std
}

#' Les inn OQR-kodebok
#'
#' les_kb_oqr_base leser inn en base-versjon av OQR-kodebok. Det gjøres ingen
#' validering eller korreksjoner på kodeboken her. Den leses inn slik den er.
#' Eneste argument er adresse, som angir filplassering for kodebok.
#'
#' Returnerer en OQR-kodebok som inneholder alt av informasjon, men som har
#' en del mangler. Variabeltyper er gjerne feil og tall- og  dato-kolonner
#' kan inneholde tekst-verdier.
#'
#' @param adresse filplassering for kodebok.
les_kb_oqr_base = function(adresse) {

  # Spesifikasjon for OQR-kodebok
  kb_spek_oqr = tribble(
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
konverter_tekst = function(d, regex, parse_funksjon, ...) {
  stopifnot(is.character(d))
  # Konverterer alle ikke-regex til NA
  d[str_detect(d, pattern = regex, negate = TRUE)] = NA

  # Leser inn resten med valgt parse-funksjon
  d = parse_funksjon(d, ...)

  d
}

#' Konverter OQR kodebok fra basisformat til standard format.
#'
#' Ved bruk av les_kb_oqr_base får vi inn en kodebok på et format som må endres
#' litt for å kunne brukes til å lese inn datadump.
#' Denne funksjonen gjør følgende endringer:
#' Kolonnenavn endres til våre standardnavn
#' Duplikate variabler reduseres innen hvert skjema
#' Statusvariabler utvides til å inneholde alle nivå
#' Obligatoriske variabler sjekkes for å se om de er aktiveringsspørsmål
#' Navn for variabeltyper konverteres til standardnavn
#' Standardkolonner velges ut
#' Unike skjemanavn tildeles basert på skjemaid
#' Se de underliggende funksjonene for mer detaljerte beskrivelser.
#'
#' @param kb_oqr Tar inn en kodebok på basisformat slik det leses inn fra les_kb_oqr_base
#'
#' @export
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
      unik = "nei",
      manglande = "nei",
      kommentar_rimeleg = NA_character_,
      utrekningsformel = NA_character_,
      logikk = NA_character_
    )

  kb_mellom = reduser_duplikate_variabler(kb_mellom)
  kb_mellom = utvid_statusvariabel(kb_mellom)
  kb_mellom = sjekk_obligatorisk(kb_mellom)
  kb_std = konverter_oqr_kb(kb_mellom)

  kb_std
}

#' Reduser duplikate variabler
#'
#' Funksjon for å trekke ut unike variabler innenfor et skjema.
#' Foreløpig er denne funksjonen laget spesifikt for les_kb_v2.R, men den kan kanskje
#' utvides til å dekke mer generelle tilfeller.
#' Tar inn en kodebok på mellomformat og returnerer en kodebok på mellomformat
#' som har unike variabler innenfor hver tabell.
#' Grunnen til at vi gjør det er at enkelte OQR-register har flere skjema som registreres
#' i samme tabell, for eksempel oppfølging 1år, oppfølging 2år osv.
#' Vi vil kun ha en variabel som gjelder for alle disse skjema innen samme tabell.
#'
#' @param kb_mellom kodebok på mellomformat
reduser_duplikate_variabler = function(kb_mellom) {
  kb_mellom = kb_mellom %>%
    distinct(skjema_id, variabel_id, verdi, verditekst, .keep_all = TRUE)
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
  # FIXME - Se om vi kan bruke insert_rows, update_rows eller upsert_rows i nye dplyr.
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

konverter_oqr_kb = function(kb_mellom) {
  kb_mellom = oqr_til_std_variabeltyper(kb_mellom)
  kb_std = velg_standardkolonner(kb_mellom)
  kb_std = tildel_unike_skjemanavn_fra_skjema_id(kb_std)

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
      "Kodeboka har variabeltypar me ikkje støttar / har standardnamn på:\n",
      str_c(nye_vartypar, collapse = ", ")
    )
  }

  # Byt ut variabeltype-verdiane med våre standardiserte namn
  kb_std$variabeltype = vartype_oqr_standard$type_standard[
    match(kb_std$variabeltype, vartype_oqr_standard$type_oqr)
  ]

  kb_std
}

sjekk_obligatorisk = function(kb_mellom) {
  stopifnot(all(!(is.na(kb_mellom$obligatorisk) |
    is.na(kb_mellom$aktiveringsspoersmaal) |
    is.na(kb_mellom$underspoersmaal))))

  kb_mellom = kb_mellom %>%
    mutate(
      obligatorisk =
        if_else(aktiveringsspoersmaal == "ja" &
          obligatorisk == "ja",
        true = "ja", false = "nei"
        )
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
  kb_std
}

tildel_unike_skjemanavn_fra_skjema_id = function(kb_std) {
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
  if (any(is.na(kod_namn))) {
    stop(error = "Det finnes overlappende skjemanavn og skjema_id, og det er ikke 1-1 forhold mellom navnene")
  }

  # Bruk omkodingstabellen til gje ut rett namn på alle ID-ane
  kb_std$skjemanavn = kod_namn[match(kb_std$skjema_id, kod_id)]

  kb_std
}

#' Legg til ekstra variabler i kodebok
#'
#' Funksjon for å legge til variabler i kodebok som finnes i datadump, men ikke
#' er med i kodebok.
#' Kolonnene skjema_id, skjemanavn, variabel_id, variabeletikett, variabeltype, unik, obligatorisk og desimaler er obligatorisk.
#' Det er mulig å legge inn flere verdier gitt at det er gyldige kolonner som er i kodebok fra før.
#'
#' @param kb_std kodebok på standardformat
#' @param ekstra_data Dataramme med variabler som skal legges til skjema. Må inneholde skjema_id, skjemanavn, variabel_id, variabeletikett, variabeltype, unik, obligatorisk og desimaler.
legg_til_variabler_kb = function(kb_std, ekstra_data) {

  # Se om kolonner i ekstra data finnes i kodebok fra før
  ekstra_kol = colnames(ekstra_data)[!colnames(ekstra_data) %in% colnames(kb_std)]
  if (length(ekstra_kol) > 0) {
    stop(
      error = "Det er kolonner i ekstra_data som ikke eksisterer i kodebok fra før:\n",
      str_c(ekstra_kol, collapse = ", ")
    )
  }

  # Se om variabler i ekstra_data finnes i kb fra før:
  overlapp = intersect(
    kb_std %>% select(skjema_id, variabel_id),
    ekstra_data %>% select(skjema_id, variabel_id)
  )
  if (nrow(overlapp) > 0) {
    stop(
      error = "Variabel i ekstra_data eksisterer i skjema fra før:\n",
      str_c(overlapp$variabel_id, collapse = ", ")
    )
  }

  kb_std = kb_std %>%
    bind_rows(ekstra_data) %>%
    arrange(forcats::fct_inorder(skjema_id))

  kb_std
}

valider_kodebok = function(kodebok) {

  # Planlagt struktur
  # valider_kb_struktur(kodebok)
  valider_kb_skjema(kodebok)
  valider_kb_kolonner(kodebok)
  valider_kb_variabler(kodebok)
}

valider_kb_struktur = function(kodebok) {}

valider_kb_skjema = function(kodebok) {

  # Sjekk at skjema_id ikke har flere skjemanavn
  skjemaid_navn_kombinasjoner = kodebok %>%
    distinct(skjema_id, skjemanavn) %>%
    nrow()

  n_skjemaid = kodebok %>%
    distinct(skjema_id) %>%
    nrow()

  if (skjemaid_navn_kombinasjoner != n_skjemaid) {
    skjema_id_duplikat = kodebok %>%
      distinct(skjema_id, skjemanavn) %>%
      filter(duplicated(skjema_id)) %>%
      pull(skjema_id)
    stop(paste0("skjema_id har ikke entydig skjemanavn\nskjema_id: ", skjema_id_duplikat))
  }

  if (any(!is.na(kodebok$kategori))) {

    # Sjekker om alle skjema har minst én kategori
    skjema_id = kodebok %>%
      distinct(skjema_id) %>%
      pull(skjema_id)

    skjema_id_kategori = kodebok %>%
      filter(!is.na(kategori)) %>%
      distinct(skjema_id) %>%
      pull(skjema_id)

    mangler_kategori = setdiff(skjema_id, skjema_id_kategori)

    if (length(mangler_kategori) > 0) {
      stop(paste0("Alle skjema må ha tilhørende kategori hvis kategorier brukes. Følgende skjema_id mangler kategori:\n", mangler_kategori))
    }

    # Sjekker at alle skjema har kategori i første rad
    mangler_kat_rad_en = kodebok %>%
      group_by(skjema_id) %>%
      slice(1) %>%
      filter(is.na(kategori)) %>%
      pull(skjema_id)

    if (length(mangler_kat_rad_en) > 0) {
      stop(paste0("Hvis kategorier brukes må det være oppgitt kategori i første rad for alle skjema"))
    }
  }
}

valider_kb_kolonner = function(kodebok) {
  aksepterte_variabeltyper = c(
    "kategorisk", "tekst", "boolsk",
    "dato", "numerisk", "kl", "dato_kl"
  )

  # sjekk at alle variabeltyper er godkjent
  ny_vartype = kodebok$variabeltype[
    !kodebok$variabeltype %in% aksepterte_variabeltyper
  ]
  if (length(ny_vartype) > 0) {
    stop(
      "Det finnes variabeltyper som ikke er støttet:\n",
      str_c(ny_vartype, collapse = ", ")
    )
  }

  # sjekk at obligatorisk kolonne er tekstformat
  stopifnot(is.character(kodebok$obligatorisk))

  # sjekk at kolonnene obligatorisk, unik og manglende kun
  # inneholder "ja" eller "nei".
  if (any(!kodebok$obligatorisk %in% c("ja", "nei")) |
    any(!kodebok$unik %in% c("ja", "nei")) |
    any(!kodebok$manglande %in% c("ja", "nei"))) {
    stop("Kolonnene obligatorisk, unik og manglande kan bare inneholde 'ja' eller 'nei'")
  }

  # sjekk at desimaler er positivt heltall hvis det er inkludert
  stopifnot(is.integer(kodebok$desimaler))
  if (any(!is.na(kodebok$desimaler))) {
    if (any(kodebok$desimaler < 0L, na.rm = TRUE)) {
      stop("Desimalkolonnen må være et ikke-negativt heltall")
    }
  }

  # sjekk at eining ikke er en tom streng
  if (any(kodebok$eining == "", na.rm = TRUE)) {
    stop("Eining kan ikke være en tom tekststreng")
  }

  # sjekk at variabel_id kun er tall, bokstaver og _. må begynne med en bokstav
  ugyldig_varnavn = kodebok$variabel_id[str_detect(kodebok$variabel_id,
    pattern = "^[a-zæøå]([0-9a-z_æøå]*[0-9a-zæøå])?$", negate = TRUE
  )]

  if (length(ugyldig_varnavn > 0)) {
    stop("Det finnes ugyldige variabelnavn:\n", str_c(ugyldig_varnavn, collapse = ", "))
  }
}

valider_kb_variabler = function(kodebok) {

  # sjekke at variabeltyper er entydige
  ulike_vartyper = kodebok %>%
    group_by(variabel_id) %>%
    summarise(antall_ulike = n_distinct(variabeltype)) %>%
    filter(antall_ulike > 1) %>%
    pull(variabel_id)

  if (length(ulike_vartyper) > 0) {
    stop("Variabler må ha entydige variabeltyper:\n", str_c(ulike_vartyper, collapse = ", "))
  }

  # sjekk at variabeletiketter er entydige
  ulike_variabeletiketter = kodebok %>%
    group_by(variabel_id) %>%
    summarise(antall_ulike = n_distinct(variabeletikett)) %>%
    filter(antall_ulike > 1) %>%
    pull(variabel_id)

  if (length(ulike_variabeletiketter > 0)) {
    stop("En variabel kan ikke ha flere ulike variabeletiketter:\n", str_c(ulike_variabeletiketter, collapse = ", "))
  }

  # sjekk at kategoriske variabler har tilsvarende verditekst for hver verdi på tvers av skjema
  flere_verditekster = kodebok %>%
    filter(variabeltype == "kategorisk") %>%
    group_by(variabel_id, verdi) %>%
    summarise(antall_verditekst = n_distinct(verditekst, .keep_all = TRUE)) %>%
    filter(antall_verditekst > 1)

  if (nrow(flere_verditekster) > 0) {
    stop(error = paste0(
      "Det finnes ",
      nrow(flere_verditekster),
      " avvik for listeverdi mellom skjema:\nVariabel: ",
      flere_verditekster$variabel_id, "\nVerdi: ",
      flere_verditekster$verdi
    ))
  }

  # sjekk at boolske variabler ikke har Obligatorisk = Nei og Unik = Ja
  feil_boolsk = kodebok %>%
    filter(
      variabeltype == "boolsk",
      obligatorisk == "nei" & unik == "ja"
    ) %>%
    pull(variabel_id)

  if (length(feil_boolsk) > 0) {
    stop("Boolske variabler kan ikke ha Obligatorisk = 'nei' og Unik = 'ja'\nVariabel: ", str_c(feil_boolsk, collapse = ", "))
  }

  # sjekke diverse ting med kategoriske variabler
  duplikat_verdi = kodebok %>%
    filter(variabeltype == "kategorisk") %>%
    group_by(skjema_id, variabel_id) %>%
    add_count(verdi, name = "antall_av_verdi") %>%
    filter(antall_av_verdi > 1) %>%
    distinct(variabel_id) %>%
    pull(variabel_id)

  na_verdi = kodebok %>%
    filter(
      variabeltype == "kategorisk",
      is.na(verdi)
    ) %>%
    pull(variabel_id)

  antall_alternativ = kodebok %>%
    filter(variabeltype == "kategorisk") %>%
    group_by(variabel_id) %>%
    summarise(antall_alternativ = n()) %>%
    filter(antall_alternativ < 2) %>%
    pull(variabel_id)

  if (length(duplikat_verdi > 0)) {
    stop(
      "Kategoriske variabler må ha unike verdier\nVariabel: ",
      str_c(duplikat_verdi, collapse = ", ")
    )
  }

  if (length(na_verdi) > 0) {
    stop(
      "Kategoriske variabler kan ikke ha NA som verdi\nVariabel: ",
      str_c(na_verdi, collapse = ", ")
    )
  }

  if (length(antall_alternativ) > 0) {
    stop(
      "Kategoriske variabler må ha minst to svaralternativ\nVariabel: ",
      str_c(antall_alternativ, collapse = ", ")
    )
  }

  # sjekker at variabler ikke har informasjon i kolonner som ikke er relevant for variabeltypen:
  feil_info_numerisk = kodebok %>%
    filter(variabeltype == "numerisk") %>%
    filter(!is.na(verdi) | !is.na(verditekst) |
      !is.na(min_dato) | !is.na(maks_dato) |
      !is.na(min_rimeleg_dato) | !is.na(maks_rimeleg_dato))

  if (nrow(feil_info_numerisk) > 0) {
    stop("Numeriske variabler kan ikke ha informasjon i kolonnene:\nverdi, verditekst, min_dato, maks_dato, min_rimeleg_dato, maks_rimeleg_dato")
  }

  feil_info_tekst = kodebok %>%
    filter(variabeltype == "tekst") %>%
    filter(!is.na(verdi) | !is.na(verditekst) | !is.na(desimaler) | !is.na(eining) |
      !is.na(min) | !is.na(maks) | !is.na(min_rimeleg) |
      !is.na(maks_rimeleg) | !is.na(min_dato) | !is.na(maks_dato) |
      !is.na(min_rimeleg_dato) | !is.na(maks_rimeleg_dato) |
      !is.na(kommentar_rimeleg) | !is.na(utrekningsformel) | !is.na(logikk))

  if (nrow(feil_info_tekst) > 0) {
    stop("Tekstvariabler kan ikke inneholde informasjon i
kolonnene:\nverdi, verditekst, desimaler, eining, min, maks, min_rimeleg,
maks_rimeleg, min_dato, maks_dato, min_rimeleg_dato, maks_rimeleg_dato,
kommentar_rimeleg, utrekningsformel, logikk")
  }

  feil_info_kategorisk = kodebok %>%
    filter(variabeltype == "kategorisk") %>%
    filter(!is.na(eining) | !is.na(desimaler) | !is.na(min) | !is.na(maks) |
      !is.na(min_rimeleg) | !is.na(maks_rimeleg) | !is.na(min_dato) |
      !is.na(maks_dato) | !is.na(min_rimeleg_dato) |
      !is.na(maks_rimeleg_dato) | !is.na(kommentar_rimeleg) |
      !is.na(utrekningsformel) | !is.na(logikk))

  if (nrow(feil_info_kategorisk) > 0) {
    stop("Kategoriske variabler kan ikke ha informasjon i kolonnene:\n
eining, desimaler, min, maks, min_rimeleg, maks_rimeleg, min_dato, maks_dato,min_rimeleg_dato, maks_rimeleg_dato,kommentar_rimeleg, utrekningsformel, logikk")
  }

  # sjekke at ikke-kategoriske variabler ikke har manglende = 'ja'
  ikke_kat_manglande = kodebok %>%
    filter(
      variabeltype != "kategorisk",
      manglande == "ja"
    ) %>%
    pull(variabel_id)

  if (length(ikke_kat_manglande) > 0) {
    stop(
      "Ikke-kategoriske variabler kan ikke ha manglende = 'ja'\nvariabel_id: ",
      str_c(ikke_kat_manglande, collapse = ", ")
    )
  }

  # sjekke for feil i relasjon mellom min og maks-verdier
  feil_relasjon = kodebok %>%
    filter(min > maks |
      min_rimeleg < min |
      min_rimeleg > maks_rimeleg |
      maks_rimeleg > maks |
      lubridate::ymd(min_dato) > lubridate::ymd(maks_dato) |
      lubridate::ymd(min_rimeleg_dato) < lubridate::ymd(min_dato) |
      lubridate::ymd(min_rimeleg_dato) > lubridate::ymd(maks_rimeleg_dato) |
      lubridate::ymd(maks_rimeleg_dato) > lubridate::ymd(maks_dato)) %>%
    pull(variabel_id)

  if (length(feil_relasjon) > 0) {
    stop("Relasjon mellom minimum og maksimum verdier er ikke ivaretatt\nvariabel_id: ", str_c(feil_relasjon, collapse = ", "))
  }

  # sjekke at rimeleg-verdier finnes hvis kommentar_rimeleg finnes
  kommentar_uten_verdier = kodebok %>%
    filter(!is.na(kommentar_rimeleg) &
      (is.na(min_rimeleg) & is.na(maks_rimeleg) &
        is.na(min_rimeleg_dato) & is.na(maks_rimeleg_dato))) %>%
    pull(variabel_id)

  if (length(kommentar_uten_verdier) > 0) {
    stop("Kommentar_rimeleg er fylt ut, men det finnes ingen min_rimeleg eller maks_rimeleg\nvariabel_id: ", str_c(kommentar_uten_verdier, collapse = ", "))
  }
}
