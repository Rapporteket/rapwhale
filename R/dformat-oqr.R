# Innlesing av kodebøker og datadumpar frå OQR.

# Les inn kodebok og gjer om til standardformat ---------------------------

# Les inn OQR-kodebok på dokumentert format og
# gjer om til vårt standardformat (kanonisk form)
#
# Denne funksjonen er laga basert på den offisielle dokumentasjonen på kodebok-
# formatet til OQR, dvs. dokumentet «KG-Klokeboken-100418-1349-11.pdf».
#
# Inndata:
#   mappe_dd: Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD, for kvart uttak)
#   reg_id:   ID som identifiserer registeret og er prefiks til alle filnamna
#   dato:     Datoen ein skal henta ut kodeboka for (tekststreng eller dato). Kan òg vera NULL, for å henta nyaste kodebok.
#   valider_kb: Skal kodeboka automatisk validerast? Ho må då vera gyldig for at ein skal få noko ut.
#
# Utdata:
#   kodeboka på standardformat (kanonisk form), med variabelnamn gjort om til små bokstavar


# Roxygen dokumentasjon

#' Konverter OQR-kodebok til standardformat
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjonen leser inn OQR-kodebok på dokumentert format og gjer om til vårt standardformat (kanonisk form).
#'
#' Returnerer kodeboka på standardformat (kanonisk form), med variabelnamn gjort om til små bokstavar.
#' @param mappe_dd Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD, for kvart uttak).
#' @param reg_id ID som identifiserer registeret og er prefiks til alle filnamna.
#' @param dato Datoen ein skal henta ut kodeboka for (tekststreng eller dato). Kan òg vera NULL, for å henta nyaste kodebok.
#' @param valider_kb Skal kodeboka automatisk validerast? Ho må då vera gyldig for at ein skal få noko ut.
#' @export
les_kb_oqr = function(mappe_dd, reg_id, dato = NULL, valider_kb = TRUE) { # fixme: Validering av kodebok?

  # Bruk siste tilgjengelege kodebok dersom ein ikkje har valt dato
  if (is.null(dato)) {
    dato = dir(mappe_dd, pattern = "^[0-9]{4}-[0-1][0-9]-[0-9]{2}$", full.names = FALSE) |>
      sort() |>
      last()
  }
  dato = lubridate::as_date(dato) # I tilfelle det var ein tekstreng

  # Les inn kodeboka
  adresse_kb = paste0(
    mappe_dd, "\\", dato, "\\",
    reg_id, "_klokeboken_", dato, ".csv"
  )
  kodebok_oqr_format = readr::stop_for_problems(read_delim(
    adresse_kb,
    delim = ";", quote = "\"",
    col_types = readr::cols(
      skjemanavn = col_character(),
      navn_i_rapporteket = col_character(),
      ledetekst = col_character(),
      obligatorisk = col_character(),
      type = col_character(),
      listeverdier = col_character(),
      listetekst = col_character(),
      normalintervall_start_numerisk = col_character(), # Sjå merknad nedanfor om årsaka til denne og dei tre neste må vera tekst
      normalintervall_slutt_numerisk = col_character(),
      maksintervall_start_numerisk = col_character(),
      maksintervall_slutt_numerisk = col_character(),
      normalintervall_start_dato = col_character(),
      normalintervall_slutt_dato = col_character(),
      maksintervall_start_dato = col_character(),
      maksintervall_slutt_dato = col_character(),
      antall_tegn = col_integer(),
      lovlige_tegn = col_character(),
      desimaler = col_integer(),
      aktiveringsspoersmaal = col_character(),
      underspoersmaal = col_character(),
      innfoert_dato = col_character(),
      utfaset_dato = col_character(),
      tabell = col_character(),
      fysisk_feltnavn = col_character(),
      kommentar = col_character(),
      variabel_id = col_character(),
      hjelpetekst = col_character()
    )
  ))

  # Dei numeriske min- og maksverdiane kan ifølgje dokumentasjonen
  # http://helseregister.no/confluence/display/KG/Klokeboken
  # òg vera tekst som referer til andre felt, eks. «birthYear»
  # eller «todayYear». Dei kan me ikkje bruka, så me fjernar
  # dei rett og slett. Dette gjer me lettast ved å prøva
  # å gjera tekst om til tal med as.numeric(), som gjev ut
  # NA for alt som ikkje ser ut som tal.
  tekst_til_tal = function(x) {
    suppressWarnings(as.numeric(x))
  }

  # Gjer om kodeboka til vårt *standardiserte* format
  # (Det finst ikkje heilt ei 1-til-1-kopling, men me
  #  gjer so godt me kan, og set verdiar til NA der
  #  det ikkje finst nokon tilsvarande.)
  #
  # fixme: Vårt kodebokformat bør nok oppdaterast til
  #        å støtta min- og maks-verdiar for datoar òg.
  kodebok = kodebok_oqr_format |>
    mutate(
      skjema_id = tabell,
      skjemanamn = skjemanavn,
      variabel_id = str_to_lower(fysisk_feltnavn),
      obligatorisk = str_to_lower(obligatorisk),
      variabeletikett = ledetekst,
      forklaring = hjelpetekst,
      variabeltype = type,
      verdi = listeverdier,
      verditekst = listetekst,
      desimalar = desimaler,
      min = tekst_til_tal(maksintervall_start_numerisk),
      maks = tekst_til_tal(maksintervall_slutt_numerisk),
      min_rimeleg = tekst_til_tal(normalintervall_start_numerisk),
      maks_rimeleg = tekst_til_tal(normalintervall_slutt_numerisk),
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

  # Ein «Statusvariabel» er eigentleg ein kategorisk variabel
  # som kan ta tre verdiar, -1, 0 og 1 (oppretta, lagra og ferdigstilt).
  # Me gjer derfor kvar statusvariabel om til ein kategorisk variabel.
  # Kodeboka må utvidast med nye rader, og me gjer det iterativt,
  # éin gong for kvar statusvariabel (i teorien litt tregt/suboptimalt,
  # men i praksis kjemperaskt, sidan me ikkje har kodebøker med tusenvis
  # av statusvariablar, berre maks éin per skjema).
  while (any(kodebok$variabeltype == "Statusvariabel")) {
    # Radnummeret til første (ubehandla) statusvariabel
    ind = which(kodebok$variabeltype == "Statusvariabel")[1]

    # Rada må bytast ut med tre rader, éi for kvar moglege verdi.
    # Dette gjer me først ved å utvida kodeboka med to ekstra,
    # identiske rader rett etter rada. Så set me inn rette verdiar.
    #
    # Gjenta aktuell rad tre gongar (når me gjer det slik,
    # funkar det òg viss rada er første eller siste rad).
    kodebok = kodebok[append(seq_len(nrow(kodebok)),
      values = c(ind, ind), after = ind
    ), ]

    # Legg rette verdiar inn i dei tre nye radene
    nyind = c(ind, ind + 1, ind + 2)
    kodebok$verdi[nyind] = -1:1
    kodebok$verditekst[nyind] = c("Opprettet", "Lagret", "Ferdigstilt")
    kodebok$variabeltype[nyind] = "Listevariabel"
  }

  # Oversikt over variabeltypar i OQR og tilhøyrande standardnamn som me brukar
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
    "TIMESTAMP", "dato_kl",
    "Dynamisk Listevariabel", "tekst"
  )

  # Stopp viss det dukkar opp variabeltypar me ikkje kjenner til
  nye_vartypar = na.omit(setdiff(kodebok$variabeltype, vartype_oqr_standard$type_oqr))
  if (length(nye_vartypar) > 0) {
    stop(
      "Kodeboka har variabeltypar me ikkje støttar / har standardnamn på: ",
      str_c(nye_vartypar, collapse = ", ")
    )
  }

  # Byt ut variabeltype-verdiane med våre standardiserte namn
  kodebok$variabeltype = vartype_oqr_standard$type_standard[
    match(kodebok$variabeltype, vartype_oqr_standard$type_oqr)
  ]

  # Fra dokumentasjonen på kodebok:
  # Obligatorisk
  # Betydning: Om variabelen må fylles ut eller ikke dersom den er synlig.
  # Det angis ikke om variabelen ved visse tilfeller er skjult for bruker.
  # Det vil si at obligatoriske variabler kan være blanke dersom de ikke
  # er relevante pga andre spørsmål i skjemaet.
  # aktiveringsspoersmaal-kolonnen i klokeboken beskriver en variabel åpner opp nye variabler for bruker "Ja" eller ikke "Nei".
  # Er denne "Ja" er den alltid synlig for bruker,
  # og vi kan vite at den da vil være obligatorisk (hvis den også er markert som obligatorisk)
  kodebok = kodebok |>
    mutate(obligatorisk = ifelse(aktiveringsspoersmaal == "Ja" & obligatorisk == "ja", "ja", "nei"))

  # I tillegg til dei definerte variablane har datadumpane seks ekstra
  # variablar, to før kodebokvariablane og fire etter. Desse er definerte
  # i dokumentasjonen til datadumpane (dokumentet «4.2 Dokumentasjon på
  # format av Datadump i register.doc»). Desse *burde* vore med kodebøkene,
  # men sidan HNIKT ikkje har klart å leggja dei til, må me gjera det sjølv.
  legg_til_ekstravar = function(kb) {
    kb_ekstra = tribble(
      ~variabel_id, ~variabeletikett, ~variabeltype, ~unik, ~obligatorisk, ~desimalar,
      "mceid", "Forløps-ID", "numerisk", "ja", "ja", 0L,
      "centreid", "RESH-ID", "tekst", "nei", "ja", NA,
      "tsupdated", "Skjema sist oppdatert", "dato_kl", "nei", "nei", NA,
      "updatedby", "Skjema oppdatert av", "tekst", "nei", "nei", NA,
      "first_time_closed", "Skjema er ferdigstilt", "dato_kl", "nei", "ja", NA,
      "first_time_closed_by", "Skjema er ferdigstilt av", "tekst", "nei", "ja", NA,
      "tscreated", "Skjema oppretta", "dato_kl", "nei", "ja", NA,
      "createdby", "Skjema oppretta av", "tekst", "nei", "ja", NA
    )
    kb_utvida = bind_rows(kb_ekstra[1:2, ], kb, kb_ekstra[3:8, ])
    kb_utvida
  }
  kodebok = kodebok |>
    nest(skjema_kb = c(-skjema_id, -skjemanamn)) |>
    mutate(skjema_kb = map(skjema_kb, legg_til_ekstravar)) |>
    unnest(cols = c(skjema_kb))

  # fixme: Sjekk at verdiane til variablane faktiske er *like* på alle tabellane
  # Det er ein føresetnad for at det nedanfor skal fungera. Viss for eksempel
  # operasjonstype «1» tyder ABC på operasjonsskjema men CDE på oppfølgingsskjemaet,
  # kan variabelen ikkje brukast, då han har tvetyding definisjon.
  #
  # Nokre variablar er med fleire gongar på same tabell (men på ulike «skjema»)
  # Me treng (og skal ha) berre første oppføring per *tabell*. For eksempel
  # viss «type komplikasjon» er registrert med variabelen «type_kompl»
  # i tabellen «kompl», og blir fylt ved 1-års, 2-års og 3-års oppfølging
  # (tre ulike skjemanamn), vil det finnast tre separate oppføringar for
  # «type_kompl», eitt for kvart skjema. Me skal berre ha første. (Men
  # merk at denne kan bestå av fleire *rader*, for kategoriske variablar.)
  kodebok = kodebok |>
    distinct(skjema_id, variabel_id, verdi, verditekst, .keep_all = TRUE)

  # Dei variabel-/kolonnenamna me brukar, i standard/fornuftig rekkjefølgje
  std_namn = c(
    "skjema_id", "skjemanamn", "kategori", "innleiing", "variabel_id",
    "variabeletikett", "forklaring", "variabeltype", "eining", "unik",
    "obligatorisk", "verdi", "verditekst", "manglande", "desimalar",
    "min", "maks", "min_rimeleg", "maks_rimeleg", "kommentar_rimeleg",
    "utrekningsformel", "logikk", "kommentar"
  )

  # Bruk vidare berre standardkolonnne, og i standard/fornuftig rekkjefølgje
  # (Må stå etter legg_til_ekstravar(), sidan denne endrar rekkjefølgja)
  kodebok = kodebok |>
    select(!!std_namn)

  # Skjemanamna heng ikkje saman med tabellnamna(!). Det ser ut til
  # at skjemanamna høyrer til dei faktiske skjemaa i innregistrerings-
  # løysinga, men kva tabell ting vert lagra i er noko anna.
  # For eksempel kan ein ha ein variabel for dødsdato på eit
  # operasjons- og/eller oppfølgingsskjema, men dødsdatoen vert
  # (heldigvis) berre lagra i pasienttabellen.
  #
  # For å få fornuftige skjemanamn til kvar tabell(-ID), vel me
  # derfor det første *ledige* skjemanamnet som den aktuelle
  # tabellen har. Viss det ikkje finst nokon ledige, brukar me
  # tabell-ID-en.
  tabell_id_til_skjemanamn = function(ids, namn) {
    # Lag ein omkodingstabell, frå tabell-ID til skjemanamn
    kod_id = unique(ids)
    kod_namn = character(length(kod_id)) # Tom vektor til å halda resultatet

    # Sjå på alle observerte kombinasjonar av tabell-ID og skjemanamn (i naturleg rekkjefølgje)
    komb = distinct(tibble(id = ids, namn))
    for (i in seq_along(kod_id)) {
      kandidatar = c(komb$namn[komb$id == kod_id[i]], kod_id[i])
      kandidatar = setdiff(kandidatar, kod_namn) # Fjern allereie brukte skjemanamn
      kod_namn[i] = kandidatar[1] # Bruk første *ledige* (vil alltid vera ein, utanom det patologiske tilfelle der skjemanamna er lik skjema-ID-ane, men ikkje med 1-1-samsvar)
    }

    # Bruk omkodingstabellen til gje ut rett namn på alle ID-ane
    kod_namn[match(ids, kod_id)]
  }
  # # Eksempel (og test)
  # ids   = c("pasreg",  "basereg", "basereg","pasreg",   "op",       "op",       "ev",       "basereg")
  # namn  = c("Pasient", "Basis",   "Basis",  "Opskjema", "Pasient",  "Opskjema", "Opskjema", "Basis")
  # fasit = c("Pasient", "Basis",   "Basis",  "Pasient",  "Opskjema", "Opskjema", "ev",       "Basis")
  # stopifnot(identical(fasit, tabell_id_til_skjemanamn(ids, namn)))

  # Fiksa oppgitt skjemanamn til noko som er unikt for kvar
  # skjema-ID (= tabell-ID)
  kodebok$skjemanamn = tabell_id_til_skjemanamn(kodebok$skjema_id, kodebok$skjemanamn)

  # Nokre kodebøker er ikkje sorterte skikkeleg etter skjema_id,
  # slik at variablar kjem hulter til bulter. Fiksar derfor dette.
  # Men sorterer *ikkje* alfabetisk, sidan den naturlege rekkjefølgja
  # ofte er meir logisk.
  kodebok = kodebok |>
    arrange(fct_inorder(skjema_id))

  # Sjekk eventuelt at kodeboka er gyldig
  if (valider_kb) {
    gyldig = kb_er_gyldig(kodebok)
    if (!gyldig) {
      stop("Kodeboka er ikkje gyldig")
    }
  }

  # Gjer om til kanonisk form
  kodebok = kb_til_kanonisk_form(kodebok)

  # Returner kodeboka
  kodebok
}


# Les datadump frå OQR-register -------------------------------------------

#' Les datadump fra OQR-register
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Les inn OQR-data frå gitt skjema ved hjelp av kodebok. \cr \cr
#' Kodeboka vert brukt til å gje alle variablane rett format (tal, tekst, dato, boolske/logiske verdiar osv.)
#' og til å sikra at datadumpen er i samsvar med kodeboka. \cr
#' Som standard treng ein ikkje oppgje kodebok; ho vert automatisk henta inn. \cr
#' Men dersom ein skal lesa inn mange skjema, er det lurare å lesa inn
#' kodeboka separat først, for at ting skal gå raskare (innlesing og validering
#' av kodeboka kan ta litt tid). \cr \cr
#' Det er òg nødvendig å gjera det slik dersom ein har kodeboka frå
#' ei anna kjelde eller viss ein vil bruka ei modifisert kodebok (generelt farleg!).
#'
#' Returnerer et R-datasett for det aktuelle skjemaet, med variabelnamn gjort om til små bokstavar.
#'
#' @param mappe_dd Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD, for kvart uttak).
#' @param reg_id ID som identifiserer registeret og er prefiks til alle filnamna.
#' @param skjema_id ID til skjemaet ein vil henta inn (brukt i filnamnet og i kolonnen «tabell» i kodeboka).
#' @param status Berre ta med skjema med desse statusverdiane (-1 = oppretta, 0 = kladd, 1 = ferdigstilt). \cr
#' Kan òg vera NULL, for å henta alt, uavhengig av status (dvs. også inkludert NA-status og ugyldige statusverdiar,
#' eller datadumpar som manglar statusvariabel \cr
#' (ikkje noko av dette \emph{skal} vera mogleg å få, men alt kan skje i denne verda ...)).
#' @param dato Datoen ein skal henta ut kodeboka for (tekststreng eller dato). Kan òg vera NULL, for å henta nyaste kodebok.
#' @param kodebok Kodebok på kanonisk form. Kan òg vera NULL, og då vert kodeboka automatisk henta inn.
#' @param valider_kb Skal kodeboka validerast? Standard er ja dersom kodeboka skal hentast inn automatisk, elles nei.
#' @param valider_dd Skal datadumpen validerast? Standard er ja.
#' @export
les_dd_oqr = function(mappe_dd, reg_id, skjema_id, status = 1, dato = NULL, kodebok = NULL,
                      valider_kb = is.null(kodebok), valider_dd = TRUE) {
  # Bruk siste tilgjengelege kodebok dersom ein ikkje har valt dato
  if (is.null(dato)) {
    dato = dir(mappe_dd, pattern = "^[0-9]{4}-[0-1][0-9]-[0-9]{2}$", full.names = FALSE) |>
      sort() |>
      last()
  }
  dato = lubridate::as_date(dato) # I tilfelle det var ein tekstreng

  # Les inn kodeboka dersom ho ikkje er spesifisert
  if (is.null(kodebok)) {
    kodebok = les_kb_oqr(mappe_dd, reg_id, dato, valider_kb = valider_kb)
  }
  # Hent ut variabelinfo frå kodeboka for det gjeldande skjemaet
  kb_akt = kodebok |>
    filter(skjema_id == !!skjema_id)

  # Kodeboka må ha informasjon om variablane i
  # i det aktuelle skjemaet for at me skal halda fram ...
  if (nrow(kb_akt) == 0) {
    stop("Kodeboka manglar informasjon om skjemaet '", skjema_id, "'")
  }

  # Les inn variabelnamna som vert brukt i datafila
  adresse_dd = paste0(
    mappe_dd, "\\", dato, "\\",
    reg_id, "_", skjema_id, "_datadump_", dato, ".csv"
  )
  varnamn_dd = tolower(scan(adresse_dd,
    fileEncoding = "UTF-8-BOM", what = "character",
    sep = ";", nlines = 1, quiet = TRUE
  ))

  # Sjekk at alle variablane i datadumpen finst i kodeboka
  # og at alle variablane i kodeboka finst i datadumpen
  varnamn_kb = unique(kb_akt$variabel_id)
  ekstra_kb = setdiff(varnamn_kb, varnamn_dd)
  ekstra_dd = setdiff(varnamn_dd, varnamn_kb)
  if (length(ekstra_kb) > 0) {
    stop(paste0(
      "Desse variablane finst berre i kodeboka:\n",
      paste(ekstra_kb, collapse = ", "), "\n"
    ))
  }
  if (length(ekstra_dd) > 0) {
    stop(paste0(
      "Desse variablane finst berre i datadumpen:\n",
      paste(toupper(ekstra_dd), collapse = ", "), "\n"
    ))
  }

  # dokumentasjonen til OpenQReg beskriver at kolonnene i datadumpen
  # er ikke nødvendigvis i samme rekkefølge som i kodeboka.
  # Vi setter navnene i kodeboka til å ha samme rekkefølge som datadumpen
  varnamn_kb = varnamn_kb[match(varnamn_dd, varnamn_kb)]

  # Datafila *kan* ikkje innehalda duplikate kolonnenamn,
  # sidan me då ikkje kan veta kva kolonne eit namn svarar til.
  # Stopp derfor viss me finn duplikate namn.
  dupnamn = duplicated(varnamn_dd)
  if (any(dupnamn)) {
    stop(
      "Datafila har duplikate variabelnamn:\n",
      str_c(varnamn_dd[dupnamn], collapse = "\n")
    )
  }

  # Hent ut første linje frå kodeboka, dvs. den linja som
  # inneheld aktuell informasjon
  kb_info = kodebok |>
    distinct(variabel_id, .keep_all = TRUE)

  # Forkortingsbokstavane som read_csv() brukar (fixme: utvide med fleire)
  spek_csv_oqr = tribble(
    ~variabeltype, ~csv_bokstav,
    "kategorisk", "c", # Sjå kommentar nedanfor
    "tekst", "c",
    "boolsk", "c", # Sjå konvertering nedanfor
    "dato_kl", "c", # Mellombels, jf. https://github.com/tidyverse/readr/issues/642 (fixme til "T" når denne er fiksa)
    "numerisk", "d",
    "dato", "D",
    "kl", "t"
  )
  spek_innlesing = tibble(variabel_id = varnamn_kb) |>
    left_join(kb_info, by = "variabel_id", relationship = "one-to-one") |>
    left_join(spek_csv_oqr, by = "variabeltype", relationship = "many-to-one")

  # Har kodeboka variablar av ein type me ikkje har lagt inn støtte for?
  # Dette skal ikkje skje, så avbryt om så er tilfelle.
  nye_typar = setdiff(kb_info$variabeltype, spek_csv_oqr$variabeltype)
  if (length(nye_typar) > 0) {
    stop(
      "Kodeboka har variablar av ein type me ikkje støttar (legg inn støtte!):\n",
      str_c(nye_typar, collapse = "\n")
    )
  }

  # er rekkefølgen lik i kodebok i innlesingsspek som i datadump?
  stopifnot(identical(spek_innlesing$variabel_id, varnamn_dd))

  # Les inn datasettet
  kol_typar = str_c(spek_innlesing$csv_bokstav, collapse = "")
  oqr_lokale = locale(
    decimal_mark = ",", grouping_mark = "",
    date_format = "%Y-%m-%d", time_format = "%H:%M:%S",
    tz = "Europe/Oslo"
  ) # Vert brukt både her og seinare
  d = read_delim(adresse_dd,
    delim = ";", quote = "\"", trim_ws = FALSE, na = "",
    escape_double = TRUE,
    col_names = spek_innlesing$variabel_id, col_types = kol_typar,
    skip = 1, # Hopp over overskriftsrada
    locale = oqr_lokale
  )
  readr::stop_for_problems(d) # Stopp viss det er nokon som helst problem med fila/innlesinga

  # Filtrer vekk skjema som ikkje har rett statusvariabel
  # (som standard vert berre ferdigstilte skjema tekne med)
  if (!is.null(status)) {
    d = d |>
      filter(status %in% !!status)
  }

  # Kategorisk er ein litt vrien variant. *Oftast* er han
  # tal, men me kan risikera at han er tekst òg. Ei løysing er å
  # alltid lesa han inn som tekst, men det er ikkje ei *god* løysing.
  # Når han er koda som tal, er det ofte betre å handsama han som tal.
  # Det ser betre ut, og det mogleggjer å bruka operatorar som < og >=
  # (eks. komplikasjonsgrad < 5) (men "10" er som kjent < "2"!).
  # Og for spørjeskjema er gjerne skåringskodane lagt inn som talkodar,
  # slik at det er fint om me kan skriva for eksempel sp1 + sp2 + ...
  # for å få ein sumskår).
  #
  # Så den *rette* måten å handtera dette på er å lesa inn kategoriske
  # verdiar som tal dersom dei moglege *verdiane* i kodeboka alle er tal,
  # og som tekst elles.

  # Først må me kunne gjenkjenna kva som er tal og kva som ikkje er det
  # (funksjonen nedanfor handterer ikkje tal på vitskapleg form,
  # som «1e-7», og det er bevisst, då slike ikkje bør stå i kodeboka,
  # og viss dei gjer det, er det nok som bokstavkodar, ikkje talkodar).
  er_tal = function(x) {
    str_detect(x, "^-?[0-9]+(\\.[0-9]+)?$")
  }
  ## Ev. kjapp test på at funksjonen fungerer som han skal
  # stopifnot(all(er_tal(c("-3", "0", "1", "997", "3.14", "-3.14", "0.7"))))
  # stopifnot(all(!er_tal(c("a", "2B", "F42.7", "-x", "1e-7", "3.", ".7")))) # Ev. godta "3." og .7"?

  # Finn dei kategoriske variablane som har berre numeriske verdiar ...
  vars_num = kb_akt |>
    filter(variabeltype == "kategorisk") |>
    group_by(variabel_id) |>
    summarise(er_talkat = all(er_tal(verdi))) |>
    filter(er_talkat) |>
    pull(variabel_id)
  # ... og gjer tilhøyrande innlesne variablar (om det er nokon) om til talvariablar
  d = d |>
    mutate(across(all_of(vars_num), as.numeric))

  # Gjer eventuelle boolske variablar om til ekte boolske variablar
  oqr_boolsk_til_boolsk = function(x) {
    # Sjekk først at det berre er gyldige verdiar
    er_gyldig = (x %in% c("0", "1")) | is.na(x)
    if (!all(er_gyldig)) {
      stop("Finst ugyldige verdiar i boolsk variabel (skal vera 0, 1 eller NA)")
    } else {
      x == 1 # Gjer om til boolsk variabel
    }
  }
  vars_boolsk = spek_innlesing$variabel_id[spek_innlesing$variabeltype == "boolsk"]
  d = d |>
    mutate(across(all_of(vars_boolsk), oqr_boolsk_til_boolsk))

  # Gjer eventuelle tidsvariablar om til ekte tidsvariablar
  # Fixme: Nødvendig pga. https://github.com/tidyverse/readr/issues/642
  #        Fjern når denne feilen er fiksa (rett då òg fixme-en
  #        lenger oppe som også handlar om dette)
  vars_datokl = spek_innlesing$variabel_id[spek_innlesing$variabeltype == "dato_kl"]
  d = d |>
    mutate(across(all_of(vars_datokl),
      .fns = \(dato_kl_vektor) readr::parse_datetime(dato_kl_vektor,
        format = "%Y-%m-%d %H:%M:%OS",
        locale = oqr_lokale
      )
    ))

  # Sjekk eventuelt at datadumpen er gyldig
  if (valider_dd) {
    gyldig = dd_er_gyldig(d, kb_akt, oblig = FALSE, rekkefolge = FALSE)
    if (!gyldig) {
      print(attr(gyldig, "rapport"), n = Inf) # Vis grunnen til at datadumpen ikkje er gyldig
      stop("Datadumpen er ikkje gyldig")
    }
  }

  # Returner datasettet
  d
}


# Eksempel på bruk  -----------------------------------------------------------

# # Les inn eksempeldata
# mappe_dd = "***FJERNA-ADRESSE***"
# kb = les_kb_oqr(mappe_dd, reg_id = "AblaNor")
# d = les_dd_oqr(mappe_dd, reg_id = "AblaNor", skjema_id = "basereg", kodebok = kb) # Ev. utelat «kb»-argumentet
# d = les_dd_oqr(mappe_dd, reg_id = "AblaNor", skjema_id = "gkv", kodebok = kb) # Ev. utelat «kb»-argumentet
# d = les_dd_oqr(mappe_dd, reg_id = "AblaNor", skjema_id = "pros", kodebok = kb) # Ev. utelat «kb»-argumentet
# d = les_dd_oqr(mappe_dd, reg_id = "AblaNor", skjema_id = "rand12", kodebok = kb) # Ev. utelat «kb»-argumentet
