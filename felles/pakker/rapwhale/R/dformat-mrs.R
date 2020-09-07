# Lesing/tolking av det elendige kodebokformatet til MRS :(

#' @importFrom magrittr %>%
#' @importFrom lubridate as_date
#' @importFrom stringr str_c
#' @importFrom rlang !!
#' @importFrom dplyr mutate left_join select filter pull distinct mutate_at
NULL

# Lag standardisert kodebok -----------------------------------------------

# Gjer om MRS-kodebok til kodebok på normalform
#
# Inndata:
#   mappe_dd: Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD, for kvart uttak)
#             Her er en antagelse at nyeste versjon av kodeboka ligger i samme mappe som datadumpene
#   dato:     Datoen ein skal henta ut kodeboka for (tekststreng eller dato). Kan òg vera NULL, for å henta nyaste kodebok.
#
# Utdata: kodeboka på standardformat (kanonisk form), med variabelnamn gjort om til små bokstavar


# Roxygen dokumentasjon

#' Konverter MRS-kodebok til standardformat
#'
#' Gjer om MRS-kodebok til kodebok på standardformat.
#'
#' Returnerer kodeboka på standardformat (kanonisk form), med variabelnamn gjort om til små bokstaver.
#'
#' @param mappe_dd Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD, for kvart uttak)
#' Her er en antagelse at nyeste versjon av kodeboka ligger i samme mappe som datadumpene.
#' @param dato Datoen ein skal henta ut kodeboka for (tekststreng eller dato). Kan òg vera NULL, for å henta nyaste kodebok.
#' @export
les_kb_mrs = function(mappe_dd, dato = NULL) {

  # Bruk siste tilgjengelege kodebok dersom ein ikkje har valt dato
  if (is.null(dato)) {
    dato = dir(mappe_dd, pattern = "^[0-9]{4}-[0-1][0-9]-[0-9]{2}$", full.names = FALSE) %>%
      sort() %>%
      last()
  }
  dato = as_date(dato) # I tilfelle det var ein tekstreng

  # Sammensatt adresse til kodeboka
  adresse_kb = paste0(mappe_dd, "\\", dato, "\\rapport.xlsx")

  # Kodeboka er laget i excel. Excel har heller ikke så mange, presise variabeltyper
  # MRS gjør det gangske enkelt med at alle kolonnene er "text".
  # Per i dag har de 6 kolonner
  kb_mrs_koltyper = rep("text", 6)

  # henter inn excel-ark navn
  ark = readxl::excel_sheets(adresse_kb)

  # funksjon for å hente inn alle ark og sette dem 1 objekt, med skjema_id lik ark-navnet
  les_excel_ark = function(ark_id) {
    readxl::read_excel(adresse_kb, col_types = kb_mrs_koltyper, sheet = ark_id) %>%
      mutate(skjema_id = !!ark_id)
  }

  # henter inn kodebok
  kb_mrs = ark %>%
    purrr::map_df(les_excel_ark)

  # ark-navnene er horrible. De er avkuttede versjoner av menneskelig-vennlige skjemanavn
  # De samsvarer ikke med skjemanavnet slik det er skrevet i filnavnet til datadumpen
  # Derfor erstatter vi disse med skjemanavnene slik de er i filnavnet til datadumpene.

  # henter inn fil som har en kolonne for skjema_id-ene i ark-navnene,
  # og en kolonne for skjema_id-ene i filnavnene til datadumpene, slik at vi vet hvilken som samsvarer med hvilken.
  # Denne er laget på forhånd, manuelt, men skal alltid legges i den nyeste mappen med datadumper v/ ny innhenting av data.
  adresse_skjema_id = paste0(mappe_dd, dato, "\\skjema_id_kobling.csv")
  d_skjema_id = readr::read_delim(
    adresse_skjema_id,
    delim = ";",
    locale = locale(encoding = "windows-1252"),
    col_types = cols(
      skjema_id_datadump = col_character(),
      skjema_id_kodebok = col_character(),
      skjema_id_kodebok_ny = col_character()
    )
  )

  # legger til "riktige" skjema_id
  kb_mrs = kb_mrs %>%
    left_join(d_skjema_id, by = c("skjema_id" = "skjema_id_kodebok_ny")) %>%
    mutate(skjema_id = skjema_id_datadump) %>%
    select(-skjema_id_datadump)

  # Indeks til rader som startar ein ny variabel
  ind_nyvar = which(!is.na(kb_mrs$Feltnavn))
  nvars = length(ind_nyvar) # Talet på variablar

  # Kor mange kodar kvar variabel har
  # (Merk at siste variabel ikkje vert
  #  avløyst av ein ny variabel, og må
  #  derfor handterast spesielt.)
  var_nverd = diff(ind_nyvar) - 1
  var_nverd[nvars] = nrow(kb_mrs) - ind_nyvar[nvars] # Sistevariabel

  # Oversikt over variabeltypar i MRS og tilhøyrande standardnamn som me brukar
  vartype_mrs_standard = tibble::tribble(
    ~type_mrs, ~type_standard,
    "Enum", "kategorisk",
    "Enkeltvalg", "kategorisk",
    "Text", "tekst",
    "Tekst", "tekst",
    "Avkrysning", "boolsk",
    "Dato/Tid", "dato_kl",
    "Dato/tid", "dato_kl",
    "Id (Guid)", "tekst",
    "Numerisk (heltall)", "numerisk", # Men sjå bruk «desimalar» lenger nede
    "Numerisk (flyttall)", "numerisk"
  )
  nye_vartypar = na.omit(setdiff(kb_mrs$Felttype, vartype_mrs_standard$type_mrs))
  if (length(nye_vartypar) > 0) {
    stop("Kodeboka har variabeltypar me ikkje har standardnamn på: ", str_c(nye_vartypar, collapse = ", "))
  }

  # Dette var sant fram til hausten 2017:
  #
  # -----
  # Lag dataramme med i utgangspunktet éi rad for kvar variabel
  # Variabelnamna brukt i datadumpen finn me:
  #   – Ikkje i kolonnen Datadumpnavn (det hadde vore for enkelt og logisk)
  #   – Ikkje i kolonnen Feltnavn (ditto)
  #   – Ikkje direkte i kolonnen Variabelnavn (ditto)
  # Men:
  #   Etter det *siste* punktumet (om dette eksisterer) i kolonnen Variabelnavn
  #   i den første rada i eit sett med rader som omhandlar ein variabel, og der
  #   settet begynner med ein ikkje-tom verdi i kolonnen som heiter Feltnavn.
  # -----
  #
  # Etter hausten 2017 ser de ut til at variabelnamna brukt i datadumpen
  # faktisk *finn* me i variabelen «Datadumpnavn»! *mind blown*
  #
  # Å finna dei andre verdiane (for eksempel kodar og kodetekst) gjer ein på
  # tilsvarande vanskelege måtar
  kodebok_utg = tibble::tibble(
    skjema_id = kb_mrs$skjema_id[ind_nyvar],
    variabel_id = kb_mrs$Variabelnavn[ind_nyvar] %>% stringr::str_replace(".*\\.", ""),
    variabeletikett = kb_mrs$Feltnavn[ind_nyvar], # Berre forklaring for *enkelte* variablar, men er det beste me har …
    variabeltype = vartype_mrs_standard$type_standard[match(kb_mrs$Felttype[ind_nyvar], vartype_mrs_standard$type_mrs)],
    obligatorisk = "nei",
    # skjema_id = d$Skjema[ind_nyvar], # fixme! Ventar spent på at denne skal dukka opp (førespurnad er send)
    verdi = NA_integer_, # Føreset førbels at MRS-kodane alltid er tal (gjer om til tekst om dette ikkje stemmer)
    verditekst = NA_character_,
    desimalar = ifelse(kb_mrs$Felttype[ind_nyvar] == "Numerisk (heltall)", 0L, NA_integer_),
    kommentar = kb_mrs$Hjelpetekst[ind_nyvar]
  )

  # Kor mange gongar kvar variabel skal gjentakast i
  # den nye kodeboka, dvs. kor mange rader han skal oppta
  reps = pmax(var_nverd, 1)

  # Utvid kodeboka slik at enum-variablane får fleire rader
  kodebok = kodebok_utg[rep(1:nvars, times = reps), ]

  # Hent ut kodane og tilhøyrande tekst til alle Enum/Enkeltvalg-variablane
  enums = kb_mrs %>%
    filter(is.na(Felttype)) %>%
    pull(`Mulige verdier`) %>%
    stringr::str_split_fixed(" = ", n = 2)

  # Legg kodane inn i den nye kodeboka,
  # med rett format (heiltal for kodar
  # og tekst for kodetekst), og på rett plass
  enum_ind = (kodebok$variabeltype == "kategorisk")
  kodebok$verdi[enum_ind] = enums[, 1] %>%
    as.numeric() # Kodar
  kodebok$verditekst[enum_ind] = enums[, 2] # Tilhøyrande tekst

  # Nokre verditekstar tyder at verdien ikkje er registrert,
  # og me markerer det i kodeboka
  kodebok = kodebok %>%
    mutate(manglande = ifelse(verditekst %in% c("---", "Velg verdi", "Ikke valgt"), "ja", "nei"))

  # gjør om kodeboka til kanonisk form
  kb_kanonisk = kb_til_kanonisk_form(kodebok)

  # Returner standardisert kodebok
  kb_kanonisk
}

# Les datadump frå MRS-register -------------------------------------------

# Les inn MRS-data frå gitt skjema ved hjelp av kodebok.
# Kodeboka vert brukt til å gje alle variablane rett format
# (tal, tekst, dato, boolske/logiske verdiar osv.) og til å
# sikra at datadumpen er i samsvar med kodeboka.
#
# Som standard treng ein ikkje oppgje kodebok; ho vert automatisk henta inn.
# Men dersom ein skal lesa inn mange skjema, er det lurare å lesa inn
# kodeboka separat først, for at ting skal gå raskare (innlesing og validering
# av kodeboka kan ta litt tid). Det er òg nødvendig å gjera det slik dersom
# ein har kodeboka frå ei anna kjelde eller viss ein vil bruka ei modifisert
# kodebok (generelt farleg!).
#
# Inndata:
#   mappe_dd:  Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD, for kvart uttak)
#              Antagelse ligger til grunn at nyeste kodebok ligger i samme mappe som de nyeste datadumpene.
#   skjema_id: ID til skjemaet ein vil henta inn (brukt i filnamnet og i kolonnen «tabell» i kodeboka)
#   versjon:   Om datadumpen er "Prod" eller om den er fra "QA". Standardverdi er "Prod".
#   dato:      Datoen ein skal henta ut kodeboka for (tekststreng eller dato). Kan òg vera NULL, for å henta nyaste kodebok.
#   kodebok:   Kodebok på kanonisk form. Kan òg vera NULL, og då vert kodeboka automatisk henta inn.
#
# Utdata:
#   R-datasett for det aktuelle skjemaet


# Roxygen dokumentasjon

#' Les datadump fra MRS-register
#'
#' Les inn MRS-data frå gitt skjema ved hjelp av kodebok. \cr
#' Kodeboka vert brukt til å gje alle variablane rett format
#' (tal, tekst, dato, boolske/logiske verdiar osv.) og til å
#' sikra at datadumpen er i samsvar med kodeboka. \cr \cr
#' Som standard treng ein ikkje oppgje kodebok; ho vert automatisk henta inn. \cr
#' Men dersom ein skal lesa inn mange skjema, er det lurare å lesa inn
#' kodeboka separat først, for at ting skal gå raskare (innlesing og validering
#' av kodeboka kan ta litt tid). \cr
#' Det er òg nødvendig å gjera det slik dersom ein har kodeboka frå ei anna
#' kjelde eller viss ein vil bruka ei modifisert
#' kodebok (generelt farleg!).
#'
#'
#' Returnerer et R-datasett for det aktuelle skjemaet.
#'
#' @param mappe_dd Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD, for kvart uttak)
#' Antagelse ligger til grunn at nyeste kodebok ligger i samme mappe som de nyeste datadumpene.
#' @param skjema_id ID til skjemaet ein vil henta inn (brukt i filnamnet og i kolonnen «tabell» i kodeboka)
#' @param versjon Om datadumpen er "Prod" eller om den er fra "QA". Standardverdi er "Prod".
#' @param dato Datoen ein skal henta ut kodeboka for (tekststreng eller dato). Kan òg vera NULL, for å henta nyaste kodebok.
#' @param kodebok Kodebok på kanonisk form. Kan òg vera NULL, og då vert kodeboka automatisk henta inn.
#' @export
les_dd_mrs = function(mappe_dd, skjema_id, versjon = "Prod", dato = NULL, kodebok = NULL) {

  # Bruk siste tilgjengelege kodebok dersom ein ikkje har valt dato
  if (is.null(dato)) {
    dato = dir(mappe_dd, pattern = "^[0-9]{4}-[0-1][0-9]-[0-9]{2}$", full.names = FALSE) %>%
      sort() %>%
      last()
  }
  dato = as_date(dato) # I tilfelle det var ein tekstreng

  # Les inn kodeboka dersom ho ikkje er spesifisert
  if (is.null(kodebok)) {
    kodebok = les_kb_mrs(mappe_dd, dato)
  }
  # Hent ut variabelinfo frå kodeboka for det gjeldande skjemaet
  kb_akt = kodebok %>%
    filter(skjema_id == !!skjema_id)

  # Adressen til datadumpen gitt datoen som vi har fått
  # Alle datadumper starter med samme prefiks + navn på skjema + dato + klokkeslett
  mappe_dd_dato = paste0(mappe_dd, "\\", dato, "\\") # mappe med dato, brukes flere ganger
  dd_navn_start = paste0("^DataDump_", versjon, "_", skjema_id, "_", dato, "_") # prefiks
  regexp_filnavn = paste0(dd_navn_start, "[0-9]{4}\\.csv$") # regexp for å finne riktig fil med klokkeslett

  # finner navnet på fila med riktig klokkslett
  filnavn = list.files(mappe_dd_dato, pattern = regexp_filnavn)
  stopifnot(length(filnavn) == 1)
  # og dermed adressen til datadumpen gitt datoen vi har fått
  adresse_dd = paste0(mappe_dd_dato, filnavn)

  # Les inn variabelnamna i datafila
  varnamn_fil = scan(adresse_dd,
    fileEncoding = "UTF-8-BOM", what = "character",
    sep = ";", nlines = 1, quiet = TRUE
  )

  # Datafila *kan* ikkje innehalda duplikate kolonnenamn,
  # sidan me då ikkje kan veta kva kolonne eit namn svarar til.
  # Stopp derfor viss me finn duplikate namn.
  dupnamn = duplicated(varnamn_fil)
  if (any(dupnamn)) {
    stop(
      "Datafila har duplikate variabelnamn:\n",
      str_c(varnamn_fil[dupnamn], collapse = "\n")
    )
  }

  # Hent ut første linje frå kodeboka, dvs. den linja som
  # inneheld aktuell informasjon
  # Henter ut den aktuelle delen av kodeboka
  kb_info = kb_akt %>%
    distinct(variabel_id, .keep_all = TRUE)

  # Me skil berre mellom heiltals- og flyttalsvariablar
  # i vår kodebok ved hjelp av «desimalar»-feltet (begge
  # talvariantane har variabeltypen «numerisk»). For å
  # kunna handtera dette riktig (les: strengt) ved
  # innlesing av data, legg me derfor til ein kunstig
  # «numerisk_heiltal»-variabeltype.
  kb_info = kb_info %>%
    mutate(variabeltype = replace(
      variabeltype,
      (variabeltype == "numerisk") & (desimalar == 0),
      "numerisk_heiltal"
    ))

  # Forkortingsbokstavane som read_csv() brukar (fixme: utvide med fleire)
  # fixme: Kategorisk er ein litt vrien variant. *Oftast* er han
  #        tal, men me kan risikera at han er tekst òg. Ei løysing er å
  #        alltid lesa han inn som tekst, men det er ikkje ei *god* løysing.
  #        Når han er koda som tal, er det ofte betre å handsama han som tal.
  #        Det ser betre ut, og det mogleggjer å bruka operatorar som < og >=
  #        (eks. komplikasjonsgrad < 5) (men "10" er som kjent < "2"!).
  #        Og for spørjeskjema er gjerne skåringskodane lagt inn som talkodar,
  #        slik at det er fint om me kan skriva for eksempel sp1 + sp2 + ...
  #        for å få ein sumskår).
  #
  #        Så den *rette* måten å handtera dette på er å lesa inn kategoriske
  #        verdiar som tal dersom dei moglege *verdiane* i kodeboka alle er tal
  #        og som tekst elles.
  #
  #        Det kunne vera aktuelt å dela opp i kategorisk_numerisk og
  #        kategorisk_tekst i vår kanoniske kodebok, men det ville komplisera
  #        annan kode som brukar kodebøkene (eks.kb_fyll()), så det bør me nok
  #        helst ikkje gjera.
  #
  #        Programmeringsmessig blir anbefalt løysing litt komplisert,
  #        men det skal me få til!
  spek_csv_mrs = tibble::tribble(
    ~variabeltype, ~csv_bokstav,
    "kategorisk", "i",
    "tekst", "c",
    "boolsk", "c", # Sjå konvertering nedanfor
    "dato_kl", "c", # Mellombels, jf. https://github.com/tidyverse/readr/issues/642 (fixme til "T" når denne er fiksa)
    "numerisk", "d",
    "numerisk_heiltal", "i"
  )
  spek_innlesing = tibble(variabel_id = varnamn_fil) %>%
    left_join(kb_info, by = "variabel_id") %>%
    left_join(spek_csv_mrs, by = "variabeltype")

  # Har kodeboka variablar av ein type me ikkje har lagt inn støtte for?
  # Dette skal ikkje skje, så avbryt om så er tilfelle.
  nye_typar = setdiff(kb_info$variabeltype, spek_csv_mrs$variabeltype)
  if (length(nye_typar) > 0) {
    stop(
      "Kodeboka har variablar av ein type me ikkje støttar (legg inn støtte!):\n",
      str_c(nye_typar, collapse = "\n")
    )
  }

  # Er det nokon variablar me manglar metadata for (dvs. variablar
  # som finst i datafila men *ikkje* i kodeboka)?
  manglar_metadata = is.na(spek_innlesing$csv_bokstav)
  ukjende_var = spek_innlesing$variabel_id[manglar_metadata]
  if (any(manglar_metadata)) {
    # Vis ikkje åtvaringa viss det berre er snakk om den siste, namnlause, tomme kolonnen som MRS automatisk legg til alle datadumpar
    if (!all(ukjende_var == "")) {
      warning(
        "Manglar metadata for nokre variablar. Dei vert derfor\n",
        "handterte som tekst og variabelnamna får prefikset «mrs_».\n",
        "Problematiske variablar:\n",
        str_c(ukjende_var, collapse = "\n")
      )
    }
    spek_innlesing$csv_bokstav[manglar_metadata] = "c"
    spek_innlesing$variabel_id[manglar_metadata] = str_c("mrs_", spek_innlesing$variabel_id[manglar_metadata])
  }

  # Les inn datasettet
  kol_typar = str_c(spek_innlesing$csv_bokstav, collapse = "")
  lokale_mrs = readr::locale(
    decimal_mark = ",", grouping_mark = "",
    date_format = "%d.%m.%Y", time_format = "%H:%M"
  )
  d = readr::read_delim(adresse_dd,
    delim = ";", quote = "\"", trim_ws = FALSE, na = "",
    col_names = spek_innlesing$variabel_id, col_types = kol_typar, skip = 1, # Hopp over overskriftsrada
    locale = lokale_mrs
  )

  # Fixme: For kategoriske variablar er det ei spesiell koding av manglande
  #        verdiar (typisk tyder -1 manglande verdi). Gjer om desse verdiane
  #        til ekte NA-verdiar. Bruk det som er markert som «manglande = ja»
  #        i kodeboka (på kanonisk form) for å finna ut kva verdiar som
  #        skal tolkast som NA.

  # Gjer om boolske variablar til ekte boolske variablar
  #
  # Fixme: Her må MRS retta opp, slik at me berre treng forhalda oss til
  #        eitt sett verdiar, eks. "False" og "True" (og tom verdi for NA).
  #        Det er stor risiko for feil dersom me må godta 5 ulike verdiar
  #        (og plutseleg tyder gjerne -1 ja i staden for NA for eit register ...).
  #        Oppdater koden til å berre støtta dette.
  #
  #        Eventuelt (viss MRS som vanleg brukar åresvis på å få ting retta
  #        opp for alle registera), gjer det mogleg å spesifisera ved
  #        funksjonskallet kva verdiar som skal tolkast til kva boolske verdiar.
  #        Eks: boolske_verdiar = list(`FALSE` = "0", `TRUE` = "1", `NA` = c("", "-1", NA))
  #
  mrs_boolsk_til_boolsk = function(x) {
    # Sjekk først at det berre er gyldige verdiar

    # Nokre datadumpar har verdiane -1, 0 og 1
    # og nokre har "False" og "True"
    # (og me ventar på at nye artar skal dukka opp ...)
    er_gyldig = (x %in% c("-1", "0", "1", "False", "True")) | is.na(x)
    if (!all(er_gyldig)) {
      stop("Finst ugyldige verdiar i boolske variablar (skal vera '-1', 'False', '1' eller 'True' eller mangla)")
    } else {
      # Usann er koda som "0" eller som "False",
      # mens sann er koda som "1" eller "True",
      # mens manglande verdi er koda som
      # "-1" eller tom verdi.
      x[(x == "-1") | (x == "")] = NA
      (x == "1") | (x == "True") # Testen i er_gyldig() sikrar at alt som ikkje er sant, er usant
    }
  }

  # datadumpen har ikke alle variablene som er nevnt i kodeboka, så vi filtrerer dem bort
  # fixme! disse må være med når vi får dem i datadumpen
  boolske_var = spek_innlesing %>%
    filter(variabeltype == "boolsk") %>%
    pull(variabel_id)
  d = d %>%
    mutate_at(boolske_var, mrs_boolsk_til_boolsk)

  # Gjer om tidsvariablar til ekte tidsvariablar
  # Fixme: Nødvendig pga. https://github.com/tidyverse/readr/issues/642
  #        Fjern når denne feilen er fiksa (rett då òg fixme-en
  #        lenger oppe som også handlar om dette)
  tid_var = spek_innlesing %>%
    filter(variabeltype == "dato_kl") %>%
    pull(variabel_id)
  d = d %>%
    mutate_at(tid_var, readr::parse_datetime, format = "%d.%m.%Y %H:%M")

  # Fila har (ved ein feil) ekstra semikolon på slutten, som fører
  # til ekstra kolonne som har tomt namn (men får prefikset mrs_).
  # Fjern denne kolonnen.
  # Fixme: Få HEMIT til å fiksa problemet i fila
  d$mrs_ = NULL

  # Returner datasettet
  d
}

# Eksempel  -----------------------------------------------------------

# # Les inn eksempeldata
# mappe = "***FJERNA-ADRESSE***"
# filnamn_kb = "Kodebok NorArtritt-fiksa.xlsx"
# ark_kb = "Inklusjonskjema. Skjemaversjon "
# filnamn_dd = "datadumper\\2017-05-18\\DataDump_Inklusjonskjema_2017-05-18.csv"
# adresse_kb = paste0(mappe, filnamn_kb)
# adresse_dd = paste0(mappe, filnamn_dd)
#
# # Les inn (ei fane i) Excel-kodeboka
# kb_mrs = read_excel(adresse_kb, sheet = ark_kb)
# kb_standard = kb_mrs_til_standard(kb_mrs)
#
# # Les inn datadump
# d = les_dd_mrs(adresse_dd, kb_standard)
#
# # Sjå nøyare på eventuelle importproblem
# problems(d)
