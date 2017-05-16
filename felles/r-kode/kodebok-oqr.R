# Lesing/tolking av det flotte kodebokformatet til OQR :)


# Oppsett -----------------------------------------------------------------

# Ikkje gjer om tekst til faktorar automatisk
options(stringsAsFactors = FALSE)

# Nødvendige pakkar
library(dplyr) # Datamassering
library(tibble) # Fornuftig datarammestruktur
library(stringr) # Tekstmassering
library(magrittr) # Funksjonar som kan brukast med røyr-operatoren
library(readr) # For innlesing av CSV-filer

# Lag standardisert kodebok -----------------------------------------------


# Hent inn OQR fil som eksempel

# Adressen til kodeboka
kb_adresse = "***FJERNA-ADRESSE***"

# Lag funksjon for å lese inn oqr-kb.

les_oqr_kb = function(kb_adresse) {
  kb = read_delim(kb_adresse,
    delim = ";", quote = "\"",
    col_types = cols(
      skjemanavn = col_character(),
      navn_i_rapporteket = col_character(),
      ledetekst = col_character(),
      obligatorisk = col_character(),
      type = col_character(),
      listeverdier = col_character(),
      listetekst = col_character(),
      normalintervall_start_numerisk = col_integer(),
      normalintervall_slutt_numerisk = col_integer(),
      maksintervall_start_numerisk = col_integer(),
      maksintervall_slutt_numerisk = col_integer(),
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
  )
  kb
}

# kb_orig = les_oqr_kb(kb_adresse)


# Gjer om OQR-kodebok til kodebok på normalform
#
# Inndata:
#   d: Dataramme med OQR-kodebok
kb_oqr_til_standard = function(d) {

  # legg inn de standardiserte navnene på variablene

  kodebok = d %>%
    mutate(
      skjema_id = tabell,
      skjemanamn = skjemanavn,
      oqr_variabel_id_norsk = navn_i_rapporteket,
      oqr_variabel_id_engelsk = variabel_id,
      variabeletikett = ledetekst,
      forklaring = hjelpetekst,
      variabeltype = type,
      verdi = listeverdier,
      verditekst = listetekst,
      desimalar = desimaler,
      min = maksintervall_start_numerisk,
      maks = maksintervall_slutt_numerisk,
      min_rimeleg = normalintervall_start_numerisk,
      maks_rimeleg = normalintervall_slutt_numerisk,
      kommentar = kommentar,
      kategori = NA,
      innleiing = NA,
      eining = NA,
      unik = NA,
      manglande = NA,
      kommentar_rimeleg = NA,
      utrekningsformel = NA,
      logikk = NA,
      obligatorisk = str_to_lower(obligatorisk),
      variabel_id = str_to_lower(variabel_id)
    )

  # Oversikt over variabeltypar i OQR og tilhøyrande standardnamn som me brukar
  vartype_oqr_standard = tribble(
    ~type_oqr, ~type_standard,
    "Listevariabel", "kategorisk",
    "Tekstvariabel", "tekst",
    "Stor tekstvariabel", "tekst",
    "Avkrysningsboks", "boolsk",
    "Datovariabel", "dato",
    "DatovariabelMangler", "dato",
    "Skjult variabel", "tekst",
    "Tallvariabel", "numerisk",
    "TallvariabelMangler", "numerisk",
    "Tidsvariabel", "kl",
    "TidsvariabelMangler", "kl",
    "TIMESTAMP", "dato_kl"
  )

  # test som stopper om kodeboka har en variabeltype vi ikke har tatt høyde for
  nye_vartypar = na.omit(setdiff(kodebok$variabeltype, vartype_oqr_standard$type_oqr))
  if (length(nye_vartypar) > 0) {
    stop("Kodeboka har variabeltypar me ikkje har standardnamn på: ", str_c(nye_vartypar, collapse = ", "))
  }

  # Indeks til rader som startar ein ny variabel
  ind_nyvar = which(!is.na(d$variabel_id))

  # Sleng de standardiserte navnene til variabeltyper på OQR-kodeboka
  kodebok$variabeltype = vartype_oqr_standard$type_standard[
    match(kodebok$variabeltype[ind_nyvar], vartype_oqr_standard$type_oqr)
  ]

  # ta bare med de variablene som vi bruker

  std_namn = c(
    "skjema_id", "skjemanamn", "kategori", "innleiing", "variabel_id",
    "variabeletikett", "forklaring", "variabeltype", "eining", "unik",
    "obligatorisk", "verdi", "verditekst", "manglande", "desimalar",
    "min", "maks", "min_rimeleg", "maks_rimeleg", "kommentar_rimeleg",
    "utrekningsformel", "logikk", "kommentar"
  )
  ekstra_namn = c("oqr_variabel_id_engelsk", "oqr_variabel_id_norsk")

  kodebok = kodebok %>%
    select_(.dots = c(std_namn, ekstra_namn))

  kodebok
}


# Les datadump frå OQR-register -------------------------------------------

# Bruk oppgitt kodebok til å henta inn data frå
# OQR-fil slik at variablane får rett format
# (tal, tekst, dato osv.)
# Argument:
#   adresse: adressa til datafila (med norske/teite variabelnamn)
#        kb: standardisert kodebok

les_dd_oqr = function(adresse, kb) {
  # Les inn variabelnamna i datafila
  varnamn_fil = scan(adresse,
    fileEncoding = "UTF-8", what = "character",
    sep = ";", nlines = 1, quiet = TRUE
  )

  # disse variabelnamna er ikkje dei vi brukar.
  # henter inn namna som vi faktisk brukar
  varnamn = kb$variabel_id[match(varnamn_fil, kb$oqr_variabel_id_norsk)] %>%
    coalesce(varnamn_fil)

  # Hent ut første linje frå kodeboka, dvs. den linja som
  # inneheld aktuell informasjon
  kb_info = kb %>%
    distinct(variabel_id, .keep_all = TRUE)

  # Forkortingsbokstavane som read_csv() brukar (fixme: utvide med fleire)
  spek_csv_oqr = tribble(
    ~variabeltype, ~csv_bokstav,
    "kategorisk", "n",
    "tekst", "c",
    "boolsk", "c", # Sjå konvertering nedanfor
    "dato_kl", "c", # Mellombels, jf. https://github.com/tidyverse/readr/issues/642 (fixme til "T" når denne er fiksa)
    "numerisk", "d",
    "dato", "D",
    "kl", "t"
  )
  spek_innlesing = tibble(variabel_id = varnamn) %>%
    left_join(kb_info, by = "variabel_id") %>%
    left_join(spek_csv_oqr, by = "variabeltype")

  # Er det nokon variablar me manglar metadata for?
  manglar_metadata = is.na(spek_innlesing$csv_bokstav)
  if (any(manglar_metadata)) {
    warning(
      "Manglar metadata for desse variablane (dei vert derfor handterte som tekst):\n",
      str_c(spek_innlesing$variabel_id[manglar_metadata], collapse = "\n")
    )
    spek_innlesing$csv_bokstav[is.na(spek_innlesing$csv_bokstav)] = "c"
  }

  # Les inn datasettet
  kol_typar = str_c(spek_innlesing$csv_bokstav, collapse = "")
  d = read_delim(adresse,
    delim = ";", quote = "\"", trim_ws = FALSE, na = "null",
    col_names = varnamn_fil, col_types = kol_typar, skip = 1, # Hopp over overskriftsrada
    locale = locale(
      decimal_mark = ",", grouping_mark = "",
      date_format = "%Y-%m-%d", time_format = "%H:%M:%S"
    )
  )

  # På grunn av UTF-8-BOM-problem, bruk dei tidlegare innehenta variabelnamna
  # (Endrar i praksis berre namn på den første variabelen.)
  # Fixme: Skal ikkje vera nødvendig i neste versjon av readr (dvs. versjon > 1.0.0):
  # https://github.com/tidyverse/readr/issues/500

  # variabler som finnes i datadump som ikke finnes i kodeboka får prefix "oqr"
  # var som ikke er i kodeboka:
  mangler_i_kb = varnamn[!(varnamn_fil %in% kb$oqr_variabel_id_norsk)]

  # setter på prefix
  # fixme! denne funker bare halvveis
  varnamn = varnamn %>%
    str_replace_all(mangler_i_kb, paste0("oqr_", mangler_i_kb))

  # byttar ut namna med dei med ønskje fra kodeboka
  names(d) = str_to_lower(varnamn)

  # Gjer om boolske variablar til ekte boolske variablar
  oqr_boolsk_til_boolsk = function(x) {
    ifelse(x == "True", TRUE, ifelse(x == "False", FALSE, NA))
  }
  boolsk_ind = which(spek_innlesing$variabeltype == "boolsk")
  d[, boolsk_ind] = lapply(d[, boolsk_ind], oqr_boolsk_til_boolsk)

  # Gjer om tidsvariablar til ekte tidsvariablar
  # Fixme: Nødvendig pga. https://github.com/tidyverse/readr/issues/642
  #        Fjern når denne feilen er fiksa (rett då òg fixme-en
  #        lenger oppe som også handlar om dette)
  dt_ind = which(spek_innlesing$variabeltype == "dato_kl")
  d[, dt_ind] = lapply(d[, dt_ind], parse_datetime, format = "%Y-%m-%d %H:%M:%S")

  # Returner datasettet
  d
}




# Eksempel  -----------------------------------------------------------

# dd_adresse = "***FJERNA-ADRESSE***"

# kb_adresse = "***FJERNA-ADRESSE***"

# # Les inn eksempeldata
# mappe_dd = "***FJERNA-ADRESSE***"
# filnamn_dd = "Datadump_Alle_variabler_numerisk.csv"
# adresse_dd = paste0(mappe_dd, filnamn_dd)

# # Les inn kodeboka
# kb_oqr = les_oqr_kb(kb_adresse)
# kb_standard = kb_oqr_til_standard(kb_oqr)

# # Les inn datadump
# d = les_dd_oqr(adresse_dd, kb_standard)
