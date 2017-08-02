# Lesing/tolking av det elendige kodebokformatet til MRS :(


# Oppsett -----------------------------------------------------------------

# Ikkje gjer om tekst til faktorar automatisk
options(stringsAsFactors = FALSE)

# Nødvendige pakkar
library(tidyverse) # Ymse nyttige pakkar
library(readxl) # Lesing av Excel-filer
library(stringr) # Tekstmassering
library(magrittr) # Funksjonar som kan brukast med røyr-operatoren
library(rlang) #

# Lag standardisert kodebok -----------------------------------------------

# Gjer om MRS-kodebok til kodebok på normalform
#
# Inndata:
#   d: Dataramme med MRS-kodebok
kb_mrs_til_standard = function(d) {
  # Indeks til rader som startar ein ny variabel
  ind_nyvar = which(!is.na(d$Feltnavn))
  nvars = length(ind_nyvar) # Talet på variablar

  # Kor mange kodar kvar variabel har
  # (Merk at siste variabel ikkje vert
  #  avløyst av ein ny variabel, og må
  #  derfor handterast spesielt.)
  var_nverd = diff(ind_nyvar) - 1
  var_nverd[nvars] = nrow(d) - ind_nyvar[nvars] # Sistevariabel

  # Oversikt over variabeltypar i MRS og tilhøyrande standardnamn som me brukar
  vartype_mrs_standard = tribble(
    ~type_mrs, ~type_standard,
    "Enum", "kategorisk",
    "Text", "tekst",
    "Avkrysning", "boolsk",
    "Dato/Tid", "dato_kl",
    "Id (Guid)", "tekst",
    "Numerisk (heltall)", "numerisk",
    "Numerisk (flyttall)", "numerisk"
  )
  nye_vartypar = na.omit(setdiff(d$Felttype, vartype_mrs_standard$type_mrs))
  if (length(nye_vartypar) > 0) {
    stop("Kodeboka har variabeltypar me ikkje har standardnamn på: ", str_c(nye_vartypar, collapse = ", "))
  }

  # Lag dataramme med i utgangspunktet éi rad for kvar variabel
  # Variabelnamna brukt i datadumpen finn me:
  #   – Ikkje i kolonnen Datadumpnavn (det hadde vore for enkelt og logisk)
  #   – Ikkje i kolonnen Feltnavn (ditto)
  #   – Ikkje direkte i kolonnen Variabelnavn (ditto)
  # Men:
  #   Etter det *siste* punktumet (om dette eksisterer) i kolonnen Variabelnavn
  #   i den første rada i eit sett med rader som omhandlar ein variabel, og der
  #   settet begynner med ein ikkje-tom verdi i kolonnen som heiter Feltnavn.
  #
  # Å finna dei andre verdiane (for eksempel kodar og kodetekst) gjer ein på
  # tilsvarande vanskelege måtar
  kodebok_utg = tibble(
    # dd_id = d$DataDumpnavn[ind_nyvar],     # Datadumpnamn (vert ikkje brukt til noko)
    variabel_id = d$Variabelnavn[ind_nyvar] %>% str_replace(".*\\.", ""),
    variabeletikett = d$Feltnavn[ind_nyvar], # Berre forklaring for *enkelte* variablar, men er det beste me har …
    variabeltype = vartype_mrs_standard$type_standard[
      match(d$Felttype[ind_nyvar], vartype_mrs_standard$type_mrs)
    ],
    obligatorisk = str_to_lower(d$Obligatorisk[ind_nyvar]),
    # skjema_id = d$Skjema[ind_nyvar], # Ventar spent på at denne skal dukka opp (førespurnad er send)
    verdi = NA_integer_, # Føreset førbels at MRS-kodane alltid er tal (gjer om til tekst om dette ikkje stemmer)
    verdi_tekst = NA_character_
  )

  # Kor mange gongar kvar variabel skal gjentakast i
  # den nye kodeboka, dvs. kor mange rader han skal oppta
  reps = pmax(var_nverd, 1)

  # Utvid kodeboka slik at enum-variablane får fleire rader
  kodebok = kodebok_utg[rep(1:nvars, times = reps), ]

  # Hent ut kodane og tilhøyrande tekst til alle Enum-variablane
  enums = d %>%
    filter(is.na(Felttype)) %>%
    extract2("Variabelnavn") %>%
    str_split_fixed(" = ", n = 2)

  # Legg kodane inn i den nye kodeboka,
  # med rett format (heiltal for kodar
  # og tekst for kodetekst), og på rett plass
  enum_ind = (kodebok$variabeltype == "kategorisk")
  kodebok$verdi[enum_ind] = enums[, 1] %>%
    as.numeric() # Kodar
  kodebok$verdi_tekst[enum_ind] = enums[, 2] # Tilhøyrande tekst

  # Nokre verditekstar tyder at verdien ikkje er registrert,
  # og me markerer det i kodeboka
  kodebok = kodebok %>%
    mutate(manglande = ifelse(verdi_tekst %in% c("---", "Velg verdi", "Ikke valgt"), "ja", "nei"))

  # Returner standardisert kodebok
  kodebok
}



# Les datadump frå MRS-register -------------------------------------------

# Bruk oppgitt kodebok til å henta inn data frå
# MRS-fil slik at variablane får rett format
# (tal, tekst, dato osv.)
# Argument:
#   adresse: adressa til datafila (med norske/teite variabelnamn)
#        kb: standardisert kodebok
les_dd_mrs = function(adresse, kb) {
  # Les inn variabelnamna i datafila
  varnamn_fil = scan(adresse,
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
  kb_info = kb %>%
    distinct(variabel_id, .keep_all = TRUE)

  # Forkortingsbokstavane som read_csv() brukar (fixme: utvide med fleire)
  spek_csv_mrs = tribble(
    ~variabeltype, ~csv_bokstav,
    "kategorisk", "n",
    "tekst", "c",
    "boolsk", "c", # Sjå konvertering nedanfor
    "dato_kl", "c", # Mellombels, jf. https://github.com/tidyverse/readr/issues/642 (fixme til "T" når denne er fiksa)
    "numerisk", "d"
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
  # Fixme: Vurder å legga til eit argument i funksjonen
  #        for å gøyma åtvaringar for standardvariablar
  #        (dvs. dei som finst i alle OQR-datadumpar)
  manglar_metadata = is.na(spek_innlesing$csv_bokstav)
  ukjende_var = spek_innlesing$variabel_id[manglar_metadata]
  if (any(manglar_metadata)) {
    warning(
      "Manglar metadata for nokre variablar. Dei vert derfor\n",
      "handterte som tekst og variabelnamna får prefikset «mrs_».\n",
      "Problematiske variablar:\n",
      str_c(ukjende_var, collapse = "\n")
    )
    spek_innlesing$csv_bokstav[manglar_metadata] = "c"
    spek_innlesing$variabel_id[manglar_metadata] = str_c("mrs_", spek_innlesing$variabel_id[manglar_metadata])
  }

  # Les inn datasettet
  kol_typar = str_c(spek_innlesing$csv_bokstav, collapse = "")
  d = read_delim(adresse,
    delim = ";", quote = "\"", trim_ws = FALSE, na = "",
    col_names = spek_innlesing$variabel_id, col_types = kol_typar, skip = 1, # Hopp over overskriftsrada
    locale = locale(
      decimal_mark = ",", grouping_mark = "",
      date_format = "%d.%m.%Y", time_format = "%H:%M:%S"
    )
  )


  # Gjer om boolske variablar til ekte boolske variablar
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
    mutate_at(tid_var, parse_datetime, format = "%d.%m.%Y %H:%M:%S")

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
