# Fletting av ymse uttrekksfiler frå Gjenopplivingsregisteret
#
# Laga av Karl Ove Hufthammer <karl.ove.hufthammer@helse-bergen.no>


# Oppsett -----------------------------------------------------------------

# Last inn nødvendige pakkar
library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)
library(purrr)

# Mapper for datafilene
mappe_nokkel = "***FJERNA-ADRESSE***"
mappe_prehosp = str_c(mappe_nokkel, "Prehospitalt\\")
mappe_reskopi = str_c(mappe_nokkel, "Leveranse\\koplingsfil\\reskopi\\")
# mappe_lev = str_c("***FJERNA-ADRESSE***", dato)
adresse_vaskefil = str_c(mappe_nokkel, "Leveranse\\koplingsfil\\prehosp-koplingsfil.csv")
adresse_loadfil = str_c(mappe_nokkel, "Leveranse\\koplingsfil\\load.txt")


# Dei ulike namna som har blitt brukt på dei aktulle variablane
# (AMIS-nummer, fødselsnummer og dato) i ulike utgåver av AMIS-eksporten
namn_amis = c(
  "amis", "Amisnummer.", "AMIS eller AMK nummer",
  "AMIS", "AMISNUMMER", "Amisnummer", "AMISNUMMER ", "AMIS ",
  "Amisnummer "
)
namn_fnr = c("Fødselsnummer", "fødselsnummer", "F.nr", "foedselsnr")
namn_dato = c(
  "DATO/KLOKKEN ", "HENVENDELSE MOTTATT AMK ", "AMBULANSEPERSONELL FREMME PÅ BESTEMMELSESSTED ",
  "DATO ", "DATO/KLOKKEN HVIS JA ", "Dato / tid for hendelse ",
  "Amb. alarmert om stans ", "Dato / tid henv. AMK ", "Dato/tid HLR startet ",
  "Tidspunkt HLR avsluttet ", "Dato/tid ankomst sykehus ", "Dato/tid vedvarende ROSC ",
  "Dato / tid amb. fremme på bestemmelsessted ", "Dato/tid aktiv nedkjøling ",
  "Dato/tid startet ", "dato", "datoklokken", "henvendelsemottattamk", "ambulansepersonellfremmepÅbes",
  "datoklokkenhvisja", "ag", "ak", "aq", "stansdato", "stansdato_d",
  "morstid", "utskrevet_dato", "sign_dato", "prosedyredato"
)



# Funksjon for test av fødselsnummer --------------------------------------

# Testar om fødselsnummer (inkludert D-nummer og H-nummer) er gyldige
#
# Basert på skildringane på Wikipedia
# https://no.wikipedia.org/wiki/F%C3%B8dselsnummer
# og
# https://lovas.info/2013/12/01/identitetsnummer-i-norge/
fnr_er_gyldig = function(x) {
  # Sjekk først at det er snakk om tekststreng,
  # ikkje tal (som ville mista førstesifferet om dette er 0)
  if (!is.character(x)) {
    stop("Fødselsnummer må vera ein tekstvektor")
  }

  # Talet på fødselsnummer me sjekkar
  n = length(x)

  # Rekn først alle fødselsnummera som gyldige
  # (og så markerer me dei som ugyldige etter
  # kvart som dei ikkje passerer testar)
  ok = !logical(n)
  names(ok) = x # Hugselapp for å sjå kva nummer me testar ...


  ##### TEST 1: Har me data i det heile tatt? #####

  # Dei som ikkje har nokon verdiar er iallfall ugyldige
  ok[is.na(x)] = FALSE


  ##### TEST 2: Har me (berre) 11 fornuftige siffer? #####
  # Sjekk at nummeret har 11 siffer og at siffera er nokolunde OK
  #
  # Fødselsnummer er bygde opp som
  #
  #   DDMMÅÅIIIKK (dag, månad, år, individnummer, kontrollsiffer)
  #
  # DD er 01-31 (ekte fødselsnummer) eller 41-71 (D-nummer)
  # MM er 01-12 (ekte fødselsnummer) eller 41-52 (H-nummer)
  # ÅÅ er 00-99
  # III = 000-999
  # KK = 00-99

  # Regulært uttrykk som lukar ut dei grovaste feila
  re_siffer = "^[0-7][0-9][0145][0-9][0-9]{7}$"
  ok[ok] = str_detect(x[ok], regex(re_siffer))


  ##### TEST 3: Ser dei først seks siffera ut som datoar? #####
  # Me vil sjekka om datodelen (dei første 6 siffera)
  # er ein gyldig dato, men får problem med at me
  # ikkje veit kva hundreår det er snakk om.
  # Er berre problem for 29. februar, og skotår er
  # dei same for alle alle hundreåra (eks. var både
  # 1804, 1904 og 2004 skotår), med unntak av år --00,
  # som var skotår i 2000 men ikkje i 1900 eller 1800
  # (eller andre hundreår som ikkje er delbare på 400).
  #
  # Latar derfor som alle år var på 2000-talet, slik
  # at me ikkje feilaktig påstår 29. februar ikkje
  # eksisterte.
  #
  # I teorien kunne me brukt individnummera
  # til å gjetta hundreår, men reglane her endrar seg
  # etter kvart som me går tom for fødselsnummer ... :(
  #
  # Viss første siffer er 4, 5, 6 eller 7, kan det vera
  # eit D-nummer. Då trekker me 4 frå sifferet for å laga fødselsdato.
  # Viss tredje siffer er 4 eller 5, kan det vera
  # eit H-nummer. Då trekker me 4 frå sifferet for å laga fødselsdato.
  dato_tekst = str_c(str_sub(x[ok], 1, 4), "20", str_sub(x[ok], 5, 6)) # Legg til hundreårsinfo
  fs_dag = as.numeric(str_sub(dato_tekst, 1, 1)) # Første siffer i dagverdien
  dnummer = fs_dag %in% 4:7 # Er det snakk om D-nummer?
  fs_mnd = as.numeric(str_sub(dato_tekst, 3, 3)) # Første siffer i månadsverdien
  hnummer = fs_mnd %in% 4:5 # Er det snakk om H-nummer?
  # Rekn ut den faktiske fødselsdatoen dersom
  # det er snakk om D-nummer eller H-nummer
  # (ved å trekka 4 frå høvesvis første dag-
  # eller månadssiffer)
  dato_dmod = str_c(fs_dag - 4, str_sub(dato_tekst, 2, 8))
  dato_hmod = str_c(str_sub(dato_tekst, 1, 2), fs_mnd - 4, str_sub(dato_tekst, 4, 8))
  # Eit nummer kan berre anten vera fødselsnummer,
  # D-nummer eller H-nummer, ikkje fleire samtidig
  # Bruk den utrekna datoen basert på kva type nummer det er.
  dato_tekst = ifelse(dnummer, dato_dmod, ifelse(hnummer, dato_hmod, dato_tekst))

  # Sjekk om det kan vera ein gyldig dato
  dato = as.Date(dato_tekst, format = "%d%m%Y")
  ok[ok][is.na(dato)] = FALSE


  ##### TEST 4: Er kontrollsiffera korrekte? #####
  # Til slutt den viktige, store og kraftige testen
  # basert på korleis fødselsnummer er designa:
  # Me sjekkar kontrollsiffera, dei to siste siffera.
  if (any(ok)) {
    # Del fødselsnummera opp i siffer
    x2 = x[ok]
    siffer = str_split_fixed(x2, "", n = 11) %>%
      t()

    # Sidan as.numeric() mistar matrisedimensjonane,
    # lat oss ta vare på dei manuelt
    di = dim(siffer)
    siffer = as.numeric(siffer)
    dim(siffer) = di

    # Koeffisientar for utrekning
    koef1 = c(3, 7, 6, 1, 8, 9, 4, 5, 2)
    koef2 = c(5, 4, 3, 2, 7, 6, 5, 4, 3, 2)

    # Rekn ut kva kontrollsiffera *skulle* vera
    # Første kontrollsiffer
    k1 = 11 - (colSums(koef1 * siffer[1:9, , drop = FALSE]) %% 11)
    k1 = ifelse(k1 == 11, 0, k1)

    # Andre kontrollsiffer
    k2 = 11 - (colSums(koef2 * rbind(siffer[1:9, , drop = FALSE], k1)) %% 11)
    k2 = ifelse(k2 == 11, 0, k2)

    # Er dei utrekna kontrollsiffera lik dei oppgjevne?
    ok[ok] = (k1 == siffer[10, ]) & (k2 == siffer[11, ])
  }

  # Alle testane er utførte. Returner info om fødselsnummeret
  # er gyldig.
  ok
}


# Viss eit fødselsnummer er ugyldig, føreslå
# liknande fødselsnummer som *er* gyldige.
# Basert på at me byter ut eitt (vilkårleg) siffer
# eller byter om to nabosiffer.
#
# OBS! *Ikkje* bruk resultata av funksjonen direkte.
# Sjekk dei oppgjevne fødselsnummera i DIPS eller
# andre kjelder for å kontrollera at dei viser
# til rett person.
fnr_foresla = function(x) {
  stopifnot(length(x) == 1 && is.character(x) && nchar(x) == 11)

  # Bytt ut einskildsiffer med eit (vilkårleg) anna
  moglege_fnr_1 = str_c(
    str_sub(x, 1, 0:10),
    rep(0:9, each = 11),
    str_sub(x, 2:12, 11)
  )

  # Byt om på to etterfølgjande siffer
  x_mat = str_split_fixed(x, "", 11) %>%
    matrix(ncol = 11, nrow = 10, byrow = TRUE)
  for (nr in 1:10) {
    x_mat[nr, nr:(nr + 1)] = x_mat[nr, (nr + 1):nr]
  }
  moglege_fnr_2 = apply(x_mat, 1, str_c, collapse = "")

  # Returner dei kandidatnummera som er gyldige
  moglege_fnr = unique(c(moglege_fnr_1, moglege_fnr_2))
  moglege_fnr[fnr_er_gyldig(moglege_fnr)]
}



# Innlesing av filer ------------------------------------------------------

# Les inn den eksisterande vaskefila
# (Lagar funksjon sidan me skal gjera
# dette fleire gongar)
les_vaskefil = function(adresse) {
  read_csv2(adresse,
    col_types = cols(
      kjeldefil = col_character(),
      amisnr = col_character(),
      dato = col_datetime(),
      fnr_orig = col_character(),
      fnr_vaska = col_character()
    ), trim_ws = FALSE
  )
}
d_vask = les_vaskefil(adresse_vaskefil)



# Hent ut prehospitale data -----------------------------------------------

# Mapper og adresser å eksportera
eksport_mapper = dir(mappe_prehosp, pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}", full.names = TRUE)
eksport_filadresser = list.files(eksport_mapper, ".*\\.xlsx?", recursive = FALSE, full.names = TRUE)

# Les inn AMIS-datafil frå valt adresse og
# gje ut dataramme med dei aktuelle variablane
les_amisdata = function(adresse_kjelde) {
  # Filnamn pluss mappa fila ligg i, men utan heile filstien
  filnamn_kjelde = adresse_kjelde %>%
    str_replace(fixed(mappe_prehosp), "")

  # Les inn fil med prehospitale data
  d_amis = read_excel(adresse_kjelde, guess_max = 10^6, trim_ws = FALSE)

  # Excel lagrar datoar som tal på ein horologisk svært rar måte (sjå ?as.Date)
  # Denne konvererer tala til ekte datoar
  tolk_excel_datotal = function(x) {
    as_datetime(as.Date(as.numeric(x), origin = as.Date("1899-12-30")), tz = "UTC")
  }

  # Ein del av AMIS-datasetta har *veldig* rare tidsformat,
  # på forma «2015-01-04 2015-01-04 03:10:00». Tolk desse òg.
  tolk_amis_rartid = function(x) {
    as.POSIXct(str_extract(x, "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"), tz = "UTC")
  } # Merk at as_datetime() ikkje gjev rett svar her; sjå https://github.com/hadley/lubridate/issues/527

  # Funksjon for å tolka ei tekstkolonne som inneheld ein kombinasjon
  # av tal (som tyder dagar frå 30. desember 1899) og dobbeltdatotidformat
  tolk_tid_tekstkol = function(x) {
    # Er naiv og tenker at viss teksten ser ut som eit tal (dvs. har berre siffer),
    # er det eit tal som representerer ein dato, og elles er det eit tekstfelt
    # som inneheld eit tidspunkt (dato + klokkeslett) ein eller annan plass
    er_datotal = function(x) {
      str_detect(x, "^[0-9]+$")
    }
    datotid = suppressWarnings(if_else(er_datotal(x), tolk_excel_datotal(x), tolk_amis_rartid(x)))
    datotid
  }

  # Mange datovariablar å velja mellom (og ingen av dei er alltid fylde ut)
  # Me ser på alle og vel den nyaste datoen. Hentar først ut alle datovariablar
  # (merk at me kan ha fleire kolonnar med same namn).
  d_amis_dato = d_amis[which(names(d_amis) %in% namn_dato)]
  er_tekst = d_amis_dato %>%
    map_lgl(is.character)
  er_tal = d_amis_dato %>%
    map_lgl(is.numeric)
  d_amis_dato[er_tekst] = lapply(d_amis_dato[er_tekst], tolk_tid_tekstkol)
  d_amis_dato[er_tal] = lapply(d_amis_dato[er_tal], tolk_excel_datotal)

  # Antar vidare at alle datovariablane er ekte tidspunktvariablar
  stopifnot(all(d_amis_dato %>% map_lgl(is.POSIXct)))
  max_na_ok = function(x) {
    if (all(is.na(x))) {
      NA
    } else {
      max(x, na.rm = TRUE)
    }
  } # Som max(), men unngå åtvaring viss alle verdiane er NA
  res_dato = d_amis_dato %>%
    apply(1, max_na_ok) %>%
    as.POSIXct(tz = "UTC")

  # Ser so på AMIS-nummer
  d_amis_amisnr = d_amis[which(names(d_amis) %in% namn_amis)]
  stopifnot(ncol(d_amis_amisnr) > 0)
  res_amisnr = as.character(d_amis_amisnr[[1]]) # Bruk første kolonne (i tilfelle det er fleire)

  # Ser so på fødselsnummer
  d_amis_fnr = d_amis[which(names(d_amis) %in% namn_fnr)]
  stopifnot(ncol(d_amis_fnr) > 0)
  res_fnr = d_amis_fnr[[1]] # Bruk første kolonne (i tilfelle det er fleire)

  # Lagar dataramme med dei relevante variablane
  res = tibble(
    kjeldefil = filnamn_kjelde,
    amisnr = res_amisnr,
    dato = res_dato,
    fnr_orig = res_fnr
  )

  # Sorter etter dato
  res = res %>%
    arrange(dato)

  # Sjekk at det ikkje finst dupliserte/manglande/tomme AMIS-nummer
  if (anyDuplicated(res$amisnr) || any(is.na(res$amisnr)) || any(res$amisnr == "")) {
    stop("Oppdaga dupliserte eller manglande AMIS-nummer")
  }

  # Returner resultatet
  res
}

# Les AMIS-data frå alle Excel-filene
d_amis = eksport_filadresser %>%
  map_df(les_amisdata)

# Fjern eventuelle pasientar som er med i fleire nummer
d_amis = d_amis %>%
  distinct(amisnr, .keep_all = TRUE)

# Nokre pasientar går igjen fleire gongar
# (pluss at ein periode ser ut til å mangla pasientar heilt)
plot(d_amis$dato)
d_amis2 = d_amis %>%
  filter(!is.na(fnr_orig))
d_amis2[duplicated(d_amis2$fnr_orig), ]



# Legg nye data til vaskefila ---------------------------------------------

# Kopier over fødselsnummer om dei er gyldige.
#
# Gjer først eit enkelt forsøk på reparasjon av fødselsnummer
# ved å fjerna alt som ikkje er tal (fjernar spesielt
# mellomrom på starten/slutten av verdien, noko som er ein
# typisk feil).
#
# Legg dei ugyldige fødselsnummera på slutten av fila,
# sortert på ein oversiktleg måte.
d_amis = d_amis %>%
  mutate(
    fnr_rep = str_replace_all(fnr_orig, "[^0-9]", ""),
    fnr_vaska = ifelse(fnr_er_gyldig(fnr_rep), fnr_rep, NA_character_)
  ) %>%
  select(-fnr_rep) %>%
  arrange(kjeldefil, desc(fnr_vaska), desc(nchar(fnr_orig)))
# Formaterer strengar som "" i staden for NA sidan
# det vert finast / lettast å redigera i utfila

# Sjå berre på dei hjertestansane som er *nye*,
# dvs. dei der me ikkje har AMIS-nummeret frå før
d_amis_nye = d_amis %>%
  filter(!(amisnr %in% d_vask$amisnr))

# Legg til dei nye AMIS-nummera
d_vask_oppdatert = d_vask %>%
  bind_rows(d_amis_nye)

# Ta reservekopi av den gamle vaskefila
filnamn_reskopi = str_c("reskopi-", format(Sys.time(), format = "%Y-%m-%d-%H-%M-%S"), ".csv")
adresse_reskopi = str_c(mappe_reskopi, filnamn_reskopi)
file.copy(adresse_vaskefil, adresse_reskopi)

# Lagra den nye vaskefila
# (Brukar write_csv2() i staden for write_*() for å
# få hermeteikn rundt alle tekstverdiane. Ser finast ut når
# ein redigerer fila manuelt.)
# Gjer om manglande verdiar (NA) til tomme tekststrengar,
# sidan det vert lettare å redigera manuelt
d_vask_oppdatert = d_vask_oppdatert %>%
  replace_na(replace = list(fnr_orig = "", fnr_vaska = ""))
write.csv2(d_vask_oppdatert, adresse_vaskefil, na = "", row.names = FALSE)



# Lag Load-fil ------------------------------------------------------------

# Les inn vaskefila på nytt
# (sidan ho kan vera manuelt oppdatert med nye
# fødselsnummer sidan sist)
d_vask = les_vaskefil(adresse_vaskefil)

# Manglande AMIS-nummer *skal* ikkje skje,
# men tar ein sjekk på det likevel. Sjekkar
# i same slengen at ingen AMIS-nummer er
# for lange (maks 32 teikn).
if (any(is.na(d_vask$amisnr) | nchar(d_vask$amisnr) > 32)) {
  stop("Finst oppføring utan AMIS-nummer eller med AMIS nummer > 32 teikn langt.")
}

# Sjekk at det ikkje finst dupliserte AMIS-nummer
# (skal heller ikkje kunna skje)
amis_dup = duplicated(d_vask$amisnr)
if (any(amis_dup)) {
  stop(
    "Finst dupliserte AMIS-nummer:\n",
    str_c(str_c('"', d_vask$amisnr[amis_dup], '"'), collapse = "\n")
  )
}

# Stopp viss det manglar stansdatoar
dato_feil = is.na(d_vask$dato)
if (any(dato_feil)) {
  stop(
    "Finst oppføringar utan stansdato. Gjeld desse AMIS-nummera:\n",
    str_c(d_vask$amisnr[dato_feil], collapse = "\n")
  )
}

# Sjå berre på oppføringar som har vaska fødselsnummer
d_load = d_vask %>%
  filter((fnr_vaska != "") & (!is.na(fnr_vaska)))

# Sjekk at alle dei vaska, ikkje-tomme fødselsnummera er gyldige
fnr_ok = fnr_er_gyldig(d_load$fnr_vaska)
if (any(!fnr_ok)) {
  stop(
    "Finst ugyldige fødselsnummer i «fnr_vaska»-feltet:\n",
    str_c(str_c('"', d_load$fnr_vaska[!fnr_ok], '"'), collapse = "\n")
  )
}

# Lagra resultatet som CSV-fil på nøyaktig det formatet IKT vil ha
# (Brukar UTC som tidssone for å unngå potensielle problem med
# klokkeslett som ikkje i eksisterer i norsk tid på grunn av
# overgang til sommartid (som sjølvsagt impliserer feil i
# kjeldedata …).)
d_load = d_load %>%
  select(dato, fnr_vaska, amisnr) %>%
  mutate(dato = format(dato,
    format = "%d.%m.%Y %H:%M", tz = "UTC"
  ))
# Må ha semikolon *etter* siste felt i kvar rad,
# og det ordnar me ved å legga til ei ekstra
# (tom) kolonne til slutt
d_load$ekstra = ""
write_delim(d_load, adresse_loadfil, delim = ";", col_names = FALSE)
