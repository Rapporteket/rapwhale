# Fletting av ymse uttrekksfiler frå Gjenopplivingsregisteret
#
# Laga 2016-11-18 av Karl Ove Hufthammer <karl.ove.hufthammer@helse-bergen.no>


# Oppsett -----------------------------------------------------------------

# Last inn nødvendige pakkar
library(tidyverse)
library(stringr)
library(readxl)

# Aktuell dato me skal sjå på filer frå
kjeldefil = "2016-12-31\\Rapport_Utstein_2016.xlsx"

# Mapper for datafilene
mappe_nokkel = "***FJERNA-ADRESSE***"
adresse_kjelde = str_c(mappe_nokkel, kjeldefil)
# mappe_lev = str_c("***FJERNA-ADRESSE***", dato)
adresse_vaskefil = str_c(mappe_nokkel, "vaskefil\\prehosp-koplingsfil.csv")
adresse_loadfil = str_c(mappe_nokkel, "vaskefil\\load.txt")



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
  moglege_fnr[fnr_gyldig(moglege_fnr)]
}



# Innlesing av filer ------------------------------------------------------

# Les inn fil med prehospitale data
d_amis = read_excel(adresse_kjelde)
d_amis$kjeldefil = kjeldefil

# Les inn den eksisterande vaskefila
# (Lagar funksjon sidan me skal gjera
# dette fleire gongar)
les_vaskefil = function(adresse) {
  read_csv2(adresse,
    col_types = cols(
      kjeldefil = col_character(),
      amisnr = col_character(),
      dato_stans = col_character(),
      fnr_orig = col_character(),
      fnr_vaska = col_character()
    )
  )
}
d_vask = les_vaskefil(adresse_vaskefil)



# Hent ut prehospitale data -----------------------------------------------

# Fixme: Køyr gjennom fleire datafiler (førebels nøyer me oss med den siste,
#        for eksempelets skuld)

# Mange datovariablar å velja mellom (og ingen av dei er alltid fylde ut)
# Startar med å sjå på denne (fixme)
d_amis$`Dato / tid henv. AMK `

# Hent ut dei aktuelle variablane og gjev dei meir fornuftige namn
d_amis2 = d_amis %>%
  select(
    kjeldefil,
    `Amisnummer `,
    `Dato / tid henv. AMK `,
    `F.nr`
  ) %>%
  rename(
    amisnr = `Amisnummer `,
    dato_stans = `Dato / tid henv. AMK `,
    fnr_orig = `F.nr`
  )

# Sjekk at det ikkje finst dupliserte/manglande/tomme AMIS-nummer
if (anyDuplicated(d_amis2$amisnr) || any(is.na(d_amis2$amisnr)) || any(d_amis2$amisnr == "")) {
  stop("Oppdaga dupliserte eller manglande AMIS-nummer")
}

# Formater tidspunkt som tekst, på ønskt format
d_amis2 = d_amis2 %>%
  mutate(dato_stans = format(dato_stans, tz = "UTC"))



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
d_amis2 = d_amis2 %>%
  mutate(
    fnr_rep = str_replace_all(fnr_orig, "[^0-9]", ""),
    fnr_vaska = ifelse(fnr_er_gyldig(fnr_rep), fnr_rep, NA_character_)
  ) %>%
  select(-fnr_rep) %>%
  arrange(desc(fnr_vaska), desc(nchar(fnr_orig)))
# Formaterer strengar som "" i staden for NA sidan
# det vert finast / lettast å redigera i utfila

# Sjå berre på dei hjertestansane som er *nye*,
# dvs. dei der me ikkje har AMIS-nummeret frå før
d_amis2_nye = d_amis2 %>%
  filter(!(amisnr %in% d_vask$amisnr))

# Legg til dei nye AMIS-nummera
d_vask_oppdatert = d_vask %>%
  bind_rows(d_amis2_nye)

# Ta reservekopi av den gamle vaskefila
filnamn_reskopi = str_c("reskopi-", format(Sys.time(), format = "%Y-%m-%d-%H-%M-%S"), ".csv")
mappe_reskopi = str_c(mappe_nokkel, "vaskefil\\reskopi\\")
adresse_reskopi = str_c(mappe_reskopi, filnamn_reskopi)
file.copy(adresse_vaskefil, adresse_reskopi)

# Lagra den nye vaskefila
# (Brukar write_csv2() i staden for write_*() for å
# få hermeteikn rundt alle verdiane. Ser finast ut når
# ein redigerer fila manuelt.)
# Gjer om manglande verdiar (NA) til tomme tekststrengar,
# sidan det vert lettare å redigera manuelt
d_vask_oppdatert = d_vask_oppdatert %>%
  replace_na(replace = list(dato_stans = "", fnr_orig = "", fnr_vaska = ""))
write.csv2(d_vask_oppdatert, adresse_vaskefil, na = "", row.names = FALSE)



# Lag Load-fil ------------------------------------------------------------

# Les inn vaskefila på nytt
# (sidan ho kan vera manuelt oppdatert med nye
# fødselsnummer sidan sist)
d_vask = les_vaskefil(adresse_vaskefil)

# Stopp viss det manglar stansdatoar
dato_feil = is.na(d_vask$dato_stans)
if (any(dato_feil)) {
  stop(
    "Finst oppføringar utan stansdato. Gjeld desse AMIS-nummera:\n",
    str_c(d_vask$amisnr[dato_feil], collapse = "\n")
  )
}

# Sjå berre på oppføringar som har *både* stansdato og fødselsnummer
d_load = d_vask %>%
  filter((!is.na(dato_stans)) & (!is.na(fnr_vaska)))

# Sjekk at alle fødselsnummera er gyldige
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
  select(dato_stans, fnr_vaska, amisnr) %>%
  mutate(dato_stans = format(as.POSIXct(dato_stans, tz = "UTC"),
    format = "%d.%m.%Y %H:%M", tz = "UTC"
  ))
# Må ha semikolon *etter* siste felt i kvar rad,
# og det ordnar me ved å legga til ei ekstra
# (tom) kolonne til slutt
d_load$ekstra = ""
write_delim(d_load, adresse_loadfil, delim = ";", col_names = FALSE)
