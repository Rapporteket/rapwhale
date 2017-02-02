# Fletting av ymse uttrekksfiler frå Gjenopplivingsregisteret
#
# Laga 2016-11-18 av Karl Ove Hufthammer <karl.ove.hufthammer@helse-bergen.no>


# Oppsett -----------------------------------------------------------------

# Last inn nødvendige pakkar
library(tidyverse)
library(stringr)
library(readxl)

# Aktuell dato me skal sjå på filer frå
dato = "2016-09-06"

# Mapper for datafilene
mappe_nokkel = paste0("***FJERNA-ADRESSE***", dato)
mappe_lev = paste0("***FJERNA-ADRESSE***", dato)



# Innlesing av filer ------------------------------------------------------

# Les inn basisfila
# read_excel() klarer ikkje heilt gjetta seg til variabeltypane sjølv,
# so me må oppgje dei manuelt
filnamn_basisfil = "Rapport_Utstein_Jan-Aug.xlsx"
d_basis = read_excel(paste0(mappe_nokkel, "\\", filnamn_basisfil),
  col_types = c(
    "numeric", "numeric", "text", "numeric", "text",
    "text", "date", "date", "text", "text",
    "text", "date", "text", "text", "text",
    "text", "text", "text", "text", "text",
    "date", "text", "text", "text", "numeric",
    "text", "text", "text", "text", "text",
    "text", "text", "date", "text", "text",
    "date", "text", "date", "text", "date", "text",
    "text", "text", "text", "date", "text",
    "text", "text", "text", "text"
  )
)

# Les inn nøkkelfila for prehospitale data
# Denne brukar me berre for å få tak i variabelen «PERSONID_FIKTIV»
filnamn_ph_nokkel = "PREHOSP_NOEKKEL_HBE.xls"
d_ph_nokkel = read_excel(paste0(mappe_nokkel, "\\", filnamn_ph_nokkel))



# Test på om fødselsnummer er gyldige ----------------------------------------------------

# Basert på skildringa på Wikipedia:
# https://no.wikipedia.org/wiki/F%C3%B8dselsnummer
fnr_gyldig = function(x) {
  # Talet på fødselsnummer
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
  # DD er 01-31 (ekte fødselsnummer) eller 41-61 (D-nummer)
  # MM er 01-12
  # ÅÅ er 00-99
  # III = 000-999
  # KK = 00-99

  # Regulært uttrykk som lukar ut dei grovaste feila
  re_siffer = "^[0-6][0-9][0-1][0-9][0-9]{7}$"
  ok[ok] = str_detect(x[ok], "^[0-9]{11}$")


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
  # Viss første siffer er 4, 5 eller 6, kan det vera
  # eit D-nummer. Då trekker me frå 3.
  dato_tekst = paste0(str_sub(x[ok], 1, 4), "20", str_sub(x[ok], 5, 6)) # Legg til hundreårsinfo
  fsiffer = as.numeric(str_sub(dato_tekst, 1, 1)) # Første siffer
  dnummer = fsiffer %in% 4:6 # Er det snakk om D-nummer?
  fsiffer = ifelse(dnummer, fsiffer - 3, fsiffer) # Trekk ev. frå 3 (for D-nummer)
  dato_tekst = paste0(fsiffer, str_sub(dato_tekst, 2, 8)) # Byt ut første siffer med modifsert førstesiffer

  # Sjekk om det kan vera ein gyldig dato
  dato = as.Date(dato_tekst, format = "%d%m%Y")
  ok[ok][is.na(dato)] = FALSE


  ##### TEST 4: Er kontrollsiffera korrekte? #####
  # Til slutt den viktige, store og kraftige testen
  # basert på korleis fødselsnummer er designa:
  # Me sjekkar kontrollsiffera, dei to siste siffera.

  # Del opp i siffer
  if (any(ok)) {
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


# Kor mange fødselsnummer er ugyldige totalt sett
sum(!fnr_gyldig(d_basis$F.nr))

# Sjå dei ugyldige fødselsnummera
x = d_basis$F.nr
x[!fnr_gyldig(x)]



# Fletting av filer -------------------------------------------------------

# Legg til prehospitale data *for* dei pasientane som har det
fnr_ok = (d_ph_nokkel$FOEDSELSNR %in% d_basis$F.nr)
if (!all(fnr_ok)) {
  stop("Manglar oppføring i basisfil for pasient(ar) i prehospital nøkkelfil.")
} else {
  d = d_basis %>%
    left_join(d_ph_nokkel, by = c("F.nr" = "FOEDSELSNR"))
}

# OBS/fixme: Men er dette så lurt, då? Kva med personar med fleire omsorgsepisode-ID-ar?

# Sjå på datafila slik ho ser ut no
d
