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



# Elementær validering ----------------------------------------------------

# Kor mange manglar fødselsnummer heilt
sum(is.na(d_basis$F.nr))

# Kor mange har ikkje 11-sifra fødselsnummer
fnr_siffer = d_basis$F.nr %>%
  str_detect("^[0-9]{11}$")
fnr_siffer_el_na = is.na(d_basis$F.nr) | !fnr_siffer
sum(fnr_siffer_el_na)

# Sjekk om fødselsnummer er gyldige
sjekk_enkelt_fnr = function(x) {
  # Sjekk først om teksten *kan* vera eit fødselsnummer
  ok = (!is.na(x)) & str_detect(x, "^[0-9]{11}$")

  # Viss teksten har 11 siffer, sjekk om dei to siste
  # siffera (kontrollsiffera) er rette
  if (ok) {
    siffer = as.numeric(str_split(x, "")[[1]])

    # Koeffisientar for utrekning
    koef1 = c(3, 7, 6, 1, 8, 9, 4, 5, 2)
    koef2 = c(5, 4, 3, 2, 7, 6, 5, 4, 3, 2)

    # Reknt ut kva kontrollsiffera *skulle* vera
    k1 = 11 - sum(siffer[1:9] * koef1) %% 11
    if (k1 == 11) {
      k1 = 0
    }
    k2 = 11 - sum(c(siffer[1:9], k1) * koef2) %% 11
    if (k2 == 11) {
      k2 = 0
    }

    # Sjekk om kontrollsiffera faktisk var slik
    ok = (k1 == siffer[[10]]) & (k2 == siffer[[11]])
  }
  ok
}
sjekk_fnr = Vectorize(sjekk_enkelt_fnr) # Litt uelegant, men fungerer fint

# Kor mange fødselsnummer er ugyldige totalt sett
sum(!sjekk_fnr(d_basis$F.nr))

# Sjå dei ugyldige fødselsnummera
x = d_basis$F.nr
x[!sjekk_fnr(x)]



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
