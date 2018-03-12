# Sjekk kodebok for Rehabiliteringsregisteret

library(tidyverse)
library(readxl)
kb = read_excel("h:/kvalreg/rehabiliteringsregisteret/kodebok-AFMR-register 0.7.xlsx", sheet = 1)

# Rett datatype for nokre (tomme) variablar som vart feiltolka til feil datatype
kb = kb %>%
  mutate(
    desimalar = as.integer(desimalar),
    eining = as.character(eining),
    kategori = skjemanamn,
    maks = as.double(maks),
  )

# Legg til standardkolonnar som manglar
kb = kb %>%
  mutate(
    innleiing = NA_character_,
    unik = "nei",
    manglande = NA_character_,
    min_rimeleg = NA_real_,
    maks_rimeleg = NA_real_,
    kommentar_rimeleg = NA_character_,
    utrekningsformel = NA_character_,
    logikk = NA_character_,
    kommentar = NA_character_
  )

# Fiks rekkjefølgja på variablane
std_namn = c(
  "skjema_id", "skjemanamn", "kategori", "innleiing", "variabel_id",
  "variabeletikett", "forklaring", "variabeltype", "eining", "unik",
  "obligatorisk", "verdi", "verditekst", "manglande", "desimalar",
  "min", "maks", "min_rimeleg", "maks_rimeleg", "kommentar_rimeleg",
  "utrekningsformel", "logikk", "kommentar"
)
kb = kb %>%
  select(!!std_namn)

# Sjekk kodeboka
source("h:/kvalreg/felles/r-kode/kodebok-valider.R", encoding = "utf-8")
kb_er_gyldig(kb)
warnings()
