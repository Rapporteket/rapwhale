# Sjekk kodebok for Rehabiliteringsregisteret

library(tidyverse)
library(readxl)
kb = read_excel("h:/kvalreg/rehabiliteringsregisteret/kodebok-AFMR-register 0.5.xlsx")

# Rett datatype og legg til manglade kolonnar
kb = kb %>%
  mutate(
    desimaler = as.integer(desimaler),
    kategori = skjemanamn,
    maksverdi = as.double(maksverdi),
  )

# Rett namn på nokre kolonnar
kb = kb %>%
  rename(
    desimalar = desimaler,
    min = minverdi,
    maks = maksverdi,
    eining = enhet
  )

# Datovariablar har fått 'eining' UTC. Nyttig informasjon,
# men datovariablar skal ikkje ha einingar (dei er berre datoar!).
# Fjernar derfor dette:
kb$eining[which(kb$eining == "UTC" & kb$variabeltype == "dato_kl")] = NA

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

# Fiks namn på tidspunktvariabeltypar
kb = kb %>%
  mutate(
    variabeltype = ifelse(variabeltype == "dato_klokkeslett", "dato_kl", variabeltype)
  )

# Sjekk kodeboka
source("h:/kvalreg/felles/r-kode/kodebok-valider.R", encoding = "utf-8")
kb_er_gyldig(kb)
warnings()
