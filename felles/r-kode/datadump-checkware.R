# Dette skal være eit R-skript med ein funksjon for lesing av CheckWare-data og kodebok.
#
# Innlesingsfunksjonen bør ta inn:
#
# - Ei mappeadresse, der mappa inneheld kodebok og tilhøyrande datadumpfiler.
# - Ein skjema-ID, som tilsvarer skjema-ID-en i kodeboka og filnamnet til datadumpen (men utan .csv).
# - og returnera ein R-tibble (eller ein feil dersom ting ikkje er i orden).
#
# Internt lastar funksjonen inn tilhøyrande kodebok og brukar denne for å lesa tilhøyrande datadumpfil.
# (Internt kan det vera to funksjonar, ein for lesing av kodeboka, jf. kode i h:\kvalreg\rehabiliteringsregisteret
# og ein for lesing av datadumpfila gitt ein kodebok-tibble på kanonisk form).
#
# Funksjonen bør òg sjekka at kodeboka er gyldig (før bruk)
# og at datadumpen er gyldig (etter importering),
# og gje feilmelding (ikkje åtvaring) dersom ikkje.
# Det bør vera mogleg å slå av desse sjekkane, men dei må vera på som standard.

#---------------------------innhenting av pakker og funksjoner--------------------------------

library(tidyverse) # livsnæring
library(magrittr) # pipe-funksjon
library(readxl)

# henter funksjon for å lage kodebok til kanonisk form + kodebok valider
source("h:/kvalreg/felles/r-kode/kodebok-valider.R", encoding = "UTF-8")

#--------------------------datainnhenting - bruker rehabiliteringsregisteret som utgangspunkt------------------------

# Les inn kodebok
mappe = "***FJERNA-ADRESSE***"
kb = read_excel(paste0(mappe, "kodebok-AFMR-register 0.8.xlsx"), sheet = 1)

# I kodeboka til rehabiliteringsregisteret er det flere kolonner som ikke
# er utfylt fordi det ikke er aktuelt.
# disse har ikke klart å få riktig tolkning for variabeltype
# Rettet datatype for nokre (tomme) variablar som vart feiltolka til feil datatype
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
    kategori = NA_character_,
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
  "skjema_id", "skjemanamn", "kategori", "innleiing", "variabel_id_checkware", "variabel_id",
  "variabeletikett", "forklaring", "variabeltype", "eining", "unik",
  "obligatorisk", "verdi", "verditekst", "manglande", "desimalar",
  "min", "maks", "min_rimeleg", "maks_rimeleg", "kommentar_rimeleg",
  "utrekningsformel", "logikk", "kommentar"
)
kb = kb %>%
  select(!!std_namn)

# Sjekk kodeboka
kb_er_gyldig(kb)
warnings()

# gjør om kodeboka til kanonisk form
kb_kanonisk = kb_til_kanonisk_form(kb)

# fixme! kb_kanonisk støtter ikke andre kolonner utenom standardkolonnene,
# derfor left_joiner vi denne inn. Fix når kb_til_kanonisk er oppdatert.

variabel_id_checkware = kb %>%
  select(variabel_id, variabel_id_checkware) %>%
  na.omit()

kb_kanonisk = kb_kanonisk %>%
  left_join(variabel_id_checkware, by = "variabel_id")

# henter inn data
filnamn = "barthel.csv"



d_barthel = read_delim(paste0(mappe, filnamn), delim = ";", na = "")

read_delim(
  adresse,
  delim = ";",
  na = "null",
  locale = locale(date_format = "%d.%m.%Y", decimal_mark = ","),
)


col_names

kol_typar = cols(
  PasientID = col_integer(),
  Fodselsdato = col_date(),
  PasientAlder = col_number(),
  PasientKjonn = col_character()
)

col_types = kol_typar
