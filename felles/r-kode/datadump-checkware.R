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

# Me skil berre mellom heiltals- og flyttalsvariablar
# i vår kodebok ved hjelp av «desimalar»-feltet (begge
# talvariantane har variabeltypen «numerisk»). For å
# kunna handtera dette riktig (les: strengt) ved
# innlesing av data, legg me derfor til ein kunstig
# «numerisk_heiltal»-variabeltype.
kb_kanonisk = kb_kanonisk %>%
  mutate(variabeltype = replace(
    variabeltype,
    (variabeltype == "numerisk") & (desimalar == 0),
    "numerisk_heiltal"
  ))

# henter ut variabelnavn for metadata som er i hvert skjema +
# for aktuelt skjema, og variabeltype
var_info = kb_kanonisk %>%
  filter(skjema_id == "meta" | skjema_id == "barthel") %>%
  distinct(variabel_id, variabel_id_checkware, variabeltype)

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
spek_csv_checkware = tribble(
  ~variabeltype, ~csv_bokstav,
  "kategorisk", "i",
  "tekst", "c",
  "boolsk", "c", # Sjå konvertering nedanfor
  "dato_kl", "c", # Mellombels, jf. https://github.com/tidyverse/readr/issues/642 (fixme til "T" når denne er fiksa)
  "dato", "c",
  "numerisk", "d",
  "numerisk_heiltal", "i"
)

spek_innlesing = var_info %>%
  left_join(spek_csv_checkware, by = "variabeltype")

# Les inn datasettet
filnamn = "barthel.csv"
adresse = paste0(mappe, filnamn)

kol_typar = str_c(spek_innlesing$csv_bokstav, collapse = "")
d_barthel = read_delim(adresse,
  delim = ";", na = "",
  col_names = spek_innlesing$variabel_id,
  quote = "\"", trim_ws = FALSE, col_types = kol_typar,
  locale = locale(
    decimal_mark = ",", grouping_mark = "",
    date_format = "%d.%m.%Y", time_format = "%H:%M:%S"
  )
)

# Datafila *kan* ikkje innehalda duplikate kolonnenamn,
# sidan me då ikkje kan veta kva kolonne eit namn svarar til.
# Stopp derfor viss me finn duplikate namn.
dupnamn = duplicated(names(d_barthel))
if (any(dupnamn)) {
  stop(
    "Datafila har duplikate variabelnamn:\n",
    str_c(d_barthel[dupnamn], collapse = "\n")
  )
}

# setter variabelnavn
kolnamn = var_info$variabel_id_checkware %>%
  setNames(var_info$variabel_id)
d_barthel = d_barthel %>%
  rename(!!!kolnamn)
