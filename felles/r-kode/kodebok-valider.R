# Valider kodebok (som skal vera på vårt standardformat)

# Les inn nødvendige pakkar
library(tidyverse)
library(readxl)
library(rlang)
library(purrr)

# Eksempeldata for testing
mappe = "h:/kvalreg/ablasjonsregisteret/"
kb = read_excel(paste0(mappe, "kodebok-utkast.xlsx"), sheet = 1)

# fixme:
#  - Legg til mange nye testar
#  - Gjer om til ein funksjon, kb_er_gyldig()
#  - Pass på at funksjonen returnerer TRUE/FALSE avhengig av om kodeboka er gyldig eller ei
#    (Gjerne ein god idé med kortslutning av funksjonen, slik at han returnerer
#    etter første åtvaring.)

# Kjed saman tekststrengar og formater enkeltelement med '-teikn rundt seg
lag_liste = function(x) {
  str_c("'", x, "'", collapse = ", ")
}

# Standard kolonnenamn som alle kodebøker skal ha
std_namn = c(
  "skjema_id", "skjemanamn", "kategori", "innleiing", "variabel_id",
  "variabeletikett", "forklaring", "variabeltype", "eining", "unik",
  "obligatorisk", "verdi", "verditekst", "manglande", "desimalar",
  "min", "maks", "min_rimeleg", "maks_rimeleg", "kommentar_rimeleg",
  "utrekningsformel", "logikk", "kommentar"
)

# Sjekk først at alle standardkolonnane er med
# (kodeboka kan ha andre kolonnar òg, men iallfall desse)
kol_manglar = setdiff(std_namn, names(kb))
if (length(kol_manglar) > 0) {
  warning(
    "Kodeboka manglar kolonnar:\n",
    paste0(kol_manglar, sep = "\n")
  )
}


# Sjå på standardkolonnane, men i rekkefølgja dei finst på i kodeboka
kb_namn = names(kb[names(kb) %in% std_namn])

# Sjekk at kolonnane kjem i standard rekkefølgje
# (men merk at me i *denne* testen godtek at
# enkelte kolonnar manglar, sidan me testar det tidlegare).
ind = match(kb_namn, std_namn)
forste_feil = which(diff(ind) < 0)[1]
if (!is.na(forste_feil)) {
  warning(paste0(
    "Feil rekkefølgje på kolonnar. Første feil:\n",
    "Kolonnen ",
    lag_liste(kb_namn[forste_feil]),
    " står *før* ",
    lag_liste(kb_namn[forste_feil + 1]),
    " men skal stå (ein eller annan plass) etter."
  ))
}

# I vidare testar føreset me at kodeboka er på ikkje-glissen form,
# dvs. at skjema_id, variabel_id og sånt er gjentatt nedover.
# Viss ho er ikkje på den forma, ordnar me det sjølv. :)
mogleg_glisne_kol = quos(skjema_id, skjemanamn, kategori, variabel_id, variabeltype)
kb = fill(kb, !!!mogleg_glisne_kol)


# Sjekk at me ikkje har duplikate skjema-ID-ar, skjemanamn eller variabel-ID-ar
# (duplikate kategoriar:
sjekk_dup = function(kb, idkol) {
  idkol = quo_name(enquo(idkol))
  ids = rle(kb[[idkol]])$values
  if (any(duplicated(ids))) {
    warning(
      "Duplikate verdiar i ", lag_liste(idkol), ":\n",
      lag_liste(unique(ids[duplicated(ids)])),
      "\n(Men merk at seinare duplikatar kan vera følgjefeil av første.)"
    )
  }
}
sjekk_dup(kb, skjema_id)
sjekk_dup(kb, skjemanamn)
sjekk_dup(kb, variabel_id) # Skal vera unik, ikkje berre innan skjema

# Sjekk at kvar variabel berre har éin (dvs. unik) variabeltype
d_varid = kb %>%
  nest(-variabel_id)
inkos_vartype = d_varid$data %>%
  map_lgl(~ length(unique(.x$variabeltype)) > 1)
if (any(inkos_vartype)) {
  warning(
    "Inkonsistente variabeltypar for:\n",
    lag_liste(d_varid$variabel_id[inkos_vartype])
  )
}


# Viss kodeboka brukar kategoriar, sjekk at alle skjema
# startar med ei kategorioverskrift
#
# # Sjekk at alle skjema startar me ei kategorioverskrift
# forsterader = kb %>% filter((!is.na(skjema_id)) & is.na(kategori))
# if(not(nrow(forsterader)==0) {
#   warning()
# }
#
# # # Les inn kodeboka
# # adresse_kb = "h:/kvalreg/ablasjonsregisteret/AblaNor_klokeboken.csv_27.07.2017.csv"
# # kb = les_oqr_kb(adresse_kb)
# kb
#
