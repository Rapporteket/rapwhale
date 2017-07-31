# Valider kodebok (som skal vera på vårt standardformat)

# Les inn nødvendige pakkar
library(tidyverse)
library(stringr)
library(readxl)
library(rlang)
library(purrr)

# Gjer kodeboka om til kanonisk form, dvs. slik at
# implisitte verdiar er fylde ut.
kb_til_kanonisk_form = function(kb) {
  # Avgrupper (i tilfelle dataramma alt er gruppert,
  # noko som kan føra til problem nedanfor
  kb = ungroup(kb)

  # Gjer kodeboka om til ikkje-glissen form,
  # dvs. at skjema_id, variabel_id og sånt er gjentatt nedover.
  mogleg_glisne_kol = quos(skjema_id, variabel_id)
  kb = kb %>%
    fill(!!!mogleg_glisne_kol)

  # Nokre andre kolonnar må òg gjerast om til ikkje-glissen form,
  # men no berre innanfor variabel-ID. For eksempel skal «forklaring»
  # gjentakast nedanfor, men skal ikkje kryssa variabelgrenser
  # (noko som kunne skje viss me brukte metoden over, sidan «forklaring»
  # òg skal kunna stå tom).
  mogleg_glisne_kol = quos(variabeletikett, forklaring, variabeltype, unik, obligatorisk)
  kb = kb %>%
    mutate(radnr = 1:n()) %>%
    group_by(variabel_id) %>% # Endrar rekkjefølgja på radene
    fill(!!!mogleg_glisne_kol) %>%
    ungroup() %>%
    arrange(radnr) %>% # Gjenopprett radrekkefølgja
    select(-radnr)

  # Tilsvarande men no innanfor skjema_id
  mogleg_glisne_kol = quos(skjemanamn, kategori)
  kb = kb %>%
    mutate(radnr = 1:n()) %>%
    group_by(skjema_id) %>% # Endrar rekkjefølgja på radene
    fill(!!!mogleg_glisne_kol) %>%
    ungroup() %>%
    arrange(radnr) %>% # Gjenopprett radrekkefølgja
    select(-radnr)

  # Fyll ut implisitte obligatoriskverdiar og unikverdiar
  # til «nei». «ja*» tyder obligatorisk berre under visse
  # vilkår, så det skal òg reknast som «nei».
  kb$obligatorisk[is.na(kb$obligatorisk) | kb$obligatorisk == "ja*"] = "nei"
  kb$unik[is.na(kb$unik) | kb$unik == "ja*"] = "nei"

  # Fyll ut implisitte verdiar for «manglande»
  kb$manglande[is.na(kb$manglande)] = "nei"

  # Returner kodeboka på kanonisk form
  kb
}

#

# Eksempeldata for testing
mappe = "h:/kvalreg/ablasjonsregisteret/"
kb = read_excel(paste0(mappe, "kodebok-utkast.xlsx"), sheet = 1)
# write.csv2("h:/kvalreg/ablasjonsregisteret/abla-kanonisk.csv")

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

# Ser vidare berre på standardkolonnane
kb = kb[std_namn]

# Sjekk at variabelformatet (tal, tekst, dato osv.) er rett
format_std = tribble(
  ~kol_namn, ~kol_klasse_std,
  "skjema_id", "character",
  "skjemanamn", "character",
  "kategori", "character",
  "innleiing", "character",
  "variabel_id", "character",
  "variabeletikett", "character",
  "forklaring", "character",
  "variabeltype", "character",
  "eining", "character",
  "unik", "character",
  "obligatorisk", "character",
  "verdi", "numeric",
  "verditekst", "character",
  "manglande", "character",
  "desimalar", "numeric",
  "min", "numeric",
  "maks", "numeric",
  "min_rimeleg", "numeric",
  "maks_rimeleg", "numeric",
  "kommentar_rimeleg", "character",
  "utrekningsformel", "character",
  "logikk", "character",
  "kommentar", "character"
)
format_kb = tibble(kol_namn = names(kb), kol_klasse = map_chr(kb, ~ class(.x)[1]))
format = left_join(format_std, format_kb, by = "kol_namn")
format_feil = format %>%
  filter(kol_klasse_std != kol_klasse)
if (nrow(format_feil) > 0) {
  warning(
    "Feil format på kolonnar:\n",
    format_feil %>% as.data.frame() %>% capture.output() %>% paste0(sep = "\n")
  )
}

# I vidare testar føreset me at kodeboka er på ikkje-glissen form,
# dvs. at skjema_id, variabel_id og sånt er gjentatt nedover.
# Viss ho er ikkje på den forma, ordnar me det sjølv. :)
kb = kb_til_kanonisk_form(kb)

# Sjekk at me ikkje har duplikate skjema-ID-ar, skjemanamn eller variabel-ID-ar,
# dvs. at alle unike verdiar kjem samanhengande nedover, utan nokre hòl
# (eks. «xxxyy» er OK, men «xxyyx» er det ikkje).
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

# Sjekk at valt variabel berre har éin verdi innanfor kvar gruppe
sjekk_ikkjevar = function(df, gruppe, varid) {
  gruppe_tekst = quo_name(enquo(gruppe))
  varid_tekst = quo_name(enquo(varid))
  nest_cols = setdiff(names(df), gruppe_tekst)
  df_grupper = df %>%
    nest_(key_col = "data", nest_cols = nest_cols) # fixme: Byt til quasi-quotation, dvs. «-!!gruppe» når det er støtta i dplyr

  ikkjeunike = df_grupper$data %>%
    map_lgl(~ length(unique(.x[[varid_tekst]])) > 1)

  if (any(ikkjeunike)) {
    warning(
      "Varierande/inkonsistente '", varid_tekst, "'-verdiar for desse '", gruppe_tekst, "'-verdiane:\n",
      lag_liste(df_grupper[[gruppe_tekst]][ikkjeunike])
    )
  }
}


# Sjekk at innanfor skjema er skjemanamn unikt
sjekk_ikkjevar(kb, skjema_id, skjemanamn)

# Sjekk at kvar variabel berre har éin (dvs. unik)
# variabeltype, variabeletikett osv.
sjekk_ikkjevar(kb, variabel_id, variabeltype)
sjekk_ikkjevar(kb, variabel_id, variabeletikett)
sjekk_ikkjevar(kb, variabel_id, forklaring)
sjekk_ikkjevar(kb, variabel_id, unik)
sjekk_ikkjevar(kb, variabel_id, obligatorisk)
sjekk_ikkjevar(kb, variabel_id, kategori) # Variablar kan ikkje kryssa kategori- eller skjemagrenser
sjekk_ikkjevar(kb, variabel_id, skjema_id)

# Sjekk at alle verdiar for kategoriske variablar er unike og ingen er NA
kb_kat = kb %>%
  filter(variabeltype == "kategorisk")
kb_kat_nest = kb_kat %>%
  nest(-variabel_id)
verdi_ok = kb_kat_nest$data %>%
  map_lgl(~ (!any(duplicated(.x$verdi) | is.na(.x$verdi))))
if (any(!verdi_ok)) {
  warning(
    "Variablar har dupliserte 'verdi'-ar eller NA som 'verdi':\n",
    lag_liste(kb_kat_nest$variabel_id[!verdi_ok])
  )
}

# *Viss* kodeboka brukar kategoriar (det er frivillig å bruka,
# men viss ein brukar det, skal alle skjema ha minst éin kategori),
# sjekk at alle skjema startar med ei kategorioverskrift
# (me sjekkar tidlegare oppe at desse er unike innanfor variabel_id)
if (any(!is.na(kb$kategori))) {
  kb_skjema = kb %>%
    nest(-skjema_id)
  har_kat = kb_skjema$data %>%
    map_lgl(~ (!is.na(.x$kategori[1])) & (.x$kategori[1] != ""))
  if (any(!har_kat)) {
    warning(
      "Nokre skjema manglar kategorioverskrift (i førsterader):\n",
      lag_liste(kb_skjema$skjema_id[!har_kat])
    )
  }
}

# Forslag til fleire testar:
# - variabeltype kan berre ta eit gitt sett verdiar (som ikkje inkluderer NA)
# - eining kan ikkje vera tom ("") (men kan vera NA)
# - unik må vera ja/nei (og ikkje NA)
# - obligatorisk må vera ja/nei (og ikkje NA)
# - manglande må vera ja/nei (og ikkje NA)
# - desimalar må vera eit heiltal (ikkje som datatype, men som at det ikkje har nokon brøkdel, dvs. x %% 1 == 0)
# - desimalar må vera >= 0
# - min må vera < maks (dersom begge finst)
# - min_rimeleg må vera < maks_rimeleg (dersom begge finst)
# - min må vera <= min_rimeleg (dersom begge finst)
# - maks må vera >= maks_rimeleg (dersom begge finst)
# - viss kommentar_rimeleg er fylt ut, må min_rimeleg *eller* maks_rimeleg vera fylt ut
# ... sikker mange andre testar me kan laga
