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

# henter funksjon for å validere datadump
source("h:/kvalreg/felles/r-kode/datadump-valider.R", encoding = "UTF-8")

# henter funksjon for å validere datadump
source("h:/kvalreg/felles/r-kode/r-til-spss.R", encoding = "UTF-8")

#--------------------------datainnhenting - bruker rehabiliteringsregisteret som utgangspunkt------------------------

lag_checkware_data = function(mappe, skjema) {

  # hent datoer på mappene
  eksport_mapper = dir(mappe, pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}", full.names = FALSE)

  # finn nyeste mappe
  nyeste_dato = eksport_mapper %>%
    sort() %>%
    last()

  # paste0 mappa
  adresse = paste0(mappe, nyeste_dato, "/")

  # innlesing av kodebok
  kb = read_excel(paste0(adresse, "kodebok.xlsx"), sheet = 1)

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

  # fixme! det skal være mulig å bruke kb_er_gyldig() på kodebok på kanonisk form
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

  les_dd_checkware = function(kb, skjema) {

    # Tar quotes rundt skjema
    skjema = quo_name(skjema)

    # filtrer på aktuelt skjema + metadata som finnes i alle skjema
    kb_skjema = kb %>%
      filter(skjema_id == "meta" | skjema_id == skjema)

    # Me skil berre mellom heiltals- og flyttalsvariablar
    # i vår kodebok ved hjelp av «desimalar»-feltet (begge
    # talvariantane har variabeltypen «numerisk»). For å
    # kunna handtera dette riktig (les: strengt) ved
    # innlesing av data, legg me derfor til ein kunstig
    # «numerisk_heiltal»-variabeltype.
    kb_skjema = kb_skjema %>%
      mutate(variabeltype = replace(
        variabeltype,
        (variabeltype == "numerisk") & (desimalar == 0),
        "numerisk_heiltal"
      ))

    # Forkortingsbokstavane som read_csv() brukar (fixme: utvide med fleire)
    spek_csv_checkware = tribble(
      ~variabeltype, ~csv_bokstav,
      "tekst", "c",
      "boolsk", "c", # Sjå konvertering nedanfor
      "dato_kl", "c", # Mellombels, jf. https://github.com/tidyverse/readr/issues/642 (fixme til "T" når denne er fiksa)
      "dato", "c",
      "numerisk", "d",
      "numerisk_heiltal", "i"
    )

    kb_skjema = kb_skjema %>%
      left_join(spek_csv_checkware, by = "variabeltype")

    # de kategoriske variablene som koder med tekst-verdier skal få character

    # kategoriske variabler skal være integer hvis de er heltall, og character hvis de har koder som ikke er tall (type ICD-10)
    # funksjoner som sjekker om en vector er et heltall, donert av Dr. Hufthammer
    er_heiltal = function(x) {
      isTRUE(all(x == suppressWarnings(as.integer(x))))
    }

    # funksjon som lager variabel i kodeboka som beskriver om det er heltall eller ikke
    tekst_eller_heiltall = function(kb) {
      if (er_heiltal(kb$verdi)) {
        kb = kb %>%
          mutate(verdi_type = "heiltal")
      } else {
        kb = kb %>%
          mutate(verdi_type = "tekst")
      }
      kb
    }

    # kjører funksjonen for å lage variabel som beskriver om en kategorisk variabel er heltall eller ikke
    kb_skjema_nest = kb_skjema %>%
      group_by(variabel_id) %>%
      nest()
    kb_skjema_nest$data = kb_skjema_nest$data %>%
      map(tekst_eller_heiltall)
    kb_skjema = unnest(kb_skjema_nest)

    # vi bruker case_when for å få inn csv_bokstav for variablene
    # som har variabeltyper avhengig av visse kriterier
    kb_skjema = kb_skjema %>%
      mutate(csv_bokstav = case_when(
        variabeltype == "kategorisk" & verdi_type == "heiltal" ~ "i",
        variabeltype == "kategorisk" & verdi_type == "tekst" ~ "c",
        TRUE ~ csv_bokstav
      ))

    # henter ut variabelnavn og variabeltype
    var_info = kb_skjema %>%
      distinct(variabel_id, variabel_id_checkware, variabeltype, csv_bokstav)
    kol_typar = str_c(var_info$csv_bokstav, collapse = "")

    # Les inn datasettet
    filnamn = paste0(skjema, ".csv")
    adresse_dd = paste0(adresse, filnamn)
    d = stop_for_problems(read_delim(adresse_dd,
      delim = ";", na = "",
      quote = "\"", trim_ws = FALSE, col_types = kol_typar,
      locale = locale(
        decimal_mark = ",", grouping_mark = "",
        date_format = "%d.%m.%Y", time_format = "%H:%M:%S"
      )
    ))

    # setter på fine variabelnavn
    kolnamn = var_info$variabel_id_checkware %>%
      setNames(var_info$variabel_id)
    d = d %>%
      rename(!!!kolnamn)

    # validerer datadumpen
    # med dd_er_gyldig funksjonen fra datadump-valider-skriptet
    er_gyldig = dd_er_gyldig(d, kb_skjema)

    if (!er_gyldig) {
      print(attr(er_gyldig, "rapport"))
      stop("Datadumpen er ikke gyldig. Se feilene over.")
    }
    d
  }

  # kjør les_dd_checkware på kodebok og skjemanavn for å tilrettelegge dataene basert på kodeboka
  d = les_dd_checkware(kb_kanonisk, skjema)

  # returner dataene
  d
}

# sjekk at funksjonen funker
mappe = "***FJERNA-ADRESSE***"

d_barthel = lag_checkware_data(mappe, skjema = "barthel")
d_moca = lag_checkware_data(mappe, skjema = "moca")
d_mrs = lag_checkware_data(mappe, "mrs")
d_nihss = lag_checkware_data(mappe, "nihss")
d_tis = lag_checkware_data(mappe, "tis")

# sender data som spss til registerets kvalitetsserver

lagre_mappe = "***FJERNA-ADRESSE***"

kb_skjema = kb_kanonisk %>%
  filter(skjema_id == "meta" | skjema_id == "barthel")

d_spss = kb_til_spss(kb_skjema, df = d_barthel)
write_sav(d_spss, paste0(lagre_mappe, "/d_barthel.sav"))
