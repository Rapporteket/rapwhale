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
library(lubridate)

# henter funksjon for å lage kodebok til kanonisk form + kodebok valider
source("h:/kvalreg/felles/r-kode/kodebok-valider.R", encoding = "UTF-8")

# henter funksjon for å validere datadump
source("h:/kvalreg/felles/r-kode/datadump-valider.R", encoding = "UTF-8")

#--------------------------les kodebok checkware funksjon-----------------------------------------------

# funksjon for å hente ut checkware-kodebøker.
# trenger adressen til kodeboka.
# kjører kodebok_er_gyldig() for å teste at kodeboka er gyldig.
# gjør kodeboka til kanonisk form
les_kb_checkware = function(adresse_kb) {

  # kodebok-kolonnetyper som skal brukes når man henter inn kodeboka
  # Noen ganger har kodeboka tomme kolonner, og kolonnetypen må defineres på forhånd uansett
  # Kodeboka er laget i excel, og dessverre må disse per i dag defineres manuelt # fixme! automatiser ved automatisert kodeboka.
  # Excel har heller ikke så mange, presise variabeltyper
  # I standardrekkefølgen på kolonner til Fagsenterets standard kodebokformat skal de 15 første kolonnene
  # skjema_id, skjema_namn, kategori, innleiing, varibel_id_checkware, variabel_id, variabeletikett, forklaring,
  # eining, obligatorisk, unik, verdi, verditekst og manglande være "text",
  # de 5 neste, desimalar, min, maks, min_rimeleg og maks_rimeleg "numeric"
  # og de 4 siste, kommentar_rimeleg, utrekningsformel, logikk, kommentar "text".
  kb_koltyper = c(rep("text", 15), rep("numeric", 5), rep("text", 4))

  # read_excel har ingen mulighet for å skille "numeric" i forhold til "integer" i col_types spesifikasjonen.
  # vi endrer desimalar manuelt til å være dette for å kunne komme gjennom kb_er_gyldig.
  # standard kodebok er at første ark inneholder informasjon. Vi har ikke satt noe standard for ark-navn.
  kb = read_excel(adresse_kb, col_types = kb_koltyper, sheet = 1) %>%
    mutate(desimalar = as.integer(desimalar))

  # Sjekk gyldigheten til kodeboka
  kb_er_gyldig(kb)

  # gjør om kodeboka til kanonisk form
  kb_kanonisk = kb_til_kanonisk_form(kb)

  # fixme! kb_kanonisk støtter ikke andre kolonner utenom standardkolonnene,
  # derfor left_joiner vi variabel_id_checkware tilbake inn. Fix når kb_til_kanonisk er oppdatert.
  variabel_id_checkware = kb %>%
    select(variabel_id, variabel_id_checkware) %>%
    na.omit()
  kb_kanonisk = kb_kanonisk %>%
    left_join(variabel_id_checkware, by = "variabel_id")
  kb_kanonisk
}

#------------------------------------------------lag datadump checkware------------------------

# funksjon for å tilrettelegge checkware-data basert på kodebok,
# hvor funksjonen automatisk henter inn kodebok som blir brukt.
# funksjonen trenger:
# - Ei mappeadresse, der mappa inneheld kodebok og tilhøyrande datadumpfiler.
# - Ein skjema-ID, som tilsvarer skjema-ID-en i kodeboka og filnamnet til datadumpen (men utan .csv).
# funksjonen sjekker at kodeboka er gyldig, med kodebok_er_gyldig() funksjonen fra kodebok-valider skriptet
# funksjonen sjekker at datadumpen er gyldig, med dd_er_gyldig() funksjonen fra datadump-valider skriptet
# krever pakkene tidyverse, magrittr og readxl
hent_checkware_data = function(mappe, skjema_id) {

  # hent datoer på mappene
  eksport_mapper = dir(mappe, pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}", full.names = FALSE)

  # finn nyeste mappe
  nyeste_dato = eksport_mapper %>%
    sort() %>%
    last()

  # paste0 mappa
  adresse = paste0(mappe, nyeste_dato, "/")
  filnamn_kb = "kodebok.xlsx"
  adresse_kb = paste0(adresse, filnamn_kb) # adressen til kodeboka

  # henter ut kodebok ved hjelp av tidligere utarbeidet kb-funksjon
  kb = les_kb_checkware(adresse_kb)

  # funksjon som henter inn checkware-data ved hjelp av en kodebok
  # funksjonen trenger en kodebok på kanonisk format og et skjema-navn (f.eks "barthel") og adressen til datadump
  # og gir dataene fine navn basert på variabel_id i kodeboka,
  # ved hjelp av variabel_id_checkware som identifiserer variablene i datadumpene
  # datadumpen får også variabeltypene som er definert i kodeboka
  les_dd_checkware = function(adresse, kb, skjema_id) {

    # filtrer på aktuelt skjema + metadata som finnes i alle skjema
    kb_skjema = kb %>%
      filter(skjema_id == "meta" | skjema_id == !!skjema_id)

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
      "dato_kl", "c", # Mellombels, jf. https://github.com/tidyverse/readr/issues/642 (!fixme til "T" når denne er fiksa)
      "dato", "D",
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
    filnamn = paste0(skjema_id, ".csv")
    adresse_dd = paste0(adresse, filnamn)
    d = stop_for_problems(read_delim(adresse_dd,
      delim = ";", na = "",
      quote = "\"", trim_ws = FALSE, col_types = kol_typar,
      locale = locale(
        decimal_mark = ",", grouping_mark = "",
        date_format = "%Y-%m-%d", time_format = "%H:%M:%S"
      )
    ))

    # setter på fine variabelnavn
    kolnamn = var_info$variabel_id_checkware %>%
      setNames(var_info$variabel_id)
    d = d %>%
      rename(!!!kolnamn)

    # siden datetime blir hentet inn som character
    # fikser vi disse til å være datetime her
    # (jf. https://github.com/tidyverse/readr/issues/642 (!fixme til "T" når denne er fiksa))
    dato_kl_var = kb %>%
      filter(variabeltype == "dato_kl") %>%
      distinct(variabel_id) %>%
      pull("variabel_id")
    d = d %>%
      mutate_at(dato_kl_var, parse_datetime, format = "%Y-%m-%d %H:%M:%S")

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
  d = les_dd_checkware(adresse, kb, skjema_id)

  # returner dataene
  d
}

# sjekk at funksjonen funker med rehabiliteringsregisteret som eksempel
mappe = "***FJERNA-ADRESSE***"
#
# d_barthel = hent_checkware_data(mappe, skjema = "barthel")
# d_moca = hent_checkware_data(mappe, skjema = "moca")
# d_mrs = hent_checkware_data(mappe, "mrs")
# d_nihss = hent_checkware_data(mappe, "nihss")
# d_tis = hent_checkware_data(mappe, "tis")
