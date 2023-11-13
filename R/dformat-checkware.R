# Dette er eit R-skript med ein funksjon for lesing av CheckWare-data og kodebok.

# Les inn kodebok og gjer om til standardformat ---------------------------

# Les inn OQR-kodebok på dokumentert format og
# gjer om til vårt standardformat (kanonisk form)

# Inndata:
#   mappe_dd: Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD, for kvart uttak)
#   reg_id:   ID som identifiserer registeret og er prefiks til alle filnamna
#   dato:     Datoen ein skal henta ut kodeboka for (tekststreng eller dato). Kan òg vera NULL, for å henta nyaste kodebok.
#
# Utdata:
#   kodeboka på standardformat (kanonisk form), med variabelnamn gjort om til små bokstavar
#
# Argumenter:
# mappe_dd: Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD, for kvart uttak)
#            Her er det gitt at nyeste kodebok legges i samme mappe som nedhentede datadumper
# dato: hvis man ønsker å hente kodebok fra en spesifikk dato. Hvis ikke hentes dette fra nyeste dato. Default til NULL.
# validering: Om man ønsker å validere kodeboka ja/nei (TRUE/FALSE).
#             Hvis man ønsker å ikke validere kodebok med kb_er_gyldig, kan man sette denne til FALSE. Default er TRUE.


# Roxygen dokumentasjon

#' Konverter Checkware kodebok til standardformat
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Les inn OQR-kodebok på dokumentert format og gjer om til vårt standardformat (kanonisk form).
#'
#' Returnerer kodeboken på standardformat (kanonisk form), med variabelnamn gjort om til små bokstavar.
#'
#' @param mappe_dd Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD,
#' for kvart uttak) Her er det gitt at nyeste kodebok legges i samme mappe som nedhentede datadumper.
#' @param dato Hvis man ønsker å hente kodebok fra en spesifikk dato. Hvis ikke hentes dette fra nyeste dato. Default er NULL.
#' @param valider_kb Om man ønsker å validere kodeboka ja/nei (TRUE/FALSE).
#' Hvis man ønsker å ikke validere kodebok med kb_er_gyldig, kan man sette denne til FALSE. Default er TRUE.
#' @export
les_kb_checkware = function(mappe_dd, dato = NULL, valider_kb = TRUE) {

  # Bruk siste tilgjengelege kodebok dersom ein ikkje har valt dato
  if (is.null(dato)) {
    dato = dir(mappe_dd, pattern = "[0-9]{4}-[0-1]{2}-[0-9]{2}", full.names = FALSE) %>%
      sort() %>%
      last()
  }
  dato = lubridate::as_date(dato) # I tilfelle det var ein tekstreng

  # Adressen til kodeboka
  adresse_kb = paste0(mappe_dd, dato, "/kodebok.xlsx")

  # kodebok-kolonnetyper som skal brukes når man henter inn kodeboka
  # Noen ganger har kodeboka tomme kolonner, og kolonnetypen må defineres på forhånd uansett
  # Kodeboka er laget i excel, og dessverre må disse per i dag defineres manuelt # fixme! automatiser ved automatisert kodebok.
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
  kb = readxl::read_excel(adresse_kb, col_types = kb_koltyper, sheet = 1) %>%
    mutate(desimalar = as.integer(desimalar))


  # Sjekk gyldigheten til kodeboka
  # Mulighet for å hoppe over sjekk hvis man setter valider_kb = FALSE
  if (valider_kb) {
    stopifnot(kb_er_gyldig(kb))
  }

  # gjør om kodeboka til kanonisk form
  kb_kanonisk = kb_til_kanonisk_form(kb)

  # fixme! kb_kanonisk støtter ikke andre kolonner utenom standardkolonnene,
  # derfor left_joiner vi variabel_id_checkware tilbake inn. Fix når kb_til_kanonisk er oppdatert.
  variabel_id_checkware = kb %>%
    fill(skjema_id) %>%
    select(skjema_id, variabel_id, variabel_id_checkware) %>%
    na.omit()
  kb_kanonisk = kb_kanonisk %>%
    left_join(variabel_id_checkware,
      by = c("skjema_id", "variabel_id"),
      relationship = "many-to-one"
    )
  kb_kanonisk
}
# kan teste at funksjonen funker med koden under
# mappe = "***FJERNA-ADRESSE***"
# kb = les_kb_checkware(mappe)

#------------------------------------------------lag datadump checkware------------------------


# Funksjon for å tilrettelegge checkware-data basert på kodebok,
# hvor funksjonen automatisk henter inn kodebok som blir brukt (hvis ønskelig).
# Kodeboka vert brukt til å gje alle variablane rett format
# (tal, tekst, dato, boolske/logiske verdiar osv.) og til å
# sikra at datadumpen er i samsvar med kodeboka.
#
# Som standard treng ein ikkje oppgje kodebok; ho vert automatisk henta inn.
# Men dersom ein skal lesa inn mange skjema, er det lurare å lesa inn
# kodeboka separat først, for at ting skal gå raskare (innlesing og validering
# av kodeboka kan ta litt tid). Det er òg nødvendig å gjera det slik dersom
# ein har kodeboka frå ei anna kjelde eller viss ein vil bruka ei modifisert
# kodebok (generelt farleg!).
#
# Videre ønsker man gjerne å ha kodeboka tilgjengelig mens man jobber, og derfor har
# vi også en separat funksjon (over) for nedhenting av kodebok.
#
# funksjonen sjekker at kodeboka er gyldig, med kodebok_er_gyldig() funksjonen fra kodebok-valider skriptet
# funksjonen sjekker at datadumpen er gyldig, med dd_er_gyldig() funksjonen fra datadump-valider skriptet
# krever pakkene tidyverse, magrittr og readxl
#
# Argumenter:
#   mappe_dd:  Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD, for kvart uttak)
#   skjema_id: ID til skjemaet ein vil henta inn (brukt i filnamnet til datadumpen og i kolonnen «skjema_id» i kodeboka)
#   dato:      Datoen ein skal henta ut kodeboka for (tekststreng eller dato). Kan òg vera NULL, for å henta nyaste kodebok.
#   kb:        Kodebok på kanonisk form. Kan òg vera NULL, og då vert kodeboka automatisk henta inn.
#   valider_dd: Om man ønsker å validere datadumpen ja/nei (TRUE/FALSE).
#             Hvis man ønsker å ikke validere datadump med dd_er_gyldig, kan man sette denne til FALSE. Default er TRUE.
#   valider_kb: Om man ønsker å validere kodeboka ja/nei (TRUE/FALSE).
#             Hvis man ønsker å ikke validere kodebok med kb_er_gyldig, kan man sette denne til FALSE. Default er TRUE.
# Utdata:
#   R-datasett for det aktuelle skjemaet, med variabelnamn gjort om til ønnskede, tilsvarende verdier funnet i kodeboka.
#   (I stedet for Q1, Q2, Q3 osv. som CheckWare ofte oppgir)


# Roxygen dokumentasjon

#' Konverter Checkware-data til format fra kodebok
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Funksjon for å tilrettelegge checkware-data basert på kodebok,
#' hvor funksjonen automatisk henter inn kodebok som blir brukt (hvis ønskelig). \cr
#' Kodeboka vert brukt til å gje alle variablane rett format
#' (tal, tekst, dato, boolske/logiske verdiar osv.) og til å
#' sikra at datadumpen er i samsvar med kodeboka. \cr \cr
#' Som standard treng ein ikkje oppgje kodebok; ho vert automatisk henta inn.
#' Men dersom ein skal lesa inn mange skjema, er det lurare å lesa inn
#' kodeboka separat først, for at ting skal gå raskare (innlesing og validering
#' av kodeboka kan ta litt tid). Det er òg nødvendig å gjera det slik dersom
#' ein har kodeboka frå ei anna kjelde eller viss ein vil bruka ei modifisert
#' kodebok (generelt farleg!).
#'
#' Returnerer et R-datasett for det aktuelle skjemaet, med variabelnamn gjort om til ønnskede, tilsvarende verdier funnet i kodeboka.
#'   (I stedet for Q1, Q2, Q3 osv. som CheckWare ofte oppgir)
#'
#' @param mappe_dd Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD, for kvart uttak).
#' @param skjema_id ID til skjemaet ein vil henta inn (brukt i filnamnet til datadumpen og i kolonnen «skjema_id» i kodeboka).
#' @param kontekst Berre ta med rader med kontekstverdi like disse (standard: T0, T1 eller T2). Sett til NULL for å ta med alle.
#' @param dato Datoen ein skal henta ut kodeboka for (tekststreng eller dato). Kan òg vera NULL, for å henta nyaste kodebok.
#' @param kodebok Kodebok på kanonisk form. Kan òg vera NULL, og då vert kodeboka automatisk henta inn.
#' @param valider_dd Om man ønsker å validere datadumpen ja/nei (TRUE/FALSE). Hvis man ønsker å ikke validere datadump med dd_er_gyldig, kan man sette denne til FALSE. Default er TRUE.
#' @param valider_kb Om man ønsker å validere kodeboka ja/nei (TRUE/FALSE). Hvis man ønsker å ikke validere kodebok med kb_er_gyldig, kan man sette denne til FALSE. Default er TRUE.
#' @export
les_dd_checkware = function(mappe_dd, skjema_id, kontekst = c("T0", "T1", "T2"), dato = NULL, kodebok = NULL, valider_dd = TRUE, valider_kb = TRUE) {

  # Bruk siste tilgjengelege kodebok dersom ein ikkje har valt dato
  if (is.null(dato)) {
    dato = dir(mappe_dd, pattern = "[0-9]{4}-[0-1]{2}-[0-9]{2}", full.names = FALSE) %>%
      sort() %>%
      last()
  }
  dato = lubridate::as_date(dato) # I tilfelle det var ein tekstreng

  # Les inn kodeboka dersom ho ikkje er spesifisert
  # her sjekkes også gyldigheten av kodeboka
  if (is.null(kodebok)) {
    kodebok = les_kb_checkware(mappe_dd, dato = dato)
  }

  # Sjekk gyldigheten til kodeboka
  # Mulighet for å hoppe over sjekk hvis man setter valider_kb = FALSE
  if (valider_kb) {
    stopifnot(kb_er_gyldig(kodebok))
  }

  # Lager objekt med filter på kodebok til å bare innneholde informasjon om aktuelt skjema til datadumpen
  # Alle datadumper kommer med ett skjema + metadata, bortsett fra treatments, som ikke inneholder metadata, bortsett fra r_id.
  if (skjema_id == "treatments") {
    kb_skjema = kodebok %>%
      filter(skjema_id == !!skjema_id)
  } else {
    kb_skjema = kodebok %>%
      filter(skjema_id == "meta" | skjema_id == !!skjema_id)
  }
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
    left_join(spek_csv_checkware,
      by = "variabeltype",
      relationship = "many-to-one"
    )

  # de kategoriske variablene som koder med tekst-verdier skal få character

  # kategoriske variabler skal være integer hvis de er heltall, og character hvis de har koder som ikke er tall (type ICD-10)
  # funksjoner som sjekker om en vector er et heltall, donert av Dr. Hufthammer
  er_heiltal = function(x) {
    isTRUE(all(x == suppressWarnings(as.integer(x))))
  }

  # Funksjon som lager variabel i kodeboka som beskriver om det er heltall eller ikke
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

  # Lager variabel som beskriver om en kategorisk variabel er heltall eller ikke
  kb_skjema_nest = kb_skjema %>%
    group_by(variabel_id) %>%
    nest()
  kb_skjema_nest$data = kb_skjema_nest$data %>%
    map(tekst_eller_heiltall)
  kb_skjema = unnest(kb_skjema_nest, cols = c(data)) %>%
    ungroup()

  # vi bruker case_when for å få inn csv_bokstav for variablene
  # som har variabeltyper avhengig av visse kriterier (per dags dato bare om kategoriske er heltall eller tekst)
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
  adresse_dd = paste0(mappe_dd, "/", dato, "/", filnamn)
  d = readr::stop_for_problems(read_delim(adresse_dd,
    delim = ";", na = "",
    quote = "\"", trim_ws = FALSE, col_types = kol_typar,
    locale = locale(
      decimal_mark = ".", grouping_mark = "",
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
  dato_kl_var = kb_skjema %>%
    filter(variabeltype == "dato_kl") %>%
    distinct(variabel_id) %>%
    pull("variabel_id")
  d = d %>%
    mutate(across(dato_kl_var, ~ readr::parse_datetime(.x, format = "%Y-%m-%d %H:%M:%S")))

  # I CheckWare vert boolske verdiar koda som "1" for sann og NA for usann.
  # Kodar derfor om til ekte boolske verdiar.
  boolsk_var = kb_skjema %>%
    filter(variabeltype == "boolsk") %>%
    distinct(variabel_id) %>%
    pull("variabel_id")
  cw_til_boolsk = function(x) {
    if (skjema_id == "treatments") { # treatments skjema har boolske variabler kodet som true/false,
      # i motsetning til alle andre skjema. treatments er felles for alle registre.
      if_else(x == "false", FALSE, TRUE)
    } else {
      stopifnot(all(x %in% c("1", NA)))
      !is.na(x)
    }
  }
  d = d %>%
    mutate(across(boolsk_var, ~ cw_til_boolsk))

  # Filtrer vekk rader som ikke har riktig kontekst
  # (som standard blir bare T0, T1 og T2 tatt med)
  if (!is.null(kontekst)) {
    d = d %>%
      filter(a_inst_context_label %in% !!kontekst)
  }

  # validerer datadumpen
  # med dd_er_gyldig funksjonen fra datadump-valider-skriptet
  # mulighet for å skru dette av med valider_dd = FALSE
  if (valider_dd) {
    er_gyldig = dd_er_gyldig(d, kb_skjema)
    if (!er_gyldig) {
      print(attr(er_gyldig, "rapport"), n = Inf)
      stop("Datadumpen er ikke gyldig. Se feilene over.")
    }
  }
  # returner dataene
  d
}


# sjekk at funksjonen funker med rehabiliteringsregisteret som eksempel
# mappe = "***FJERNA-ADRESSE***"
# d_barthel = les_dd_checkware(mappe, skjema_id = "barthel")
# d_moca = les_dd_checkware(mappe, skjema = "moca")
# d_mrs = les_dd_checkware(mappe, "mrs")
# d_nihss = les_dd_checkware(mappe, "nihss")
# d_tis = les_dd_checkware(mappe, "tis")
# d_legeinn = les_dd_checkware(mappe, "legeinn")
# d_legeut = les_dd_checkware(mappe, "legeut")
# d_sykinn = les_dd_checkware(mappe, "sykinn")
# d_sykut = les_dd_checkware(mappe, "sykut")
