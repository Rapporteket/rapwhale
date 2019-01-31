# Lesing/tolking av det flotte kodebokformatet til OQR :)
# og av OQR-datadumpar.


# Oppsett -----------------------------------------------------------------

# Ikkje gjer om tekst til faktorar automatisk
options(stringsAsFactors = FALSE)

# Nødvendige pakkar
library(tidyverse) # Ymse standardpakkar
library(lubridate) # Datohandtering
library(stringr) # Tekstmassering
library(magrittr) # Funksjonar som kan brukast med røyr-operatoren
library(readr) # For innlesing av CSV-filer


# Les inn kodebok og gjer om til standardformat ---------------------------

# Les inn OQR-kodebok på dokumentert format og
# gjer om til vårt standardformat (kanonisk form)
#
# Inndata:
#   mappe_dd: Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD, for kvart uttak)
#   reg_id:   ID som identifiserer registeret og er prefiks til alle filnamna
#   dato:     Datoen ein skal henta ut kodeboka for (tekststreng eller dato). Kan òg vera NULL, for å henta nyaste kodebok.
#
# Utdata:
#   kodeboka på standardformat (kanonisk form), med variabelnamn gjort om til små bokstavar
#
les_kb_oqr = function(mappe_dd, reg_id, dato = NULL) { # fixme: Validering av kodebok?

  # Bruk siste tilgjengelege kodebok dersom ein ikkje har valt dato
  if (is.null(dato)) {
    dato = dir(mappe_dd, pattern = "[0-9]{4}-[0-1]{2}-[0-9]{2}", full.names = FALSE) %>%
      sort() %>%
      last()
  }
  dato = as_date(dato) # I tilfelle det var ein tekstreng

  # Les inn kodeboka
  adresse_kb = paste0(
    mappe_dd, "\\", dato, "\\",
    reg_id, "_klokeboken.csv_", format(dato, "%d.%m.%Y"), ".csv"
  )
  kodebok_oqr_format = stop_for_problems(read_delim(
    adresse_kb,
    delim = ";", quote = "\"",
    col_types = cols(
      skjemanavn = col_character(),
      navn_i_rapporteket = col_character(),
      ledetekst = col_character(),
      obligatorisk = col_character(),
      type = col_character(),
      listeverdier = col_character(),
      listetekst = col_character(),
      normalintervall_start_numerisk = col_character(), # Sjå merknad nedanfor om årsaka til denne og dei tre neste må vera tekst
      normalintervall_slutt_numerisk = col_character(),
      maksintervall_start_numerisk = col_character(),
      maksintervall_slutt_numerisk = col_character(),
      normalintervall_start_dato = col_character(),
      normalintervall_slutt_dato = col_character(),
      maksintervall_start_dato = col_character(),
      maksintervall_slutt_dato = col_character(),
      antall_tegn = col_integer(),
      lovlige_tegn = col_character(),
      desimaler = col_integer(),
      aktiveringsspoersmaal = col_character(),
      underspoersmaal = col_character(),
      innfoert_dato = col_character(),
      utfaset_dato = col_character(),
      tabell = col_character(),
      fysisk_feltnavn = col_character(),
      kommentar = col_character(),
      variabel_id = col_character(),
      hjelpetekst = col_character()
    )
  ))

  # Dei numeriske min- og maksverdiane kan ifølgje dokumentasjonen
  # http://helseregister.no/confluence/display/KG/Klokeboken
  # òg vera tekst som referer til andre felt, eks. «birthYear»
  # eller «todayYear». Dei kan me ikkje bruka, så me fjernar
  # dei rett og slett. Dette gjer me lettast ved å prøva
  # å gjera tekst om til tal med as.numeric(), som gjev ut
  # NA for alt som ikkje ser ut som tal.
  tekst_til_tal = function(x) {
    suppressWarnings(as.numeric(x))
  }

  # Gjer om kodeboka til vårt *standardiserte* format
  # (Det finst ikkje heilt ei 1-til-1-kopling, men me
  #  gjer so godt me kan, og set verdiar til NA der
  #  det ikkje finst nokon tilsvarande.)
  #
  # fixme: Vårt kodebokformat bør nok oppdaterast til
  #        å støtta min- og maks-verdiar for datoar òg.
  kodebok = kodebok_oqr_format %>%
    mutate(
      skjema_id = tabell,
      skjemanamn = skjemanavn,
      variabel_id = str_to_lower(fysisk_feltnavn),
      obligatorisk = str_to_lower(obligatorisk),
      variabeletikett = ledetekst,
      forklaring = hjelpetekst,
      variabeltype = type,
      verdi = listeverdier,
      verditekst = listetekst,
      desimalar = desimaler,
      min = tekst_til_tal(maksintervall_start_numerisk),
      maks = tekst_til_tal(maksintervall_slutt_numerisk),
      min_rimeleg = tekst_til_tal(normalintervall_start_numerisk),
      maks_rimeleg = tekst_til_tal(normalintervall_slutt_numerisk),
      kommentar = kommentar,
      kategori = NA_character_,
      innleiing = NA_character_,
      eining = NA_character_,
      unik = NA_character_,
      manglande = NA_character_,
      kommentar_rimeleg = NA_character_,
      utrekningsformel = NA_character_,
      logikk = NA_character_
    )

  # Ein «Statusvariabel» er eigentleg ein kategorisk variabel
  # som kan ta tre verdiar, -1, 0 og 1 (oppretta, lagra og ferdigstilt).
  # Me gjer derfor kvar statusvariabel om til ein kategorisk variabel.
  # Kodeboka må utvidast med nye rader, og me gjer det iterativt,
  # éin gong for kvar statusvariabel (i teorien litt tregt/suboptimalt,
  # men i praksis kjemperaskt, sidan me ikkje har kodebøker med tusenvis
  # av statusvariablar, berre maks éin per skjema).
  while (any(kodebok$variabeltype == "Statusvariabel")) {
    # Radnummeret til første (ubehandla) statusvariabel
    ind = which(kodebok$variabeltype == "Statusvariabel")[1]

    # Rada må bytast ut med tre rader, éi for kvar moglege verdi.
    # Dette gjer me først ved å utvida kodeboka med to ekstra,
    # identiske rader rett etter rada. Så set me inn rette verdiar.
    #
    # Gjenta aktuell rad tre gongar (når me gjer det slik,
    # funkar det òg viss rada er første eller siste rad).
    kodebok = kodebok[append(seq_len(nrow(kodebok)),
      values = c(ind, ind), after = ind
    ), ]

    # Legg rette verdiar inn i dei tre nye radene
    nyind = c(ind, ind + 1, ind + 2)
    kodebok$verdi[nyind] = -1:1
    kodebok$verditekst[nyind] = c("Opprettet", "Lagret", "Ferdigstilt")
    kodebok$variabeltype[nyind] = "Listevariabel"
  }

  # Oversikt over variabeltypar i OQR og tilhøyrande standardnamn som me brukar
  vartype_oqr_standard = tribble(
    ~type_oqr, ~type_standard,
    "Listevariabel", "kategorisk",
    "Tekstvariabel", "tekst",
    "Stor tekstvariabel", "tekst",
    "Avkrysningsboks", "boolsk",
    "Datovariabel", "dato",
    "Skjult variabel", "tekst",
    "Tallvariabel", "numerisk",
    "Tidsvariabel", "kl",
    "TIMESTAMP", "dato_kl"
  )

  # Stopp viss det dukkar opp variabeltypar me ikkje kjenner til
  nye_vartypar = na.omit(setdiff(kodebok$variabeltype, vartype_oqr_standard$type_oqr))
  if (length(nye_vartypar) > 0) {
    stop(
      "Kodeboka har variabeltypar me ikkje støttar / har standardnamn på: ",
      str_c(nye_vartypar, collapse = ", ")
    )
  }

  # Byt ut variabeltype-verdiane med våre standardiserte namn
  kodebok$variabeltype = vartype_oqr_standard$type_standard[
    match(kodebok$variabeltype, vartype_oqr_standard$type_oqr)
  ]

  # Dei variabelnamna me brukar, og i ei fornuftig rekkjefølgje
  std_namn = c(
    "skjema_id", "skjemanamn", "kategori", "innleiing", "variabel_id",
    "variabeletikett", "forklaring", "variabeltype", "eining", "unik",
    "obligatorisk", "verdi", "verditekst", "manglande", "desimalar",
    "min", "maks", "min_rimeleg", "maks_rimeleg", "kommentar_rimeleg",
    "utrekningsformel", "logikk", "kommentar"
  )


  # Ta med dei namna me brukar, i fornuftig rekkjefølgje
  kodebok = kodebok %>%
    select(!!std_namn)

  # I tillegg til dei definerte variablane har datadumpane seks ekstra
  # variablar, to før kodebokvariablane og fire etter. Desse er definerte
  # i dokumentasjonen til datadumpane(dokumentet «4.2 Dokumentasjon på
  # format av Datadump i register.doc»)
  legg_til_ekstravar = function(kb) {
    kb_ekstra = tribble(
      ~variabel_id, ~variabeletikett, ~variabeltype, ~unik, ~obligatorisk, ~desimalar,
      "mceid", "Forløps-ID", "numerisk", "ja", "ja", 0,
      "centreid", "RESH-ID", "numerisk", "nei", "ja", 0,
      "tsupdated", "Skjema sist oppdatert", "dato_kl", "nei", "nei", NA,
      "updatedby", "Skjema oppdatert av", "tekst", "nei", "nei", NA,
      "tscreated", "Skjema oppretta", "dato_kl", "nei", "ja", NA,
      "createdb", "Skjema oppretta av", "tekst", "nei", "ja", NA
    )
    kb_utvida = bind_rows(kb_ekstra[1:2, ], kb, kb_ekstra[3:6, ])
    kb_utvida
  }
  kodebok = kodebok %>%
    nest(-skjema_id, -skjemanamn) %>%
    mutate(data = map(data, legg_til_ekstravar)) %>%
    unnest()

  # fixme: Vurder om det er nødvendig å køyra funksjonen som gjer
  #        kodebok om til kanonisk form (eller om ho alt *er* på kanonisk form)

  # Returner kodeboka
  kodebok
}


# Les datadump frå OQR-register -------------------------------------------

# Les inn OQR-data frå gitt skjema ved hjelp av kodebok.
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
# Inndata:
#   mappe_dd:  Adressa til datadump-mappa (som inneheld éi undermappe, med namn på forma ÅÅÅÅ-MM-DD, for kvart uttak)
#   reg_id:    ID som identifiserer registeret og er prefiks til alle filnamna
#   skjema_id: ID til skjemaet ein vil henta inn (brukt i filnamnet og i kolonnen «tabell» i kodeboka)
#   status:    Berre ta med skjema med desse statusverdiane (-1 = oppretta, 0 = kladd, 1 = ferdigstilt).
#              Kan òg vera NULL, for å henta alt, uavhengig av status (dvs. også inkludert NA-status
#              og ugyldige statusverdiar, eller datadumpar som manglar statusvariabel
#              (ikkje noko av dette *skal* vera mogleg å få, men alt kan skje i denne verda ...)).
#   dato:      Datoen ein skal henta ut kodeboka for (tekststreng eller dato). Kan òg vera NULL, for å henta nyaste kodebok.
#   kb:        Kodebok på kanonisk form. Kan òg vera NULL, og då vert kodeboka automatisk henta inn.
#
# Utdata:
#   R-datasett for det aktuelle skjemaet, med variabelnamn gjort om til små bokstavar.
#
les_dd_oqr = function(mappe_dd, reg_id, skjema_id, status = 1, dato = NULL, kb = NULL) { # fixme: Legg på dd-validering?
  # Bruk siste tilgjengelege kodebok dersom ein ikkje har valt dato
  if (is.null(dato)) {
    dato = dir(mappe_dd, pattern = "[0-9]{4}-[0-1]{2}-[0-9]{2}", full.names = FALSE) %>%
      sort() %>%
      last()
  }
  dato = as_date(dato) # I tilfelle det var ein tekstreng

  # Les inn kodeboka dersom ho ikkje er spesifisert
  if (is.null(kb)) {
    kb = les_kb_oqr(mappe_dd, reg_id, dato) # fixme: Ev. validering
  }
  # Hent ut variabelinfo frå kodeboka for det gjeldande skjemaet
  kb_akt = kb %>%
    filter(skjema_id == !!skjema_id)

  # Kodeboka må ha informasjon om variablane i
  # i det aktuelle skjemaet for at me skal halda fram ...
  if (nrow(kb_akt) == 0) {
    stop("Kodeboka manglar informasjon om skjemaet '", skjema_id, "'")
  }

  # Les inn variabelnamna som vert brukt i datafila
  adresse_dd = paste0(
    mappe_dd, "\\", dato, "\\",
    reg_id, "_", skjema_id, "_datadump.csv_", format(dato, "%d.%m.%Y"), ".csv"
  )
  varnamn_dd = tolower(scan(adresse_dd,
    fileEncoding = "UTF-8-BOM", what = "character",
    sep = ";", nlines = 1, quiet = TRUE
  ))

  # Sjekk at alle variablane i datadumpen finst i kodeboka
  # og at alle variablane i kodeboka finst i datadumpen
  # (og i same rekkjefølgje)
  varnamn_kb = unique(kb_akt$variabel_id)
  if (!identical(varnamn_kb, varnamn_dd)) {
    feilmelding = "Er ikkje same variablar i kodeboka og i datadumpfila.\n"
    ekstra_kb = setdiff(varnamn_kb, varnamn_dd)
    ekstra_dd = setdiff(varnamn_dd, varnamn_kb)
    if (length(ekstra_kb) >= 0) {
      feilmelding = paste0(
        feilmelding, "Desse variablane finst berre i kodeboka:\n",
        paste(ekstra_kb, collapse = ", "), "\n"
      )
    }
    if (length(ekstra_dd) >= 0) {
      feilmelding = paste0(
        feilmelding, "Desse variablane finst berre i datadumpen:\n",
        paste(toupper(ekstra_dd), collapse = ", "), "\n"
      )
    }
    stop(feilmelding)
  }

  # Datafila *kan* ikkje innehalda duplikate kolonnenamn,
  # sidan me då ikkje kan veta kva kolonne eit namn svarar til.
  # Stopp derfor viss me finn duplikate namn.
  dupnamn = duplicated(varnamn_dd)
  if (any(dupnamn)) {
    stop(
      "Datafila har duplikate variabelnamn:\n",
      str_c(varnamn_dd[dupnamn], collapse = "\n")
    )
  }

  # disse variabelnamna er ikkje dei vi brukar.
  # henter inn namna som vi faktisk brukar
  # avhengig om variabelnavnene er norske eller engelske
  # i de ulike kodebøkene velger vi å matche mot norske
  # eller engelske navn for å få inn de vi vil ha
  if (dd_kolnamn_er_norsk) {
    dd_kolid = "oqr_variabel_id_norsk"
  } else {
    dd_kolid = "oqr_variabel_id_engelsk"
  }
  varnamn = kb$variabel_id[match(varnamn_fil, kb[[dd_kolid]])] %>%
    coalesce(varnamn_fil)

  # Hent ut første linje frå kodeboka, dvs. den linja som
  # inneheld aktuell informasjon
  kb_info = kb %>%
    distinct(variabel_id, .keep_all = TRUE)

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
  spek_csv_oqr = tribble(
    ~variabeltype, ~csv_bokstav,
    "kategorisk", "n",
    "tekst", "c",
    "boolsk", "c", # Sjå konvertering nedanfor
    "dato_kl", "c", # Mellombels, jf. https://github.com/tidyverse/readr/issues/642 (fixme til "T" når denne er fiksa)
    "numerisk", "d",
    "dato", "D",
    "kl", "t"
  )
  spek_innlesing = tibble(variabel_id = varnamn) %>%
    left_join(kb_info, by = "variabel_id") %>%
    left_join(spek_csv_oqr, by = "variabeltype")

  # Har kodeboka variablar av ein type me ikkje har lagt inn støtte for?
  # Dette skal ikkje skje, så avbryt om så er tilfelle.
  nye_typar = setdiff(kb_info$variabeltype, spek_csv_oqr$variabeltype)
  if (length(nye_typar) > 0) {
    stop(
      "Kodeboka har variablar av ein type me ikkje støttar (legg inn støtte!):\n",
      str_c(nye_typar, collapse = "\n")
    )
  }

  # Er det nokon variablar me manglar metadata for (dvs. variablar
  # som finst i datafila men *ikkje* i kodeboka)?
  # Fixme: Vurder å legga til eit argument i funksjonen
  #        for å gøyma åtvaringar for standardvariablar
  #        (dvs. dei som finst i alle OQR-datadumpar)
  manglar_metadata = is.na(spek_innlesing$csv_bokstav)
  ukjende_var = spek_innlesing$variabel_id[manglar_metadata]
  if (any(manglar_metadata)) {
    warning(
      "Manglar metadata for nokre variablar. Dei vert derfor\n",
      "handterte som tekst og variabelnamna vert gjorde om til\n",
      "små bokstavar og får prefikset «oqr_».\n",
      "Problematiske variablar:\n",
      str_c(ukjende_var, collapse = "\n")
    )
    spek_innlesing$csv_bokstav[manglar_metadata] = "c"
    spek_innlesing$variabel_id[manglar_metadata] = str_to_lower(str_c("oqr_", spek_innlesing$variabel_id[manglar_metadata]))
  }

  # Les inn datasettet
  kol_typar = str_c(spek_innlesing$csv_bokstav, collapse = "")
  d = stop_for_problems(read_delim(adresse,
    delim = ";", quote = "\"", trim_ws = FALSE, na = "null",
    col_names = spek_innlesing$variabel_id, col_types = kol_typar, skip = 1, # Hopp over overskriftsrada
    locale = locale(
      decimal_mark = ",", grouping_mark = "",
      date_format = datoformat, time_format = "%H:%M:%S"
    )
  ))

  # Filtrer vekk skjema som ikkje har rett statusvariabel
  # (som standard vert berre ferdigstilte skjema tekne med)
  if (!is.null(status)) {
    d = filter(status %in% !!status)
  }


  # Gjer om boolske variablar til ekte boolske variablar
  oqr_boolsk_til_boolsk = function(x) {
    # Sjekk først at det berre er gyldige verdiar
    er_gyldig = (x %in% 0:1) | is.na(x)
    if (!all(er_gyldig)) {
      stop("Finst ugyldige verdiar i boolsk variablar (skal vera 0, 1 eller NA)")
    } else {
      x == 1 # Gjer om til boolsk variabel
    }
  }
  boolsk_ind = which(spek_innlesing$variabeltype == "boolsk")
  d[, boolsk_ind] = lapply(d[, boolsk_ind], oqr_boolsk_til_boolsk)

  # Gjer om tidsvariablar til ekte tidsvariablar
  # Fixme: Nødvendig pga. https://github.com/tidyverse/readr/issues/642
  #        Fjern når denne feilen er fiksa (rett då òg fixme-en
  #        lenger oppe som også handlar om dette)
  dt_ind = which(spek_innlesing$variabeltype == "dato_kl")
  d[, dt_ind] = lapply(d[, dt_ind], parse_datetime, format = "%Y-%m-%d %H:%M:%S")

  # Returner datasettet
  d
}


# Eksempel  -----------------------------------------------------------

# # Les inn eksempeldata
# mappe_dd = "***FJERNA-ADRESSE***"
# filnamn_dd = "Datadump_Alle_variabler_numerisk.csv"
# adresse_dd = paste0(mappe_dd, filnamn_dd)
#
# # Les inn kodeboka
# adresse_kb = "***FJERNA-ADRESSE***"
# kb = les_oqr_kb(adresse_kb)
#
# # Les inn datadump
# d = les_dd_oqr(adresse_dd, kb)
#
# # Sjå nøyare på eventuelle importproblem
# problems(d)
