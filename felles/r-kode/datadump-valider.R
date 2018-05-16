# datavalideringsskript

# nødvendige pakker
library(tidyverse)
library(ruler) # pakke for validering
library(rlang)

# lager fiktivt datasett som inneholder
# en del feil som skal oppdages i valider_datadump-funksjonen

# d = tribble(
#   ~pasid, ~kjonn, ~alder, ~vekt, ~frisk,
#   11,     0,      16.23,     30,    TRUE,
#   12,     1,        22,     50,    FALSE,
#   13,     1,       -14,    60,    FALSE,
#   14,     NA,       80,     70.7,  TRUE,
#   15,     3,       900,    1000,  NA
#   )

# lager en fiktiv kodebok som hører til det fiktive datasettet

# kb = tribble(
#   ~variabel_id, ~variabeltype, ~min, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst,
#   "pasid", "tekst", NA, NA, TRUE, NA, NA, NA,
#   "alder", "numerisk", 18,  NA, TRUE,       0,     NA,         NA,
#   "vekt", "numerisk", 45, 200, TRUE,        0, NA, NA,
#   "kjonn", "kategorisk", NA, NA, TRUE, NA, 0, "kvinne",
#   "kjonn", "kategorisk", NA, NA, TRUE, NA, 1, "mann",
#   "frisk", "boolsk", NA, NA, TRUE, NA, NA, NA
#   )

#---------------------------------------------------------Tester--------------------------------------------------------------

# Funksjon for å lage regler basert på informasjon
# fra kodeboka. Krever en kodebok (kb)
# Argumentet "oblig" gjør det mulig å velge om man ønsker å sjekke obligatoriske felt.
# dette fordi MRS-kodebøker har feil i sin obligatorisk-koding,
# og dermed er uaktuelt for testing. Defaulter er TRUE.

lag_regelsett = function(kb, oblig = TRUE) {

  # for å lage regler må kodeboka ha følgende kolonner:
  nodvar = c("variabel_id", "variabeltype", "min", "maks", "desimalar", "verdi", "verditekst")
  # kolonner i kb
  kol = names(kb)

  # Stopp hvis kodeboka mangler helt nødvendige kolonner
  if (!all(nodvar %in% kol)) {
    manglende_nodvar = which(!nodvar %in% kol)
    kb_mangler = nodvar[manglende_nodvar]
    stop(paste0("Kodeboka mangler obligatoriske kolonner: ", str_c("'", kb_mangler, "'", collapse = ", "), "."))
  }

  # Stopp hvis variabel_id eller variabeltype mangler verdi
  for (kol in c("variabel_id", "variabeltype")) {
    if (any(is.na(kb[[kol]]))) {
      stop(paste0("Kodeboka har manglende verdier for variabel_id og/eller variabeltype."))
    }
  }

  # funksjon for å hente ut ønsket område av kodeboka
  # som brukes i en gitt test. F.eks, om man ønsker å teste min-verdier
  # i en datadump, så trenger man bare informasjon om min-verdier
  # for variabler som faktisk *har* min-verdier.
  kb_filter = function(kb, kolonne) {

    # henter ut delen av kodeboka som
    # har en verdi i en aktuelle kolonnen
    kb_utsnitt = kb %>%
      filter(!is.na(kb[kolonne])) %>%
      select(variabel_id, kolonne) %>%
      rename(varnamn = "variabel_id")
    # rename funksjonen støtter ikke expressions.
    # altså kan man ikke ha gverdi = past0(kolonne) i kallet til rename() ovenfor
    # kjører koden under for å endre navn på kolonne til noe som brukes generelt i alle tester
    kolonnenavn_plassering = which(names(kb_utsnitt) == kolonne)
    names(kb_utsnitt)[kolonnenavn_plassering] = "gverdi"

    # returner objektet
    kb_utsnitt
  }

  kb_min = kb_filter(kb, "min")
  kb_maks = kb_filter(kb, "maks")
  kb_des = kb_filter(kb, "desimalar")
  kb_oblig = kb_filter(kb, "obligatorisk")

  # trenger 4 filter for kodeboka for ulike typer variabel
  kb_rename = kb %>%
    rename(varnamn = "variabel_id") # bruker denne lenger nede også
  kb_kat = kb_rename %>%
    filter(variabeltype == "kategorisk") %>%
    select(varnamn, verdi) %>%
    rename(gverdi = "verdi")
  kb_num = kb_rename %>%
    filter(variabeltype == "numerisk" | variabeltype == "kategorisk" | variabeltype == "utrekna") %>%
    distinct(varnamn)
  kb_boolsk = kb_rename %>%
    filter(variabeltype == "boolsk") %>%
    select(varnamn)
  kb_tekst = kb_rename %>%
    filter(variabeltype == "tekst") %>%
    select(varnamn)

  # Stopp hvis kategoriske variabler mangler verdi eller verditekst
  if (any(is.na(kb_kat$gverdi))) {
    stop(paste0("Kategoriske variabler mangler verdier for verdi."))
  }

  #---------------------------------------------min-verdier------------------------------------------------------

  # Lager regel for at verdier i data
  # skal være større eller lik min-verdi i kodebok.
  # Lager en egen regel for hver aktuelle rad i
  # datarammen.

  # lager tom liste som vi fyller reglene i
  l_min_maks = list()

  if (nrow(kb_min) > 0) {
    sjekk_min = kb_min %>%
      pmap(function(varnamn, gverdi) {
        new_function(
          alist(df = ),
          expr(transmute_at(df, vars(foo = !!varnamn), rules(min_ok = . >= !!gverdi)))
        )
      }) %>%
      setNames(paste0("min_", kb_min$varnamn))

    # fyller regelen i lista
    l_min_maks = append(l_minmaks, sjekk_min)
  }
  #  -----------------------------maks-------------------------------------------

  # Lager "rules" som tester maks-verdier
  if (nrow(kb_maks) > 0) {
    sjekk_maks = kb_maks %>%
      pmap(function(varnamn, gverdi) {
        new_function(
          alist(df = ),
          expr(transmute_at(df, vars(foo = !!varnamn), rules(maks_ok = . <= !!gverdi)))
        )
      }) %>%
      setNames(paste0("maks_", kb_maks$varnamn))
    # fyller regelen i lista
    l_min_maks = append(l_minmaks, sjekk_maks)
  }
  # lager en cell-pack med maks-sjekkene
  er_innfor_min_og_maks = cell_packs(l_min_maks)

  #-----------------------------------------desimaler-------------------------------------------------

  # Lager "rules" som tester at variablene har riktig antall desimaler
  sjekk_des = kb_des %>%
    pmap(function(varnamn, gverdi) {
      new_function(
        alist(df = ),
        expr(transmute_at(df, vars(foo = !!varnamn), rules(des_ok = round(., gverdi) == .)))
      )
    }) %>%
    setNames(paste0("des_", kb_des$varnamn))

  # lager en cell-pack med des-sjekkene
  har_riktig_ant_des = cell_packs(sjekk_des)

  #---------------------------------------obligatoriske felt----------------------------------------------

  if (oblig) {

    # Lager "rules" på at obligatoriske variabler ikke skal ha noen missing.
    sjekk_oblig = kb_oblig %>%
      pmap(function(varnamn, gverdi) {
        new_function(
          alist(df = ),
          expr(transmute_at(df, vars(foo = !!varnamn), rules(gverdi = !is.na(.))))
        )
      }) %>%
      setNames(paste0("oblig_", kb_oblig$varnamn))

    # lager en cell-pack med oblig-sjekkene
    oblig_har_ingen_missing = cell_packs(sjekk_oblig)
    oblig_har_ingen_missing
  }
  #------------------------------------------kategoriske verdier----------------------------------------

  # Lager "rules" som sier at verdiene til kategoriske variabler må være tilstede i kodeboka
  kb_kat_kompakt = kb_kat %>%
    nest(gverdi)
  sjekk_kat = kb_kat_kompakt %>%
    pmap(function(varnamn, data) {
      gverdi = data$gverdi
      new_function(
        alist(df = ),
        expr(transmute_at(df, vars(foo = !!varnamn), rules(gyl_kat = . %in% !!gverdi | is.na(.))))
      )
    }) %>%
    setNames(paste0("kat_", kb_kat_kompakt$varnamn))

  # lager en cell-pack med verdi-sjekkene
  kat_er_innfor_verdier = cell_packs(sjekk_kat)

  #-----------------------------------------variabeltype--------------------------------------------------------

  # lager en liste for å få inn regler avhengig av hvilke som eksisterer i kodeboka
  l_vartype = list()
  # Lager "rules" som tester om en kolonne i datasettet er samme som i kodeboka.
  # en sjekk for numeriske variabler
  if (nrow(kb_num) > 0) {
    sjekk_num = kb_num %>%
      pmap(function(varnamn) {
        new_function(
          alist(df = ),
          expr(summarise_at(df, vars(foo = !!varnamn), rules(vartype_ok = is.numeric(.))))
        )
      }) %>%
      setNames(paste0("num_", kb_num$varnamn))
    # appender regelen til lista
    l_vartype = append(l_vartype, sjekk_num)
  }
  # boolske
  if (nrow(kb_boolsk) > 0) {
    sjekk_boolsk = kb_boolsk %>%
      pmap(function(varnamn) {
        new_function(
          alist(df = ),
          expr(summarise_at(df, vars(foo = !!varnamn), rules(vartype_ok = is.logical(.))))
        )
      }) %>%
      setNames(paste0("boolsk_", kb_boolsk$varnamn))
    # appender regelen til lista
    l_vartype = append(l_vartype, sjekk_boolsk)
  }
  # tekstvariabler
  if (nrow(kb_tekst) > 0) {
    sjekk_tekst = kb_tekst %>%
      pmap(function(varnamn) {
        new_function(
          alist(df = ),
          expr(summarise_at(df, vars(foo = !!varnamn), rules(vartype_ok = is.character(.))))
        )
      }) %>%
      setNames(paste0("tekst_", kb_tekst$varnamn))
    # appender regelen til lista
    l_vartype = append(l_vartype, sjekk_tekst)
  }

  # lager en col-pack med variabeltype-sjekkene
  er_riktig_variabeltype = col_packs(l_vartype)

  #-----------------------------------------alle variabelnavn tilstede-------------------------------------------------------
  # Test sjekker at alle variablenavn i datadump er med i kodeboka (samtidig)
  # og at alle varibelnavn i kodebok er med i datadump
  alle_var_er_med = data_packs(
    sjekk_alle_varnavn_dd = . %>% summarise(all(alle_varnavn = names(.) %in% (kb %>% distinct(variabel_id))$variabel_id)),
    sjekk_alle_varnavn_kb = . %>% summarise(all(alle_varnavn = (kb %>% distinct(variabel_id))$variabel_id %in% names(.)))
  )

  #-------------------------------------lik rekkefølge på variabelnavn som i kodebok----------------------------------

  # sjekk at rekkefølgen på kolonner er lik mellom data og kodebok
  er_lik_rekkefolge = data_packs(
    sjekk_rekkefolge = . %>% summarise(rekkefolge_varnavn = identical(names(.), (kb %>% distinct(variabel_id))$variabel_id))
  )

  regelsett = list(
    er_innfor_min_og_maks,
    har_riktig_ant_des,
    kat_er_innfor_verdier,
    er_riktig_variabeltype,
    alle_var_er_med,
    er_lik_rekkefolge
  )
  # legger kun til objekt for oblig hvis vi ønsker å teste det
  if (oblig) {
    regelsett = c(regelsett, list(oblig_har_ingen_missing))
  }

  # returnerer lista
  regelsett
}

# Returner sann viss og berre viss datadumpen er gyldig
# er reglane som følgjer frå kodeboka
dd_er_gyldig = function(df, kb, ...) {
  regelsett = lag_regelsett(kb, ...)
  har_feil = df %>%
    expose(regelsett) %>%
    any_breaker()
  !har_feil
}

# regelsett = lag_regelsett(kb, oblig = TRUE)
#
# # funksjon for å teste regelsettet på datadump
# sjekk_dd = function(d, regelsett){
#
#   rapport = d %>% expose(regelsett) %>% get_report
#   rapport
# }
#
# #sjekk at funksjonen funker
# sjekk_dd(d, regelsett)
#
#
# # # Test at funksjonen fungerer
# library(testthat)
# test_adr = "h:/kvalreg/felles/r-kode/datadump-valider-testar.R"
#  test_file(test_adr, reporter="minimal") # *Veldig* kort og konsist samandrag
#  test_file(test_adr, reporter="check")   # 13-linjes samandrag
#  test_file(test_adr, reporter="summary") # Alt (tar stor plass viss det er mange mislykka testar)

#--------------------------------------er samme variabelnavn som i kodebok------------------------------------------

# # sjekker at hver enktelt av variabelnavna er de samme som i kodeboka
# er_samme_navn = data_packs(
#   sjekk_pasid = . %>% summarise(navn_pasid = names(.)[1] %in% (kb$variabel_id)),
#   sjekk_kjonn = . %>% summarise(navn_kjonn = names(.)[2] %in% (kb$variabel_id))
# )
#
# # Finner feil og rapporterer hvilken pasient og variabel som gjelder
# # for feil i variabelnavn
# d %>% expose(er_samme_navn) %>%
#   get_report()

# fixme! testen over burde generelaiseres til å kjøre testen for hver variabel i datarammen.
# et forsøk nedenfor er en start, men denne fungerer ikke.
# det skal være en data_pack()
# Lager "rules" som tester om en variabelnavn i datadumpen
# ikke eksisterer i kodeboka
# sjekk_navn = (kb %>% distinct(variabel_id) %>% rename(varnamn = "variabel_id")) %>%
#   pmap(function(varnamn) {
#     new_function(alist(df=),
#                  expr(summarise(df, navn_ok = names(.)[.] %in% varnamn))
#   )
#   }) %>% setNames(paste0("namn_", names(.)[.]))
#
# er_samme_navn = data_packs(sjekk_navn)
#
#
# # Finner feil og rapporterer hvilken pasient og variabel som gjelder
# # for feil i variabelnavn
# d %>% expose(er_samme_navn) %>%
#   get_report()
