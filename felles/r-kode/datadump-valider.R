# datavalideringsskript

# nødvendige pakker
library(tidyverse)
library(ruler) # pakke for validering
library(rlang)

# lager fiktivt datasett som inneholder
# en del feil som skal oppdages i valider_datadump-funksjonen

d = tribble(
  ~pasid, ~kjonn, ~alder, ~vekt, ~frisk,
  11, 0, 16.23, 30, TRUE,
  12, 1, 22, 50, FALSE,
  13, 1, -14, 60, FALSE,
  14, NA, 80, 70.7, TRUE,
  15, 3, 900, 1000, NA
)

# lager en fiktiv kodebok som hører til det fiktive datasettet

kb = tribble(
  ~varabel_id, ~variabeltype, ~min, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst,
  "pasid", "tekst", NA, NA, TRUE, NA, NA, NA,
  "alder", "numerisk", 18, NA, TRUE, 0, NA, NA,
  "vekt", "numerisk", 45, 200, TRUE, 0, NA, NA,
  "kjonn", "kategorisk", NA, NA, TRUE, NA, 0, "kvinne",
  "kjonn", "kategorisk", NA, NA, TRUE, NA, 1, "mann",
  "frisk", "boolsk", NA, NA, TRUE, NA, NA, NA
)

#---------------------------------------------------------Tester--------------------------------------------------------------

# funksjon for å hente ut ønsket område av kodeboka
# som brukes i en gitt test. F.eks, om man ønsker å teste min-verdier
# i en datadump, så trenger man bare informasjon om min-verider
# for variabler som faktisk *har* min-verdier.
kb_filter = function(kb, kolonne) {

  # henter ut delen av kodeboka som
  # har en verdi i en aktuelle kolonnen
  kb_utsnitt = kb %>%
    filter(!is.na(kb[kolonne])) %>%
    select(varabel_id, kolonne) %>%
    rename(varnamn = "varabel_id")
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
kb_kat = kb_filter(kb, "verdi")

#---------------------------------------------min-verdier------------------------------------------------------

# # Gammal løysing, der gverdi og varnamn ikkje kom ut som konstantar
# eks = kb_min %>%
#   pmap(function(varnamn, gverdi) {
#     . %>% transmute_at(vars(foo = varnamn), rules(min_ok = . >= gverdi))
#   })

# Ny, forbetra løysing
sjekk_min = kb_min %>%
  pmap(function(varnamn, gverdi) {
    new_function(
      alist(df = ),
      expr(transmute_at(df, vars(foo = !!varnamn), rules(min_ok = . >= !!gverdi)))
    )
  }) %>%
  setNames(paste0("min_", kb_min$varnamn))

# eks=list(min_alder = . %>% transmute_at(vars(foo="alder"), rules(min_ok = . >= 18)),
#      min_vekt = . %>% transmute_at(vars(foo="vekt"), rules(min_ok = . >= 45)))
er_innfor_min = cell_packs(sjekk_min)
d %>%
  expose(er_innfor_min) %>%
  get_report()

#  -----------------------------maks-------------------------------------------

# Lager "rules" som tester maks-verdier i en funksjon
sjekk_maks = kb_maks %>%
  pmap(function(varnamn, gverdi) {
    new_function(
      alist(df = ),
      expr(transmute_at(df, vars(foo = !!varnamn), rules(maks_ok = . <= !!gverdi)))
    )
  }) %>%
  setNames(paste0("maks_", kb_maks$varnamn))

# lager en cell-pack med maks-sjekkene
er_innfor_maks = cell_packs(sjekk_maks)
# tester dataene
d %>%
  expose(er_innfor_maks) %>%
  get_report()

#-----------------------------------------desimaler-------------------------------------------------

# Lager "rules" som tester desverdier i en funksjon
sjekk_des = kb_des %>%
  pmap(function(varnamn, gverdi) {
    new_function(
      alist(df = ),
      expr(transmute_at(df, vars(foo = !!varnamn), rules(des_ok = round(., gverdi) == !!gverdi)))
    )
  }) %>%
  setNames(paste0("des_", kb_des$varnamn))

# lager en cell-pack med maks-sjekkene
har_riktig_ant_des = cell_packs(sjekk_des)
# tester dataene
d %>%
  expose(har_riktig_ant_des) %>%
  get_report()

#---------------------------------------obligatoriske felt----------------------------------------------

# Lager "rules" som tester maks-verdier i en funksjon
sjekk_oblig = kb_oblig %>%
  pmap(function(varnamn, gverdi) {
    new_function(
      alist(df = ),
      expr(transmute_at(df, vars(foo = !!varnamn), rules(gverdi = !is.na(.))))
    )
  }) %>%
  setNames(paste0("oblig_", kb_oblig$varnamn))

# lager en cell-pack med maks-sjekkene
oblig_har_ingen_missing = cell_packs(sjekk_oblig)
# tester dataene
d %>%
  expose(oblig_har_ingen_missing) %>%
  get_report()

#------------------------------------------kategoriske verdier----------------------------------------

# Lager "rules" som tester maks-verdier i en funksjon
sjekk_kat = kb_kat %>%
  pmap(function(varnamn, gverdi) {
    gverdi = kb_kat$gverdi
    new_function(
      alist(df = ),
      expr(transmute_at(df, vars(foo = !!varnamn), rules(gyl_kat = . %in% gverdi | is.na(.))))
    )
  }) %>%
  setNames(paste0("kat_", kb_kat$varnamn))

# lager en cell-pack med maks-sjekkene
kat_er_innfor_verdier = cell_packs(sjekk_kat)
# tester dataene
d %>%
  expose(kat_er_innfor_verdier) %>%
  get_report()

#-----------------------------------------variabeltype--------------------------------------------------------

# trenger 3 filter for kodeboka for hver type variabel
kb_num = kb %>%
  filter(variabeltype == "numerisk" | variabeltype == "kategorisk" | variabeltype == "utrekna") %>%
  distinct(varabel_id) %>%
  rename(varnamn = "varabel_id")
kb_boolsk = kb %>%
  filter(variabeltype == "boolsk") %>%
  select(varabel_id) %>%
  rename(varnamn = "varabel_id")
kb_tekst = kb %>%
  filter(variabeltype == "tekst") %>%
  select(varabel_id) %>%
  rename(varnamn = "varabel_id")

# Lager "rules" som tester om en kolonne i datasettet er samme som i kodeboka.
# en sjekk for numeriske variabler
sjekk_num = kb_num %>%
  pmap(function(varnamn) {
    new_function(
      alist(df = ),
      expr(summarise_at(df, vars(foo = !!varnamn), rules(vartype_ok = is.numeric(.))))
    )
  }) %>%
  setNames(paste0("num_", kb_num$varnamn))
# boolske
sjekk_boolsk = kb_boolsk %>%
  pmap(function(varnamn) {
    new_function(
      alist(df = ),
      expr(summarise_at(df, vars(foo = !!varnamn), rules(vartype_ok = is.logical(.))))
    )
  }) %>%
  setNames(paste0("boolsk_", kb_boolsk$varnamn))
# tekstvariabler
sjekk_tekst = kb_tekst %>%
  pmap(function(varnamn) {
    new_function(
      alist(df = ),
      expr(summarise_at(df, vars(foo = !!varnamn), rules(vartype_ok = is.character(.))))
    )
  }) %>%
  setNames(paste0("tekst_", kb_tekst$varnamn))

# lager en col-pack med variabeltype-sjekkene
er_riktig_variabeltype = col_packs(sjekk_num, sjekk_boolsk, sjekk_tekst)
# tester dataene
d %>%
  expose(er_riktig_variabeltype) %>%
  get_report()

#-----------------------------------------alle variabelnavn tilstede-------------------------------------------------------
# Test sjekker at alle variablenavn i datadump er med i kodeboka (samtidig)
# og at alle varibelnavn i kodebok er med i datadump
alle_er_med = data_packs(
  sjekk_alle_varnavn_dd = . %>% summarise(all(alle_varnavn = names(.) %in% (kb %>% distinct(varabel_id))$varabel_id)),
  sjekk_alle_varnavn_kb = . %>% summarise(all(alle_varnavn = (kb %>% distinct(varabel_id))$varabel_id %in% names(.)))
)

# Rapporterer noen variabelnavn ikke er med i kodeboka
# er alle riktige kommer en tom tibble
d %>%
  expose(alle_er_med) %>%
  get_report()

#--------------------------------------er samme variabelnavn som i kodebok------------------------------------------

# sjekker at hver enktelt av variabelnavna er de samme som i kodeboka
er_samme_navn = data_packs(
  sjekk_pasid = . %>% summarise(navn_pasid = names(.)[1] %in% (kb$varabel_id)),
  sjekk_kjonn = . %>% summarise(navn_kjonn = names(.)[2] %in% (kb$varabel_id))
)

# Finner feil og rapporterer hvilken pasient og variabel som gjelder
# for feil i variabelnavn
d %>%
  expose(er_samme_navn) %>%
  get_report()

# fixme! testen over burde generelaiseres til å kjøre testen for hver variabel i datarammen.
# et forsøk nedenfor er en start, men denne fungerer ikke.
# det skal være en data_pack()
# Lager "rules" som tester om en variabelnavn i datadumpen
# ikke eksisterer i kodeboka
# sjekk_navn = (kb %>% distinct(varabel_id) %>% rename(varnamn = "varabel_id")) %>%
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

#-------------------------------------lik rekkefølge på variabelnavn som i kodebok----------------------------------

# sjekk at rekkefølgen på kolonner er lik mellom data og kodebok
er_lik_rekkefolge = data_packs(
  sjekk_rekkefolge = . %>% summarise(rekkefolge_varnavn = identical(names(.), (kb %>% distinct(varabel_id))$varabel_id))
)

# Finner feil og rapporterer hvilken pasient og variabel som gjelder
# for feil i rekkefølge på variabelnavn
d %>%
  expose(er_lik_rekkefolge) %>%
  get_report()
