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
  ~varid, ~vartype, ~min, ~maks, ~oblig, ~des, ~verdi, ~verditekst,
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
    select(varid, kolonne) %>%
    rename(varnamn = "varid")
  # rename funksjonen støtter ikke expressions.
  # altså kan man ikke ha gverdi = past0(kolonne) i kallet til rename() ovenfor
  # kjører koden under for å endre navn på kolonne til noe som brukes generelt i alle tester
  kolonnenavn_plassering = which(names(kb_utsnitt) == kolonne)
  names(kb_utsnitt)[kolonnenavn_plassering] = "gverdi"

  # returner objektet
  kb_utsnitt
}

kb_min = kb_filter(kb, kolonne = "min")
kb_maks = kb_filter(kb, kolonne = "maks")
kb_des = kb_filter(kb, kolonne = "des")

#---------------------------------------------mihn-verdier------------------------------------------------------

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
sjekk_min

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
sjekk_maks

# lager en cell-pack med maks-sjekkene
er_innfor_maks = cell_packs(sjekk_maks)
# tester dataene
d %>%
  expose(er_innfor_maks) %>%
  get_report()

#-----------------------------------------desimaler-------------------------------------------------

# sjekker at antall desimaler er ok
har_riktig_ant_desimaler = cell_packs(
  sjekk_desimal = . %>% transmute_at(vars(age = alder), rules(des_alder = round(alder, 0) == alder))
)
# Finner feil og rapporterer hvilken pasient og variabel som gjelder
# for desimal-verdier
d %>%
  expose(har_riktig_ant_desimaler) %>%
  get_report() %>%
  left_join((d %>% transmute(id = 1:n(), pasid, alder)), by = "id") %>%
  print(.validate = FALSE)

#---------------------------------------obligatoriske felt----------------------------------------------

# sjekker at obligatoriske felt er fylt ut
har_ingen_missing = cell_packs(
  sjekk_oblig = . %>% transmute_at(vars(gender = kjonn), rules(oblig_kjonn = !is.na(kjonn)))
)
# Finner feil og rapporterer hvilken pasient og variabel som gjelder
# for obligatoriske variabler med missing-verdier
d %>%
  expose(har_ingen_missing) %>%
  get_report() %>%
  left_join(d %>% transmute(id = 1:n(), pasid, kjonn), by = "id") %>%
  print(.validate = FALSE)

# sjekker at kategoriske variabler bare har gyldige verdier
har_gyldig_verdi = cell_packs(
  . %>% transmute_at(vars(gender = kjonn), rules(gyl_kjonn = kjonn %in% 0:1 | is.na(kjonn))) # kategoriske variabler trenger ikke nødvendigvis å være obligatoriske
)

# Finner feil og rapporterer hvilken pasient og variabel som gjelder
# for kategoriske variabler og ugyldige verdier
d %>%
  expose(har_gyldig_verdi) %>%
  get_report() %>%
  left_join((d %>% transmute(id = 1:n(), pasid, kjonn)), by = "id") %>%
  print(.validate = FALSE)


# sjekker at variabeltype er i tråd med kodeboka
er_riktig_variabeltype = col_packs(
  sjekk_pasid = . %>% summarise_at(vars(patient_id = pasid), rules(vartype_pasid = is.character(pasid))),
  sjekk_alder = . %>% summarise_at(vars(age = alder), rules(vartype_alder = is.numeric(alder))),
  sjekk_frisk = . %>% summarise_at(vars(well = frisk), rules(vartype_frisk = is.logical(frisk)))
)

# Finner feil og rapporterer hvilken pasient og variabel som gjelder
# for feil i variabeltype
d %>%
  expose(er_riktig_variabeltype) %>%
  get_report()

# Test sjekker at alle variablenavn i datadump er med i kodeboka (samtidig)
alle_er_med = data_packs(
  sjekk_alle_varnavn = . %>% summarise(all(alle_varnavn = names(.) %in% (kb %>% distinct(varid))$varid))
)

# Rapporterer noen variabelnavn ikke er med i kodeboka
# er alle riktige kommer en tom tibble
d %>%
  expose(alle_er_med) %>%
  get_report()

# sjekker at hver enktelt av variabelnavna er de samme som i kodeboka
er_samme_navn = data_packs(
  sjekk_pasid = . %>% summarise(navn_pasid = names(.)[1] %in% (kb$varid)),
  sjekk_kjonn = . %>% summarise(navn_kjonn = names(.)[2] %in% (kb$varid))
)

# Finner feil og rapporterer hvilken pasient og variabel som gjelder
# for feil i variabelnavn
d %>%
  expose(er_samme_navn) %>%
  get_report()

# sjekk at rekkefølgen på kolonner er lik mellom data og kodebok
er_lik_rekkefolge = data_packs(
  sjekk_rekkefolge = . %>% summarise(rekkefolge_varnavn = identical(names(.), (kb %>% distinct(varid))$varid))
)

# Finner feil og rapporterer hvilken pasient og variabel som gjelder
# for feil i rekkefølge på variabelnavn
d %>%
  expose(er_lik_rekkefolge) %>%
  get_report()
