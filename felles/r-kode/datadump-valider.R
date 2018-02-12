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
  "pasid", "numerisk", NA, NA, TRUE, NA, NA, NA,
  "alder", "numerisk", 18, NA, TRUE, 0, NA, NA,
  "vekt", "numerisk", 45, 200, TRUE, 0, NA, NA,
  "kjonn", "kategorisk", NA, NA, TRUE, NA, 0, "kvinne",
  "kjonn", "kategorisk", NA, NA, TRUE, NA, 1, "mann",
  "frisk", "boolsk", NA, NA, TRUE, NA, NA, NA
)

#---------------------------------------------------------Tester--------------------------------------------------------------

# Dette funker ------------------------------------------------------------

# Kode for regelsett for minimumsverdiar
kb_min = kb %>%
  filter(!is.na(min)) %>%
  select(varid, min) %>%
  rename(varnamn = "varid", gverdi = "min")

eks = kb_min %>%
  pmap(function(varnamn, gverdi) {
    . %>%
      transmute_at(vars(foo = varnamn), rules(min_ok = . >= gverdi))
  }) %>%
  setNames(paste0("min_", kb_min$varnamn))

# eks=list(min_alder = . %>% transmute_at(vars(foo="alder"), rules(min_ok = . >= 18)),
#      min_vekt = . %>% transmute_at(vars(foo="vekt"), rules(min_ok = . >= 45)))
er_innfor_min = cell_packs(eks)
d %>%
  expose(er_innfor_min) %>%
  get_report()

#  ------------------------------------------------------------------------


# sjekker at variabler er innfor min-verdier
er_innfor_min = cell_packs(
  sjekk_min = . %>% transmute_at(vars(age = alder), rules(min_alder = . >= 18))
)

# Finner feil og rapporterer hvilken pasient og variabel som gjelder
# for min-verdier
d %>%
  expose(er_innfor_min) %>%
  get_report() %>%
  left_join((d %>% transmute(id = 1:n(), pasid, alder, vekt)), by = "id") %>%
  print(.validate = FALSE)

# sjekker at variabler er innfor maks-verdier
er_innfor_maks = cell_packs(
  sjekk_maks = . %>% transmute_at(vars(weight = vekt), rules(maks_vekt = . <= 200))
)

# Finner feil og rapporterer hvilken pasient og variabel som gjelder
# for maks-verider
d %>%
  expose(er_innfor_maks) %>%
  get_report() %>%
  left_join((d %>% transmute(id = 1:n(), pasid, vekt)), by = "id") %>%
  print(.validate = FALSE)

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
