# Skript for å hente ut tilfeldige celler fra datasett til validering.

# Nødvendige pakkar
library(tidyverse)
library(haven)

# Mappe på kvalitetstenaren
# Husk at mappen er slik at man har valgt directory fra source file location
grunnmappe = "***FJERNA-ADRESSE***"

# Hent datoen til siste tilgjengelege uttrekk
dato_uttrekk = list.dirs(grunnmappe, recursive = FALSE, full.names = FALSE) %>%
  sort() %>%
  last() %>%
  as.Date()

# Dato for siste registrering er dagen før datoen for
# siste uttrekk, sidan dataa vert oppdaterte kvar natt
dato_sistreg = dato_uttrekk - 1

# Adressa til den siste datafila
mappe = paste0(grunnmappe, dato_uttrekk)
filnamn = "SoReg_08_Datadump_arsrapport.csv"
adresse = paste0(mappe, "\\", filnamn)

# Les inn data
# Manuell (æsj!) spesifikasjon av kolonnetypar
# (fordi me manglar kodebok for denne datadumpen)
kol_typar = cols(
  PasientID = col_integer(),
  Fodselsdato = col_date(format = ""),
  PasientAlder = col_number(),
  PasientKjonn = col_character(),
  OpererendeRESH = col_integer(),
  OperererendeSykehus = col_character(),
  BR_Vekt = col_integer(),
  BR_Hoyde = col_integer(),
  BR_BMI = col_double(),
  ForlopsID = col_integer(),
  OperasjonsID = col_integer(),
  Operasjonsdato = col_date(format = ""),
  OperasjonVekt = col_integer(),
  TidlFedmeOp = col_integer(),
  Operasjonsmetode = col_integer(),
  UtskrivelsesDato = col_date(format = "%Y-%m-%d"),
  BR_BesoksDato = col_date(format = "%Y-%m-%d"),
  LiggeDogn = col_integer(),
  OP_GSAvstPylorus = col_integer(),
  Behandling30Dager = col_integer(),
  KomplAlvorGrad = col_integer(),
  `6U_Vekt` = col_integer(),
  `6U_Hoyde` = col_integer(),
  `6U_RESH` = col_integer(),
  `6U_KontrollType` = col_integer(),
  `1Aar_Vekt` = col_integer(),
  `1Aar_Hoyde` = col_integer(),
  `1Aar_RESH` = col_integer(),
  `1Aar_OppfolgingsType` = col_integer(),
  EttAarBMI = col_double()
)
d_full = read_delim(
  adresse,
  delim = ";",
  na = "null",
  locale = locale(date_format = "%Y-%m-%d", decimal_mark = "."),
  col_types = kol_typar
)

# Feilmelding viss datafila inneheld variablar som me
# ikkje har kolonnespesifikasjon for
manglar_spek = setdiff(names(d_full), names(kol_typar$cols))
if (length(manglar_spek) > 0) {
  stop(
    "Manglar kolonnespesifikasjon for følgjande variablar (rediger kol_typar):\n",
    paste0(manglar_spek, sep = "\n")
  )
}


# alle mangler verdi for OP_GSAvstPylorus?
nrow(d_full)

sum(is.na(d_full$OP_GSAvstPylorus))

# Fjern utrekna variablar og RESH-ID i oppfølgingar
d = d_full %>%
  select(-contains("BMI"), -contains("_RESH"), -OP_GSAvstPylorus)

# Oversikt over namn på indeksvariablar og datavariablar
ind_vars = c(
  "PasientID", "Fodselsdato", "PasientAlder", "PasientKjonn",
  "OpererendeRESH", "OperererendeSykehus", "ForlopsID", "OperasjonsID"
)
data_vars = names(d) %>%
  setdiff(ind_vars)

# Definer hvor mange variabler som skal hentes for hver pasient.
nvars = 10

# Plukk ut tilfeldige datakolonnar for kvar rad og lagra
# namnet på kolonnane i ein eigen variabel.
#
# Dette kan gjerast på mange måtar (slå deg laus). Det viktige er
# berre at me får éi rad for kvar tilfeldig valde celle, rada inneheld
# alle tilhøyrande indeks- og datavariablar og at namnet på
# cella vert lagra i kolonnen «varnamn».
res = by_row(d, function(x) {
  sample(data_vars, nvars)
}, .collate = "rows", .to = "varnamn")

# Legg til info om kva kolonne resultatet skal lagrast i
vartypar = sapply(res[data_vars], class)
if (!all(vartypar %in% c("integer", "numeric", "Date"))) {
  stop("Finst variabel som ikkje er verken tal eller dato.")
}
vartypar = vartypar %>%
  recode(integer = "reg_tal", numeric = "reg_tal", Date = "reg_dato")
res$res_kol = vartypar[match(res$varnamn, names(res[data_vars]))]

# Behold en variabel som indikerer hvilken type variabel hver variabel er
res = res %>%
  mutate(vartype = recode(res_kol, "reg_tal" = "tal", "reg_dato" = "dato"))

# Legg til aktuelle resultatkolonnar,
# med rett variabelklasse (tal, dato &c.)
res = res %>%
  mutate(
    reg_tal = NA_real_, epj_tal = NA_real_,
    reg_dato = NA_real_, epj_dato = NA_real_
  )
class(res$reg_dato) = "Date"
class(res$epj_dato) = "Date"


# Funksjon for å flytta dataverdiane til rett kolonne
# (er laga for å funka på datarammer med éin unik verdi for res_kol)
flytt_resultat = function(df) {
  df[[df$res_kol[1]]] = df[[df$varnamn[1]]]
  df
}

# For kvar variabel, flytt verdiane til rett kolonne
res = res %>%
  group_by(varnamn) %>%
  do(flytt_resultat(.))

# Fjern dei gamle datakolonnane
res = res[!(names(res) %in% data_vars)] %>%
  select(-.row, -res_kol)

# Resultatene skal ryddes slik at det er sortert etter sykehus, tilfeldig rekkefølge på pasientene,
# men rader med samme pasientnummer skal komme etter hverandre, og variablene skal
# stå i en rekkefølge som er lett å bruke.
res = res %>%
  mutate(rekkefolge = factor(PasientID, levels = sample(unique(PasientID)))) %>%
  arrange(OperererendeSykehus, rekkefolge, match(varnamn, data_vars)) %>%
  select(-rekkefolge)
res

# Eksporter data til kvalitetsserveren som en SPSS fil (.sav)
# for hvert sykehus
res %>%
  group_by(OperererendeSykehus) %>%
  do({
    res_adresse = paste0(grunnmappe, "..\\valideringsdata\\", .$OperererendeSykehus[1], ".sav")
    write_sav(., res_adresse)
    tibble()
  })
