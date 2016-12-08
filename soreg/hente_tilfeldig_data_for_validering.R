
# Skript for å hente ut tilfeldige celler fra datasett til validering.

library(dplyr)
library(magrittr)
library(rapwhale)
library(tidyr)
library(haven)

# fixme! disse pakkene skal være en del av avhengighetene til rapwhale
library(lubridate)
library(readr)


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
filnamn = "SoReg_2_Datadump_arsrapport.csv"
adresse = paste0(mappe, "\\", filnamn)

# Les inn data

d_full = read_delim(
  adresse,
  delim = ";",
  na = "null",
  locale = locale(date_format = "%d.%m.%Y", decimal_mark = "."),
  col_types = cols(
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
    UtskrivelsesDato = col_date(format = "%d.%m.%y 00:00"),
    LiggeDogn = col_integer(),
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
)
# Fjern utrekna variablar og RESH-ID i oppfølgingar
d = d_full %>%
  select(-contains("BMI"), -contains("_RESH"))

# Hent ut indeksvariabler for seg, og alle variabler som potensielt kan bli utvalgt for seg.
ind_vars = c(
  "PasientID", "Fodselsdato", "PasientAlder", "PasientKjonn",
  "OpererendeRESH", "OperererendeSykehus", "ForlopsID", "OperasjonsID"
)
data_vars = names(d) %>%
  setdiff(ind_vars)

nvars = 3
d_ind = d[, ind_vars]
d_data = d[, data_vars]

# Standard, tom dataramme for innfylling
nulltab = tibble(
  varnamn = character(),
  reg_tal = numeric(), epj_tal = numeric(),
  reg_dato = numeric(), epj_dato = numeric()
)
class(nulltab$reg_dato) = "Date"
class(nulltab$epj_dato) = "Date"


res_liste = list()
for (i in 1:nrow(d_ind)) {
  aktvars = sample(data_vars, 3)
  innfylling = nulltab
  for (j in seq_along(aktvars)) {
    aktvar = aktvars[j]

    reskol = switch(class(d_data[[aktvar]]),
      integer = "reg_tal",
      numeric = "reg_tal",
      Date = "reg_dato",
      stop("Ugyldig dataformat!")
    )
    innfylling[j, "varnamn"] = aktvar
    innfylling[j, reskol] = d_data[i, aktvar]
  }
  res_liste[[length(res_liste) + 1]] =
    rep(list(d_ind[i, ]), length(aktvars)) %>%
    bind_rows() %>%
    bind_cols(innfylling)
}
res = bind_rows(res_liste)

res = res %>%
  mutate(rekkefolge = factor(PasientID, levels = sample(unique(PasientID)))) %>%
  arrange(rekkefolge, match(varnamn, data_vars)) %>%
  select(-rekkefolge)
res

res_adresse = paste0(grunnmappe, "..\\valideringsdata\\valideringsdatasett.sav")
write_sav(res, res_adresse)
