# Funksjon for å hekta variabeltekstar på kategoriske variablar basert på kodebok
#
# Sjå fila kodebok-fyll-kravspek.pdf for detaljert kravspek for implementasjon


# Ikke gjjør om streng-variabler til faktorer automatisk
options(stringsAsFactors = FALSE)


# Nødvendige pakkar
library(tidyverse)
library(testthat)
library(dplyr)
library(magrittr)
library(stringr)


# Eksempel:


# Eksempeldatasett
d = tribble(
  ~pasid, ~kjonn, ~alder, ~med, ~prem,
  101, 2, 18, 3, 2,
  102, 1, 37, 4, 2,
  103, 1, 17, 1, 3
)

# Eksempelkodebok
kb = tribble(
  ~var_id, ~verdi, ~verditekst,
  "kjonn", 1, "mann",
  "kjonn", 2, "kvinne",
  "med", 1, "Antibac",
  "med", 2, "Insulin",
  "med", 3, "Ibux",
  "med", 4, "Globoid",
  "gensp", 1, "misfornogd",
  "gensp", 2, "både og",
  "gensp", 3, "fornøgd"
)

# for å teste funksjonen bruker jeg df
df = d

# Definisjon av funksjon
kb_fyll = function(df, kb, ..., .suffiks = "_tekst") {
  vnamn_d = c("kjonn", "med")
  vnamn_kb = c("kjonn", "med") # hva skulle denne brukes til?

  for (i in seq_along(vnamn_d)) {

    # * Gjer noko med i ... * /* magi */ *

    koder = kb %>%
      filter(var_id %in% vnamn_d[i])

    nytt_namn = str_c(vnamn_d[i], .suffiks)

    df[[nytt_namn]] = factor(df[[vnamn_d[i]]],
      levels = koder$verdi,
      labels = koder$verditekst
    ) %>%
      as.character()

    # fixme: *Fiks rekkefølge*
    df
  }

  df
}

d %>%
  kb_fyll(kb)


# Test at funksjonen fungerer
test_adr = "h:/kvalreg/felles/r-kode/kodebok-fyll-testar.R"
test_file(test_adr, reporter = "minimal") # *Veldig* kort og konsist samandrag
test_file(test_adr, reporter = "check") # 13-linjes samandrag
test_file(test_adr, reporter = "summary") # Alt (tar stor plass viss det er mange mislykka testar)
