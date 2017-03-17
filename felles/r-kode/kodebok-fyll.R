# Funksjon for å hekta variabeltekstar på kategoriske variablar basert på kodebok
#
# Sjå fila kodebok-fyll-kravspek.pdf for detaljert kravspek for implementasjon

# Nødvendige pakkar
library(tidyverse)
library(testthat)
library(pryr)
library(stringr)
library(purrr)



# Eksempeldata (for testing – fixme: fjern når ein er ferdig med testing) --------

# Eksempeldatasett
d = tribble(
  ~pasid, ~kjonn, ~alder, ~med, ~prem,
  101, 2, 18, 3, 2,
  102, 1, 37, 4, 2,
  103, 1, 17, 1, 3
)

# Eksempelkodebok
kb = tribble(
  ~variabel_id, ~verdi, ~verditekst,
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

  # Namn på variablar som skal fyllast ut
  arg = named_dots(...)
  vnamn_d = names(arg) # Namn i datasettet
  # Viss ein ikkje har valt variablar, bruk alle som finst i kodeboka
  if (length(vnamn_d) == 0) {
    vnamn_d = intersect(names(df), kb$variabel_id)
    vnamn_kb = vnamn_d # Tilsvarande namn i kodeboka
  } else {
    vnamn_kb = map_chr(arg, as.character) # Tilsvarande namn i kodeboka
  }

  # Feilmeldingar eller åtvaringar dersom datasettet og/eller
  # kodeboka og/eller funksjonskallet inneheld feil

  # Viss ein ber om variablar som ikkje finst i kodeboka
  lag_liste = function(x) {
    str_c("'", x, "'", collapse = ", ")
  } # Kjed saman tekststrengar
  berre_kb = setdiff(vnamn_kb, kb$variabel_id)
  n_feil = length(berre_kb)
  if (n_feil > 0) {
    stop(str_c(
      ifelse(n_feil > 1, "Variablar", "Variabel"),
      " finst ikkje i kodeboka: ", lag_liste(berre_kb)
    ))
  }


  # Gå gjennom kvar variabel og legg til verditekstar
  for (i in seq_along(vnamn_d)) {
    # Namn på variabelen (i datasettet)
    vnamn = vnamn_d[i]

    # Delen av kodeboka som gjeld den aktuelle variabelen
    koder = kb %>%
      filter(variabel_id %in% vnamn_kb[i])

    # Det nye namnet på variabelen
    nytt_namn = str_c(vnamn_d[i], .suffiks)

    # Hent verditekster fra kodebok og legg til i datasettet
    df[[nytt_namn]] = factor(df[[vnamn]],
      levels = koder$verdi,
      labels = koder$verditekst
    )

    # Plasser den nye variabelen på rett sted i datasettet
    ind_opp = which(names(df) == vnamn)
    ind_nye = which(names(df) == nytt_namn)
    ind_venstre = c(1:ind_opp, ind_nye) # fixme: legg på unique()
    df = df[c(ind_venstre, setdiff(seq_along(df), ind_venstre))]
  }

  # Returner oppdatert datasett
  df
}

# Kjapt eksempel på bruk
d %>%
  kb_fyll(kb, med, kjonn, prem = gensp)


# Test at funksjonen fungerer
test_adr = "h:/kvalreg/felles/r-kode/kodebok-fyll-testar.R"
test_file(test_adr, reporter = "minimal") # *Veldig* kort og konsist samandrag
test_file(test_adr, reporter = "check") # 13-linjes samandrag
test_file(test_adr, reporter = "summary") # Alt (tar stor plass viss det er mange mislykka testar)
