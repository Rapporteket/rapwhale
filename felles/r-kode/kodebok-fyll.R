# Funksjon for å hekta variabeltekstar på kategoriske variablar basert på kodebok
#
# Sjå fila kodebok-fyll-kravspek.pdf for detaljert kravspek for implementasjon

# Nødvendige pakkar
library(tidyverse)
library(testthat)

# Definisjon av funksjon
kb_fyll = function(df, kb, ..., .suffiks = "_tekst") {
  df # Byt ut med *magi* ... :)
}

# Test at funksjonen fungerer
test_adr = "h:/kvalreg/felles/r-kode/kodebok-fyll-testar.R"
test_file(test_adr, reporter = "minimal") # *Veldig* kort og konsist samandrag
test_file(test_adr, reporter = "check") # 13-linjes samandrag
test_file(test_adr, reporter = "summary") # Alt (tar stor plass viss det er mange mislykka testar)
