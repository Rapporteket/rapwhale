# Testar for kb_fyll()-funksjonen, jf. kravspekken

# Nødvendige pakkar
library(tibble)
library(dplyr)
library(testthat)

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


# Side 3
test_that("Enkel bruk utan nokon argument fungerer", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~kjonn_tekst, ~alder, ~med, ~med_tekst, ~prem,
    101, 1, "kvinne", 18, 3, "Ibux", 2,
    102, 2, "mann", 37, 4, "Globoid", 2,
    103, 3, "mann", 17, 1, "Antibac", 3
  )
  expect_identical(d %>% kb_fyll(kb), d_fylt)
})
