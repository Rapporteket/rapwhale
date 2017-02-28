# Testar for kb_fyll()-funksjonen, jf. kravspekken


# Innlasting av pakkar og datasett ----------------------------------------

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


# Ymse testar basert på kravspek ------------------------------------------

# Først det grunnleggjande
context("Grunnleggjande funksjonalitet")

# Side 4
test_that("Enkel bruk utan nokon argument fungerer (side 4)", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~kjonn_tekst, ~alder, ~med, ~med_tekst, ~prem,
    101, 1, "kvinne", 18, 3, "Ibux", 2,
    102, 2, "mann", 37, 4, "Globoid", 2,
    103, 3, "mann", 17, 1, "Antibac", 3
  )
  expect_identical(d %>% kb_fyll(kb), d_fylt)
})

# Side 5
test_that("Val av variabel å fylla ut fungerer (side 5)", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~kjonn_tekst, ~alder, ~med, ~prem,
    101, 1, "kvinne", 18, 3, 2,
    102, 2, "mann", 37, 4, 2,
    103, 3, "mann", 17, 1, 3
  )
  expect_identical(d %>% kb_fyll(kb, kjonn), d_fylt)
})

# Side 6
test_that("Val av variabel som har anna namn i kodeboka fungerer (side 6)", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~kjonn_tekst, ~alder, ~med, ~prem, ~prem_tekst,
    101, 1, "kvinne", 18, 3, 2, "både og",
    102, 2, "mann", 37, 4, 2, "både og",
    103, 3, "mann", 17, 1, 3, "fornøgd"
  )
  expect_identical(d %>% kb_fyll(kb, kjonn, prem = "gensp"), d_fylt)
})


# Gje feilmelding og åtvaringar der det trengst
context("Feilmeldingar og åtvaringar")

test_that("Feilmelding ved bruk av variabel med implisitt namn som ikkje finst i kodeboka (side 7)", {
  expect_error(d %>% kb_fyll(kb, kjonn, test), "Variabel finst ikkje i kodeboka: 'test'")
  expect_error(d %>% kb_fyll(kb, kjonn, test, hei), "Variablar finst ikkje i kodeboka: 'test', 'hei'")
  expect_error(d %>% kb_fyll(kb, test, hei), "Variablar finst ikkje i kodeboka: 'test', 'hei'")
})

test_that("Feilmelding ved bruk av variabel med eksplisitt namn som ikkje finst i kodeboka (side 8)", {
  expect_error(d %>% kb_fyll(kb, kjonn, prem = "gen"), "Variabel finst ikkje i kodeboka: 'gen'")
})

test_that("Åtvaring (men NA-verdi) viss datasettet inneheld verdiar som aktuell variabel ikkje har i kodeboka (side 9)", {
  expect_warning(d %>% kb_fyll(kb[-6, ]), "Variabelen 'med' har ugyldig verdi (vart gjort om til NA): '4'")
  expect_warning(d %>% kb_fyll(kb[-c(3, 6), ]), "Variabelen 'med' har ugyldige verdiar (vart gjort om til NA): '1', '4'")
  expect_warning(d %>% kb_fyll(kb[-1, ]), "Variabelen 'kjonn' har ugyldig verdi (vart gjort om til NA): '1'")
})

test_that("Feilmelding viss kodeboka ikkje inneheld dei nødvendige kolonnane (side 10)", {
  expect_error(d %>% kb_fyll(iris), "Ugyldig kodebok. Må ha kolonnane 'var_id', 'verdi' og 'verditekst'")
  expect_error(d %>% kb_fyll(kb[3:1]), NA) # Godta forskjellig rekkjefølgje
  expect_error(d %>% kb_fyll(cbind(x = 1:nrow(kb), kb[3:1], y = 1:nrow(kb))), NA) # Godta ekstrakolonnar
})


# Handter suffiks
context("Støtte for sjølvvalt suffiks")

test_that("Val av suffiks fungerer (side 11)", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~alder, ~med, ~med_hei, ~prem,
    101, 1, 18, 3, "Ibux", 2,
    102, 2, 37, 4, "Globoid", 2,
    103, 3, 17, 1, "Antibac", 3
  )
  expect_identical(d %>% kb_fyll(kb, med, .suffiks = "_hei"), d_fylt)
})

test_that("Tomt suffiks fungerer (og gjev åtvaring) (side 12)", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~alder, ~med, ~prem,
    101, "kvinne", 18, "Ibux", 2,
    102, "mann", 37, "Globoid", 2,
    103, "mann", 17, "Antibac", 3
  )
  expect_identical(d %>% kb_fyll(kb, suffiks = ""), d_fylt)
  expect_warning(d %>% kb_fyll(kb, suffiks = ""), "Overskriv variabel: 'kjonn'")
  expect_warning(d %>% kb_fyll(kb, suffiks = ""), "Overskriv variabel: 'med'")
})

test_that("Overskriving av variablar ved ikkje-tomt suffiks gjev også åtvaring (men fungerer)", {
  d2 = tribble(
    ~pasid, ~kjonn, ~kjonntest,
    101, 2, "Vellykka",
    102, 1, "Vellyka",
    103, 1, "Mislykka (er kvinne)"
  )
  d2_fylt = tribble(
    ~pasid, ~kjonn, ~kjonntest,
    101, 2, "kvinne",
    102, 1, "mann",
    103, 1, "mann"
  )
  expect_warning(d2 %>% kb_fyll(kb, suffiks = "test"), "Overskriv variabel: 'kjonn'")
  expect_identical(d2 %>% kb_fyll(kb, suffiks = "test"), d2_fylt)
})



# Grensetilfelle og småplukk
context("Grensetilfelle og småplukk")

test_that("Variabelkolonnar som står heilt først eller sist i datasettet fungerer òg", {
  d2 = d %>%
    select(kjonn, med)
  d_fylt = tribble(
    ~kjonn, ~med,
    "kvinne", "Ibux",
    "mann", "Globoid",
    "mann", "Antibac"
  )
  expect_identical(d2 %>% kb_fyll(kb, suffiks = ""), d2_fylt)
})
