# Testar for kb_fyll()-funksjonen, jf. kravspekken


# Innlasting av pakkar og datasett ----------------------------------------

# Nødvendige pakkar
library(tibble)
library(dplyr)
library(magrittr)
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

# Nivåa til dei ulike faktorane (i rett rekkjefølgje)
niv_kjonn = kb %>%
  filter(var_id == "kjonn") %>%
  extract2("verditekst")
niv_med = kb %>%
  filter(var_id == "med") %>%
  extract2("verditekst")
niv_gensp = kb %>%
  filter(var_id == "gensp") %>%
  extract2("verditekst")

# Ymse testar basert på kravspek ------------------------------------------

# Først det grunnleggjande
context("Grunnleggjande funksjonalitet")

# Side 4
test_that("Enkel bruk utan nokon argument fungerer (side 4)", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~kjonn_tekst, ~alder, ~med, ~med_tekst, ~prem,
    101, 2, "kvinne", 18, 3, "Ibux", 2,
    102, 1, "mann", 37, 4, "Globoid", 2,
    103, 1, "mann", 17, 1, "Antibac", 3
  )
  d_fylt %<>% mutate(
    kjonn_tekst = factor(kjonn_tekst, levels = niv_kjonn),
    med_tekst = factor(med_tekst, levels = niv_med)
  )
  expect_identical(d %>% kb_fyll(kb), d_fylt)
})

# Side 5
test_that("Val av variabel å fylla ut fungerer (side 5)", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~kjonn_tekst, ~alder, ~med, ~prem,
    101, 2, "kvinne", 18, 3, 2,
    102, 1, "mann", 37, 4, 2,
    103, 1, "mann", 17, 1, 3
  )
  d_fylt %<>% mutate(kjonn_tekst = factor(kjonn_tekst, levels = niv_kjonn))
  expect_identical(d %>% kb_fyll(kb, kjonn), d_fylt)
})

# Side 6
test_that("Val av variabel som har anna namn i kodeboka fungerer (side 6)", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~kjonn_tekst, ~alder, ~med, ~prem, ~prem_tekst,
    101, 2, "kvinne", 18, 3, 2, "både og",
    102, 1, "mann", 37, 4, 2, "både og",
    103, 1, "mann", 17, 1, 3, "fornøgd"
  )
  d_fylt %<>% mutate(
    kjonn_tekst = factor(kjonn_tekst, levels = niv_kjonn),
    prem_tekst = factor(prem_tekst, levels = niv_gensp)
  )
  expect_identical(d %>% kb_fyll(kb, kjonn, prem = "gensp"), d_fylt)
})

test_that("Variablar med faktornivå i spesiell rekkjefølgje fungerer", {
  # Ser på kva som skjer viss kodeboka har nivå i ei rekkjefølgje
  # som ikkje er lik rekkjefølgja ein får når ein sorterer
  # (enten som tal eller som tekst). Det er rekkjefølgja i kodeboka
  # som skal brukast.
  d2 = tibble(pasid = 1:5, hei = c(12, 1, 100, 12, 1), alder = seq(10, 50, 10))
  kb2 = tribble(
    ~var_id, ~verdi, ~verditekst,
    "hei", 100, "foo",
    "hei", 1, "bar",
    "hei", 12, "baz"
  )
  kb3 = kb2 %>%
    mutate(verdi = as.character(verdi))
  # Merk at kb2$verdi er ulik sort(kb2$verdi) og at begge er ulik sort(kb3$verdi)

  # Dette skal bli resultatet, uavhengig av kva kodebokvariant ein brukar
  d_fylt = d2 %>%
    mutate(hei_tekst = factor(hei, levels = kb2$verdi, labels = kb2$verditekst)) %>%
    dplyr::select(pasid, hei, hei_tekst, alder)
  expect_identical(d2 %>% kb_fyll(kb2), d_fylt)
  expect_identical(d2 %>% kb_fyll(kb3), d_fylt)
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

  # Sjekk at ein får NA-verdiar der det manglar i kodeboka
  # (men ikkje NA-verdiar der det ikkje manglar, sjølv om det
  # er snakk om same variabel)
  d_fylt = d %>%
    kb_fyll(kb[-6, ])
  expect_true(is.na(d_fylt$med_tekst[2]))
  expect_equal(d_fylt$med_tekst[3], "Antibac")
})

test_that("Åtvaring (men resultat) viss kodeboka ikkje inneheld *nokon* variablar som finst i datasettet", {
  kb2 = kb
  kb2$var_id = paste0("x_", kb2$var_id)

  expect_identical(d %>% kb_fyll(kb2, kjonn), d)
  expect_warning(d %>% kb_fyll(kb2), "Kodeboka inneheld ingen variablar som finst i datasettet.")
})

test_that("Feilmelding viss kodeboka ikkje inneheld dei nødvendige kolonnane (side 10)", {
  expect_error(d %>% kb_fyll(iris), "Ugyldig kodebok. Må ha kolonnane 'var_id', 'verdi' og 'verditekst'.")
  expect_error(d %>% kb_fyll(kb[3:1]), NA) # Godta forskjellig rekkjefølgje
  expect_error(d %>% kb_fyll(cbind(x = 1:nrow(kb), kb[3:1], y = 1:nrow(kb))), NA) # Godta ekstrakolonnar
})

test_that("NA-verdiar i kodeboka vert oppdaga", {
  kb2 = kb
  kb2$var_id[2] = NA

  kb3 = kb
  kb3$verdi[2] = NA

  kb4 = kb
  kb4$verditekst[2] = NA

  expect_error(d %>% kb_fyll(kb2), "Ugyldig kodebok. Kolonnen 'var_id' har NA-verdi(ar).")
  expect_error(d %>% kb_fyll(kb3), "Ugyldig kodebok. Kolonnen 'verdi' har NA-verdi(ar).")
  expect_error(d %>% kb_fyll(kb4), "Ugyldig kodebok. Kolonnen 'verditekst' har NA-verdi(ar).")
})

test_that("Dupliserte verdiar i kodeboka vert oppdaga", {
  kb2 = kb
  kb2$verdi[5] = kb2$verdi[4] # Duplisert verdi

  kb3 = kb
  kb3$verditekst[5] = kb3$verditekst[4] # Duplisert verditekst

  kb4 = kb
  kb4$verditekst[9] = kb4$verditekst[1] # *Ikkje* duplisert verditekst (er frå annan variabel)

  kb5 = kb
  kb5$foo = 3 # *Ikkje* duplisert verdi (er frå kolonne som ikkje er 'verdi' eller 'verditekst')

  expect_error(d %>% kb_fyll(kb2), "Ugyldig kodebok. Variabelen 'med' har dupliserte verdiar i kolonnen 'verdi'.")
  expect_error(d %>% kb_fyll(kb3), "Ugyldig kodebok. Variabelen 'med' har dupliserte verdiar i kolonnen 'verditekst'.")
  expect_error(d %>% kb_fyll(kb4), NA)
  expect_error(d %>% kb_fyll(kb5), NA)
})

test_that("Kodebokkolonnar lagra som faktorar vert oppdaga (og straffa!)", {
  kb2 = kb3 = kb4 = kb5 = kb
  kb2$var_id = factor(kb2$var_id)
  kb3$verdi = factor(kb3$verdi)
  kb4$verditekst = factor(kb4$verditekst)
  kb5$foo = factor("test") # Ekstrakolonne, som det er uproblematisk at er faktor

  expect_error(d %>% kb_fyll(kb2), "Ugyldig kodebok. Kolonnen 'var_id' er faktor.")
  expect_error(d %>% kb_fyll(kb3), "Ugyldig kodebok. Kolonnen 'verdi' er faktor.")
  expect_error(d %>% kb_fyll(kb4), "Ugyldig kodebok. Kolonnen 'verditekst' er faktor.")
  expect_error(d %>% kb_fyll(kb5), NA) # Skal ikkje gje åtvaring
})



# Handter suffiks
context("Støtte for sjølvvalt suffiks")

test_that("Val av suffiks fungerer (side 11)", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~alder, ~med, ~med_hei, ~prem,
    101, 2, 18, 3, "Ibux", 2,
    102, 1, 37, 4, "Globoid", 2,
    103, 1, 17, 1, "Antibac", 3
  )
  d_fylt %<>% mutate(med_hei = factor(med_hei, levels = niv_med))
  expect_identical(d %>% kb_fyll(kb, med, .suffiks = "_hei"), d_fylt)
})

test_that("Tomt suffiks fungerer (og gjev åtvaring) (side 12)", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~alder, ~med, ~prem,
    101, "kvinne", 18, "Ibux", 2,
    102, "mann", 37, "Globoid", 2,
    103, "mann", 17, "Antibac", 3
  )
  d_fylt %<>% mutate(
    kjonn = factor(kjonn, levels = niv_kjonn),
    med = factor(med, levels = niv_med)
  )
  expect_identical(d %>% kb_fyll(kb, suffiks = ""), d_fylt)
  expect_warning(d %>% kb_fyll(kb, suffiks = ""), "Overskriv variabel: 'kjonn'")
  expect_warning(d %>% kb_fyll(kb, suffiks = ""), "Overskriv variabel: 'med'")
})

test_that("Overskriving av variablar ved *ikkje-tomt* suffiks gjev også åtvaring (men fungerer)", {
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
  d2_fylt %<>% mutate(kjonntest = factor(kjonntest, levels = niv_kjonn))
  expect_warning(d2 %>% kb_fyll(kb, suffiks = "test"), "Overskriv variabel: 'kjonn'")
  expect_identical(d2 %>% kb_fyll(kb, suffiks = "test"), d2_fylt)
})



# Grensetilfelle og småplukk
context("Grensetilfelle og småplukk")

test_that("Variabelkolonnar som står heilt først eller sist i datasettet fungerer òg", {
  d2 = d %>%
    dplyr::select(kjonn, med)
  d_fylt = tribble(
    ~kjonn, ~med,
    "kvinne", "Ibux",
    "mann", "Globoid",
    "mann", "Antibac"
  )
  d_fylt %<>% mutate(
    kjonn = factor(kjonn, levels = niv_kjonn),
    med = factor(med, levels = niv_med)
  )
  expect_identical(d2 %>% kb_fyll(kb, suffiks = ""), d_fylt)
})

test_that("Lause variablar som heiter det same som variablar i datasettet fører ikkje til problem", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~kjonn_tekst, ~alder, ~med, ~prem,
    101, 2, "kvinne", 18, 3, 2,
    102, 1, "mann", 37, 4, 2,
    103, 1, "mann", 17, 1, 3
  )
  d_fylt %<>% mutate(kjonn_tekst = factor(kjonn_tekst, levels = niv_kjonn))
  kjonn = "med"
  expect_identical(d %>% kb_fyll(kb, kjonn), d_fylt)
})
