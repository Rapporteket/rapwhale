# Testar for kb_fyll()-funksjonen, jf. kravspekken
context("Kodebok fyll")

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
  "gensp", 1, "misfornøgd",
  "gensp", 2, "både og",
  "gensp", 3, "fornøgd"
)

# Nivåa til dei ulike faktorane (i rett rekkjefølgje)
niv_kjonn = kb %>%
  filter(variabel_id == "kjonn") %>%
  pull(verditekst)
niv_med = kb %>%
  filter(variabel_id == "med") %>%
  pull(verditekst)
niv_gensp = kb %>%
  filter(variabel_id == "gensp") %>%
  pull(verditekst)

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
  d_fylt = d_fylt %>%
    mutate(
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
  d_fylt = d_fylt %>%
    mutate(kjonn_tekst = factor(kjonn_tekst, levels = niv_kjonn))
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
  d_fylt = d_fylt %>%
    mutate(
      kjonn_tekst = factor(kjonn_tekst, levels = niv_kjonn),
      prem_tekst = factor(prem_tekst, levels = niv_gensp)
    )
  expect_identical(d %>% kb_fyll(kb, kjonn, prem = "gensp"), d_fylt)
  expect_identical(d %>% kb_fyll(kb, kjonn, prem = gensp), d_fylt)
})

test_that("Variablar med faktornivå i spesiell rekkjefølgje fungerer", {
  # Ser på kva som skjer viss kodeboka har nivå i ei rekkjefølgje
  # som ikkje er lik rekkjefølgja ein får når ein sorterer
  # (enten som tal eller som tekst). Det er rekkjefølgja i kodeboka
  # som skal brukast.
  d2 = tibble(pasid = 1:5, hei = c(12, 1, 100, 12, 1), alder = seq(10, 50, 10))
  kb2 = tribble(
    ~variabel_id, ~verdi, ~verditekst,
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

test_that("Feilmelding ved bruk av variabel med eksplisitt namn som ikkje finst i datasettet (men i kodeboka)", {
  d2 = d %>%
    dplyr::select(-med)
  d3 = d %>%
    dplyr::select(-med, -kjonn)
  expect_error(d2 %>% kb_fyll(kb, med), "Variabel finst ikkje i datasettet: 'med'")
  expect_error(d3 %>% kb_fyll(kb, med, kjonn), "Variablar finst ikkje i datasettet: 'med', 'kjonn'")
})

test_that("Åtvaring (men NA-verdi) viss datasettet inneheld verdiar som aktuell variabel ikkje har i kodeboka (side 9)", {
  expect_warning(d %>% kb_fyll(kb[-6, ]), "Variabelen 'med' har ugyldig verdi (vart gjort om til NA): '4'", fixed = TRUE)
  expect_warning(d %>% kb_fyll(kb[-c(3, 6), ]), "Variabelen 'med' har ugyldige verdiar (vart gjort om til NA): '1', '4'", fixed = "TRUE") # Sorter dei ugyldige verdiane
  expect_warning(d %>% kb_fyll(kb[-1, ]), "Variabelen 'kjonn' har ugyldig verdi (vart gjort om til NA): '1'", fixed = "TRUE") # Eintal og berre vist éin gong, sjølv om feilen er i fleire rader

  # Sjekk at ein får NA-verdiar der det manglar i kodeboka
  # (men ikkje NA-verdiar der det ikkje manglar, sjølv om det
  # er snakk om same variabel)
  d_fylt = suppressWarnings(d %>% kb_fyll(kb[-6, ]))
  expect_true(is.na(d_fylt$med_tekst[2]))
  expect_equal(as.character(d_fylt$med_tekst[1]), "Ibux")
  expect_equal(as.character(d_fylt$med_tekst[3]), "Antibac")
})

test_that("Ikkje åtvaring eller feilmelding viss datasettet inneheld NA-verdiar", {
  d2 = d
  d2$kjonn[3] = NA

  expect_warning(d2 %>% kb_fyll(kb), NA)
  expect_error(d2 %>% kb_fyll(kb), NA)
})

test_that("Åtvaring (men resultat) viss kodeboka ikkje inneheld *nokon* variablar som finst i datasettet", {
  kb2 = kb
  kb2$variabel_id = paste0("x_", kb2$variabel_id)

  d2 = suppressWarnings(d %>% kb_fyll(kb2))
  expect_identical(d2, d)
  expect_warning(d %>% kb_fyll(kb2), "Kodeboka inneheld ingen variablar som finst i datasettet.")
})

test_that("Feilmelding viss kodeboka ikkje inneheld dei nødvendige kolonnane (side 10)", {
  feilmelding = "Ugyldig kodebok. Obligatoriske kolonnar er 'variabel_id', 'verdi' og 'verditekst'."
  expect_error(d %>% kb_fyll(iris), feilmelding)
  expect_error(d %>% kb_fyll(kb[-1]), feilmelding)
  expect_error(d %>% kb_fyll(kb[-2]), feilmelding)
  expect_error(d %>% kb_fyll(kb[-3]), feilmelding)
  expect_error(d %>% kb_fyll(kb[3:1]), NA) # Godta forskjellig rekkjefølgje
  expect_error(d %>% kb_fyll(cbind(x = 1:nrow(kb), kb[3:1], y = 1:nrow(kb))), NA) # Godta ekstrakolonnar
})

test_that("NA-verdiar i kodeboka vert oppdaga", {
  kb2 = kb
  kb2$variabel_id[2] = NA

  kb3 = kb
  kb3$verdi[2] = NA

  kb4 = kb
  kb4$verditekst[2] = NA

  expect_error(d %>% kb_fyll(kb2), "Ugyldig kodebok. Kolonnen 'variabel_id' har NA-verdi(ar).", fixed = TRUE)
  expect_error(d %>% kb_fyll(kb3), "Ugyldig kodebok. Kolonnen 'verdi' har NA-verdi(ar).", fixed = TRUE)
  expect_error(d %>% kb_fyll(kb4), "Ugyldig kodebok. Kolonnen 'verditekst' har NA-verdi(ar).", fixed = TRUE)
})

test_that("Dupliserte verdiar i kodeboka vert oppdaga", {
  kb2 = kb
  kb2$verdi[5] = kb2$verdi[4] # Duplisert verdi
  d2 = filter(d, med != 3)

  kb3 = kb
  kb3$verditekst[5] = kb3$verditekst[4] # Duplisert verditekst

  kb4 = kb
  kb4$verditekst[9] = kb4$verditekst[1] # *Ikkje* duplisert verditekst (er frå annan variabel)

  kb5 = kb
  kb5$foo = 3 # *Ikkje* duplisert verdi (er frå kolonne som ikkje er 'verdi' eller 'verditekst')

  expect_error(d2 %>% kb_fyll(kb2), "Ugyldig kodebok. Variabelen 'med' har dupliserte verdiar i kolonnen 'verdi'.")
  expect_error(d %>% kb_fyll(kb3), "Ugyldig kodebok. Variabelen 'med' har dupliserte verdiar i kolonnen 'verditekst'.")
  expect_error(d %>% kb_fyll(kb4), NA)
  expect_error(d %>% kb_fyll(kb5), NA)
})

test_that("NA-verdiar i 'verdi' vert godtekne så lenge dei berre er blant ikkje-kategoriske variablar", {
  # Legg til eit par ikkje-kategoriske variablar først i kodeboka
  kb2 = dplyr::bind_rows(kb[1:2, ], kb)
  kb2$variabel_id[1:2] = c("pasient_fnr", "alder")
  kb2$verdi[1:2] = NA
  kb2$verditekst[1:2] = NA

  kb2$variabeltype = "kategorisk"
  kb2$variabeltype[1:2] = c("tekst", "numerisk")

  # Skal ikkje gje åtvaring eller feilmelding, sidan
  # NA-verdiane ikkje er blant dei kategoriske variablane
  expect_error(d %>% kb_fyll(kb2), NA)
  expect_warning(d %>% kb_fyll(kb2), NA)
})

test_that("Kodebokkolonnar lagra som faktorar vert oppdaga (og straffa!)", {
  kb2 = kb3 = kb4 = kb5 = kb
  kb2$variabel_id = factor(kb2$variabel_id)
  kb3$verdi = factor(kb3$verdi)
  kb4$verditekst = factor(kb4$verditekst)
  kb5$foo = factor("test") # Ekstrakolonne, som det er uproblematisk at er faktor

  expect_error(d %>% kb_fyll(kb2), "Ugyldig kodebok. Kolonnen 'variabel_id' er faktor.")
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
  d_fylt = d_fylt %>%
    mutate(med_hei = factor(med_hei, levels = niv_med))
  expect_identical(d %>% kb_fyll(kb, med, .suffiks = "_hei"), d_fylt)
})

test_that("Tomt suffiks fungerer (og gjev åtvaring) (side 12)", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~alder, ~med, ~prem,
    101, "kvinne", 18, "Ibux", 2,
    102, "mann", 37, "Globoid", 2,
    103, "mann", 17, "Antibac", 3
  )
  d_fylt = d_fylt %>%
    mutate(
      kjonn = factor(kjonn, levels = niv_kjonn),
      med = factor(med, levels = niv_med)
    )
  expect_identical(suppressWarnings(d %>% kb_fyll(kb, .suffiks = "")), d_fylt)
  expect_warning(d %>% kb_fyll(kb, .suffiks = ""), "Overskriv variabel: 'kjonn'")
  expect_warning(d %>% kb_fyll(kb, .suffiks = ""), "Overskriv variabel: 'med'")
})

test_that("Overskriving av variablar ved *ikkje-tomt* suffiks gjev også åtvaring (men fungerer)", {
  d2 = tribble(
    ~pasid, ~kjonn, ~kjonntest,
    101, 2, "Vellykka",
    102, 1, "Vellykka",
    103, 1, "Mislykka (er kvinne)"
  )
  d2_fylt = tribble(
    ~pasid, ~kjonn, ~kjonntest,
    101, 2, "kvinne",
    102, 1, "mann",
    103, 1, "mann"
  )
  d2_fylt = d2_fylt %>%
    mutate(kjonntest = factor(kjonntest, levels = niv_kjonn))
  expect_warning(d2 %>% kb_fyll(kb, .suffiks = "test"), "Overskriv variabel: 'kjonntest'")
  expect_identical(suppressWarnings(d2 %>% kb_fyll(kb, .suffiks = "test")), d2_fylt)
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
  d_fylt = d_fylt %>%
    mutate(
      kjonn = factor(kjonn, levels = niv_kjonn),
      med = factor(med, levels = niv_med)
    )
  expect_identical(suppressWarnings(d2 %>% kb_fyll(kb, .suffiks = "")), d_fylt)
})

test_that("Lause variablar som heiter det same som variablar i datasettet fører ikkje til problem", {
  d_fylt = tribble(
    ~pasid, ~kjonn, ~kjonn_tekst, ~alder, ~med, ~prem,
    101, 2, "kvinne", 18, 3, 2,
    102, 1, "mann", 37, 4, 2,
    103, 1, "mann", 17, 1, 3
  )
  d_fylt = d_fylt %>%
    mutate(kjonn_tekst = factor(kjonn_tekst, levels = niv_kjonn))
  d2_fylt = d_fylt %>%
    mutate(prem_tekst = factor(c("både og", "både og", "fornøgd"), levels = niv_gensp))
  kjonn = "med"
  gensp = "med"
  expect_identical(d %>% kb_fyll(kb, kjonn), d_fylt)
  expect_identical(d %>% kb_fyll(kb, prem = "gensp", kjonn), d2_fylt)
  expect_identical(d %>% kb_fyll(kb, prem = "gensp", kjonn), d %>% kb_fyll(kb, prem = gensp, kjonn))
})

test_that("Funksjonen er idempotent", {
  # Kan ikkje testa alt, men nokre få eksempel sikrar det viktigaste
  expect_identical(d %>% kb_fyll(kb), suppressWarnings(d %>% kb_fyll(kb) %>% kb_fyll(kb)))
  expect_identical(d %>% kb_fyll(kb, med), suppressWarnings(d %>% kb_fyll(kb, med) %>% kb_fyll(kb, med)))
  expect_identical(d %>% kb_fyll(kb, med, prem = "gensp"), suppressWarnings(d %>% kb_fyll(kb, med, prem = "gensp") %>% kb_fyll(kb, med, prem = "gensp")))
})
