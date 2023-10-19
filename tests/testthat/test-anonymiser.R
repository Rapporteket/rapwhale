# Testar til anonymiseringsfunksjonfunksjonen


# Testar ------------------------------------------------------------------

context("Anonymisering: Testar for anonymiseringsfunksjonfunksjonen")

# Datasett 1: hofteoperasjonar
pas_ids_hofteop = c(2, 4, 6, 3, 4, 7, 7, 1, 3, 7)

# Datasett 2: hofteoperasjonar med NA-verdiar
pas_ids_hofteop_na = c(2, 4, 6, 3, 4, 7, 7, NA, 3, 7)

# Datasett 3: kneoperasjonar (inneheld nokre ID-ar frå datasett 1, samt nokre nye)
pas_ids_kneop = c(1, 6, 2, 3, 9, 8, 8, 12, 3, 3)

# Datasett 4: oppfølging (inneheld dei fleste ID-ar frå datasett 1 og datasett 2, samt 1 ny)
pas_ids_oppf = c(1, 3, 2, 5, 4, 9, 7, 8, 11)

# Datasett 1 og 2
pas_ids = c(pas_ids_hofteop, pas_ids_kneop)


# Testar at lag_anonymiseringsfunksjon() fungerar som den skal ------------------------------------------

test_that("Skal gje advarsel ved NA-verdiar", {
  expect_warning(lag_anonymiseringsfunksjon(pas_ids_hofteop_na), "ID-vektoren inneheld NA-verdiar")
})

# Testar at funksjonen som kjem ut av lag_anonymiseringsfunksjon() fungerar som den skal ----------------

test_that("Skal gje feilmelding ved ukjende ID-ar (utenom NA-ID-ar)", {
  anonymiser_mittreg = lag_anonymiseringsfunksjon(pas_ids)
  expect_error(anonymiser_mittreg(pas_ids_oppf), "ID-vektoren inneheld nye (ukjende) ID-ar", fixed = TRUE)

  # Utfunksjonen skal *ikkje* gje feilmeldinga "ID-vektoren inneheld nye (ukjende) ID-ar"
  # dersom alle dei nye (ukjende) verdiane berre er NA-verdiar,
  # berre åtvaring om det finst NA-verdiar
  expect_error(suppressWarnings(anonymiser_mittreg(c(pas_ids, NA)), NA))
  expect_warning(anonymiser_mittreg(c(pas_ids, NA)), "ID-vektoren inneheld NA-verdiar", all = TRUE)
})

test_that("ID-ar som finst i fleire skjema får like anonymiserte ID-ar på tvers av skjemaa", {
  anonymiser_mittreg = lag_anonymiseringsfunksjon(pas_ids)
  test_idsamsvar = function(d1, d2) {
    expect_identical(
      match(d1, d2),
      match(
        suppressWarnings(anonymiser_mittreg(d1)),
        suppressWarnings(anonymiser_mittreg(d2))
      )
    )
  }
  test_idsamsvar(pas_ids_hofteop, pas_ids_kneop)
  test_idsamsvar(pas_ids_kneop, pas_ids_hofteop)
  test_idsamsvar(pas_ids_hofteop_na, pas_ids_kneop)
  test_idsamsvar(pas_ids_kneop, pas_ids_hofteop_na)
})


# Testar anonymiser() fungerar som den skal -----------------------------------------------

# Kanskje litt mange testar for ein såpass enkel funksjon,
# men no er me iallfall sikra at ting fungerer nokolunde. :)


# Grunnleggjande testar -------------------------------------------------------------------

context("Anonymisering: Anonymisering fungerer (på eksempeldata)")

# Eksempeldata
pas_nr = c(3, 5, 1, 1, 6, 3, 7, 3, 10, 12)
pas_dato = as.Date(pas_nr, "2000-01-01")
pas_id = paste0("P", pas_nr)
n_unik = dplyr::n_distinct(pas_id)

test_that("Funksjonen kan køyrast (utan åtvaringar/feilmeldingar)", {
  expect_no_warning(anonymiser(pas_id))
  expect_no_warning(anonymiser(pas_nr))
  expect_no_warning(anonymiser(pas_dato))
  expect_no_error(anonymiser(pas_id))
  expect_no_error(anonymiser(pas_nr))
  expect_no_error(anonymiser(pas_dato))
})

# Vidare testar me *stort sett* berre tekstversjonen
# av ID-ane. Fungerer det der, fungerer det nok
# for andre typar data òg.

test_that("Gjev ut rett mengd element, og av rett type", {
  expect_length(anonymiser(pas_id), length(pas_id))
  expect_length(unique(anonymiser(pas_id)), n_unik)
  expect_type(anonymiser(pas_id), "integer")
})

test_that("Gjev feilmelding ved feil datatypar", {
  expect_error(anonymiser(1:5, "5"))
  expect_error(anonymiser(iris))
})

test_that("Anonymiserte ID-ar har startnr, startnr + 1, ...", {
  expect_identical(sort(unique(anonymiser(pas_id))), 1001:(1000 + n_unik))
  expect_identical(sort(unique(anonymiser(pas_id, startnr = 101))), 101:(100 + n_unik))
})

test_that("Alle pasientene får same anonymiserte ID for kvar oppføring dei har", {
  # Sjekkar at det fungerer for både tal, tekst og datoar
  for (x in list(pas_nr, pas_id, pas_dato)) {
    res = anonymiser(x)
    expect_length(unique(res[c(1, 6, 8)]), 1)
    expect_length(unique(res[c(3, 4)]), 1)
  }
})

test_that("Fungerer òg viss inndataelementa er lik dei som inngår i utdata", {
  pas2_tekst = c("1002", "1001", "1001", "1003")
  pas2_tal = as.integer(pas2_tekst)
  ok_anonymisert = function(x) {
    all((!x[1] %in% x[2:4]) & (x[2] == x[3]) & (x[3] != x[4]) &
      (sort(unique(x)) == c(1001, 1002, 1003)))
  }
  # Kan gå bra tilfeldigvis, så sjekkar mange gongar
  walk(1:10, ~ expect_true(ok_anonymisert(anonymiser(pas2_tekst))))
  walk(1:10, ~ expect_true(ok_anonymisert(anonymiser(pas2_tal))))
})

# Test av faktorar der det finst nivå som ikkje er brukte i
# inndatavektoren. Basert på løysingsforslag som var omtrent slikt:
#   x_fak = as.factor(x)
#   nye = sample.int(length(unique(x))) + startnr - 1
#   levels(x_fak) = nye # Gjev feilmelding eller feil svar
#   as.integer(as.character(x_fak))
test_that("Anonymisering av faktorar med ubrukte nivå eller NA fungerer òg", {
  pas_id_ekstra = factor(c("d", "a"),
    levels = c("b", "a", "c", "d")
  )
  pas_id_na = factor(c("d", "a", NA))

  # Skal ikkje gje åtvaringar/feilmeldingar
  expect_no_warning(anonymiser(pas_id_ekstra))
  expect_no_error(anonymiser(pas_id_ekstra))
  expect_no_error(suppressWarnings(anonymiser(pas_id_na)))

  ok_anonymisert_ekstra = function(x) {
    identical(x, c(1001L, 1002L)) ||
      identical(x, c(1002L, 1001L))
  }
  ok_anonymisert_na = function(x) {
    identical(x, c(1001L, 1002L, NA)) ||
      identical(x, c(1002L, 1001L, NA))
  }
  # Kan gå bra tilfeldigvis, så sjekkar mange gongar
  walk(1:10, ~ expect_true(ok_anonymisert_ekstra(
    anonymiser(pas_id_ekstra)
  )))
  walk(1:10, ~ expect_true(ok_anonymisert_na(
    suppressWarnings(anonymiser(pas_id_na))
  )))
})


# Grensetilfelle --------------------------------------------------------------------------

context("Anonymisering: Grensetilfelle (få/mange data) fungerer")

test_that("Gjev rett svar når det er 2 unike element", {
  x = c("p2", "p1", "p2", "p2")
  res = anonymiser(x)
  expect_true(identical(res, c(1001L, 1002L, 1001L, 1001L)) ||
    identical(res, c(1002L, 1001L, 1002L, 1002L)))
})

test_that("Gjev rett svar når det er 1 unikt element", {
  expect_identical(anonymiser(c("p2", "p2", "p2")), rep(1001L, 3))
})

# Test som feila når funksjonen var implementert som
# as.numeric(factor(pas_id, levels = sample(unique(pas_id)))) - 1 + startnr
# (men fungerer viss pass_id er tekst vektor)
test_that("Gjev rett svar når det er 1 unikt element og dette er tal", {
  expect_identical(anonymiser(c(4321, 4321, 4321)), rep(1001L, 3))
})

test_that("Gjev rett svar når det er 0 element", {
  expect_identical(anonymiser(character()), integer())
})

test_that("Gjev rett svar når det er mange (unike/ikkje-unike) element", {
  # Mange unike
  n = 1e4
  x = sample(n, replace = FALSE)
  expect_identical(sort(anonymiser(x)), seq_len(n) + 1000L)

  # Mange, men ikkje-unike
  x = sample(n, replace = TRUE)
  res = anonymiser(x)
  expect_identical(
    as.numeric(forcats::fct_inorder(factor(x))),
    as.numeric(forcats::fct_inorder(factor(res)))
  )
})


# Handtering av NA-verdiar ----------------------------------------------------------------

# Eventuelle NA-verdiar skal gje åtvaring,
# men skal anonymisert til NA-verdiar
context("Anonymisering: Handterer NA-verdiar riktig")

# Eksempeldata
x_med_na = c("p1", "p2", NA, "p1", NA)

test_that("Gjev åtvaring (men ikkje feilmelding) ved NA-verdiar", {
  expect_warning(anonymiser(x_med_na), "ID-vektoren inneheld NA-verdiar", fixed = TRUE)
  expect_no_error(suppressWarnings(anonymiser(x_med_na)))
})

test_that("NA-verdiar kjem ut som NA-verdiar (og andre verdiar vert OK)", {
  res = suppressWarnings(anonymiser(x_med_na))
  expect_identical(res[c(3, 5)], c(NA_integer_, NA_integer_))
  expect_identical(res[1], res[4])
  expect_identical(is.na(res), c(FALSE, FALSE, TRUE, FALSE, TRUE))
})
