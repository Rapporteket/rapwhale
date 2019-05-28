# Anonymiseringsfunksjonfunksjon

# Innlasting av pakkar og datasett --------------------------------------------------------

# Nødvendige pakkar
library(tidyverse)



# 1. Eksempeldatasett til testing

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



context("Testar for anonymiseringsfunksjonfunksjonen")

# Testar at lag_ano_funk() fungerar som den skal ------------------------------------------

# Test som skal gje feilmelding dersom det finst NA-verdiar blant ID-ane
test_that("Funksjonen skal gje advarsel ved NA-verdiar", {
  expect_warning(lag_ano_funk(pas_ids_hofteop_na), "ID-vektoren inneheld NA-verdiar")
})

# Testar at funksjonen som kjem ut av lag_ano_funk() fungerar som den skal ----------------

# Test som skal gi advarsel dersom "ut-funksjonen" av lag_ano_funk() blir brukt på ukjende ID-ar
test_that("Funksjonen skal gi advarsel ved ukjende ID-ar (utenom NA-ID-ar)", {
  anonymiser_mittreg = lag_ano_funk(pas_ids)
  expect_warning(anonymiser_mittreg(pas_ids_oppf), "ID-vektoren inneheld nye ID-ar")

  # Skal ikkje gje advarsel dersom alle dei nye verdiane berre er NA-verdiar
  expect_warning(anonymiser_mittreg(c(pas_ids, NA)), "ID-vektoren inneheld NA-verdiar", all = TRUE)
})

# Test som skal gi feilmelding dersom like ID-ar frå ulike skjema får ulike anonymiserte ID-ar
test_that("ID-ar som finst i fleire skjema får like anonymiserte ID-ar på tvers av skjemaa", {
  anonymiser_mittreg = lag_ano_funk(pas_ids)
  expect_identical(
    match(pas_ids_hofteop, pas_ids_kneop),
    match(anonymiser_mittreg(pas_ids_hofteop), anonymiser_mittreg(pas_ids_kneop))
  )
  expect_identical(
    match(pas_ids_kneop, pas_ids_hofteop),
    match(anonymiser_mittreg(pas_ids_kneop), anonymiser_mittreg(pas_ids_hofteop))
  )
  expect_identical(
    match(pas_ids_hofteop_na, pas_ids_kneop),
    match(
      suppressWarnings(anonymiser_mittreg(pas_ids_hofteop_na)),
      anonymiser_mittreg(pas_ids_kneop)
    )
  )
  expect_identical(
    match(pas_ids_kneop, pas_ids_hofteop_na),
    match(
      anonymiser_mittreg(pas_ids_kneop),
      suppressWarnings(anonymiser_mittreg(pas_ids_hofteop_na))
    )
  )
})



# Testar anonymiser() fungerar som den skal -----------------------------------------------

# Kanskje litt mange testar for ein såpass enkel funksjon,
# men no er me iallfall sikra at ting fungerer nokolunde. :)


# Grunnleggjande testar -------------------------------------------------------------------

context("Anonymisering fungerer (på eksempeldata)")

# Eksempeldata
pas_nr = c(3, 5, 1, 1, 6, 3, 7, 3, 10, 12)
pas_dato = as.Date(pas_nr, "2000-01-01")
pas_id = paste0("P", pas_nr)
n_unik = n_distinct(pas_id)

test_that("Funksjonen kan køyrast (utan åtvaringar/feilmeldingar)", {
  expect_warning(anonymiser(pas_id), NA)
  expect_warning(anonymiser(pas_nr), NA)
  expect_warning(anonymiser(pas_dato), NA)
  expect_error(anonymiser(pas_id), NA)
  expect_error(anonymiser(pas_nr), NA)
  expect_error(anonymiser(pas_dato), NA)
})

# Vidare testar me *stort sett* berre tekstversjonen
# av ID-ane. Fungerer det der, fungerer det nok
# for andre typar data òg.

test_that("Gjev ut rett mengd element, og av rett type", {
  expect_length(anonymiser(pas_id), length(pas_id))
  expect_length(unique(anonymiser(pas_id)), n_unik)
  expect_type(anonymiser(pas_id), "integer")
})

# Gjev feilmeldingar ved ugyldige inndata
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


# Grensetilfelle --------------------------------------------------------------------------

context("Grensetilfelle (få/mange data) fungerer")

test_that("Gjev rett svar når det er 2 unike element", {
  x = c("p2", "p1", "p2", "p2")
  res = anonymiser(x)
  expect_true(identical(res, c(1001L, 1002L, 1001L, 1001L)) ||
    identical(res, c(1002L, 1001L, 1002L, 1002L)))
})

test_that("Gjev rett svar når det er 1 unikt element", {
  expect_identical(anonymiser(c("p2", "p2", "p2")), rep(1001L, 3))
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
    as.numeric(fct_inorder(factor(x))),
    as.numeric(fct_inorder(factor(res)))
  )
})


# Handtering av NA-verdiar ----------------------------------------------------------------

# Eventuelle NA-verdiar skal gje åtvaring,
# men skal anonymisert til NA-verdiar
context("Handterer NA-verdiar riktig")

# Eksempeldata
x_med_na = c("p1", "p2", NA, "p1", NA)

test_that("Gjev åtvaring (men ikkje feilmelding) ved NA-verdiar", {
  expect_warning(anonymiser(x_med_na), "ID-vektoren inneheld NA-verdiar", fixed = TRUE)
  expect_error(suppressWarnings(anonymiser(x_med_na)), NA)
})

test_that("NA-verdiar kjem ut som NA-verdiar (og andre verdiar vert OK)", {
  res = suppressWarnings(anonymiser(x_med_na))
  expect_identical(res[c(3, 5)], c(NA_integer_, NA_integer_))
  expect_identical(res[1], res[4])
  expect_identical(is.na(res), c(FALSE, FALSE, TRUE, FALSE, TRUE))
})
