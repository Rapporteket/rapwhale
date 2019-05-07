# Anonymiseringsfunksjonfunksjon

# Innlasting av pakkar og datasett ----------------------------------------

# Nødvendige pakkar
library(tidyverse)



# 1. Eksempeldatasett til testing

# Datasett 1: hofteoperasjoner
pas_ids_hofteop = c(2, 4, 6, 3, 4, 7, 7, 1, 3, 7)

# Datasett 2: hofteoperasjoner med NA-verdier
pas_ids_hofteop_na = c(2, 4, 6, 3, 4, 7, 7, NA, 3, 7)

# Datasett 3: kneoperasjoner (inneholder noen id-er fra datasett 1, samt noen nye)
pas_ids_kneop = c(1, 6, 2, 3, 9, 8, 8, 12, 3, 3)

# Datasett 4: oppfølging (inneholder de fleste id-er fra datasett 1 og datasett 2, samt 1 ny)
pas_ids_oppf = c(1, 3, 2, 5, 4, 9, 7, 8, 11)

# Datasett 1 og 2
pas_ids = c(pas_ids_hofteop, pas_ids_kneop)




context("Testar for anonymiseringsfunksjonfunksjonen")

#--------------- Tester at lag_ano_funk funker som den skal

# Test som skal gi feilmelding dersom det finnes NA-verdier blant ID-ene
test_that("Funksjonen skal gi advarsler ved NA-verdier", {
  expect_warning(lag_ano_funk(pas_ids_hofteop_na), "ID-vektoren inneholder NA-verdier")
})

#--------------- Tester at funksjonen som kommer ut av lag_ano_funk funker som den skal

# Test som skal gi feilmelding dersom "ut-funksjonen" av lag_ano_funk() blir brukt på ukjente ID-er
test_that("Funksjonen skal gi advarsel ved ukjente ID-er (utenom NA-ID-er)", {
  anonymiser_mittreg = lag_ano_funk(pas_ids)
  expect_warning(anonymiser_mittreg(pas_ids_oppf), "ID-vektoren inneholder nye ID-er")

  # Skal ikke gi advarsel dersom alle de nye verdiene bare er NA-verdier
  expect_warning(anonymiser_mittreg(c(pas_ids, NA)), "ID-vektoren inneholder NA-verdier", all = TRUE)
})

# Flere potensielle tester...
# 1. Test at hvis man har matet inn lag_ano_funk med et sett med IDer som kommer fra to forskjellige kilder (fra f. eks to skjema)
#    og man da kjører denne for å få anonymiser_id_vektor (anonymiser_mittreg), og deretter bruker anonymiser_id_vektor på
#    alle kildene, så blir IDene like på tvers av kildene (en test på at funksjonen gjør det den skal)

# Test som skal gi feilmelding dersom like ID-er fra ulike skjemaer får ulike anonymiserte ID-er
test_that("pas_ids_oppf inneholder ikke nye ID-er", {
  expect_that(
    anyNA(match(
      match(anonymiser_mittreg_1(pas_ids_hofteop), anonymiser_mittreg_1(pas_ids_kneop)),
      match(anonymiser_mittreg_1(pas_ids_hofteop), anonymiser_mittreg_1(pas_ids_kneop))
    )),
    equals(FALSE)
  )
})

# Eksempelfunksjoner for å sjekke den den siste testen

# Denne skal passere testen
lag_ano_funk_1 = function(x) {
  function(y) {
    100 * y
  }
}

# Denne skal ikke passere
lag_ano_funk_2 = function(x) {
  function(y) {
    sample(10 * y)
  }
}

anonymiser_mittreg_1 = lag_ano_funk_1(pas_ids)
anonymiser_mittreg_2 = lag_ano_funk_2(pas_ids)




# Testar for anonymiser()-funksjonen
#
# Kanskje litt mange testar for ein såpass enkel funksjon,
# men no er me iallfall sikra at ting fungerer nokolunde. :)


# Grunnleggjande testar ---------------------------------------------------

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


# Grensetilfelle ----------------------------------------------------------

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


# Handtering av NA-verdiar ------------------------------------------------

# Eventuelle NA-verdiar skal gje åtvaring,
# men skal anonymisert til NA-verdiar
context("Handterer NA-verdiar riktig")

# Eksempeldata
x_med_na = c("p1", "p2", NA, "p1", NA)

test_that("Gjev åtvaring (men ikkje feilmelding) ved NA-verdiar", {
  expect_warning(anonymiser(x_med_na), "ID-vektoren inneheld NA-verdiar", fixed = TRUE)
  expect_error(anonymiser(x_med_na), NA)
})

test_that("NA-verdiar kjem ut som NA-verdiar (og andre verdiar vert OK)", {
  res = anonymiser(x_med_na)
  expect_identical(res[c(3, 5)], c(NA_integer_, NA_integer_))
  expect_identical(res[1], res[4])
  expect_identical(is.na(res), c(FALSE, FALSE, TRUE, FALSE, TRUE))
})
