# Anonymiseringsfunksjonfunksjon


library(testthat) # for testing av at funksjonen funker

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
#

pas_ids_hofteop
pas_ids_kneop
anonymiser_mittreg = lag_ano_funk(pas_ids)
anonymiser_mittreg(pas_ids_hofteop)
anonymiser_mittreg(pas_ids_kneop)

# Denne skal passere testen
lag_ano_funk = function(x) {
  function(y) {
    100 * y
  }
}

# Denne skal ikke passere
lag_ano_funk = function(x) {
  function(y) {
    sample(10 * y)
  }
}
