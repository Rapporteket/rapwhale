# Anonymiseringsfunksjonfunksjon

# 1. Eksempeldatasett til testing

# Datasett 1: hofteoperasjoner
pas_ids_hofteop = c(2, 4, 6, 3, 4, 7, 7, 1, 3, 7)

# Datasett 2: hofteoperasjoner med NA-verdier
pas_ids_hofteop_na = c(2, 4, 6, 3, 4, 7, 7, NA, 3, 7)

# Datasett 3: kneoperasjoner (inneholder noen id-er fra datasett 1, samt noen nye)
pas_ids_kneop = c(1, 6, 2, 3, 9, 8, 8, 12)

# Datasett 4: oppfølging (inneholder de fleste id-er fra datasett 1 og datasett 2, samt 1 ny)
pas_ids_oppf = c(1, 3, 2, 5, 4, 9, 7, 8, 11)

# Datasett 1 og 2
pas_ids = c(pas_ids_hofteop, pas_ids_kneop)

anonymiser_reg = function(alle_ids, startnr = 101) {
  anonymiser_id_vektor = function(id_vektor) {
    if (any(is.na(id_vektor))) {
      warning("ID-vektoren inneholder NA-verdier")
    }
    if (any(is.na(match(id_vektor[!is.na(id_vektor)], pas_ids)))) {
      warning("ID-vektoren inneholder nye ID-er")
    }
    fra = sort(unique(id_vektor))
    til = as.integer(sample.int(length(fra)) + startnr - 1)
    til[match(id_vektor, fra)]
  }
  anonymiser_id_vektor
}

anonymiser_mittreg = anonymiser_reg(pas_ids, 101)
anonymiser_mittreg(pas_ids_oppf)
anonymiser_mittreg(pas_ids_hofteop_na)

library(testthat)

# Test som skal gi feilmelding dersom "ut-funksjonen" av lag_ano_funk() blir brukt på ukjente ID-er
test_that("Funksjonen kan kjøres uten å gi feilmeldinger/advarsler", {
  expect_warning(anonymiser_mittreg(pas_ids_oppf), NA)
})

# Test som skal gi feilmelding dersom det finnes NA-verdier blant ID-ene
test_that("Funksjonen kan kjøres uten å gi feilmeldinger/advarsler", {
  expect_warning(anonymiser_mittreg(pas_ids_hofteop_na), NA)
})
