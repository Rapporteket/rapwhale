# Anonymiseringsfunksjonfunksjon


library(testthat) # for testing av at funksjonen funker

lag_ano_funk = function(alle_ids, startnr = 101) {
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
