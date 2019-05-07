# Anonymiseringsfunksjonfunksjon

lag_ano_funk = function(alle_ids, startnr = 101) {
  if (any(is.na(alle_ids))) {
    warning("ID-vektoren inneholder NA-verdier")
  }
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


# Test at funksjonen fungerer
# OBS: Må køyra koden for definering av eksempeldatasett
#      (i test_adr) før ein køyrer testane (fixme)
library(testthat)
test_adr = "H:\\kvalreg\\felles\\r-kode\\anonymiser-tester.R"
test_file(test_adr, reporter = "minimal") # *Veldig* kort og konsist samandrag
test_file(test_adr, reporter = "check") # 13-linjes samandrag
test_file(test_adr, reporter = "summary") # Alt, med fint samandrag i starten
