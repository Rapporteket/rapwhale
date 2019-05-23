# Anonymiseringsfunksjonfunksjon

lag_ano_funk = function(id_vektor, startnr = 101) {
  if (any(is.na(id_vektor))) {
    warning("ID-vektoren inneheld NA-verdiar")
  }
  fra = sort(unique(id_vektor))
  til = as.integer(sample.int(length(fra)) + startnr - 1)

  anonymiser_id_vektor = function(id_vektor_ny) {
    if (any(is.na(id_vektor_ny))) {
      warning("ID-vektoren inneheld NA-verdiar")
    }
    if (any(is.na(match(id_vektor_ny[!is.na(id_vektor_ny)], id_vektor)))) {
      warning("ID-vektoren inneheld nye ID-ar")
    }
    nye_ids = til[match(id_vektor_ny, fra)]
    nye_ids
  }
  anonymiser_id_vektor
}

anonymiser = function(x, startnr = 1001) {
  lag_ano_funk(x, startnr = startnr)(x)
}

# Test at funksjonen fungerer
# OBS: Må køyra koden for definering av eksempeldatasett
#      (i test_adr) før ein køyrer testane (fixme)
library(testthat)
test_adr = "H:\\kvalreg\\felles\\r-kode\\anonymiser-tester.R"
test_file(test_adr, reporter = "minimal") # *Veldig* kort og konsist samandrag
test_file(test_adr, reporter = "check") # 13-linjes samandrag
test_file(test_adr, reporter = "summary") # Alt, med fint samandrag i starten
test_file(test_adr)
