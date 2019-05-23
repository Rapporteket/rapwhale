# Anonymiseringsfunksjonfunksjon

lag_ano_funk = function(x, startnr = 101) {
  if (any(is.na(x))) {
    warning("ID-vektoren inneheld NA-verdiar")
  }
  fra = sort(unique(x))
  til = as.integer(sample.int(length(fra)) + startnr - 1)

  anonymiser_x_utvalg = function(x_utvalg) {
    if (any(is.na(x_utvalg))) {
      warning("ID-vektoren inneheld NA-verdiar")
    }
    if (any(is.na(match(x_utvalg[!is.na(x_utvalg)], x)))) {
      warning("ID-vektoren inneheld nye ID-ar")
    }
    nye_ids = til[match(x_utvalg, fra)]
    nye_ids
  }
  anonymiser_x_utvalg
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
