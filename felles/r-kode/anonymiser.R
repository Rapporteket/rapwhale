# Anonymiseringsfunksjonfunksjon

# Funksjonen lag_ano_funk() tar inn to argument. Det første (x) er ein vektor med ID-ar og det andre
# (startnr) er startnummeret for dei anonymiserte id-ane (standardverdi = 101). Funksjonen gjev
# ut ein anonymiseringsfunksjon som fungerer slik at ID-ar (som finst i fleire datasett) får dei
# same anonymiserte ID-ane på tvers av datasetta.
lag_ano_funk = function(x, startnr = 101) {
  # Funksjonen gjev advarsel dersom x inneheld NA-verdiar
  if (any(is.na(x))) {
    warning("ID-vektoren inneheld NA-verdiar")
  }
  # Dei unike ID-ane i x blir sortert i stigande rekkefølgje
  fra = sort(unique(x))
  # Det blir så trekt tilfeldige verdiar frå intervallet [startnr, startnr + antall unike ID-ar i x]
  til = as.integer(sample.int(length(fra)) + startnr - 1)

  # Funksjonen anonymiser_x_utvalg() tar inn eitt argument (x_utvalg) som er det datasettet (f.eks.
  # eit skjema frå eit register) som skal anonymiserast.
  anonymiser_x_utvalg = function(x_utvalg) {
    # Funksjonen skal gje advarsel dersom x_utvalg inneheld NA-verdiar
    if (any(is.na(x_utvalg))) {
      warning("ID-vektoren inneheld NA-verdiar")
    }
    # Funksjonen skal gje advarsel dersom x_utvalg innheld ID-ar som ikkje finst i x (desse verdiane
    # vil ikkje bli anonymiserte)
    if (any(is.na(match(x_utvalg[!is.na(x_utvalg)], x)))) {
      warning("ID-vektoren inneheld nye ID-ar")
    }
    # match(x_utvalg, fra) gjev ut ved kva indeksar i "fra" ID-ane i "x_utvalg" blir "matcha" ved.
    # til[match(x_utvalg, fra)] gjev ut (dei anonymiserte) ID-ane til "til" ved indeksane som
    # match(x_utvalg, fra) gjev ut.
    nye_ids = til[match(x_utvalg, fra)]
    # Funksjonen (anonymiser_x_utvalg()) returnerer dei anonymiserte ID-ane (nye_ids)
    nye_ids
  }
  # Funksjonen (lag_ano_funk()) returnerer anonymiseringsfunksjonen
  anonymiser_x_utvalg
}

# Hjelpefunksjonen anonymiser() vil vera tidsbesparande dersom ein berre har eitt datasett. Funksjonen
# gjer at ein slepp å kalla anonymiseringsfunksjonen på det same datasettet to gonger.
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
