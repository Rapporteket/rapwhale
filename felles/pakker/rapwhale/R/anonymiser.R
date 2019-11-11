#' @importFrom rlang is_empty
NULL

#' Anonymiseringsfunksjonfunksjon
#'
#' Funksjonen lag_ano_funk() tar inn to argument og gjev ut ein
#' anonymiseringsfunksjon (anonymiser_x_utvalg) som fungerer
#' slik at ID-ar (som finst i fleire datasett) får dei same
#' anonymiserte ID-ane på tvers av datasetta.
#'
#' @param x Ein vektor med ID-ar
#' @param startnr Startnummeret for dei anonymiserte id-ane (standardverdi = 101)
#' @export

lag_ano_funk = function(x, startnr = 1001) {
  if (any(is.na(x))) {
    warning("ID-vektoren inneheld NA-verdiar")
  }
  fra = sort(unique(x))
  til = as.integer(sample.int(length(fra)) + startnr - 1)

  ano_funk = function(x_utvalg) {
    if (any(is.na(x_utvalg))) {
      warning("ID-vektoren inneheld NA-verdiar")
    }
    if (!is_empty(na.omit(setdiff(x_utvalg, x)))) {
      warning("ID-vektoren inneheld nye (ukjende) ID-ar")
    }
    til[match(x_utvalg, fra)]
  }
  ano_funk
}

# Hjelpefunksjonen anonymiser() vil vera tidsbesparande dersom ein berre har eitt datasett.
# Funksjonen gjer at ein slepp å kalla anonymiseringsfunksjonen på det same datasettet to gonger.
anonymiser = function(x, startnr = 1001) {
  lag_ano_funk(x, startnr = startnr)(x)
}
