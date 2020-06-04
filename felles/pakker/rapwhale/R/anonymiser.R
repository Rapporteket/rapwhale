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

#' Anonymiseringsfunksjon
#'
#' Hjelpefunksjon for lag_ano_funk() som kan brukes hvis det bare er ett datasett som skal anonymiseres.
#' Funksjonen gjør at du slipper å kalle anonymiseringsfunksjonen på det samme datasettet to ganger.
#' Tar inn en vektor med ID'er og returnerer en vektor med ny numerisk id.
#'
#' @param x vektor med ID'er som skal anonymiseres.
#' @param startnr løpenummer for ny ID. Starnr definerer første nummer i rekken.
#'
#' @export
anonymiser = function(x, startnr = 1001) {
  lag_ano_funk(x, startnr = startnr)(x)
}
