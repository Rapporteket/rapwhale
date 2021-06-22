#' @importFrom rlang is_empty
NULL

#' Lag funksjon for anonymisering av datavektorar
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Lagar ein funksjon som kan brukast til å anonymisera verdiane i ein vektor
#' ved å byta dei ut med tilfeldige tal.
#' Funksjonen ein får ut,
#' er deterministisk, dvs. han gjev alltid ut same utverdi
#' («tilfeldige» tal)
#' for same innverdi.
#'
#' @param x Vektor (av valfri type/klasse) med verdiar som skal anonymiserast.
#' @param startnr Minste verdi for dei tilfeldige tala som skal genererast.
#'
#' @details
#' Denne funksjonen er meint å brukast når ein har verdiar
#' (for eksempel pasient-ID-ar)
#' som inngår i fleire datasett og som skal anonymiserast.
#' Då er det viktig at dei vert anonymiserte til same verdiar
#' på tvers av datasetta.
#'
#' Sjå [anonymiser()] for informasjon om argumenta og detaljar
#' om anonymiseringa (desse er like i denne funksjonen).
#'
#' @note
#' Funksjonen må køyrast på ein vektor som inneheld *alle*
#' verdiane ein i framtida ønskjer å anonymisera.
#' (Viss ein vil anonymisera pasient-ID-ar i eit kvalitetsregister,
#' er dette typisk data frå eit inklusjonsskjema eller eit pasientskjema).
#' Viss det ikkje finst ein slik vektor frå før,
#' må ein laga ein,
#' ved å samla alle verdiane ein vil anonymisera på tvers av datasett.
#'
#' @seealso
#' Bruk heller [anonymiser()] dersom du berre vil anonymisera éin vektor.
#'
#' @return
#' Funksjon som tar inn ein vektor med verdiar
#' (som alle må inngå i `x`)
#' og gjev ut ein heiltalsvektor med tilsvarande anonymiserte verdiar.
#' Han gjev ut feilmelding viss ein prøver
#' å bruka han på «ukjende» verdiar.
#'
#' @export
#'
#' @examples
#' d_operasjon = tibble::tibble(
#'   pasient_id = c(2, 5, 7, 5, 8),
#'   sjukehus = c(
#'     "Bergen", "Oslo", "Stavanger",
#'     "Oslo", "Stavanger"
#'   )
#' )
#' d_oppfolging = tibble::tibble(
#'   pasient_id = c(7, 8),
#'   komplikasjonar = c(TRUE, FALSE)
#' )
#'
#' anonymiser_pasids = lag_ano_funk(d_operajson$pasient_id)
#'
#' d_operasjon_ano = dplyr::mutate(d_operasjon, pasient_id = anonymiser_pasids(pasient_id))
#' d_oppfolging_ano = dplyr::mutate(d_oppfolging, pasient_id = anonymiser_pasids(pasient_id))
#' d_operasjon_ano
#' d_oppfolging_ano
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
      stop("ID-vektoren inneheld nye (ukjende) ID-ar")
    }
    til[match(x_utvalg, fra)]
  }
  ano_funk
}

#' Anonymiser datavektor
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Anonymiserer verdiane i ein vektor ved å byta dei ut med tilfeldige tal.
#'
#' @inheritParams lag_ano_funk
#'
#' @details
#' Alle verdiane i  `x` vert bytte ut med tilfeldige tal frå følgja
#' `startnr`, `startnr + 1`, `startnr + 2`, …,
#' men slik at viss ein verdi finst fleire gongar i `x`,
#' får han alltid same (tilfeldige) utverdi.
#'
#' Innverdiane kan vera av valfri type, for eksempel tekst,
#' tal eller datoar,
#' men utverdiane vil alltid vera heiltal.
#' Eventuelle `NA`-verdiar kjem ut som `NA`-verdiar,
#' men med ei åtvaring.
#'
#' @seealso
#' Viss du vil anonymisera verdiar som inngår i fleire datasett,
#' bruk heller [lag_ano_funk()].
#'
#' @return
#' Heiltalsvektor med anonymiserte verdiar.
#'
#' @export
#'
#' @examples
#' d = tibble::tibble(
#'   pasient_id = c(2, 5, 7, 5, 8),
#'   sjukehus = c(
#'     "Bergen", "Oslo", "Stavanger",
#'     "Oslo", "Stavanger"
#'   )
#' )
#'
#' dplyr::mutate(d,
#'   pasient_id_ano = anonymiser(pasient_id),
#'   sjukehus_ano = anonymiser(sjukehus, startnr = 11)
#' )
anonymiser = function(x, startnr = 1001) {
  lag_ano_funk(x, startnr = startnr)(x)
}
