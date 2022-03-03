#' Erstatt tall lik 0 med nulltekst
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Erstatter tall som er lik 0 automatisk med nulltekst.
#'
#' @param x Numerisk vektor med minst ett element.
#' @param nulltekst Tekst som skal erstatte `x`-elementer lik 0.
#'
#' @details
#' Hver tallverdi i `x`,
#' med unntak av 0,
#' blir erstattet av tallverdien i
#' teksformat.
#' Verider lik 0 blir erstattet med `nulltekst`.
#'
#' @return
#' Tekstvektor av samme lengde som `x`,
#' hvor verider lik 0 er erstattet av `nulltekst`.
#'
#' @export
#'
#' @examples
#' erstatt_0(c(0, 1, 2), "ingen")
