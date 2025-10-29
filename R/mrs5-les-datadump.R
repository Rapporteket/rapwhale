
# Les datadump for et skjema
mrs5_les_datadump = function(filsti, kb, skjemanavn = NULL) {
#' Konverter tekstvektor til boolsk
#' 
#' @description
#' Tar inn en tekst-vektor og returnerer samme vektor som ekte boolean. 
#' 
#' Foreløpig er vi kun kjent med tilfeller hvor disse verdiene er kodet som 1 og
#' 0, men tidligere har det vært andre versjoner også, så funksjonen må kanskje 
#' utvides for å også dekke disse. 
#' 
#' Konverterer nå: 
#' \itemize{
#'    \item 1, True til TRUE
#'    \item 0, False til FALSE
#'    \item -1, "" til NA
#'    }
#' 
#' @param x Tekstvektor som skal konverteres til boolean. 
#'
#' @returns
#' returnerer `x` hvor variabler spesifisert i kodebok som boolske er 
#' konvertert til ekte boolske variabler. 
#' 
#' @keywords internal
#' 
#' @examples
#' eksempel = c("1", "0", "1", NA)
#' 
#' mrs5_konverter_til_logisk(eksempel)
mrs5_konverter_til_logisk = function(x) {
  
}


