
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
  
  assert_that(is.character(x) | is.logical(x),
              msg = "Inndata må være en tekst-vektor.")
  assert_that(all(x %in% c("-1", "0", "1", "False", "True", "TRUE", "FALSE", "", NA)),
              msg = "Forventet inndata er en av: '-1', '0', '1', 'True', 'False', 'TRUE', 'FALSE'.")
  
  x[(x == "-1") | (x == "")] = NA
  (x == "1") | (x == "True") | (x == "TRUE")
  
}

#' Håndtere dato/dato-kl variabler fra MRS5
#' 
#' I MRS finens det ingen skille mellom hva som er en dato og hva som er 
#' dato-klokkeslett. Disse variablene leses inn som tekst og konverteres her
#' til henholdsvis Date eller POSIXct avhengig av om det finnes klokkeslett i 
#' variabelen. 
#' 
#'
#' @param x Variabel som skal konverteres til dato eller dato_klokkeslett. 
#'
#' @returns
#' Variabel konvertert til Date eller POSIXct-format. 
#'
#'@keywords internal
mrs5_håndter_dato_kl = function(x) {
  
  assert_that(is.character(x), msg = "Inndata må være en tekst-vektor.")
  
  if(any(str_detect(x, ":"), na.rm =TRUE)) {
    
    withCallingHandlers(
    parse_date_time(x, orders = "dmy HM"), 
    warning = function(w) {
      msg = conditionMessage(w)
      if (identical(msg, "All formats failed to parse. No formats found.")) {
        stop("Klarte ikke å finne dato-klokkeslett med forventet format: 'YYYY.MM.DD HH:MM'.", call. = FALSE)
    
      }
    }
    )
    } else {
      withCallingHandlers(
      parse_date_time(x, orders = "dmy"), 
      warning = function(w) {
        msg = conditionMessage(w)
        if(identical(msg, "All formats failed to parse. No formats found.")) {
          stop("Klarte ikke å finne dato med forventet format: 'YYYY.MM.DD'.", call. = FALSE)
     
        }
      }
      )
    }
}


