#' Koblingsliste variabeltyper
#' 
#' Hjemmelaget koblingsliste for variabeltyper i de ulike datakildene vi bruker. 
#' Brukes for å konvertere ulike kodebøker til vårt kanoniske format. 
#' Sist Oppdatert 2025-10-28. 
#' 
#' @format ## `koblingsliste_vartyper`
#' En tibble med 7 rader og 5 kolonner: 
#' \describe{
#'    \item{Kanonisk}{Variabeltype slik den er navngitt i kanonisk kodebok}
#'    \item{MRS5}{Variabeltype slik den er navngitt i MRS5-struktur}
#'    \item{OQR}{Variabeltype slik den er navngitt i OQR-struktur}
#'    \item{readr}{Bokstavkode fra readr for å lese inn gitt variabeltype}
#'    \item{Beskrivelse}{Beskrivelse av variabelinnhold}
#'    }
#' @source Hjemmelaget
"koblingsliste_vartyper"
