#' Periode til tidslinje
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Tar inn en vektor med årstall, en vektor med periode-tilhørighet og et
#' heltall som sier hvor mange deler året skal deles inn i.
#'
#' @param aar Vektor med årstall (på heltallsformat)
#' @param delnummer Heltallsvektor som angir hvilken periode datapunktet tilhører
#' @param antall_deler Heltall som angir hvor mange like deler året skal deles
#' inn i.
#'
#' @return
#' Gir ut en vektor med desimaltall som representerer kordinatene for de ulike
#' punktene i inndata fordelt på en akse.
#'
#' @export
#'
#' @examples
#' aarstall = c(2014, 2015, 2016, 2017)
#' delnummer = c(1, 2, 2, 4)
#' antall_deler = 4
#' periode_til_tidslinje(aarstall, delnummer, antall_deler)
periode_til_tidslinje = function(aar, delnummer, antall_deler) {

  # Sjekk at inndata er i riktig format
  stopifnot(length(aar) == length(delnummer))
  if (any(na.omit(aar) != floor(na.omit(aar)))) {
    stop("aar må være heltall")
  }
  if (length(antall_deler) != 1) {
    stop("antall_deler må ha lengde 1")
  }
  if (antall_deler < 1) {
    stop("antall_deler må være >= 1")
  }
  if (antall_deler != floor(antall_deler)) {
    stop("antall_deler må være et heltall")
  }
  if (!all(na.omit(unique(delnummer)) %in% 1:antall_deler)) {
    stop(paste0("delnummer må være verdier i 1:", antall_deler))
  }
  if (any(is.na(aar) | is.na(delnummer))) {
    warning("Inndata inneholder NA-verdier")
  }

  # Returnere koordinater
  (aar + delnummer / antall_deler - (1 / antall_deler) / 2)
}

#' Tid til tidslinje
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Tar inn en vektor med datoer og et heltall som angir hvor mange deler året skal
#' deles inn i. Håndterer også klokkeslett, helt ned til sekunder
#' i utregning av koordinat.
#'
#' @param dato Vektor med datoer som det skal regnes koordinat for.
#' @param antall_deler Antall deler året skal deles inn i.
#'
#' @return
#' Returnerer en vektor med desimaltall som representerer koordinatene for de
#' ulike datapunktene i inndata.
#' @export
#'
#' @examples
#' # Eksempel med dato
#' dato = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
#' antall_deler = 4
#' tid_til_tidslinje(dato = dato, antall_deler = antall_deler)
#'
#' # Eksempel med klokkeslett
#' dato_med_klokkeslett = as.POSIXct(c(
#'   "2019-01-01 12:00", "2019-04-01 12:00",
#'   "2019-08-01 12:00", "2019-12-01 12:00"
#' ))
#' antall_deler = 5
#' tid_til_tidslinje(dato = dato_med_klokkeslett, antall_deler = antall_deler)
tid_til_tidslinje = function(dato, antall_deler) {

  # Sjekke inndata
  if (length(antall_deler) != 1) {
    stop("antall_deler må ha lengde 1")
  }
  if (antall_deler < 1) {
    stop("antall_deler må være >= 1")
  }
  if (antall_deler != floor(antall_deler)) {
    stop("antall_deler må være et heltall")
  }

  if (!(lubridate::is.timepoint(dato))) { # Både Date og POSIXt-format er timepoint.
    stop("Dato-vektor er ikke i Date- eller POSIXt-format")
  }

  # er dato i Date format settes klokkeslett til 12:00 (konverterer dato til POSIXlt)
  if (lubridate::is.Date(dato)) {
    lubridate::hour(dato) = 12
    lubridate::tz(dato) = "UTC"

    # Se om tidssone er registrert, sett til UTC hvis ikke.
  } else {
    if (is.null(attr(dato, "tzone"))) {
      lubridate::tz(dato) = "UTC"
    }

    # Hvis format er POSIXlt, men time, minutt og sekund er 0 settes tidspunkt til 1200.
    if (all(unlist(dato[1])[1:3] == 0)) {
      lubridate::hour(dato) = 12
    }
  }

  if (any(is.na(dato) == TRUE)) {
    warning("Det finnes NA-verdier i dato-vektor")
  }

  # Finne midtpunkt for alle årstall som er representert i dato.
  endepunkt = (1:antall_deler) * (1 / antall_deler)

  unike_ar = sort(unique(lubridate::year(dato)))
  aar = c(min(unike_ar), endepunkt + rep(unike_ar, each = length(endepunkt)))

  aar_midtpunkt = numeric(length = length(unike_ar) * antall_deler)
  for (i in 1:length(aar_midtpunkt)) {
    aar_midtpunkt[i] = (aar[i] + aar[i + 1]) / 2
  }

  # Finner hvilket intervall hver observasjon tilhører.
  nye_pkt = findInterval(
    x = lubridate::decimal_date(dato),
    vec = aar, rightmost.closed = FALSE,
    left.open = FALSE
  ) # (2019,2020]

  # Returnerer koordinater
  (aar_midtpunkt[nye_pkt])
}
