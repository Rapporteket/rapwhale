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
#' @details
#' Funksjonen legger til en desimaltallsverdi til `aar`,
#' som blir bestemt av `delnummer` og `antall_deler`.
#' Argumentet `antall_deler` forteller hvor mange intervall man ønsker
#' å dele året inn i.
#' Om `delnummer` er 1,
#' betyr det da at dette
#' `aar`-elementet skal hører til i det første intervallet.
#' Hvert intervall blir representert med midtpunktet i intervallet.
#' Dermed,
#' om man velge å dele året inn i kvartaler,
#' blir det første punktet lik `2019.125`,
#' hvor `aar = 2019`.
#'
#' @note
#' Om man har datoer med måned og dag,
#' ikke kun årstall,
#' kan man eventuelt bruke [tid_til_tidslinje()],
#' som finner `delnummer` automatisk.
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
    stop("delnummer må være verdier i 1:", antall_deler)
  }
  if (any(is.na(aar) | is.na(delnummer))) {
    warning("Inndata inneholder NA-verdier")
  }

  # Returnere koordinater
  aar + (delnummer - 0.5) / antall_deler
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
#' @param antall_deler Heltall som angir antall deler året skal deles inn i.
#'
#' @details
#' Brukes til å dele inn året i `antall_deler` intervaller,
#' og plasserer `dato` i midten av intervallet datoen hører hjemme.
#' Funksjonen håndterer `dato`-argument av klassene `Date` og `POSIXt`,
#' altså dato med og uten klokkeslett.
#' Om en dato gis utenklokkeslett,
#' antar funksjonen klokkeslettet 12:00.
#'
#' @note
#' Om man for eksempel ønsker å dele opp året i kvartaler,
#' vil ikke grensene for hvert kvartal være de vanlige grensene.
#' Den vanlige grensen vil for første kvartal være frem til
#' (men ikke inkludert) 1. april 00:00.
#' Denne funksjonen sier at første kvartal inneholder en fjerdedel av året,
#' altså `365 / 4 = 91,25` dager.
#' Da varer første kvartal frem til (men ikke inkludert) 2. april 07:00.
#' I et skuddår vil grensen for første kvartal her være noe annerledes.
#' Legg også merke til at dersom man ikke manuelt skriver inn et klokkeslett,
#' antas klokkeslettet 12:00.
#' Dermed vil `as.Date("2019-04-02")` bli satt til andre kvartal.
#' I et skuddår varer første kvartal,
#' i følge denne funksjonen,
#' frem til (men ikke inkludert) 1. april 13:00.
#' Dermed vil 1. april 13:00 være en del av andre kvartal i et skuddår,
#' og del av første kvartal i et ikke-skuddår.
#'
#' På tross av denne feilen,
#' fører denne løsningen til stor fleksibilitet i hvordan man ønsker å dele
#' inn året.
#' Det er for eksempel ikke noe problem å dele inn året i 654 deler.
#'
#' @return
#' Returnerer en vektor med desimaltall som representerer koordinatene for de
#' ulike datapunktene i inndata.
#'
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
  }

  if (anyNA(dato)) {
    warning("Det finnes NA-verdier i dato-vektor")
  }

  # Finne midtpunkt for alle årstall som er representert i dato.
  endepunkt = (1:antall_deler) * (1 / antall_deler)

  unike_ar = unique(lubridate::year(dato))
  aar = unique(sort(c(unike_ar, endepunkt + rep(unike_ar, each = length(endepunkt)))))

  aar_midtpunkt = (aar[-length(aar)] + aar[-1]) / 2

  # Finner hvilket intervall hver observasjon tilhører.
  nye_pkt = findInterval(
    x = lubridate::decimal_date(dato),
    vec = aar, rightmost.closed = FALSE,
    left.open = FALSE
  )

  # Returnerer koordinater
  (aar_midtpunkt[nye_pkt])
}
