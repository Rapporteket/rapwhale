
library(lubridate)

# Funksjonen periode_til_tidslinje() tar inn en vektor med årstall, en vektor med periode-tilhørighet og et heltall som sier hvor mange deler året skal deles opp i.
# Utdata er en vektor med desimaltall som representerer koordinatene for de ulike periodene i inndata fordelt på en akse.

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

# Funksjonen tid_til_tidslinje() tar inn en vektor med datoer og et heltall som sier hvor mange deler året skal deles opp i. Tilsvarende som funksjonen
# periode_til_tidslinje returnerer den en vektor med desimaltall som representerer koordinatene for de ulike inndata.
# tid_til_tidslinje inkluderer også klokkeslett, helt ned til sekunder i utregning av koordinat.

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

  if (!(is.timepoint(dato))) { # Både Date og POSIXt-format er timepoint.
    stop("Dato-vektor er ikke i Date- eller POSIXt-format")
  }

  # er dato i Date format settes klokkeslett til 12:00 (konverterer dato til POSIXlt)
  if (is.Date(dato)) {
    hour(dato) = 12
    tz(dato) = "UTC"

    # Se om tidssone er registrert, sett til UTC hvis ikke.
  } else {
    if (is.null(attr(dato, "tzone"))) {
      tz(dato) = "UTC"
    }

    # Hvis format er POSIXlt, men time, minutt og sekund er 0 settes tidspunkt til 1200.
    if (all(unlist(dato[1])[1:3] == 0)) {
      hour(dato) = 12
    }
  }

  if (any(is.na(dato) == TRUE)) {
    warning("Det finnes NA-verdier i dato-vektor")
  }

  # Finne midtpunkt for alle årstall som er representert i dato.
  endepunkt = (1:antall_deler) * (1 / antall_deler)

  unike_ar = sort(unique(year(dato)))
  aar = c(min(unike_ar), endepunkt + rep(unike_ar, each = length(endepunkt)))

  aar_midtpunkt = numeric(length = length(unike_ar) * antall_deler)
  for (i in 1:length(aar_midtpunkt)) {
    aar_midtpunkt[i] = (aar[i] + aar[i + 1]) / 2
  }

  # Finner hvilket intervall hver observasjon tilhører.
  nye_pkt = findInterval(x = decimal_date(dato), vec = aar, rightmost.closed = FALSE, left.open = FALSE) # (2019,2020]

  # Returnerer koordinater
  (aar_midtpunkt[nye_pkt])
}

# TESTER
library(testthat)
test_adr = "H:\\kvalreg\\felles\\R-kode\\del_aar_tester.R"
test_file(test_adr, reporter = "minimal") # *Veldig* kort og konsist samandrag
test_file(test_adr, reporter = "check") # 13-linjes samandrag
test_file(test_adr, reporter = "summary") # Alt, med fint samandrag i starten
test_file(test_adr)
