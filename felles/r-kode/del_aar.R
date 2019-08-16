
# Konvertere tid til punkter på en akse:
library(lubridate)

# Funksjonen tar inn en vektor med årstall, en vektor med periode-tilhørighet og et heltall som sier hvor mange deler året skal deles opp i.
# Utdata er en vektor med desimaltall som representerer de ulik periodene i inndata.

del_aar = function(aar, delnummer, antall_deler) {

  # Sjekk at inndata er i riktig format
  stopifnot(length(aar) == length(delnummer))
  if (any(is.na(aar))) {
    warning("År inneholder NA-verdier")
  }
  if (any(is.na(delnummer))) {
    warning("Delnummer inneholder NA-verdier")
  }
  if (antall_deler < 1) {
    stop("Antall deler er < 1")
  }

  (aar + delnummer / antall_deler - (1 / antall_deler) / 2)
}


# Funksjonen lag_periode tar inn en vektor med datoer og et heltall som sier hvor mange deler året skal deles opp i.

lag_periode = function(dato, antall_deler) {

  # Sjekke inndata
  if (!(is.timepoint(dato))) { # TRUE for både Date og POSIXt-format
    stop("Dato-vektor er ikke i Date- eller POSIXt-format")
  }

  # er dato i Date format settes klokkeslett til 12:00 (konverterer dato til POSIXlt)
  if (is.Date(dato)) {
    hour(dato) = 12

    # Hvis inndata ikke er Date må det nødvendigvis være POSIX*t format.
    # Av disse er det lettest å bruke POSIXlt format for å identifisere om det inneholder et klokkeslett.
    # Alle POSIxlt konverteres til POSIXlt før vi sjekker om det eksisterer timer, minutt eller sekunder registrert.
    # Finnes det setter vi tidssone til UTC.
    # Finnes ingen klokkeslett setter vi det til 12:00
  } else {
    dato = as.POSIXlt(dato)
    if (any(unlist(dato[1])[1:3] != 0)) {
      tz(dato) = "UTC"
    } else {
      hour(dato) = 12
    }
  }

  if (any(is.na(dato) == TRUE)) {
    warning("Det finnes NA-verdier i dato-vektor")
  }


  # Finne midtpunkt for alle årstall som er representert i dato.
  endepunkt = 1:antall_deler * (1 / antall_deler)

  unike_ar = sort(unique(year(dato)))
  aar = c(min(unike_ar), endepunkt + rep(unike_ar, each = length(endepunkt)))

  aar_midtpunkt = numeric(length = length(unike_ar) * antall_deler)
  for (i in 1:length(aar_midtpunkt)) {
    aar_midtpunkt[i] = (aar[i] + aar[i + 1]) / 2
  }

  # Finner hvilket intervall hver observasjon tilhører.
  nye_pkt = findInterval(x = decimal_date(dato), vec = aar, rightmost.closed = FALSE, left.open = FALSE) # [2019,2020)

  # Returnerer utverdi
  (aar_midtpunkt[nye_pkt])
}

# TESTER
library(testthat)
test_adr = "H:\\kvalreg\\felles\\R-kode\\del_aar_tester.R"
test_file(test_adr, reporter = "minimal") # *Veldig* kort og konsist samandrag
test_file(test_adr, reporter = "check") # 13-linjes samandrag
test_file(test_adr, reporter = "summary") # Alt, med fint samandrag i starten
test_file(test_adr)
