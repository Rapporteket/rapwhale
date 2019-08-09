
# Konvertere tid til punkter på en akse:
library(lubridate)
library(tidyverse)


# Funksjonen tar inn en vektor med årstall, en vektor med periode-tilhørighet og et heltall som sier hvor mange deler året skal deles opp i.
del_aar = function(aar, delnummer, antall_deler) {

  # Sjekk at inndata er i riktig format
  stopifnot(length(aar) == length(delnummer))
  if (any(is.na(aar))) {
    warning("År inneholder NA-verdier")
  } else if (any(is.na(delnummer))) {
    warning("Delnummer inneholder NA-verdier")
  }
  if (any(na.omit(aar) != floor(na.omit(aar)))) {
    stop("År er ikke heltall")
  }
  if (antall_deler != floor(antall_deler)) {
    stop("Antall deler er ikke et heltall.")
  }

  # Regne ut intervall-bredde, og midtpunkt for hvert intervall
  bredde = 1 / antall_deler
  endepunkt = c(0:antall_deler * bredde)

  midtpunkt = vector(mode = "numeric", length = (length(endepunkt) - 1))
  for (i in 1:length(endepunkt) - 1) {
    midtpunkt[i] = (endepunkt[i] + endepunkt[i + 1]) / 2
  }

  (aar + midtpunkt[delnummer])
}


# Funksjonen lag_periode tar inn en vektor med datoer og et heltall som sier hvor mange deler året skal deles opp i.
# Den kan ta et vilkårlig antall deler, men vil ved antall = 2, 4 eller 12 benytte seg av del_aar for å ivareta korrekt fordeling av datoer rundt periode-skift. Dette gjøres siden forskjellig antall dager i mnd/kvartal/halvår håndteres bedre av month()- og quarter()-funksjon i lubridate.

lag_periode = function(dato, antall_deler) {
  if (!(is.Date(dato))) {
    stop("Dato-vektor er ikke i Date-format")
  }

  if (antall_deler %in% c(2, 4, 12)) {

    # Halv-års-inndeling:
    if (antall_deler == 2) {
      delnummer = case_when(
        month(dato) <= 6 ~ 1,
        month(dato) > 6 ~ 2,
        TRUE ~ NA_real_
      )
    }

    # Kvartal
    if (antall_deler == 4) {
      delnummer = quarter(dato)

      # Måned
    } else if (antall_deler == 12) {
      delnummer = month(dato)
    }

    del_aar(
      aar = year(dato),
      delnummer = delnummer,
      antall_deler = antall_deler
    )
  } else {
    if (any(is.na(dato) == TRUE)) {
      warning("Det finnes NA-verdier i dato-vektor")
    } # Vil helst bruke tid_1 i disse tilfellene, da det er naturlige avgrensninger for de forskjellige tids-periodene, med ujevnt antall dager i hver periode.

    # Finne intervaller og midtpunkt for hver tids-periode
    bredde = 1 / antall_deler
    endepunkt = c(1:antall_deler * bredde) # mangler 0 for første år.

    # Midtpunkt
    unike_ar = sort(unique(na.omit(year(dato))))
    aar = c(min(unike_ar), endepunkt + rep(unike_ar, each = length(endepunkt)))
    aar_midtpunkt = vector(mode = "numeric", length = (length(aar) - 1))

    for (i in 1:length(aar_midtpunkt)) {
      aar_midtpunkt[i] = (aar[i] + aar[i + 1]) / 2
    }

    # Finner hvilket intervall hver observasjon tilhører.
    nye_pkt = findInterval(x = decimal_date(dato), vec = aar)

    # Returnerer utverdi
    (aar_midtpunkt[nye_pkt])
  }
}

# TESTER
library(testthat)
test_adr = "H:\\R\\R_scripts\\Ny mappe\\tidslinje_tester.R"
test_file(test_adr, reporter = "minimal") # *Veldig* kort og konsist samandrag
test_file(test_adr, reporter = "check") # 13-linjes samandrag
test_file(test_adr, reporter = "summary") # Alt, med fint samandrag i starten
test_file(test_adr)
