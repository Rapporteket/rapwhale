#' Overordnet sumskår-funksjon
#'
#' Skal ta inn et datasett, en vektor med variabelnavn og en skåringstabell.
#'
#' @param d Dataramme/tibble med en kolonne for hvert spørsmål.
#' @param variabelnavn Vektor med gamle og nye variabelnavn. fixme: stemmer dette?
#' @param skaaringstabell Skåringstabell med tre kolonner (variabel, verdi, koeffisient).
#'
#' @return \code{d} utvidet med en eller flere kolonner som inneholder sumskår(er).
#'     fixme: skal bare sumskår(er) returneres?
#'     skal skaaringstabell være et argument i denne funksjonen?

skaar = function(d, variabelnavn, skaaringstabell) {
  d = endre_variabelnavn(d, variabelnavn)
  sjekk_variabelnavn(d, variabelnavn = skaaringstabell$variabel)
  sjekk_variabelverdier(d, verditabell = select(skaaringstabell, variabel, verdi))
  regn_sumskaar(d, skaaringstabell)
}

#' Funksjon for å endre variabelnavn
#'
#' Skal ta inn et datasett og en vektor med variabelnavn.
#'
#' @param d Dataramme/tibble med en kolonne for hvert spørsmål.
#' @param variabelnavn Vektor med gamle og nye variabelnavn. fixme: stemmer dette?
#'
#' @return \code{d} med nøyaktig samme variabelnavn som i skåringstabellen. fixme: stemmer dette?

endre_variabelnavn = function(d, variabelnavn) {

}

#' Funksjon for å sjekke variabelnavn
#'
#' Skal ta inn et datasett og en vektor som inneholder variabelnavnene i skåringstabellen.
#' Funksjonen gir feilmelding hvis datasettet inneholder variabelnavn som ikke finnes i skåringstabellen.
#'
#' @param d Dataramme/tibble med en kolonne for hvert spørsmål.
#' @param variabelnavn Vektor som inneholder variabelnavnene i skåringstabellen.
#'
#' @return Skal gi feilmelding hvis \code{d} inneholder variabelnavn som ikke
#'     finnes i skåringstabellen. Sumskår blir da ikke regnet ut. Funksjonen
#'     oppgir også hvilke variabelnavn som er ugyldige.

sjekk_variabelnavn = function(d, variabelnavn) {
  var_mangler = unique(variabelnavn[!(variabelnavn %in% names(d))])
  var_mangler_tekst = paste0(var_mangler, collapse = ", ")
  if (length(var_mangler) > 0) {
    stop("Mangler kolonner: ", var_mangler_tekst)
  }
}

#' Funksjon for å sjekke variabelverdier
#'
#' Skal ta inn et datasett, en verditabell og et argument som bestemmer om NA-verdier skal
#' regnes som gyldige. Funksjonen gir feilmelding hvis verditabellen ikke er satt opp
#' riktig og/eller hvis datasettet inneholder verdier som ikke finnes i verditabellen.
#'
#' @param d Dataramme/tibble med nøyaktig samme variabelnavn som finnes i 'variabel'-kolonnen i \code{verditabell}.
#' @param verditabell Dataramme/tibble med to kolonner ('variabel' og 'verdi'), som sier hvilke verdier
#'     som er gyldige for hvilke variabler.
#' @param godta_manglende Om NA-verdier skal regnes som gyldige (selv om de ikke er nevnt i \code{verditabell}).
#'
#' @return Skal gi feilmelding hvis \code{verditabell} ikke er tibble/data.frame
#'     og/eller mangler en av / begge kolonnene 'variabel' og 'verdi'. Hvis alle verdiene
#'     i \code{d} er gyldige skal funksjonen gi beskjed om dette, og hvis det finnes
#'     ugyldige verdier i \code{d} skal funksjonen gi beskjed om hvilke variabler og
#'     verdier dette gjelder. Sumskår blir da ikke regnet ut.
#'     fixme: sjekk_variabelverdier må utvides slik at den gir beskjed om at alle verdier
#'     er gyldige dersom dette er tilfellet og at den tar hensyn til om manglende verdier
#'     skal bli godtatt eller ikke.

sjekk_variabelverdier = function(d, verditabell, godta_manglende) {
  if (!(is.data.frame(verditabell) &&
    all(hasName(verditabell, c("variabel", "verdi"))))) {
    stop("Inndata må være tibble/data.frame og inneholde kolonnene 'variabel' og 'verdi'")
  }
  d_ugyldige_verdier = finn_ugyldige_verdier(d, verditabell, godta_manglende)
  if (nrow(d_ugyldige_verdier) > 0) {
    oppsummering = oppsummer_ugyldige_verdier(d_ugyldige_verdier)
    stop(oppsummering)
  }
}

#' Funksjon for å finne ugyldige verdier
#'
#' Gir ut oversikt over ugyldige verdier i et datasett.
#'
#' @param d Dataramme/tibble med nøyaktig samme variabelnavn som finnes i 'variabel'-kolonnen i \code{verditabell}.
#' @param verditabell Dataramme/tibble med to kolonner ('variabel' og 'verdi'), som sier hvilke verdier som er gyldige
#'     for hvilke variabler.
#' @param godta_manglende Om NA-verdier skal regnes som gyldige (selv om de ikke er nevnt i \code{verditabell}).
#'
#' @return Dataramme/tibble som inneholder radnummer, variabelnavn og feilverdi for de ugyldige verdiene.
#'     Sortert etter radnummer og så rekkefølge i \code{d}.

finn_ugyldige_verdier = function(d, verditabell, godta_manglende) {
  radnr_ugyldige = integer()
  variabler_ugyldige = character()
  verdier_ugyldige = numeric()
  for (var_d in names(d)) {
    verdier_d = d[[var_d]]
    mulige_verdier = verditabell %>%
      filter(variabel == !!var_d) %>%
      pull(verdi)
    verdier_d_ugyldige = setdiff(verdier_d, mulige_verdier)
    if (length(verdier_d_ugyldige) > 0) {
      indeks_ugyldige = which(verdier_d %in% verdier_d_ugyldige)
      radnr_ugyldige = append(radnr_ugyldige, indeks_ugyldige)
      variabler_ugyldige = append(variabler_ugyldige, rep(var_d, times = length(indeks_ugyldige)))
      verdier_ugyldige = append(verdier_ugyldige, verdier_d[indeks_ugyldige])
    }
  }
  oversikt_ugyldige = arrange(
    tibble(
      radnr = radnr_ugyldige,
      variabel = variabler_ugyldige,
      feilverdi = verdier_ugyldige
    ),
    radnr
  )
  oversikt_ugyldige
}

#' Funksjon for å presentere ugyldige verdier på en god måte
#'
#' Gir ut en oversiktlig fremstilling av ugyldige verdier i et datasett.
#'
#' @param d_ugyldige Dataramme/tibble som inneholder radnummer, variabelnavn og feilverdi for de
#'     ugyldige verdiene. \code{d_ugyldige} blir returnert av funksjonen \code{finn_ugyldige_verdier()}.
#'
#' @return Tekststreng som inneholder variabelnavn og tilhørende feilverdier (sortert alfabetisk
#'     etter variabelnavn). Hvis det ikke finnes ugyldige verdier returneres tekststrengen
#'     "Alle verdiene er gyldige".

oppsummer_ugyldige_verdier = function(d_ugyldige) {
  if (nrow(d_ugyldige) > 0) {
    oppsummert = d_ugyldige %>%
      group_by(variabel) %>%
      summarise(feil_verdier = paste0(feilverdi, collapse = ", ")) %>%
      summarise(feil_variabler_verdier = paste0(variabel, ": ", feil_verdier, collapse = "\n")) %>%
      summarise(feiltekst = paste0("Fant ", nrow(d_ugyldige), " ugyldige verdier:\n", feil_variabler_verdier))
    pull(oppsummert, feiltekst)
  } else {
    "Alle verdiene er gyldige"
  }
}


regn_sumskaar = function(d, skaaringstabell) {

}

################################# Midlertidig kode ################################

#     if (godta_manglende) {
#       verdier_koblingstabell = append(verdier_koblingstabell, NA)
#     }
