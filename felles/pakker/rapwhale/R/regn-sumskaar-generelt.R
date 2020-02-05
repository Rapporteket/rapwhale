#' Overordnet sumskår-funksjon
#'
#' Skal ta inn et datasett, en vektor med variabelnavn og en skåringstabell.
#'
#' @param d Datasett med en kolonne for hvert spørsmål.
#' @param variabelnavn Vektor med gamle og nye variabelnavn. fixme: stemmer dette?
#' @param skaaringstabell Skåringstabell med tre kolonner (variabel, verdi, koeffisient).
#'
#' @return Datasettet som blir tatt inn utvidet med en
#'     eller flere kolonner med utregnet sumskår(er). fixme: skal bare sumskår(er) returneres?
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
#' @param d Datasett med en kolonne for hvert spørsmål.
#' @param variabelnavn Vektor med gamle og nye variabelnavn. fixme: stemmer dette?
#'
#' @return Datasettet som blir tatt inn, med riktige variabelnavn. fixme: stemmer dette?

endre_variabelnavn = function(d, variabelnavn) {

}

#' Funksjon for å sjekke variabelnavn
#'
#' Skal ta inn et datasett og en vektor som inneholder variabelnavnene i skåringstabellen.
#' Funksjonen gir feilmelding hvis datasettet inneholder variabelnavn som ikke finnes i skåringstabellen.
#'
#' @param d Datasett med en kolonne for hvert spørsmål.
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

#' Gir ut oversikt over ugyldige verdier i et datasett.
#'
#' @param d Dataramme/tibble med nøyaktig samme variabelnavn som finnes i variabel-kolonnen i verditabell.
#' @param verditabell Dataramme/tibble med to kolonner (variabel og verdi), som sier hvilke verdier som er gyldige
#'     for hvilke variabler.
#' @param godta_manglende Om NA-verdier skal regnes som gyldige (selv om de ikke er nevnt i verditabell).
#'
#' @return Dataramme med radnummer, variabelnavn og feilverdi for ugyldige verdier. Sortert etter radnummer
#'     og så rekkefølge i d.

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

# Funksjon som skal ta inn datarammen som finn_ugyldige_verdier() returnerer og presentere
# feilverdiene på en god måte

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


################################# Midlertidig kode ################################

# midl_funk = function(d, koblingstabell, godta_manglende) {
#   if (!(is.data.frame(koblingstabell) && all(hasName(koblingstabell, c("variabel", "verdi"))))) {
#     stop("Inndata må være tibble/data.frame og inneholde kolonnene 'variabel' og 'verdi'")
#   }
#
#   # variabelnavn_akt = names(d)[names(d)%in% koblingstabell$variabel]
#   alle_ugyldige = vector()
#   for (var_d in names(d)) {
#     verdier_d = d[[var_d]]
#     verdier_koblingstabell = koblingstabell %>%
#       filter(variabel == !!var_d) %>%
#       pull(verdi)
#
#     if (godta_manglende) {
#       verdier_koblingstabell = append(verdier_koblingstabell, NA)
#     }
#     verdier_d_ugyldige = setdiff(verdier_d, verdier_koblingstabell)
#     if (length(verdier_d_ugyldige) > 0) {
#       alle_ugyldige = append(alle_ugyldige, paste0(var_d, ": ", paste0(verdier_d_ugyldige, collapse = ", ")))
#     }
#   }
#   alle_ugyldige = unique(alle_ugyldige)
#   if(length(alle_ugyldige) > 0) {
#     stop("Ugyldige verdier: ", paste0(alle_ugyldige, collapse = ", "))
#   }
# }
#
#
#
# regn_sumskaar = function(d, skaaringstabell) {
#
# }
