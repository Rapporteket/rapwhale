# Generelle funksjoner for utregning av sumskårer

# Overordnet funksjon
skaar = function(d, variabelnavn, skaaringstabell) {
  d = endre_variabelnavn(d, variabelnavn)
  sjekk_variabelnavn(d, variabelnavn = skaaringstabell$variabel)
  sjekk_variabelverdier(d, verditabell = select(skaaringstabell, variabel, verdi))
  regn_sumskaar(d, skaaringstabell)
}

# Endre variabelnavn
endre_variabelnavn = function(d, variabelnavn) {

}

# Funksjon som sjekker at variabelnavnene i d er identiske til variabelnavnene i skaaringstabell
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

#'
#' @param d Dataramme/tibble med nøyaktig samme variabelnavn som finnes i variabel-kolonnen i verditabell.
#' @param verditabell Dataramme/tibble med kolonne variabel og verdi, som sier hvilke verdier som er gyldige
#'     for hvilke variabler.
#' @param godta_manglende Om NA-verdier skal regnes som gydlige (selv om de ikke er nevnt i verditabell).
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
      radnr = as.integer(radnr_ugyldige),
      variabel = as.character(variabler_ugyldige),
      feilverdi = as.numeric(verdier_ugyldige)
    ),
    radnr
  )
  oversikt_ugyldige
}

# Funksjon som skal ta inn datarammen som finn_ugyldige_verdier() returnerer og presentere
# feilverdiene på en god måte

oppsummer_ugyldige_verdier = function(d_ugyldige) {

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
