# Generelle funksjoner for utregning av sumskårer


# Spesifikke funksjoner

endre_variabelnavn = function(d, variabelnavn) {

}

# Skal sjekke at variabelnavnene i d er identiske til variabelnavnene i skaaringstabell

sjekk_variabelnavn = function(d, variabelnavn) {
  var_mangler = unique(variabelnavn[!(variabelnavn %in% names(d))])
  var_mangler_tekst = paste0(var_mangler, collapse = ", ")
  if (length(var_mangler) > 0) {
    stop("Mangler kolonner: ", var_mangler_tekst)
  }
}

sjekk_variabelverdier = function(d,
                                 verditabell = skaaringstabell %>%
                                   select(variabel, verdi),
                                 godta_manglende) {

}

regn_sumskaar = function(d, skaaringstabell) {

}
