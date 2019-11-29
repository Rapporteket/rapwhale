# Funksjon for skåring av RAND-12

# Overordnet funksjon

skaar_rand12 = function(d, variabelnavn, metode = "farivar_2007_oblique") {
  stopifnot(metode = "farivar_2007_oblique")
  skaaringstabell = tibble(variabel = "variabel", verdi = "verdi", koeffisient = "koeffisient")
  d = endre_variabelnavn(d, variabelnavn)
  sjekk_variabelnavn(d, variabelnavn = skaaringstabell$variabel)
  sjekk_variabelverdier(d, verditabell = skaaringstabell %>% select(variabel, verdi), godta_manglende)
  regn_sumskaar(d, skaaringstabell)
}

# Spesifikke funksjoner

endre_variabelnavn = function(d, variabelnavn) {

}

# Skal sjekke at variabelnavnene i d er identiske til variabelnavnene i skaaringstabell

sjekk_variabelnavn = function(d, variabelnavn) {
  var_mangler = variabelnavn[!(variabelnavn %in% names(d))]
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
