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

sjekk_variabelnavn = function(d, variabelnavn = skaaringstabell$variabel) {

}

sjekk_variabelverdier = function(d,
                                 verditabell = skaaringstabell %>%
                                   select(variabel, verdi),
                                 godta_manglende) {

}

regn_sumskaar = function(d, skaaringstabell) {

}
