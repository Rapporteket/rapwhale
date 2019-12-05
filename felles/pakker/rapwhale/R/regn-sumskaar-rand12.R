# Funksjon for skåring av RAND-12

# Overordnet funksjon

skaar_rand12 = function(d, variabelnavn, metode = "farivar_2007_oblique") {
  stopifnot(metode = "farivar_2007_oblique")
  skaaringstabell = tibble(variabel = "variabel", verdi = "verdi", koeffisient = "koeffisient")
  d = endre_variabelnavn(d, skaaringstabell$variabel)
  sjekk_variabelnavn(d, variabelnavn)
  sjekk_variabelverdier(d, verditabell = skaaringstabell %>% select(variabel, verdi), godta_manglende)
  regn_sumskaar(d, skaaringstabell)
}
