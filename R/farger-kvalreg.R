#' Lagre offisielle fargekoder i miljøet
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Funksjonen gir ut en statisk liste med fargekoder for de offisielle fargene
#' som skal brukes i figurer i årsrapporter.
#'
#' @return En liste med `farger_hoved`, `farger_noyt` og `farger_kontr`.
#' @export
#' @examples
#' # les inn fargekoder
#' farger = farger_kvalreg()
#'
#' farger
#'
#' scales::show_col(unlist(farger))
farger_kvalreg = function() {
  # Dei offisielle fargene
  col_prim = c(
    "#000059", "#084594", "#2171b5",
    "#4292c6", "#6baed6", "#c6dbef"
  ) # Primærfarge (mørk til lys)
  col_noyt = c("#4D4D4D", "#737373", "#A6A6A6", "#DADADA") # Nøytralfarge
  col_kontr = "#FF7260" # Kontrastfarge

  farger = list(
    "farger_hoved" = col_prim,
    "farger_noyt" = col_noyt,
    "farger_kontr" = col_kontr
  )
  farger
}

#' Lag mørkare/lysare fargar
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Gjer ein vektor med fargar mørkare.
#' Brukar CIELAB-fargerommet til utrekningar
#' (i staden for RGB-fargerommet), for
#' betre resultat (meir tilpassa korleis
#' synet vårt fungerer).
#'
#' @param fargar ein vektor med fargar.
#' @param grad seier kor mykje mørkare fargen skal gjerast (so bruk negative verdiar for å gjera han lysare).
#'
#' @export
#' @examples
#' # Vektor med fargar
#' fargar = c("#000059", "#084594", "#2171b5")
#'
#' # Lag vektor med fargar mørkare
#' fargar_morkare = farge_morkare(fargar)
#' fargar_morkare
#'
#' # Lag vektor med fargar lysare
#' fargar_lysare = farge_morkare(fargar, grad = -5)
#' fargar_lysare
#'
#' scales::show_col(c(fargar_morkare, fargar, fargar_lysare))
farge_morkare = function(fargar, grad = 5) {
  farge_lab = as(colorspace::hex2RGB(fargar), "LAB")
  farge_lab@coords[, 1] = pmax(farge_lab@coords[, 1] - grad, 0)
  farge_rgb = as(farge_lab, "RGB")
  farge_rgb@coords[] = pmax(farge_rgb@coords, 0)
  farge_rgb@coords[] = pmin(farge_rgb@coords, 1)
  colorspace::hex(farge_rgb)
}
