#' Farger for bruk i figurer
#'
#' @description
#' Inneholder offisielle fargekoder som skal brukes i figurer
#' i årsrapporter.
#'
#' @return En liste med `farger_hoved`, `farger_noyt` og `farger_kontr`.
#' @export
farger_kvalreg = function() {

  # Dei offisielle fargene (som eg ikkje er så glad i)
  # du mener, som INGEN liker.
  colPrim = c("#000059", "#084594", "#2171b5", "#4292c6", "#6baed6", "#c6dbef") # Primærfarge (mørk til lys)
  colNoyt = c("#4D4D4D", "#737373", "#A6A6A6", "#DADADA") # Nøytralfarge
  colKontr = "#FF7260" # Kontrastfarge

  farger = list("farger_hoved" = colPrim, "farger_noyt" = colNoyt, "farger_kontr" = colKontr)
  farger
}

#' Lag mørkare/lysare fargar
#'
#' Gjer ein vektor med fargar mørkare.
#' Brukar CIELAB-fargerommet til utrekningar
#' (i staden for RGB-fargerommet), for
#' betre resultat (meir tilpassa korleis
#' synet vårt fungerer).
#' @param fargar en vektor med farger.
#' @param grad seier kor mykje mørkare fargen skal gjerast (so bruk negative verdiar for å gjera han lysare).
#' @export
farge_morkare = function(fargar, grad = 5) {
  farge_lab = as(colorspace::hex2RGB(fargar), "LAB")
  farge_lab@coords[, 1] = pmax(farge_lab@coords[, 1] - grad, 0)
  farge_rgb = as(farge_lab, "RGB")
  farge_rgb@coords[] = pmax(farge_rgb@coords, 0)
  farge_rgb@coords[] = pmin(farge_rgb@coords, 1)
  colorspace::hex(farge_rgb)
}
