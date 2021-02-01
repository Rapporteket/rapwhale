#' Valider valideringsdatasett
#'
#' @description Tek inn eit valideringsdatasett og returnerer TRUE eller FALSE
#'              alt etter om det er på rett format eller ikkje.
#'
#' @param d_vld valideringsdatasett som skal sjekkast.
#'
#' @details
#' Funksjonen sjekkar om `d_vld` er på rett format, og returnerer så TRUE
#' eller FALSE.
#'
#' For å vera eit gyldig valideringsdatasett må `d_vld` vera ein data.frame
#' eller tibble og ha tekstkolonnar `vld_varnamn` og `vld_vartype`.
#' `vld_varnamn` og `vld_vartype` kan ikkje ha `NA`-verdiar eller tomme
#' tekststrengar. `vld_vartype` må starta med ein bokstav, og berre innehalda
#' bokstavar og/eller siffer. Kombinasjonen av verdiar til `vld_varnamn` og
#' variablar som ikkje startar med `vld_` skal vera unik for kvar rad. For
#' kvar unike verdi `x` av `vld_vartype` så skal det finnast ein variabel
#' `vld_verdi_intern_x` og `vld_verdi_ekstern_x`. Kolonnar med namn som
#' `vld_tull` er ikkje lov (`vld_` er reservert prefiks).
#'
#' @return TRUE/FALSE, alt etter om inndata er på rett format eller ikkje.
#' @export
#'
#' @examples
#' d_vld = tibble(
#'   pasid = 101, vld_varnamn = "vekt", vld_vartype = "tal",
#'   vld_verdi_intern_tal = 76,
#'   vld_verdi_ekstern_tal = as.numeric(NA)
#' )
#'
#' er_valideringsdatasett_gyldig(d_vld)
er_valideringsdatasett_gyldig = function(d_vld) {
  gyldig = TRUE

  # Oversikt over alle kolonnenamn og kolonnenamn for «vld_»-kolonnane,
  # for seinare bruk
  kolnamn_alle = names(d_vld)
  kolnamn_vld = stringr::str_subset(kolnamn_alle, "^vld_")
  kolnamn_verdikol = stringr::str_subset(kolnamn_vld, "^vld_verdi_")
  kolnamn_indekskol = setdiff(kolnamn_alle, c(kolnamn_verdikol, "vld_vartype"))

  # Inndata må vera data.frame/tibble og må ha tekstkolonnar vld_varnamn og vld_vartype
  if (!(is.data.frame(d_vld) &&
    all(has_name(d_vld, c("vld_varnamn", "vld_vartype")))
  )) {
    return(FALSE)
  }
  if (!(typeof(d_vld$vld_varnamn) == "character" &&
    typeof(d_vld$vld_vartype) == "character")) {
    return(FALSE)
  }

  # vld_varnamn og vld_vartype kan ikkje ha NA-verdiar eller tomme tekstrengar
  if (anyNA(d_vld$vld_varnamn) ||
    anyNA(d_vld$vld_vartype) ||
    any(d_vld$vld_varnamn == "") ||
    any(d_vld$vld_vartype == "")
  ) {
    return(FALSE)
  }

  # vld_vartype må starta med ein bokstav, og berre innehalda bokstavar og/eller siffer
  vartypar_er_gyldige = stringr::str_detect(
    d_vld$vld_vartype,
    "^[[:alpha:]][[:alnum:]]*$"
  )
  if (!all(vartypar_er_gyldige)) {
    return(FALSE)
  }

  # Datasett med kolonnar med namn som «vld_tull» skal reknast som
  # ugyldige («vld_» er reservert prefiks)
  regexp_gyldige_kolnamn = paste0(
    "^vld_varnamn|vld_vartype|",
    "vld_verdi_(intern|ekstern)_[[:alpha:]][[:alnum:]]*$"
  )
  gyldige_kolnamn = stringr::str_detect(kolnamn_vld, regexp_gyldige_kolnamn)
  if (!all(gyldige_kolnamn)) {
    return(FALSE)
  }

  # Viss vld_verdi_intern_x finst, finst også vld_verdi_ekstern_x, og vice versa
  vartypar_i_verdikol = kolnamn_verdikol %>%
    stringr::str_replace("^vld_verdi_(intern|ekstern)_", "") %>%
    unique()
  lag_kolnamn_verdikol = function(vartypar) {
    c(
      paste0("vld_verdi_intern_", vartypar),
      paste0("vld_verdi_ekstern_", vartypar)
    )
  }
  kolnamn_som_skal_finnast = lag_kolnamn_verdikol(vartypar_i_verdikol)
  verdikol_finst = kolnamn_som_skal_finnast %in% kolnamn_verdikol
  if (!all(verdikol_finst)) {
    return(FALSE)
  }

  # vld_verdi_intern_x og vld_verdi_ekstern_x skal vera same type/klasse
  for (vartype in vartypar_i_verdikol) {
    kolnamn_intern = paste0("vld_verdi_intern_", vartype)
    kolnamn_ekstern = paste0("vld_verdi_ekstern_", vartype)
    klasse_intern = class(d_vld[[kolnamn_intern]])
    klasse_ekstern = class(d_vld[[kolnamn_ekstern]])
    if (!identical(klasse_intern, klasse_ekstern)) {
      return(FALSE)
    }
  }

  # For kvar unike verdi x av vld_vartype så skal det finnast ein
  # variabel vld_verdi_intern_x og vld_verdi_ekstern_x
  # (Obs: Dessverre litt kodeduplisering for tidlegare test på kolonnenamn,
  #       men kan ikkje enkelt bruka generell funksjon, sidan
  #       me skal returnera FALSE ved feil, men ikkje returnera elles.)
  vartypar_som_skal_finnast = unique(d_vld$vld_vartype)
  if (length(vartypar_som_skal_finnast) > 0) {
    kolnamn_som_skal_finnast = lag_kolnamn_verdikol(vartypar_som_skal_finnast)
    verdikol_finst = kolnamn_som_skal_finnast %in% kolnamn_verdikol
    if (!all(verdikol_finst)) {
      return(FALSE)
    }
  }

  # Viss vld_vartype = x, så må vld_verdi_intern_y og
  # vld_verdi_ekstern_y vera tomme viss x != y
  for (kolnamn in kolnamn_verdikol) {
    vartype_akt_kol = str_replace(kolnamn, "^vld_verdi_(intern|ekstern)_", "")
    radnr_ikkje_akt_vartype = which(d_vld$vld_vartype != vartype_akt_kol)
    verdiar = d_vld[[kolnamn]][radnr_ikkje_akt_vartype]
    if (!all(is.na(verdiar))) {
      return(FALSE)
    }
  }

  # Kvar kombinasjon av verdiar til indekskolonnane (dvs. til vld_varnamn
  # eller variablar som ikkje startar med «vld_») skal vera unike
  if (any(duplicated(d_vld[kolnamn_indekskol]))) {
    return(FALSE)
  }

  gyldig
}
