#' Analyser valideringsdatasett
#'
#' @description Tek inn eit datasett og ein samanliknarfunksjon, og returnerer
#'              datasettet med ein ekstra kolonne med TRUE/FALSE for kvar rad
#'              alt etter om verdiane som samanliknast er «like».
#'
#' @param d_vld Datasett som skal validerast.
#' @param samanliknar Samanliknarfunksjon, standard `samanlikn_identisk()`.
#'
#' @details
#' Funksjonen tek inn ein datasett `d_vld` og ein samanliknarfunksjon
#' `samanliknar`. `d_vld` må vera på rett format, dvs.
#' `er_valideringsdatasett_gyldig(d_vld)` må vera TRUE.
#'
#' `samanliknar` skal vera ein funksjon som tek inn tre like lange vektorar,
#' der den fyrste inneheld namna på variablane, og dei to siste er verdiane som
#' skal samanliknast elementvis. Den skal returnera ein logisk vektor med same
#' lengd som vektorane den tek inn, som er TRUE/FALSE alt etter om elementa er
#' «like». Kva som vert rekna som «like» varierar mellom ulike samanliknarfunksjonar.
#'
#' `analyser_valideringsdatasett()` brukar `samanliknar` til å samanlikna
#' `vld_verdi_intern_x` og `vld_verdi_ekstern_x` for kvar rad der `vld_vartype = x`.
#'
#' Til slutt returnerast det opphavlege datasettet `d_vld` med ein ekstra
#' kolonne `vld_verdi_er_like` som er TRUE/FALSE for kvar rad alt etter om
#' verdiane som vart samanlikna er «like».
#'
#' @return Returnerer opphavleg datasett med ein ekstra kolonne `vld_verdi_er_like`
#'         som er TRUE/FALSE for kvar rad alt etter om verdiane som skal
#'         samanliknast på den rada er «like» i følge `samanliknar`.
#' @export
#'
#' @examples
#' d_vld = tibble(
#'   pasid = 101, vld_varnamn = "vekt", vld_vartype = "tal",
#'   vld_verdi_intern_tal = 76,
#'   vld_verdi_ekstern_tal = as.numeric(NA)
#' )
#' analyser_valideringsdatasett(d_vld)
#'
#' d_vld = tibble(
#'   pasid = c(101:104), vld_varnamn = rep("diabetes", 4),
#'   vld_vartype = rep("logisk", 4),
#'   vld_verdi_intern_logisk = c(TRUE, NA, FALSE, TRUE),
#'   vld_verdi_ekstern_logisk = c(TRUE, NA, TRUE, NA)
#' )
#' analyser_valideringsdatasett(d_vld)
analyser_valideringsdatasett = function(d_vld, samanliknar = samanlikn_identisk) {
  if (!er_valideringsdatasett_gyldig(d_vld)) {
    stop("Datasettet er ikkje på rett format")
  }


  # Gjev ut valideringsdatasettet med info i ekstrakolonne om verdiane
  # i kvar rad er «like»

  vartypar = unique(d_vld$vld_vartype)

  if (length(vartypar) == 0) {
    d_vld$vld_verdiar_er_like = logical()
  }

  for (vartype in vartypar) {
    radnr_med_vartype = which(d_vld$vld_vartype == vartype)

    verdi_intern = glue::glue("vld_verdi_intern_{vartype}")
    verdi_ekstern = glue::glue("vld_verdi_ekstern_{vartype}")

    er_like = samanliknar(
      d_vld[[verdi_intern]][radnr_med_vartype],
      d_vld[[verdi_ekstern]][radnr_med_vartype],
      d_vld$vld_varnamn[radnr_med_vartype]
    )

    # Feilmelding viss samanliknar gjev ugyldige verdiar

    if (anyNA(er_like)) {
      stop("NA-verdiar frå samanliknaren")
    }

    if (class(er_like) != "logical") {
      stop("Ikkje logisk vektor frå samanliknaren")
    }

    antal_rader_vartype = nrow(d_vld[radnr_med_vartype, ])
    if (length(er_like) != antal_rader_vartype) {
      stop("Utdata frå samanliknaren har feil lengd")
    }

    d_vld[radnr_med_vartype, "vld_verdiar_er_like"] = er_like
  }
  d_vld
}
