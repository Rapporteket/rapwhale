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
  # Skal stoppa med feilmelding dersom inndata ikkje er gyldig
  if (!er_valideringsdatasett_gyldig(d_vld)) {
    stop("Datasettet er ikkje på rett format")
  }


  # Gjev ut valideringsdatasettet med info i ekstrakolonne om verdiane
  # i kvar rad er «like»

  d_vld_med_rekkefolgje = tibble(d_vld, rekkefolgje = c(1:nrow(d_vld)))

  vartypar_som_finst = unique(d_vld$vld_vartype)

  d_vld_verdiar_er_like = d_vld_med_rekkefolgje[c(), ]
  d_vld_verdiar_er_like = tibble(d_vld_verdiar_er_like, vld_verdiar_er_like = logical())

  for (vartype in vartypar_som_finst) {
    d_vld_vartype = filter(d_vld_med_rekkefolgje, vld_vartype == vartype)

    intern = glue::glue("vld_verdi_intern_{vartype}")
    ekstern = glue::glue("vld_verdi_ekstern_{vartype}")

    er_like = samanliknar(
      d_vld_vartype[["vld_varnamn"]],
      d_vld_vartype[[intern]], d_vld_vartype[[ekstern]]
    )

    # Feilmelding viss samanliknar gjev NA-verdiar
    if (anyNA(er_like)) {
      stop("NA-verdiar frå samanliknaren")
    }

    # Feilmelding viss utdata frå samanliknar har feil lengd
    if (length(er_like) != length(d_vld_vartype[[intern]])) {
      stop("Utdata frå samanliknaren har feil lengd")
    }

    d_vld_vartype = tibble(d_vld_vartype, vld_verdiar_er_like = er_like)

    d_vld_verdiar_er_like = add_row(d_vld_verdiar_er_like, d_vld_vartype)
  }

  # Skal ta vare på opphavleg rekkefølgje
  d_vld_verdiar_er_like = d_vld_verdiar_er_like %>%
    arrange(rekkefolgje) %>%
    select(-rekkefolgje)

  # Skal ta vare på opphavleg gruppering
  gruppering = groups(d_vld)
  d_vld_verdiar_er_like = d_vld_verdiar_er_like %>%
    group_by(.dots = gruppering)

  return(d_vld_verdiar_er_like)
}
