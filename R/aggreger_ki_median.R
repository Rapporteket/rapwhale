# FIXME - Legge inn støtte for konfidensintervall og oppdater eventuelt 
# dokumentasjon.

#' Regn ut kvalitetsindikator for median
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Regner ut kvalitetsindikator med median av kontinuerlige data,
#' basert på et standard datasett for dette.
#' Gir foreløpig ikke ut konfidensinterval. 
#'
#' @param d_ki_ind
#' Datasett som gitt ut av en standard KI-funksjon
#' for numeriske/kontinuerlige data.
#' Se detaljer nedenfor.
#'
#' @details
#' Inndatasettet må inneholde minst de to variablene
#' `ki_x` og `ki_aktuell`, der `ki_x` er den numeriske variabelen
#' det skal beregnes median for og `ki_aktuell` en
#' logisk variabel som indikerer om observasjonen skal inkluderes i
#' beregningen eller ikke.
#' Se vignetten for kvalitetsindikatorar
#' (\code{vignette("ki-funksjonar", package = "rapwhale")})
#' for mer detaljert informasjon, både om krav til gyldige inndata
#' og om bruk.
#'
#' Funksjonen gir ut aggregert datasett med estimert median og antall 
#' observasjoner hvor `ki_aktuell` er sann.
#' Hvis inndataene er gruppert, blir resultatet regnet ut på gruppenivå,
#' med én rad per gruppe.
#'
#' @return
#' Ugruppert tibble eller `data.frame` (avhengig av inndataene) med følgende
#' kolonner:
#' \item{est}{Kvalitetsindikatoren, dvs. median
#' av `ki_x` for radene som har sanne `ki_aktuell`-verdier
#' (eventuelt per gruppe).}
#' \item{n_aktuell}{Antall observasjoner som er inkludert i
#' beregningen av `est` (antall sanne `ki_aktuell`).}
#' I tillegg vil det være kolonner for alle grupperingsvariablene.
#' @export
#' @examples
#' # Pakke for bruk av tibble-objekt og rør-operatoren
#' library(dplyr)
#'
#' # Eksempeldata
#' d = tibble(
#'   pasid = 1:8,
#'   sykehus = rep(c("Haukeland", "Førde", "Voss"), times = c(3, 2, 3)),
#'   ki_x = c(6.23, 3.05, 12.2, 4.8, 1.2, NA_real_, 1.9, 7.4),
#'   ki_aktuell = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
#' )
#'
#' # Utregnet kvalitetsindikator for hele datasettet
#' aggreger_ki_median(d)
#'
#' # Eventuelt med 90 %-konfidensintervall
#' aggreger_ki_median(d, konf_niva = 0.9)
#'
#' # Gruppert på sykehusnivå
#' d |>
#'   group_by(sykehus) |>
#'   aggreger_ki_median()
#'
#' # Merk at sykehusene ovenfor blir vist i alfabetisk rekkefølge,
#' # siden grupperingsvariabelen «sykehus» var en tekstvariabel.
#' # Hvis du vil ha en annen rekkefølge, gjør den om til faktor først.
#' d = mutate(d, sykehus = factor(sykehus,
#'   levels = c("Haukeland", "Førde", "Voss")
#' ))
#' d |>
#'   group_by(sykehus) |>
#'   aggreger_ki_median()
aggreger_ki_median = function(d_ki_ind) {
  
  if (!(is.data.frame(d_ki_ind) && all(hasName(d_ki_ind, c("ki_x", "ki_aktuell"))))) {
    stop("Inndata må være tibble/data.frame med kolonnene «ki_x» og «ki_aktuell»")
  }
  if (anyNA(d_ki_ind$ki_aktuell) || (!(is.logical(d_ki_ind$ki_aktuell)))) {
    stop("«ki_aktuell» må være TRUE eller FALSE")
  }
  if (!(is.numeric(d_ki_ind$ki_x))) {
    stop("«ki_x» må være numerisk")
  }
  if (any(d_ki_ind$ki_aktuell & is.na(d_ki_ind$ki_x))) {
    stop("«ki_x» må være en numerisk verdi hvis «ki_aktuell» er TRUE")
  }
  # if (!is.numeric(konf_niva) || konf_niva <= 0 || konf_niva >= 1) {
  #   stop("«konf_niva» må være et tall mellom 0 og 1")
  # }
  
  d_ki_ind |>
    summarise(
      est = median(ki_x[ki_aktuell]) |>
        replace_na(NA), # Gjør NaN om til NA
      n_aktuell = sum(ki_aktuell),
      .groups = "drop"
    )
}
