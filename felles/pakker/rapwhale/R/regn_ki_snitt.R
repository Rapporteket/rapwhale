#' @importFrom purrr possibly map_dbl pluck
#' @importFrom tidyr replace_na
#' @importFrom dplyr summarise mutate select groups
NULL
#' Regn ut Kvalitetsindikator - Gjennomsnitt:
#'
#' Funksjon for å regne ut kvalitetsindikatorer for gjennomsnitt.
#' Tar inn et datasett som inkluderer variablene ki_x og ki_aktuell,
#' og returnerer en summering av datasettet for kvalitetsindikatoren.
#' ki_x er tallet det skal regnes gjennomsnitt for, og ki_aktuell er en indikator
#' for om pasienten skal være med i utregningen eller ikke. Utdata inkluderer snittverdi,
#' antall aktuelle, og nedre og øvre konfidensintervall.
#' Hvis inndata er gruppert vil funksjonen regne ut verdiene på gruppenivå.
#'
#' @param d_ki_ind Inndata som inkluderer ki_x og ki_aktuell.
#' @param alfa Verdi for å bestemme bredde på konfidensintervall, standardverdi er 0.05
#' @export
#' @examples
#' # Eksempeldata
#' d = tibble::tibble(
#'   pasid = 1:50,
#'   sykehus = sample(c("Haukeland", "Haugesund", "Voss"), size = 50, replace = TRUE),
#'   ki_aktuell = sample(c(TRUE, FALSE), size = 50, replace = TRUE),
#'   ki_x = rnorm(n = 50, mean = 55, sd = 2)
#' )
#'
#' # Viser resultat for alle rader i inndata
#' d %>%
#'   aggreger_ki_snitt()
#'
#' # Resultat kan grupperes. Her er det gruppert på sykehusnivå, og
#' # konfidensintervallet er justert til 90 % ved å endre på alfa.
#' d %>%
#'   group_by(sykehus) %>%
#'   aggreger_ki_snitt(alfa = 0.1)
aggreger_ki_snitt = function(d_ki_ind, alfa = 0.05) {

  # Teste inndata
  if (!(is.data.frame(d_ki_ind) && all(hasName(d_ki_ind, c("ki_x", "ki_aktuell"))))) {
    stop("Inndata må være tibble/data.frame med kolonnene 'ki_x' og 'ki_aktuell'")
  }
  if (any(is.na(d_ki_ind$ki_aktuell)) || (!(is.logical(d_ki_ind$ki_aktuell)))) {
    stop("'ki_aktuell' må være TRUE eller FALSE")
  }
  if (!(is.numeric(d_ki_ind$ki_x))) {
    stop("'ki_x' må være numerisk")
  }
  if (any(d_ki_ind$ki_aktuell & is.na(d_ki_ind$ki_x))) {
    stop("'ki_x' må være en numerisk verdi hvis 'ki_aktuell' er TRUE")
  }

  # Husk å legge inn tester for dette tilfellet.
  konfint = function(x) {
    konfint_robust = possibly(
      ~ t.test(.x)$conf.int,
      otherwise = c(NA_real_, NA_real_)
    )
    konfint_robust(x)
  }

  d_ki_ind %>%
    summarise(
      est = mean(ki_x[ki_aktuell], na.rm = TRUE) %>%
        replace_na(NA), # Gjør NaN om til NA
      konfint = list(konfint(ki_x[ki_aktuell])),
      n_aktuell = sum(ki_aktuell, na.rm = TRUE)
    ) %>%
    mutate(
      konfint_nedre = map_dbl(konfint, pluck, 1),
      konfint_ovre = map_dbl(konfint, pluck, 2)
    ) %>%
    select(!!!groups(d_ki_ind), est, konfint_nedre, konfint_ovre, n_aktuell)
}
