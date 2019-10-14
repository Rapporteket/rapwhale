
#' @importFrom rlang sym !!
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by summarise case_when
NULL
#' Regn ut Kvalitetsindikator - Andel:
#'
#' Funksjon for å regne ut kvalitetsindikatorer for andeler.
#' Tar inn et datasett på 101-format, og returnerer et estimert resultat
#'
#' @param d_ki_ind Inndata på 101-format
#' @param alpha Verdi for å bestemme bredde på konfidensintervall, default er 0.05
#' @export
aggreger_ki_prop = function(d_ki_ind, alpha = 0.05) {

  # "Inndata må ha både 'ki_krit_teller' og 'ki_krit_nevner'"
  if (!(exists("ki_krit_nevner", d_ki_ind) & exists("ki_krit_teller", d_ki_ind))) {
    stop("Inndata må ha både 'ki_krit_teller' og 'ki_krit_nevner'")
  }

  if (is.numeric(c(d_ki_ind$ki_krit_nevner, d_ki_ind$ki_krit_teller))) {
    "Kriterievariablene må være tall"
  }

  # Sjekk at kriterie-variabler er kun gyldige verdier (0,1,NA):
  if (any(!d_ki_ind$ki_krit_teller %in% c(0, 1, NA)) |
    any(!d_ki_ind$ki_krit_nevner %in% c(0, 1))) {
    stop("kriterie variablene inneholder ugyldige verdier")
  }

  # Sjekke at det ikke finnes observasjoner hvor teller_krit er oppfylt når nevner_krit ikke er oppfylt.
  stopifnot(all(d_ki_ind$ki_krit_teller <= d_ki_ind$ki_krit_nevner |
    is.na(d_ki_ind$ki_krit_teller)))

  # regn ki:
  d_agg_prop =
    d_ki_ind %>%
    summarise(
      est = case_when(
        sum(ki_krit_nevner) == 0 ~ 0,
        TRUE ~ sum(ki_krit_teller, na.rm = TRUE) / sum(ki_krit_nevner)
      ),
      konfint_nedre = est - qnorm(1 - (alpha / 2)) * sqrt(est * (1 - est) / nrow(d_ki_ind)),
      konfint_ovre = est + qnorm(1 - (alpha / 2)) * sqrt(est * (1 - est) / nrow(d_ki_ind)),
      ki_teller = sum(ki_krit_teller, na.rm = TRUE),
      ki_nevner = sum(ki_krit_nevner)
    )

  as_tibble(d_agg_prop)
}
