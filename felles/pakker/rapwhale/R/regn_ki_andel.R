#' @importFrom rlang sym !!
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by summarise case_when bind_cols groups
NULL
#' Regn ut Kvalitetsindikator - Andel:
#'
#' Funksjon for å regne ut kvalitetsindikatorer for andeler.
#' Tar inn et datasett som inkluderer variablene ki_krit_teller og ki_krit_nevner,
#' og returnerer en summering av datasettet for kvalitetsindikatoren. Utdata inkluderer andel,
#' antall i nevner, antall i teller i tillegg til øvre og nedre konfidensintervall.
#' Hvis inndata er gruppert vil funksjonen regne ut verdiene på gruppenivå.
#'
#' @param d_ki_ind Inndata som inkluderer ki_krit_teller og ki_krit_nevner
#' @param alfa Verdi for å bestemme bredde på konfidensintervall, standardverid er 0.05
#' @export
#' @examples
#' # Eksempeldata
#' d = tibble::tibble(
#'   pasid = 1:10,
#'   sykehus = sample(c("Haukeland", "Haugesund", "Voss"), size = 10, replace = TRUE),
#'   ki_krit_teller = sample(c(TRUE, FALSE), size = 10, replace = TRUE),
#'   ki_krit_nevner = TRUE
#' )
#'
#' # Viser resultat for alle rader i inndata
#' d %>%
#'   aggreger_ki_prop()
#'
#' # Resultat kan grupperes. Her er det gruppert på sykehusnivå, og
#' # konfidensintervallet er justert til 90 % ved å endre på alfa.
#' d %>%
#'   group_by(sykehus) %>%
#'   aggreger_ki_prop(alfa = 0.1)
aggreger_ki_prop = function(d_ki_ind, alfa = 0.05) {
  # Teste inndata
  if (!(is.data.frame(d_ki_ind) && all(hasName(d_ki_ind, c("ki_krit_teller", "ki_krit_nevner"))))) {
    stop("Inndata må være tibble/data.frame med kolonnene 'ki_krit_teller' og 'ki_krit_nevner'")
  }
  if (!(is.logical(d_ki_ind$ki_krit_teller) && is.logical(d_ki_ind$ki_krit_nevner))) {
    stop("Kriterievariablene må være boolsk")
  }
  if (!all(d_ki_ind$ki_krit_nevner %in% c(T, F))) {
    stop("'ki_krit_nevner' må være TRUE eller FALSE")
  }
  if (!all(
    (d_ki_ind$ki_krit_teller %in% c(F, T, NA)) &
      ((d_ki_ind$ki_krit_teller %in% c(F, T) & d_ki_ind$ki_krit_nevner == T) |
        (d_ki_ind$ki_krit_teller %in% c(F, NA) & d_ki_ind$ki_krit_nevner == F))
  )) {
    stop("'ki_krit_teller' må være TRUE eller FALSE hvis 'ki_krit_nevner' er TRUE, og FALSE eller NA hvis 'ki_krit_nevner' er FALSE")
  }
  if (any(lengths(attr(d_ki_ind, "groups")$.rows) == 0)) {
    warning("Det finnes grupper uten observasjoner i grupperingsvariabel")
  }

  # Beregne utdata
  d_sammendrag = d_ki_ind %>%
    summarise(
      ki_teller = as.integer(sum(ki_krit_teller, na.rm = TRUE)),
      ki_nevner = as.integer(sum(ki_krit_nevner)),
      est = ki_teller / ki_nevner
    ) %>%
    select(!!!groups(d_ki_ind), est, ki_teller, ki_nevner)

  # Legg til konfidensintervall
  konfint = binom::binom.wilson(d_sammendrag$ki_teller, d_sammendrag$ki_nevner, conf.level = 1 - alfa)
  d_sammendrag$konfint_nedre = konfint$lower
  d_sammendrag$konfint_ovre = konfint$upper

  # Sørg for at manglende estimat alltid blir returnert som NA
  # (og ikke for eksempel NaN, som vi får ved 0/0)
  d_sammendrag = d_sammendrag %>%
    mutate_at(vars(est, konfint_nedre, konfint_ovre),
      tidyr::replace_na,
      replace = NA
    )

  d_sammendrag
}
