#' @importFrom rlang sym !!
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by summarise case_when bind_cols
NULL
#' Regn ut Kvalitetsindikator - Andel:
#'
#' Funksjon for å regne ut kvalitetsindikatorer for andeler.
#' Tar inn et datasett på 101-format, og returnerer et estimert resultat
#'
#' @param d_ki_ind Inndata på 101-format
#' @param alpha Verdi for å bestemme bredde på konfidensintervall, default er 0.05
#' @export
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
    )

  # Fiks kolonnerekkefølgje (uelegant!)
  d_sammendrag = bind_cols(
    select(d_sammendrag, -ki_teller, -ki_nevner),
    select(d_sammendrag, ki_teller, ki_nevner)
  )

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
