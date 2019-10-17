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
  if (!(is.numeric(d_ki_ind$ki_krit_teller) && is.numeric(d_ki_ind$ki_krit_nevner))) {
    stop("Kriterievariablene må være tall")
  }
  if (!all(d_ki_ind$ki_krit_nevner %in% c(0, 1))) {
    stop("'ki_krit_nevner' må være 0 eller 1")
  }
  if (!all(
    (d_ki_ind$ki_krit_teller %in% c(0, 1, NA)) &
      ((d_ki_ind$ki_krit_teller %in% c(0, 1) & d_ki_ind$ki_krit_nevner == 1) |
        (d_ki_ind$ki_krit_teller %in% c(0, NA) & d_ki_ind$ki_krit_nevner == 0))
  )) {
    stop("'ki_krit_teller' må være 0 eller 1 hvis 'ki_krit_nevner' er 1, og 0 eller NA hvis 'ki_krit_nevner' er 0")
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
