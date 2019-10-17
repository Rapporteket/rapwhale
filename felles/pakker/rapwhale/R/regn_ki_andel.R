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

  # Beregne konfidensintervall og utdata:
  d_summary = d_ki_ind %>%
    summarise(
      est = as.integer(sum(ki_krit_teller, na.rm = TRUE)) / as.integer(sum(ki_krit_nevner)),
      ki_teller = as.integer(sum(ki_krit_teller, na.rm = TRUE)),
      ki_nevner = as.integer(sum(ki_krit_nevner))
    )
  d_ci = d_summary %>%
    dplyr::group_modify(~ binom::binom.wilson(.x$ki_teller, .x$ki_nevner, alpha = alpha))
  names(d_ci)[5:6] = c("konfint_nedre", "konfint_ovre")
  d_agg_prop = dplyr::bind_cols(d_summary, as_tibble(d_ci)[5:6])

  d_agg_prop = d_agg_prop %>%
    mutate(
      est = dplyr::if_else(ki_nevner == 0, NA_real_, est),
      ki_nevner = dplyr::if_else(ki_nevner == 0, NA_integer_, ki_nevner),
      ki_teller = dplyr::if_else(ki_nevner == 0, NA_integer_, ki_teller),
      konfint_nedre = dplyr::if_else(ki_nevner == 0, NA_real_, konfint_nedre),
      konfint_ovre = dplyr::if_else(ki_nevner == 0, NA_real_, konfint_ovre)
    )

  (d_agg_prop)
}
