library(tidyverse)
aggreger_ki_snitt = function(d_ki_ind, alfa = 0.05) {

  # Sjekke hvor mange som er inkludert i hver gruppe
  # tom = length(group_size(d_ki_ind)) - length(group_size(d_ki_ind %>% filter(ki_aktuell == TRUE)))

  # Teste inndata
  if (!(is.data.frame(d_ki_ind) && all(hasName(d_ki_ind, c("ki_x", "ki_aktuell"))))) {
    stop("Inndata må være tibble/data.frame med kolonnene 'ki_x' og 'ki_aktuell'")
  }
  if (!(is.logical(d_ki_ind$ki_aktuell))) {
    stop("'ki_aktuell' må være boolsk")
  }
  if (!(is.numeric(d_ki_ind$ki_x))) {
    stop("'ki_x' må være numerisk")
  }
  if (any(d_ki_ind$ki_aktuell & is.na(d_ki_ind$ki_x))) {
    stop("'ki_x' må være en numerisk verdi hvis 'ki_aktuell' er TRUE")
  }

  # Husk å legge inn tester for dette tilfellet.
  konfint = function(x) {
    konfint_robust = purrr::possibly(
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
      n_aktuell = sum(ki_aktuell)
    ) %>%
    mutate(
      konfint_nedre = map_dbl(konfint, pluck, 1),
      konfint_ovre = map_dbl(konfint, pluck, 2)
    ) %>%
    select(!!!groups(d_ki_ind), est, konfint_nedre, konfint_ovre, n_aktuell)
}
