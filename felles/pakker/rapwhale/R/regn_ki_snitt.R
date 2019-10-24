
aggreger_ki_snitt = function(d_ki_ind, alfa = 0.05) {
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
  if (any(d_ki_ind$ki_aktuell == TRUE & is.na(d_ki_ind$ki_x))) {
    stop("'ki_x' må være en numerisk verdi hvis 'ki_aktuell' er TRUE")
  }

  konf = t.test(x = d_ki_ind$ki_x, conf.level = 1 - alfa)
  d_ki_ind %>%
    summarise(
      est = mean(ki_x, na.rm = TRUE),
      konfint_nedre = as.numeric(unlist(konf)["conf.int1"]),
      konfint_ovre = as.numeric(unlist(konf)["conf.int2"]),
      n_aktuell = sum(ki_aktuell)
    )
}
