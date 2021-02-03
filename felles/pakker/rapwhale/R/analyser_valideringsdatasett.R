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

    er_like = samanlikn_identisk(
      d_vld_vartype[[varnamn]],
      d_vld_vartype[[intern]], d_vld_vartype[[ekstern]]
    )

    d_vld_vartype = tibble(d_vld_vartype, vld_verdiar_er_like = er_like)

    d_vld_verdiar_er_like = add_row(d_vld_verdiar_er_like, d_vld_vartype)
  }

  d_vld_verdiar_er_like = d_vld_verdiar_er_like %>%
    arrange(rekkefolgje) %>%
    select(-rekkefolgje)

  return(d_vld_verdiar_er_like)
}
