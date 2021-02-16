lag_valideringsdatasett = function(d_reg, indvars, vartypar = NULL) {
  # Lag oversikt over datavariablar
  datavars = d_reg %>%
    names() %>%
    setdiff(indvars)

  # Lag oversikt over variabeltypar
  rvartypar = map(d_reg[datavars], ~ class(.x)) %>%
    map_chr(first)
  # Dersom eigne namn på variabeltypane ikkje er gitt brukar me R-klassen
  if (is.null(vartypar)) {
    vartypar = rvartypar
  }
  unike_rvartypar = unique(rvartypar)
  unike_vartypar = unique(vartypar)

  # Lag koplingstabell med datavariablar, vartypar og info om kvar resultatet
  # skal lagrast
  d_kopling = tibble(vld_varnamn = datavars, vld_vartype = vartypar) %>%
    mutate(res_kol = paste0("vld_verdi_intern_", vld_vartype))


  # Lag valideringsdatasett

  d_vld = d_reg
  d_vld$vld_varnamn = rep(list(datavars), nrow(d_vld))
  d_vld = d_vld %>%
    unnest(vld_varnamn) %>%
    left_join(d_kopling, by = "vld_varnamn")

  # Legg til aktuelle resultatkolonnar, med rett variabelklasse
  prefiksverdiar = c("vld_verdi_intern_", "vld_verdi_ekstern_")
  for (i in seq_len(length(unike_vartypar))) {
    rvartype = unike_rvartypar[i]
    vartype = unike_vartypar[i]
    for (prefiks in prefiksverdiar) {
      vld_varnamn = paste0(prefiks, vartype)
      d_vld[[vld_varnamn]] = as.numeric(NA)
      class(d_vld[[vld_varnamn]]) = rvartype
    }
  }

  # Funksjon for å flytta dataverdiane til rett kolonne
  # (er laga for å køyrast på datarammer med *éin unik* verdi for res_kol)
  #
  # (Det går raskast å flytta alle verdiane for kvar variabel i éin jafs.
  # Derfor gjer me det slik.)
  flytt_resultat = function(df) {
    df[[df$res_kol[1]]] = df[[df$vld_varnamn[1]]]
    df
  }

  # For kvar variabel, flytt verdiane til rett kolonne
  d_vld = d_vld %>%
    group_by(vld_varnamn) %>%
    do(flytt_resultat(.)) %>% # Ev. bruka purr-funksjonar til dette?
    ungroup()

  # Fjern gamle datavariabelkolonnar og hjelpekolonne
  d_vld = d_vld %>%
    select(-datavars, -"res_kol")
  d_vld
}
