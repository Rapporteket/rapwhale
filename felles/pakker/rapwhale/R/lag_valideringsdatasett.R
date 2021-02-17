lag_valideringsdatasett = function(d_reg, indvars, vartypar = NULL) {
  vars = names(d_reg)
  if (!all(indvars %in% vars)) {
    stop("Alle indeksvariablane finst ikkje i datasettet")
  }

  if (anyDuplicated(indvars)) {
    stop("Duplikatverdiar i indeksvariablane")
  }

  prim_nokkel = select(d_reg, all_of(indvars))
  if (any(duplicated(prim_nokkel))) {
    stop("Indeksvariablane identifiserer ikkje alle radene unikt")
  }

  # Lag oversikt over datavariablar
  datavars = vars %>%
    setdiff(indvars)

  # Lag oversikt over variabeltypar
  rvartypar = map(d_reg[datavars], ~ class(.x)) %>%
    map_chr(first) %>%
    unname()
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
  d_vld$vld_varnamn = rep(list(datavars), nrow(d_reg))
  d_vld = d_vld %>%
    unnest(vld_varnamn) %>%
    left_join(d_kopling, by = "vld_varnamn")
  antal_rader = nrow(d_vld)
  d_vld$rekkjefolgje = seq_len(antal_rader)

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
  if (antal_rader > 0) {
    d_vld = d_vld %>%
      group_by(vld_varnamn) %>%
      do(flytt_resultat(.)) %>% # Ev. bruka purr-funksjonar til dette?
      ungroup()
  }

  # Fjern gamle datavariabelkolonnar og hjelpekolonne
  d_vld = d_vld %>%
    arrange(rekkjefolgje) %>%
    select(-all_of(datavars), -c("res_kol", "rekkjefolgje"))
  d_vld
}
