#' Lag valideringsdatasett
#'
#' @description
#' Lagar eit valideringsdatasett med data frå eit registerdatasett klart til å
#' fyllast ut med og sjekkast opp mot data frå ein gullstandard (typisk
#' pasientjournal).
#'
#' @param d_reg Datasett (dataramme/tibble).
#' @param indvars Vektor med indeksvariablar.
#' @param vartypar Vektor med spesifiserte vartypar. Standard `NULL`.
#'
#' @details
#' Funksjonen tek inn eit datasett `d_reg`, ein vektor `indvars` og eventuelt
#' ein vektor `vartypar` og returnerer eit valideringsdatasett.
#'
#' Datasettet `d_reg` skal vera anten data.frame eller tibble.
#'
#' Vektoren `indvars` inneheld namna på alle indeksvariablane. Desse må alle
#' finnast som kolonnar i `d_reg`, og kombinasjonen av dei må unikt identifisera
#' kvar rad. Kolonnane i `d_reg` som ikkje er indeksvariablar er datavariablar
#' som skal validerast.
#'
#' Vektoren `vartypar` inneheld spesifiserte namn på kva type verdiar kvar
#' datavariabel har. Det kan vera for eksempel norske namn som "tal", "dato" og
#' så vidare. Dette vert brukt til å laga namn på kolonnane med verdiar som skal
#' sjekkast og fyllast ut. Standardverdi `NULL` vert brukt dersom ein berre vil
#' bruka klassane variablane har i R.
#'
#' Valideringsdatasettet som vert gjeve ut vil ha éi rad for kvar datavariabel
#' for kvar rad i det opphavlege datasettet. Kvar rad i valideringsdatasettet
#' vil då innehalda verdien frå éin datavariabel i éi rad frå det opphavlege
#' datasettet. Denne verdien vil vera lagra i kolonnen `vld_verdi_intern_x`, der
#' `x` er typen til den aktuelle variabelen. Valideringsdatasettet  vil vera
#' på formatet som er definert i [er_valideringsdatasett_gyldig()], med tomme
#' kolonnar `vld_verdi_ekstern_x`, klare til å fyllast ut og sidan sjekkast mot
#' verdiane i `vld_verdi_intern_x`.
#'
#' @return Valideringsdatasett som er klart til å fyllast ut med og sjekkast
#'         opp mot data frå ein gullstandard (typisk pasientjournal).
#'
#' @export
#'
#' @examples
#' # Eksempel på datasett frå eit register:
#' d_reg = tibble::tribble(
#'   ~pasid, ~dato_inn, ~dato_ut, ~vekt, ~hogd, ~biverk, ~biverk_hovud, ~biverk_mage, ~biverk_fot,
#'   5, as.Date("2020-06-07"), as.Date("2020-06-15"), 78, 183, TRUE, FALSE, TRUE, TRUE,
#'   5, as.Date("2020-12-13"), as.Date("2020-12-13"), 50, 179, TRUE, FALSE, TRUE, TRUE,
#'   7, as.Date("2020-08-09"), as.Date("2020-08-13"), 711, 196, TRUE, TRUE, TRUE, TRUE,
#'   13, as.Date("2021-01-05"), NA, NA, 163, FALSE, NA, NA, NA,
#'   14, as.Date("2021-01-05"), as.Date("2021-01-09"), 101, 182, TRUE, TRUE, FALSE, FALSE
#' )
#'
#' # Indeksvariablar:
#' indvars = c("pasid", "dato_inn")
#'
#' d_vld = lag_valideringsdatasett(d_reg, indvars)
#' d_vld
#'
#' # Eksempel på spesifisering av variabeltypar:
#' vartypar = c("dato", "tal", "tal", "logisk", "logisk", "logisk", "logisk")
#'
#' d_vld = lag_valideringsdatasett(d_reg, indvars, vartypar)
#' d_vld
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
