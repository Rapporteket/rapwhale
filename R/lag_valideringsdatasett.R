#' Lag valideringsdatasett
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Lagar eit valideringsdatasett med data frå eit registerdatasett klart til å
#' fyllast ut med og sjekkast opp mot data frå ein gullstandard (typisk
#' pasientjournal).
#'
#' @param d_reg Datasett (dataramme/tibble).
#' @param indvars Vektor med indeksvariablar.
#'
#' @details
#' Funksjonen tek inn eit datasett `d_reg` og ein vektor `indvars` og
#' returnerer eit valideringsdatasett.
#'
#' Datasettet `d_reg` skal vera anten data.frame eller tibble.
#'
#' Vektoren `indvars` inneheld namna på alle indeksvariablane. Desse må alle
#' finnast som kolonnar i `d_reg`, og kombinasjonen av dei må unikt identifisera
#' kvar rad. Kolonnane i `d_reg` som ikkje er indeksvariablar er datavariablar
#' som skal validerast.
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
#' d_reg
#'
#' # Indeksvariablar:
#' indvars = c("pasid", "dato_inn")
#'
#' d_vld = lag_valideringsdatasett(d_reg, indvars)
#' d_vld
lag_valideringsdatasett = function(d_reg, indvars) {
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


  d_vartypar = tibble::tibble(
    vartypar = map(d_reg[datavars], ~ class(.x)) %>%
      unname(), # Lag oversikt over variabeltypar

    vartypar_sammenlagt = map_chr(
      vartypar,
      \(x) paste(x,
        collapse = "_"
      )
    ) # Slå sammen elementer fra samme klass i en string
  )
  d_vartypar_distinct = distinct(d_vartypar)


  if (any(duplicated(d_vartypar_distinct$vartypar_sammenlagt))) {
    stop("Samanslåing av klassane til dei ulike variabeltypane gjev ikkje eintydige resultat")
  }

  # Lag koplingstabell med datavariablar, vartypar og info om kvar resultatet
  # skal lagrast
  d_kopling = tibble(vld_varnamn = datavars, vld_vartype = d_vartypar$vartypar_sammenlagt) %>%
    mutate(res_kol = paste0("vld_verdi_intern_", vld_vartype))


  # Lag valideringsdatasett

  d_vld = d_reg
  d_vld$vld_varnamn = rep(list(datavars), nrow(d_reg))
  d_vld = d_vld %>%
    unnest(vld_varnamn) %>%
    left_join(d_kopling, by = "vld_varnamn", relationship = "many-to-one")
  antal_rader = nrow(d_vld)
  d_vld$rekkjefolgje = seq_len(antal_rader)

  # Legg til aktuelle resultatkolonnar, med rett variabelklasse
  prefiksverdiar = c("vld_verdi_intern_", "vld_verdi_ekstern_")
  for (i in seq_len(nrow(d_vartypar_distinct))) {
    for (prefiks in prefiksverdiar) {
      vld_varnamn = paste0(prefiks, d_vartypar_distinct$vartypar_sammenlagt[i])
      d_vld[vld_varnamn] = NA_real_
      class(d_vld[[vld_varnamn]]) = d_vartypar_distinct$vartypar[[i]]
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
      split(group_indices(.)) %>%
      purrr::map_df(flytt_resultat) %>%
      ungroup()
  }

  # Fjern gamle datavariabelkolonnar og hjelpekolonne
  d_vld = d_vld %>%
    arrange(rekkjefolgje) %>%
    select(-all_of(datavars), -c("res_kol", "rekkjefolgje"))
  d_vld
}
