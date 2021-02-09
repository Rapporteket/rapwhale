#' Analyser valideringsdatasett
#' 
#' @description Tek inn eit valideringsdatasett og ein samanliknarfunksjon, 
#'              og returnerer datasettet med ein ekstra kolonne med `TRUE` 
#'              eller `FALSE` for kvar rad der verdiane som samanliknast er 
#'              høvesvis «like» eller ikkje.
#'
#' @param d_vld Valideringsdatasett (dataramme/tibble).
#' @param samanliknar Samanliknarfunksjon, standard `samanlikn_identisk()`.
#' 
#' @details 
#' Funksjonen tek inn eit valideringsdatasett `d_vld` og ein samanliknarfunksjon 
#' `samanliknar`, og returnerer datasettet med ein ekstra kolonne `vld_verdiar_er_like`. 
#' 
#' `d_vld` må vera på rett format, dvs. `er_valideringsdatasett_gyldig(d_vld)`
#'  må vera `TRUE`. Sjå `?er_valideringsdatasett_gyldig` for meir info.
#' 
#' `samanliknar` skal vera ein funksjon som tek inn tre like lange vektorar, 
#' der dei to fyrste er verdiane som skal samanliknast elementvis, og den siste 
#' inneheld namna på variablane. Den skal returnera ein logisk vektor med same
#' lengd som vektorane den tek inn, som er `TRUE` eller `FALSE` alt etter om 
#' elementa er høvesvis «like» eller ei. Kva som vert rekna som «like» varierer 
#' mellom ulike samanliknarfunksjonar.
#' 
#' `analyser_valideringsdatasett()` brukar `samanliknar` til å samanlikna 
#' `vld_verdi_intern_x` og `vld_verdi_ekstern_x` for kvar rad der `vld_vartype = x`.
#' 
#' Til slutt returnerast det opphavlege datasettet `d_vld` med ein ekstra 
#' kolonne `vld_verdiar_er_like` som er `TRUE` for kvar rad der verdiane som 
#' vart samanlikna er «like», og `FALSE` der dei ikkje er «like».
#'
#' @return Returnerer opphavleg datasett med ein ekstra kolonne `vld_verdiar_er_like`
#'         som er `TRUE` for kvar rad der verdiane som skal samanliknast er 
#'         «like» i følgje `samanliknar`, og `FALSE` der dei ikkje er «like».
#' @export
#'
#' @examples
#' Eksempel på gyldig valideringsdatasett med både «like» og ikkje «like» 
#' verdiar i følgje standard samanliknar `samanlikn_identisk()`.
#' d_vld = tibble::tibble(
#'   pasient_id = c(5, 5, 5, 7),
#'   dato_operasjon = as.Date(c("2020-06-07", "2020-12-13", "2020-12-13", "2021-02-05")),
#'   kjonn = c("mann", "mann", "mann", "kvinne"),
#'   sjukehus = c("Bergen", "Bergen", "Førde", "Stavanger"),
#'   vld_varnamn = c("vekt", "vekt", "diabetes", "diabetes"),
#'   vld_vartype = c("tal", "tal", "logisk", "logisk"),
#'   vld_verdi_intern_tal = c(78, 88, NA, NA),
#'   vld_verdi_ekstern_tal = c(78, 90, NA, NA),
#'   vld_verdi_intern_logisk = c(NA, NA, TRUE, FALSE),
#'   vld_verdi_ekstern_logisk = c(NA, NA, TRUE, NA))
#' analyser_valideringsdatasett(d_vld)
analyser_valideringsdatasett = function(d_vld, samanliknar = samanlikn_identisk){
  if(!er_valideringsdatasett_gyldig(d_vld)){
    stop("Datasettet er ikkje på rett format")
  }
  
  
  # Gjev ut valideringsdatasettet med info i ekstrakolonne om verdiane 
  # i kvar rad er «like»
  
  vartypar = unique(d_vld$vld_vartype)
  
  if(length(vartypar) == 0){
    d_vld$vld_verdiar_er_like = logical()
  }
  
  for(vartype in vartypar){
    radnr_med_vartype = which(d_vld$vld_vartype == vartype)
    
    verdi_intern = glue::glue("vld_verdi_intern_{vartype}")
    verdi_ekstern = glue::glue("vld_verdi_ekstern_{vartype}")
    
    er_like = samanliknar(d_vld[[verdi_intern]][radnr_med_vartype], 
                          d_vld[[verdi_ekstern]][radnr_med_vartype],
                          d_vld$vld_varnamn[radnr_med_vartype])
    
    # Feilmelding viss samanliknar gjev ugyldige verdiar
    
    if(anyNA(er_like)){
      stop("NA-verdiar frå samanliknaren")
    }
    
    if(class(er_like) != "logical"){
      stop("Ikkje logisk vektor frå samanliknaren")
    }
    
    antal_rader_vartype = nrow(d_vld[radnr_med_vartype,])
    if(length(er_like) != antal_rader_vartype){
      stop("Utdata frå samanliknaren har feil lengd")
    }
    
    d_vld[radnr_med_vartype, "vld_verdiar_er_like"] = er_like
  }
  d_vld
}
