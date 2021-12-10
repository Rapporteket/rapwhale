#' @importFrom rlang has_name
#' @importFrom dplyr mutate across all_of
NULL
#' Erstatt ukjent verdi
#'
#' Erstatter utvalgte 'ukjente' verdier med NA. Noen datasett 
#' (primært MRS-register) har variabler hvor manglende besvarelser får verdi -1,
#' 99 eller andre mer eller mindre vilkårlige verdier for å indikere at spørsmålet
#' ikke er besvart. I tillegg kan det også finnes ekte NA-verdier, slik at 
#' beregning av kompletthet kompliseres. 
#' 
#' @param data tibble/data.frame som inneholder variabel hvor 
#' ukjente verdier skal erstattes. 
#' @param variabel tekststreng med navn på variabel hvor ukjente verdier skal 
#' erstattes.  
#' @param na_vektor vektor med verdier som skal erstattes med NA. Må være samme 
#' datatype som 'variabel'. 
#'
#' @return
#' Returnerer opprinnelig datasett hvor ukjente verdier er erstattet med NA for 
#' oppgitt variabel. 
#' 
#' @export
#'
#' @examples
#' d = tibble::tibble(pas_id = c(1, 2, 3, 4, 5, 6), 
#'                    var_1 = c(1, 2, -1, 99, NA, 5))
#' erstatt_ukjent(data = d, variabel = "var_1", na_vektor = c(-1, 99))
erstatt_ukjent = function(data, variabel, na_vektor) {
  
  if (!has_name(data, variabel)) {
    stop(paste0("'", variabel, "' mangler i inndata"))
  }
  
  d = data %>% 
    mutate(across(all_of(variabel), ~replace(., . %in% na_vektor, NA))) 
  d  
}


#' beregn kompletthet
#' 
#' Regner ut antall og andel manglende observasjoner for en variabel. Tar inn 
#' et datasett og en variabel det skal beregnes kompletthet for. 
#'
#' @param data Tibble/data.frame som inneholder variabel det skal beregnes 
#' kompletthet for. 
#' @param variabel tekststreng med navn på variabel det skal beregnes kompletthet for. 
#'
#' @return
#' Returnerer en tibble med følgende kolonner: 
#' grupperingsvariabel - Hvis inndata er gruppert vil grupperingsvariabel være 
#' med i utdata. 
#' Variabel - Navn på variabel det er beregnet kompletthet for. 
#' Totalt_antall - Totalt antall observasjoner per gruppe i inndata. 
#' Antall_na - Antall observasjoner som er NA i inndata. 
#' Andel_na - Andel observasjoner som er NA i inndata. 
#' 
#' @export
#'
#' @examples
#' # Ugrupperte inndata: 
#' d = tibble::tibble(pas_id = c(1, 2, 3, 4, 5, 6), 
#'                    sykehus = c("HUS", "HUS", "SVG", "SVG", "SVG", "OUS"),
#'                    var_1 = c(1, 2, -1, 99, NA, 5))
#' beregn_kompletthet(data = d, variabel = "var_1", na_vektor = c(-1, 99))
#' 
#' # Grupperte inndata: 
#' #' d = tibble::tibble(pas_id = c(1, 2, 3, 4, 5, 6), 
#'                    sykehus = c("HUS", "HUS", "SVG", "SVG", "SVG", "OUS"),
#'                    var_1 = c(1, 2, -1, 99, NA, 5)) %>%
#'        group_by(sykehus)
#'        
#' beregn_kompletthet(data = d, variabel = "var_1", na_vektor = c(-1, 99))
beregn_kompletthet = function(data, variabel) {
  
  data_ut = data %>% 
    summarise(variabelnavn = variabel, 
              totalt_antall = n(), 
              antall_na = sum(is.na(!!sym(variabel))), 
              andel_na = sum(is.na(!!sym(variabel)))/n(), 
              .groups = "keep")
  
  data_ut
}


#' Beregn kompletthet med ukjent
#' 
#' Beregner antall og andel NA, både med og uten inkludering av ukjente verdier. 
#' Ukjente verdier defineres i na_vektor. Tanken er at besvarelser som 
#' "Velg Verdi", "Ukjent", "Ikke besvart" eller lignende kan inkluderes her. 
#'
#' @param data Tibble/data.frame som inneholder variabel det skal beregnes 
#' kompletthet for. 
#' @param variabel Streng med navn på variabel det skal beregnes kompletthet for. 
#' @param na_vektor Vektor av samme datatype som 'variabel', som angir hvilke 
#' verdier som skal erstattes med NA. 
#'
#' @return
#' Returnerer et aggregert datasett med kolonnene: 
#' Grupperingsvariabel hvis inndata er gruppert. 
#' Variabel - Navn for variabel det er beregnet kompletthet for. 
#' Totalt_antall - Antall rader per gruppe
#' Antall_na - Antall NA i opprinnelig data. 
#' Andel_na - Andel NA i opprinnelig data. 
#' Antall_na_med_ukjent - Antall NA når verdier fra NA-vektor er erstattet med NA. 
#' Andel_na_med_ukjent - Andel NA når verdier er NA-vektor er erstattet med NA. 
#' @export
#'
#' @examples
#' d = tibble::tibble(pas_id = c(1, 2, 3, 4, 5, 6),
#'                    sykehus = c("A", "A", "B", "B", "B", "C") 
#'                    var_1 = c(1, 2, -1, 99, NA, 5))
#' beregn_kompletthet_med_ukjent(data = d, variabel = "var_1", na_vektor = c(-1, 99))
beregn_kompletthet_med_ukjent = function(data, variabel, na_vektor) {
  
  d_na = beregn_kompletthet(data, variabel = variabel)
  data_uten_ukjent = erstatt_ukjent(data, variabel = variabel, na_vektor = na_vektor)
  d_na_ukjent = beregn_kompletthet(data_uten_ukjent, variabel = variabel)
  
  d_na_ukjent = d_na_ukjent %>% 
    select(-variabelnavn, -totalt_antall) %>% 
    rename(antall_na_med_ukjent = antall_na, 
           andel_na_med_ukjent = andel_na)
  
  data_ut = d_na %>% bind_cols(d_na_ukjent)
  
  data_ut
}



