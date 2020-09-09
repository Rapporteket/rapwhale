#' @importFrom tibble tibble tribble
NULL
#' Skår RAND-12-spørreskjema
#'
#' @description
#' Regner ut sumskårer (`PCSC12` og `MCSC12`) for RAND-12-spørreskjema i
#' et datasett basert på en RAND-12-spesifikk skåringstabell. Sjekker
#' også at alle verdiene i datasettet er i samsvar med skåringstabellen.
#'
#' @param d Dataramme/tibble som inneholder RAND-12-spørreskjema-
#'     variabler + eventuelt andre variabler. Spørreskjema-variablene må
#'     være numeriske.
#' @param variabelnavn Navn på RAND-12-spørreskjema-variabler i
#'     datasettet som ikke er identiske med navnene i den RAND-12-
#'     spesifikke skåringstabellen. Bruk syntaksen
#'     `c(std_navn_1 = "dd_navn_1", std_navn_2 = "dd_navn_2")`.
#'     Nye navn trenger kun oppgis for spørreskjema-variabler som har
#'     avvikende navn fra skåringstabellen. Hvis `NULL`, blir
#'     det antatt at alle navnene er i samsvar med skåringstabellen.
#' @param metode Metode for skåring. Standard er "farivar_2007_oblique",
#'     foreløpig er det ikke mulig å velge andre skåringsmetoder.
#' @param godta_manglende Skal manglende verdier (`NA`-verdier) i
#'     spørreskjema-variablene i `d` godtas (som standard nei)? Hvis
#'     ikke, blir det gitt ut en feilmelding om det finnes manglende
#'     verdier.
#'
#' @details
#' Funksjonen baserer seg på [skaar_datasett()]. Den RAND-12-spesifikke
#' skåringstabellen blir definert i funksjonen. Datasettet som blir
#' returnert av [skaar_datasett()] inneholder sumskår-kolonnene
#' `PCSC12` og `MCSC12`. For alle verdier i `PCSC12`(ikke NA-verdier)
#' blir konstantleddet 62.37966 lagt til, mens for alle verdier i
#' `MCSC12` blir konstantleddet 65.38813 lagt til.
#'
#' Funksjonen gir feilmelding dersom noen av verdiene i `d` ikke er i
#' samsvar med skåringstabellen.
#'
#' Se [skaar_datasett()] og underfunksjoner for detaljer
#' om funksjonalitet.
#'
#' @return Datasett likt `d`, men med sumskårene `PCSC12` og `MCSC12`
#'   lagt til, eventuelt erstattet. Sumskår-kolonnene blir i
#'   utgangspunktet lagt til på slutten av `d`, `PCSC12` først og så
#'   `MCSC12`. Hvis `d` imidlertid alt innholder en variabel med navnet
#'   `PCSC12` eller `MCSC12`, blir denne denne stående der den er, men
#'   overskrevet med nyutregnet sumskår. Det blir i så fall gitt ut en
#'   advarsel.
#' @export
skaar_rand12 = function(d, variabelnavn = NULL,
                        metode = "farivar_2007_oblique",
                        godta_manglende = TRUE) {
  stopifnot(metode == "farivar_2007_oblique")

  skaaringstabell = tribble(
    ~delskala, ~variabel, ~verdi, ~koeffisient,
    "PCSC12", "GH1", 1, 0,
    "PCSC12", "GH1", 2, -1.09399,
    "PCSC12", "GH1", 3, -2.48820,
    "PCSC12", "GH1", 4, -4.56043,
    "PCSC12", "GH1", 5, -6.90853,
    "PCSC12", "PF02", 1, -3.61039,
    "PCSC12", "PF02", 2, -1.52769,
    "PCSC12", "PF02", 3, 0,
    "PCSC12", "PF04", 1, -3.28556,
    "PCSC12", "PF04", 2, -1.49769,
    "PCSC12", "PF04", 3, 0,
    "PCSC12", "RP2", 1, -3.72452,
    "PCSC12", "RP2", 2, 0,
    "PCSC12", "RP3", 1, -4.48695,
    "PCSC12", "RP3", 2, 0,
    "PCSC12", "RE2", 1, -0.27441,
    "PCSC12", "RE2", 2, 0,
    "PCSC12", "RE3", 1, -0.87743,
    "PCSC12", "RE3", 2, 0,
    "PCSC12", "BP2", 1, 0,
    "PCSC12", "BP2", 2, -2.76223,
    "PCSC12", "BP2", 3, -5.21603,
    "PCSC12", "BP2", 4, -7.60094,
    "PCSC12", "BP2", 5, -10.32862,
    "PCSC12", "MH3", 1, 0,
    "PCSC12", "MH3", 2, -0.24474,
    "PCSC12", "MH3", 3, -0.53677,
    "PCSC12", "MH3", 4, -0.38979,
    "PCSC12", "MH3", 5, -0.47407,
    "PCSC12", "MH3", 6, -0.64678,
    "PCSC12", "VT2", 1, 0,
    "PCSC12", "VT2", 2, -1.19645,
    "PCSC12", "VT2", 3, -2.28701,
    "PCSC12", "VT2", 4, -3.43746,
    "PCSC12", "VT2", 5, -4.68268,
    "PCSC12", "VT2", 6, -5.94178,
    "PCSC12", "MH4", 1, -1.32335,
    "PCSC12", "MH4", 2, -0.75981,
    "PCSC12", "MH4", 3, -0.53385,
    "PCSC12", "MH4", 4, -0.38595,
    "PCSC12", "MH4", 5, -0.15932,
    "PCSC12", "MH4", 6, 0,
    "PCSC12", "SF2", 1, -2.57689,
    "PCSC12", "SF2", 2, -3.29868,
    "PCSC12", "SF2", 3, -2.42780,
    "PCSC12", "SF2", 4, -1.21560,
    "PCSC12", "SF2", 5, 0,
    "MCSC12", "GH1", 1, 0,
    "MCSC12", "GH1", 2, -0.54378,
    "MCSC12", "GH1", 3, -1.45741,
    "MCSC12", "GH1", 4, -2.78736,
    "MCSC12", "GH1", 5, -4.28199,
    "MCSC12", "PF02", 1, 0.21329,
    "MCSC12", "PF02", 2, 0.15672,
    "MCSC12", "PF02", 3, 0,
    "MCSC12", "PF04", 1, 0.12950,
    "MCSC12", "PF04", 2, 0.08028,
    "MCSC12", "PF04", 3, 0,
    "MCSC12", "RP2", 1, -0.67652,
    "MCSC12", "RP2", 2, 0,
    "MCSC12", "RP3", 1, -0.73255,
    "MCSC12", "RP3", 2, 0,
    "MCSC12", "RE2", 1, -3.37939,
    "MCSC12", "RE2", 2, 0,
    "MCSC12", "RE3", 1, -3.38503,
    "MCSC12", "RE3", 2, 0,
    "MCSC12", "BP2", 1, 0,
    "MCSC12", "BP2", 2, -0.85395,
    "MCSC12", "BP2", 3, -1.45064,
    "MCSC12", "BP2", 4, -2.24871,
    "MCSC12", "BP2", 5, -3.57055,
    "MCSC12", "MH3", 1, 0,
    "MCSC12", "MH3", 2, -1.91559,
    "MCSC12", "MH3", 3, -3.87498,
    "MCSC12", "MH3", 4, -5.60048,
    "MCSC12", "MH3", 5, -7.67490,
    "MCSC12", "MH3", 6, -9.27580,
    "MCSC12", "VT2", 1, 0,
    "MCSC12", "VT2", 2, -1.96823,
    "MCSC12", "VT2", 3, -3.95386,
    "MCSC12", "VT2", 4, -6.11303,
    "MCSC12", "VT2", 5, -8.13254,
    "MCSC12", "VT2", 6, -10.46333,
    "MCSC12", "MH4", 1, -14.96225,
    "MCSC12", "MH4", 2, -11.60997,
    "MCSC12", "MH4", 3, -7.91401,
    "MCSC12", "MH4", 4, -4.63416,
    "MCSC12", "MH4", 5, -2.15359,
    "MCSC12", "MH4", 6, 0,
    "MCSC12", "SF2", 1, -3.51605,
    "MCSC12", "SF2", 2, -4.19005,
    "MCSC12", "SF2", 3, -3.20648,
    "MCSC12", "SF2", 4, -1.71673,
    "MCSC12", "SF2", 5, 0
  )

  d_orig_inkl_sumskaarer = skaar_datasett(
    d, skaaringstabell,
    variabelnavn, godta_manglende
  )

  # Legger til konstantledd i sumskår-kolonnene hvor det ikke er NA-verdier
  d_orig_inkl_sumskaarer$PCSC12 = d_orig_inkl_sumskaarer$PCSC12 + 62.37966
  d_orig_inkl_sumskaarer$MCSC12 = d_orig_inkl_sumskaarer$MCSC12 + 65.38813

  d_orig_inkl_sumskaarer

  # d = endre_variabelnavn(d, skaaringstabell$variabel)
  # sjekk_variabelnavn(d, variabelnavn)
  # sjekk_variabelverdier(d, verditabell = skaaringstabell %>% dplyr::select(variabel, verdi), godta_manglende)
  # regn_sumskaar(d, skaaringstabell)
}
