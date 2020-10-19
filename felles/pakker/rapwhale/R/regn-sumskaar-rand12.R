#' @importFrom tibble tibble tribble
#' @importFrom Rdpack reprompt
NULL
#' Skår RAND-12-/SF-12-spørreskjema
#'
#' @description
#' Regner ut fysisk og psykisk sumskår (PCS og MCS)
#' for RAND-12-/SF-12-spørreskjema basert valgfri skåringsalgoritme.
#' Sjekker også at alle variabelnavnene og
#' verdiene/besvarelsene er gyldige.
#'
#' @param d Dataramme/tibble som inneholder
#'     spørreskjema-variabler + eventuelt andre variabler.
#'     Variabelnavnene for de tolv spørsmålene skal i utgangspunktet være
#'     `rand_1`, `rand_2a`, `rand_2b`, `rand_3a`, `rand_3b`, `rand_4a`,
#'     `rand_4b`, `rand_5`, `rand_6a`, `rand_6b`, `rand_6c` og `rand_7`
#'     (se `variabelnavn`).
#'     Se detaljer nedenfor for informasjon om koding.
#' @param variabelnavn Hvis `d` ikke bruker standard variabelnavn,
#'     kan du her oppgi kobling mellom standard variabelnavn (se ovenfor)
#'     og de brukte variabelnavnene. Bruk syntaksen
#'     `c(std_navn_1 = "brukt_navn_1", std_navn_2 = "brukt_navn_2")`.
#'     Navnekobling trenger kun oppgis for de variablene som ikke har
#'     standardnavn. Sett til `NULL` hvis alle har standardnavn.
#' @param algoritme Algoritme for skåring av spørreskjemaet.
#'     Foreløpig er det bare mulig å velge `"farivar_2007_oblique"`,
#'     som gir sumskårer basert på en korrelert (skrå/«oblique»)
#'     faktormodell som definert i
#'     \insertCite{Farivar2007;textual}{rapwhale}.
#' @param godta_manglende Skal manglende verdier (`NA`-verdier) i
#'     spørreskjema-variablene i `d` godtas (som standard nei)? Hvis
#'     ikke, blir det gitt ut en feilmelding om det finnes manglende
#'     verdier.
#'
#' @details
#' Funksjonen regner ut både fysisk og psykisk sumskår (PCS og MCS)
#' for RAND-12- eller SF-12-spørreskjema og legger disse til datasettet.
#' Den sjekker også at alle variabelnavnene og verdiene/besvarelsene
#' er kodet riktig.
#'
#' Skjemavariablene skal kodes slik at første svaralternativ for hvert
#' spørsmål har tallkoden 1, andre svaralternativ har tallkoden 2 osv.
#' For spørsmålene `rand_1`, `rand_5`, `rand_6a` og `rand_6b` tilsvarer
#' altså lav verdi god helse, mens for resten av spørsmålene tilsvarer lav
#' verdi dårlig helse.
#'
#' Skåringsalgoritmen i \insertCite{Farivar2007;textual}{rapwhale} er basert på
#' engelsk versjon av SF-12 versjon 1 (med amerikansk referansepopulasjon),
#' men den kan også brukes for (og er foreløpig det beste alternativet for)
#' RAND-12 norsk versjon 1 og SF-12 norsk versjon 1.1 og 1.2.
#'
#' @note Funksjonen kan *ikke* brukes for skåring av versjon 2 av SF-12
#' (dette skjemaet har for noen spørsmål flere mulige svaralternativer).
#'
#' @seealso Se [skaar_datasett()] og underfunksjoner for skåring
#' av generelle spørreskjemaer.
#'
#' @references
#' \insertAllCited{}
#'
#' @return Datasett likt `d`, men med sumskårene `rand12_pcs` og
#'   `rand12_mcs` lagt til, eventuelt erstattet. Sumskår-kolonnene blir
#'   i utgangspunktet lagt til på slutten av `d`.
#'   Hvis `d` imidlertid alt innholder en variabel med
#'   navnet `rand12_pcs` eller `rand12_mcs`, blir denne denne stående
#'   der den er, men overskrevet med nyutregnet sumskår. Det blir i så
#'   fall gitt ut en advarsel.
#' @examples
#' d_eks = tibble::tribble(
#'   ~rand_1, ~rand_2a, ~rand_2b, ~rand_3a, ~rand_3b, ~rand_4a, ~rand_4b, ~rand_5, ~rand_6a, ~rand_6b, ~rand_6c, ~rand_7,
#'   5, 1, 1, 1, 1, 1, 1, 5, 6, 6, 1, 2, # Min PCS12
#'   1, 3, 3, 2, 2, 2, 2, 1, 1, 1, 6, 5, # Maks PCS12
#'   5, 3, 3, 1, 1, 1, 1, 5, 6, 6, 1, 2, # Min MCS12
#'   1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 6, 5, # Maks MCS12
#'   5, 1, 1, 1, 1, 1, 1, 5, 6, 6, 1, 1
#' ) # Mest negativt svar på alle enkeltspørsmål
#' skaar_rand12(d_eks, algoritme = "farivar_2007_oblique")
#' @export
skaar_rand12 = function(d, variabelnavn = NULL,
                        algoritme,
                        godta_manglende = TRUE) {
  stopifnot(algoritme == "farivar_2007_oblique")

  skaaringstabell = tribble(
    ~delskala, ~variabel, ~verdi, ~koeffisient,
    "rand12_pcs", "rand_1", 1, 0,
    "rand12_pcs", "rand_1", 2, -1.09399,
    "rand12_pcs", "rand_1", 3, -2.48820,
    "rand12_pcs", "rand_1", 4, -4.56043,
    "rand12_pcs", "rand_1", 5, -6.90853,
    "rand12_pcs", "rand_2a", 1, -3.61039,
    "rand12_pcs", "rand_2a", 2, -1.52769,
    "rand12_pcs", "rand_2a", 3, 0,
    "rand12_pcs", "rand_2b", 1, -3.28556,
    "rand12_pcs", "rand_2b", 2, -1.49769,
    "rand12_pcs", "rand_2b", 3, 0,
    "rand12_pcs", "rand_3a", 1, -3.72452,
    "rand12_pcs", "rand_3a", 2, 0,
    "rand12_pcs", "rand_3b", 1, -4.48695,
    "rand12_pcs", "rand_3b", 2, 0,
    "rand12_pcs", "rand_4a", 1, -0.27441,
    "rand12_pcs", "rand_4a", 2, 0,
    "rand12_pcs", "rand_4b", 1, -0.87743,
    "rand12_pcs", "rand_4b", 2, 0,
    "rand12_pcs", "rand_5", 1, 0,
    "rand12_pcs", "rand_5", 2, -2.76223,
    "rand12_pcs", "rand_5", 3, -5.21603,
    "rand12_pcs", "rand_5", 4, -7.60094,
    "rand12_pcs", "rand_5", 5, -10.32862,
    "rand12_pcs", "rand_6a", 1, 0,
    "rand12_pcs", "rand_6a", 2, -0.24474,
    "rand12_pcs", "rand_6a", 3, -0.53677,
    "rand12_pcs", "rand_6a", 4, -0.38979,
    "rand12_pcs", "rand_6a", 5, -0.47407,
    "rand12_pcs", "rand_6a", 6, -0.64678,
    "rand12_pcs", "rand_6b", 1, 0,
    "rand12_pcs", "rand_6b", 2, -1.19645,
    "rand12_pcs", "rand_6b", 3, -2.28701,
    "rand12_pcs", "rand_6b", 4, -3.43746,
    "rand12_pcs", "rand_6b", 5, -4.68268,
    "rand12_pcs", "rand_6b", 6, -5.94178,
    "rand12_pcs", "rand_6c", 1, -1.32335,
    "rand12_pcs", "rand_6c", 2, -0.75981,
    "rand12_pcs", "rand_6c", 3, -0.53385,
    "rand12_pcs", "rand_6c", 4, -0.38595,
    "rand12_pcs", "rand_6c", 5, -0.15932,
    "rand12_pcs", "rand_6c", 6, 0,
    "rand12_pcs", "rand_7", 1, -2.57689,
    "rand12_pcs", "rand_7", 2, -3.29868,
    "rand12_pcs", "rand_7", 3, -2.42780,
    "rand12_pcs", "rand_7", 4, -1.21560,
    "rand12_pcs", "rand_7", 5, 0,
    "rand12_mcs", "rand_1", 1, 0,
    "rand12_mcs", "rand_1", 2, -0.54378,
    "rand12_mcs", "rand_1", 3, -1.45741,
    "rand12_mcs", "rand_1", 4, -2.78736,
    "rand12_mcs", "rand_1", 5, -4.28199,
    "rand12_mcs", "rand_2a", 1, 0.21329,
    "rand12_mcs", "rand_2a", 2, 0.15672,
    "rand12_mcs", "rand_2a", 3, 0,
    "rand12_mcs", "rand_2b", 1, 0.12950,
    "rand12_mcs", "rand_2b", 2, 0.08028,
    "rand12_mcs", "rand_2b", 3, 0,
    "rand12_mcs", "rand_3a", 1, -0.67652,
    "rand12_mcs", "rand_3a", 2, 0,
    "rand12_mcs", "rand_3b", 1, -0.73255,
    "rand12_mcs", "rand_3b", 2, 0,
    "rand12_mcs", "rand_4a", 1, -3.37939,
    "rand12_mcs", "rand_4a", 2, 0,
    "rand12_mcs", "rand_4b", 1, -3.38503,
    "rand12_mcs", "rand_4b", 2, 0,
    "rand12_mcs", "rand_5", 1, 0,
    "rand12_mcs", "rand_5", 2, -0.85395,
    "rand12_mcs", "rand_5", 3, -1.45064,
    "rand12_mcs", "rand_5", 4, -2.24871,
    "rand12_mcs", "rand_5", 5, -3.57055,
    "rand12_mcs", "rand_6a", 1, 0,
    "rand12_mcs", "rand_6a", 2, -1.91559,
    "rand12_mcs", "rand_6a", 3, -3.87498,
    "rand12_mcs", "rand_6a", 4, -5.60048,
    "rand12_mcs", "rand_6a", 5, -7.67490,
    "rand12_mcs", "rand_6a", 6, -9.27580,
    "rand12_mcs", "rand_6b", 1, 0,
    "rand12_mcs", "rand_6b", 2, -1.96823,
    "rand12_mcs", "rand_6b", 3, -3.95386,
    "rand12_mcs", "rand_6b", 4, -6.11303,
    "rand12_mcs", "rand_6b", 5, -8.13254,
    "rand12_mcs", "rand_6b", 6, -10.46333,
    "rand12_mcs", "rand_6c", 1, -14.96225,
    "rand12_mcs", "rand_6c", 2, -11.60997,
    "rand12_mcs", "rand_6c", 3, -7.91401,
    "rand12_mcs", "rand_6c", 4, -4.63416,
    "rand12_mcs", "rand_6c", 5, -2.15359,
    "rand12_mcs", "rand_6c", 6, 0,
    "rand12_mcs", "rand_7", 1, -3.51605,
    "rand12_mcs", "rand_7", 2, -4.19005,
    "rand12_mcs", "rand_7", 3, -3.20648,
    "rand12_mcs", "rand_7", 4, -1.71673,
    "rand12_mcs", "rand_7", 5, 0
  )

  d_orig_inkl_sumskaarer = skaar_datasett(
    d, skaaringstabell,
    variabelnavn, godta_manglende
  )

  # Legger til konstantledd i sumskår-kolonnene hvor det ikke er NA-verdier
  d_orig_inkl_sumskaarer$rand12_pcs = d_orig_inkl_sumskaarer$rand12_pcs + 62.37966
  d_orig_inkl_sumskaarer$rand12_mcs = d_orig_inkl_sumskaarer$rand12_mcs + 65.38813

  d_orig_inkl_sumskaarer
}
