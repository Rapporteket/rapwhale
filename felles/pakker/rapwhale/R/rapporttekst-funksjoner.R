# Dette skriptet inneholder en rekke funksjoner som er potensielt nyttige
# (og noen, uunnværlige) i registersammenheng og i andre sammenheng.

#' @importFrom magrittr %>%
#' @importFrom stringr str_flatten
NULL

# LaTeX-/rapportskrivingsfunksjoner ---------------------------------------

### Funksjon for å legge inn tall i en rapport som bruker LaTeX

# Tar inn eit tal x og viser det som \num{x}, som (om nødvendig)
# legg inn fine mellomrom som tusenskiljeteikn og endrar
# desimalpunktum til desimalkomma.

# Innargument:
#   desimalar: talet på desimalar etter komma (rund av og vis så mange desimalar)
#      tabell: talet vert brukt i ein tabell og skal derfor ha tabelltekst
#
# Argumentet «tabell» burde vore unødvendig, men siunitx *insisterer*
# på å endra skrifta til \textrm, sjølv om eg har slått på alle moglege
# detect-argument (og prøvd mykje anna, og søkt på nettet etter løysingar
# (bruk søkeorda «siunitx» og «fontspec»)). Alle andre løysingar eg har
# funne gjer at anten vert ikkje rett skrift brukt i brødteksten eller så vert
# ikkje rett tekst brukt i tabellforklaringa eller så vert ikkje rett tekst
# brukt i sjølve tabellen. (Merk at me brukar ulik skrift i tabell-/
# figurforklaringa, sjølv om dei begge er Calibri. Ein ser lettast forskjellen
# ved å studera 1-tala.)
#
# Som ei nødløysing har me ordna det slik at me kan manuelt velja at
# tabellskrifta skal brukast når me kallar num()-funksjonen.


#' Konverter tall til LaTeX-format
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjon som tar inn ein vektor med tal x og konverterer han til riktig format i LaTeX, og (om nødvendig) legg inn fine mellomrom som tusenskiljeteikn og endrar desimalpunktum til desimalkomma.
#'
#' \code{NA}-verdiar vert gjorde om til tankestrekar. \cr\cr
#' Argumentet «tabell» burde vore unødvendig, men siunitx \emph{insisterer}
#' på å endra skrifta til \code{\\textrm}, sjølv om eg har slått på alle moglege
#' detect-argument (og prøvd mykje anna, og søkt på nettet etter løysingar
#' (bruk søkeorda «siunitx» og «fontspec»)). Alle andre løysingar eg har
#' funne gjer at anten vert ikkje rett skrift brukt i brødteksten eller så vert
#' ikkje rett tekst brukt i tabellforklaringa eller så vert ikkje rett tekst
#' brukt i sjølve tabellen. (Merk at me brukar ulik skrift i tabell-/
#' figurforklaringa, sjølv om dei begge er Calibri. Ein ser lettast forskjellen
#' ved å studera 1-tala.). \cr\cr
#' Som ei nødløysing har me ordna det slik at me kan manuelt velja at
#' tabellskrifta skal brukast når me kallar num()-funksjonen.
#' @param x Tallet en ønsker å skrive på LaTeX-format.
#' @param desimalar Hvor mange desimaler skal inkluderes etter komma? (Rund av og vis så mange desimaler).
#' @param tabell \code{TRUE} eller \code{FALSE} for å indikere om tallet skal brukes i en tabell, og dermed skal ha tabelltekst.
#' @export
#' @examples
#' # Til bruk i setningar i latex
#' paste0("Store tal som ", num(123456789), " får mellomrom som tusenskiljeteikn.")
#' paste0("Pi avrunda til fire desimalar er ", num(pi, desimalar = 4), ".")
#'
#' # Til bruk i tabell i latex
#' num(pi, desimalar = 2, tabell = TRUE)
num = function(x, desimalar, tabell = FALSE) {
  # Argument til \num-kommandoen
  arg = NULL
  if (tabell) {
    arg = "text-rm=\\tablefont"
  }

  # Spesialtilpass kommandoen etter talet på desimalar
  if (!missing(desimalar)) {
    # \num kan runda av for oss, men rundar av i R
    # for å sikra avrundinga vert identisk som ved
    # andre plassar der me ikkje brukar \num.
    x = round(x, desimalar)

    # Viss me vil ha desimalar, vis *alle* desimalane
    # (eks. vert både 3.1 og 3.123 vist som 3,1),
    # også for heiltal (eks. vert 3 vist som 3.0).
    if (desimalar > 0) {
      arg = arg %>%
        append(c(
          paste0("round-precision=", desimalar),
          "round-integer-to-decimal=true"
        ))
    }
  }
  # Legg til argumentliste
  argtekst = paste0("[", paste0(arg, collapse = ", "), "]")

  # Returner LaTeX-kode for talformatering. Me legg *heile* kommandoen
  # mellom {} for å hindra problem ved bruk for eksempel inni shortcap-delen
  # av \caption[shortcap]{longcap} (eventuelle ]-teikn vert elles tolka
  # til å avslutta shortcap-argumentet, jf. https://tex.stackexchange.com/a/78416)
  x_form = paste0("{\\num", argtekst, "{", format(x, scientific = FALSE), "}}")
  x_form[is.na(x)] = paste0("{", if (tabell) {
    "\\tablefont"
  }, "\\textendash{}}")
  x_form
}

# FIXME -Fjerne akse_prosent funksjon når alle rapporter er oppdatert til å bruke
# akse_prosent_format.

#' Vis desimaltall som prosent
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param x Tallet som skal skrives som prosent.
#' @param accuracy Antall desimaler som skal benyttes.
#' @param decimal.mark desimaltegn.
#' @param ... Ytterligere argumenter.
#'
#' @export
akse_prosent = function(x, accuracy = 1, decimal.mark = ",", ...) {
  scales::percent(x,
    suffix = " %",
    accuracy = accuracy, decimal.mark = decimal.mark, ...
  )
}


### Prosent med norsk stavemåte i aksenotasjoner

# fixme: Bør rydda opp i prosentfunksjonane slik at dei alle
#        tar same argument og elles er meir gjennomtenkte
#        (krev gjerne endringar i filene som brukar dei).

#' Formater akse med prosentformat
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjonen brukes i ggplot-kall og konverterer akselabels til å vise
#' prosent med riktig format. Standard er mellomrom før prosenttegnet
#' (slik det skal være på norsk), eks. 0.5 --> "50 %", og med komma
#' som desimaltegn. Som standard blir tallene vist
#' med én desimal, men dette kan endres ved å spesifisere `antall_desimaler`.
#'
#' @param antall_desimaler Antall desimaler skal vises.
#' @param decimal.mark Instilling for desimaltall. Standard er ",".
#' @param ... Ytterligere argumenter.
#' @export
#' @examples
#' ggplot(iris, aes(x = Sepal.Length, y = Petal.Width)) +
#'   geom_point() +
#'   scale_y_continuous(labels = akse_prosent_format(antall_desimaler = 2))
akse_prosent_format = function(antall_desimaler = 1, decimal.mark = ",", ...) {
  accuracy = 1 / (10^antall_desimaler)

  scales::percent_format(
    suffix = " %",
    accuracy = accuracy, decimal.mark = decimal.mark, ...
  )
}


#' Formater akser med tallformat
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjonen brukes i ggplot-kall og konverterer akselabels til å vise tall
#' med tusenskille (standard er " ") og ønsket antall desimaler (standard er 2).
#'
#' @param antall_desimaler Hvor mange desimaler skal vises.
#' @param decimal.mark Instilling for desimaltegn. Standard er ",".
#' @param big.mark Instilling for tusenskille. Standard er " ".
#' @param ... Ytterligere argumenter.
#' @export
#' @examples
#' a = tibble::tibble(
#'   x = rnorm(100, 7500, 1000),
#'   y = runif(100, 0, 1)
#' )
#'
#' ggplot(a, aes(x = x, y = y)) +
#'   geom_point() +
#'   scale_x_continuous(labels = akse_tall_format(antall_desimaler = 0)) +
#'   scale_y_continuous(labels = akse_prosent_format(antall_desimaler = 1))
akse_tall_format = function(antall_desimaler = 2, decimal.mark = ",", big.mark = " ", ...) {
  accuracy = 1 / (10^antall_desimaler)

  scales::number_format(accuracy = accuracy, big.mark = big.mark, decimal.mark = decimal.mark)
}


# Liknande funksjon for formatering av prosentverdiar som LaTeX-tekst.

#' Vis prosent-verdi som LaTeX-tekst
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tar inn et tall og konverterer det til LaTeX-kommando for å skrive prosent-tegn i tekst.
#' @param x Tallet som skal skrives som prosentverdi.
#' @param desimalar Antall desimaler som skal vises.
#' @param tabell TRUE eller FALSE for å indikere om tallet skal brukes i en tabell, og dermed skal ha tabelltekst.
#' @export
#' @examples
#' menn = 5
#' kvinner = 7
#' andel_menn = menn / (menn + kvinner)
#'
#' # Til bruk i setning i latex
#' paste0("Andel menn er ", prosent(andel_menn), ".")
#'
#' # Til bruk i tabell i latex
#' prosent(andel_menn, desimalar = 1, tabell = TRUE)
prosent = function(x, desimalar = 0, tabell = FALSE) {
  prosent_tekst = x %>%
    purrr::map_chr(~ num(100 * .x, desimalar, tabell = tabell) %>%
      stringr::str_c("\\prosent"))
  ifelse(is.na(x), "\\textendash{}", prosent_tekst)
}
