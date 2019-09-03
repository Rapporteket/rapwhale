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


### Prosent med norsk stavemåte i aksenotasjoner

# fixme: Bør rydda opp i prosentfunksjonane slik at dei alle
#        tar same argument og elles er meir gjennomtenkte
#        (krev gjerne endringar i filene som brukar dei).

#' Vis desimaltal som prosent
#'
#' Tar inn eit desimaltal og viser det som prosent,
#' med mellomrom før prosentteiknet (slik det skal vera
#' på norsk), eks. 0.5 --> "50 %", og med komma
#' som desimalteikn. Som standard vert tala viste
#' med éin desimal, men dette kan ein endra ved
#' å spesifisera «accuracy», for eksempel «accuracy = 0.1»
#' for éin desimal eller «accuracy = .05» for å runda av til
#' næraste halve promille. Bruk «accuracy = NULL» for
#' automatisk/«smart» val av desimalar (vanlegvis ikkje tilrådd).
#'
#' @param x Tallet som skal skrives som prosent.
#' @param accuracy Antall desimaler som skal benyttes.
#' @param decimal.mark Skal desimalskille være "," eller "." Standard er ",".
#' @param ... Ytterligere argumenter.
#' @export
akse_prosent = function(x, accuracy = 1, decimal.mark = ",", ...) {
  scales::percent(x,
    suffix = " %",
    accuracy = accuracy, decimal.mark = decimal.mark, ...
  )
}
# Liknande funksjon for formatering av prosentverdiar som LaTeX-tekst.

#' Vis prosent-verdi som LaTeX-tekst
#'
#' Tar inn et tall og konverterer det til LaTeX-kommando for å skrive prosent-tegn i tekst.
#' @param x Tallet som skal skrives som prosentverdi.
#' @param desimalar Antall desimaler som skal vises.
#' @param tabell TRUE eller FALSE for å indikere om tallet skal brukes i en tabell, og dermed skal ha tabelltekst.
#' @export
prosent = function(x, desimalar = 0, tabell = FALSE) {
  prosent_tekst = x %>%
    purrr::map_chr(~ num(100 * .x, desimalar, tabell = tabell) %>%
      stringr::str_c("\\prosent"))
  ifelse(is.na(x), "\\textendash{}", prosent_tekst)
}


#' Koble sammen ord
#'
#' Funksjon som kjeder sammen ord grammatisk riktig. \cr
#' Hvis det er to ord, kjedes det med " og ", f.eks "Per og Kari".
#' Hvis det er flere, kjedes det med ", " bortsett fra siste ord som får " og ", f.eks "Per, Kari og Pål.". \cr \cr
#'
#' Tar inn en vektor med ord, og returenerer en tekststreng med ett element.
#'
#' @param ord En vektor med de ordene som skal kjedes sammen.
#' @param skiljeteikn Skilletegn mellom ordene. Standard er ",".
#' @param og Ord som settes inn mellom de to siste ordene. Standard er "og".
#' @export
kjed_ord = function(ord, skiljeteikn = ", ", og = " og ") {

  # gjør om missing til "NA" som tekststreng
  ord = stringr::str_replace_na(ord)

  # antall ord
  n = length(ord)

  # hvis det er er ingenting i objektet, returnes...ingenting
  if (n == 0) {
    tekst = paste0("")
    warning("")
  }
  if (n == 1) {
    tekst = ord
  } # Hvis det er 2 ord, bindes ordene sammen med " og ", men dette kan også endres i argumentet og.
  # F.eks om du vil bruke &-teignet
  if (n == 2) {
    tekst = str_flatten(ord, og)
  }
  # hvis det er flere enn 2 ord, bindes ordene sammen med skiljeteiknet, her komma, bortsett fra siste ord som får " og ".
  if (n > 2) {
    tekst = paste0(str_flatten(ord[1:n - 1], skiljeteikn), og, ord[n])
  }
  tekst
}
