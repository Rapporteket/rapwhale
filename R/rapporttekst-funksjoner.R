# Dette skriptet inneholder en rekke funksjoner som er potensielt nyttige
# (og noen, uunnværlige) i registersammenheng og i andre sammenheng.

# LaTeX-/rapportskrivingsfunksjoner ---------------------------------------

### Funksjon for å legge inn tall i en rapport som bruker LaTeX

#' Konverter tal til LaTeX-format
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tar inn ein vektor med tal og gjev ut ein vektor
#' med LaTeX-kode for finformatert vising av tala,
#' for eksempel med tusenskiljeteikn
#' og med avrunding til eit fast tal desimalar.
#'
#' @param x Vektor med tala ein ønskjer å få ut på LaTeX-format.
#' @param desimalar Kor mange desimalar som skal visast etter kommaet.
#'
#' @details
#' Tala vert viste med tusenskiljeteikn («12 345», ikkje «12345»),
#' desimalteiknet vert
#' (viss språket er norsk)
#' komma («3,14»),
#' ikkje punktum («3.14»),
#' negative tal vert viste med ekte minusteikn («−42»),
#' ikkje bindestrek («-42»),
#' `NA`-verdiar vert viste som ein kort tankestrek («–»)
#' og `NULL`-verdiar returnerer ein tom tekstvektor.
#'
#' Tala vert avrunda til `desimalar` desimalar
#' med den vanlege avrundingsregelen i R.
#' Viss `desimalar` er NULL,
#' vert det vist så mange desimalar som
#' [format()]-funksjonen viser som standard.
#' (Men,
#' i motsetning til den funksjonen,
#' vert tala aldri viste i eksponentiell notasjon.)
#'
#' Merk at funksjonen berre skal brukast på vanlege tal,
#' ikkje årstal,
#' fødselsnummer eller liknande.
#'
#' Det vert brukt ein funksjon gjort tilgjengeleg av
#' `kvalreg-rapport`-klassen
#' for formatering av tala.
#' Nøyaktig kva funksjon som vert brukt,
#' kan endrast i framtida,
#' men funksjonaliteten vil vera lik.
#'
#' @note Viss `desimalar` er oppgjeve,
#'   vert det alltid vist *nøyaktig* så mange desimalar,
#'   sjølv om dei siste vert 0.
#'
#' @return
#' Vektor med LaTeX-kode.
#'
#' @export
#' @examples
#' # Til bruk i setningar i LaTeX
#' paste0("Store tal som ", num(123456789), " får mellomrom som tusenskiljeteikn.")
#' paste0("Pi avrunda til fire desimalar er ", num(pi, desimalar = 4), ".")
#' paste0("Nulldesimalar vert òg viste: ", num(12, desimalar = 2))
num = function(x, desimalar = NULL) {
  checkmate::assert_numeric(x)
  if (rlang::is_empty(x)) {
    x_form = as.character(x)
  } else {
    if (!is.null(desimalar)) {
      # LaTeX-kommandoen kan runda av for oss,
      # men me rundar av i R for å sikra at avrundinga
      # vert identisk som ved andre plassar der
      # me ikkje brukar LaTeX
      x = round(x, desimalar)
    }

    nsmall = if (is.null(desimalar)) {
      0L
    } else {
      desimalar
    }
    # Må køyra format() separat på kvart element for å unngå
    # at alle elementa får like mange desimalar (viss «desimalar» er NULL)
    x_form = map_chr(x, format, nsmall = nsmall, scientific = FALSE)
    x_form = paste0("\\numprint{", x_form, "}")
    x_form[is.na(x)] = "\\textendash{}"

    # Me legg *heile* LaTeX-kommandoen
    # mellom {} for å hindra problem ved bruk for eksempel inni shortcap-delen
    # av \caption[shortcap]{longcap} (eventuelle ]-teikn vert elles tolka
    # til å avslutta shortcap-argumentet, jf. https://tex.stackexchange.com/a/78416)
    x_form = paste0("{", x_form, "}")
  }
  x_form
}
### Prosent med norsk stavemåte i aksenotasjoner

# fixme: Bør rydda opp i prosentfunksjonane slik at dei alle
#        tar same argument og elles er meir gjennomtenkte
#        (krev gjerne endringar i filene som brukar dei).

#' Formater akse med prosentformat
#'
#' @description
#' `r lifecycle::badge("maturing")`
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
#' @return En funksjon med spesifiserte innstillinger for prosentformat.
#' @export
#' @examples
#' # Pakke for å laga figurar
#' library(ggplot2)
#'
#' ggplot(iris, aes(x = Sepal.Width / Sepal.Length, y = Petal.Width / Petal.Length)) +
#'   geom_point() +
#'   scale_x_continuous(labels = akse_prosent_format()) +
#'   scale_y_continuous(labels = akse_prosent_format(antall_desimaler = 0))
akse_prosent_format = function(antall_desimaler = 1,
                               decimal.mark = ",", # nolint: object_name_linter.
                               ...) {
  accuracy = 1 / (10^antall_desimaler)

  scales::percent_format(
    suffix = " %",
    accuracy = accuracy, decimal.mark = decimal.mark, ...
  )
}


#' Formater akser med tallformat
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Funksjonen brukes i ggplot-kall og konverterer akselabels til å vise tall
#' med tusenskille (standard er " ") og ønsket antall desimaler (standard er 2).
#'
#' @param antall_desimaler Hvor mange desimaler skal vises.
#' @param decimal.mark Instilling for desimaltegn. Standard er ",".
#' @param big.mark Instilling for tusenskille. Standard er " ".
#' @param ... Ytterligere argumenter.
#' @return En funksjon med spesifiserte innstillinger for tallformat.
#' @export
#' @examples
#' # Pakke for å laga figurar
#' library(ggplot2)
#' library(tibble)
#'
#' a = tibble(
#'   x = rnorm(100, 7500, 1000),
#'   y = runif(100, 0, 1)
#' )
#'
#' ggplot(a, aes(x = x, y = y)) +
#'   geom_point() +
#'   scale_x_continuous(labels = akse_tall_format(antall_desimaler = 0)) +
#'   scale_y_continuous(labels = akse_prosent_format(antall_desimaler = 1))
akse_tall_format = function(antall_desimaler = 2,
                            decimal.mark = ",", # nolint: object_name_linter.
                            big.mark = " ", # nolint: object_name_linter.
                            ...) {
  accuracy = 1 / (10^antall_desimaler)

  scales::number_format(accuracy = accuracy, big.mark = big.mark, decimal.mark = decimal.mark)
}


# Liknande funksjon for formatering av prosentverdiar som LaTeX-tekst.

#' Vis prosent-verdi som LaTeX-tekst
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tar inn en vektor med tall som konverteres til prosent
#' og gir ut en vektor med LaTeX-kode for finformatert
#' visning av tallene med prosenttegn.
#' Tallene blir avrundet til et bestemt antall desimaler.
#'
#' @param x Vektor med tallene en ønsker å konvertere til prosent og få ut på LaTeX-format.
#' @param desimalar Antall desimaler som skal vises etter kommaet.
#'
#' @details
#' Tallene blir konvertert til prosent og blir vist med LaTeX-kode
#' for formatering til prosenttegn
#' («0.1234» blir «12.34 %»).
#'
#' Funksjonen [num()] blir brukt til finformatering av tallene.
#' Funksjonaliteten til `num` er gitt i [beskrivelsen av funksjonen][num()].
#'
#' @return
#' Vektor med LaTeX-kode.
#'
#' @export
#' @examples
#' menn = 5
#' kvinner = 7
#' andel_menn = menn / (menn + kvinner)
#'
#' # Til bruk i setning i latex
#' paste0("Andel menn er ", prosent(andel_menn), ".")
#' paste0("Andel menn med 2 desimaler er ", prosent(andel_menn, desimalar = 2), ".")
prosent = function(x, desimalar = 0) {
  checkmate::assert_numeric(x)
  prosent_tall = num(100 * x, desimalar)
  if_else(is.na(x), prosent_tall, str_c(prosent_tall, "\\prosent"))
}
