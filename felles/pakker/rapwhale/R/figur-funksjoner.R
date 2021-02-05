# Dette skriptet inneholder en rekke funksjoner som er potensielt nyttige
# (og noen, uunnværlige) i registersammenheng og i andre sammenheng.

#' @importFrom magrittr %>%
#' @importFrom rlang enexpr syms eval_bare maybe_missing
#' @importFrom qicharts2 qic
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous
NULL

# Graffunksjoner ----------------------------------------------------------

#' Flytt-opp funksjon.
#'
#' Funksjon som flytter opp labels inni grafer hvis de kolliderer
#' @param y y-koordinat til (midten av) tekstane
#' @param tekst teksten i tekstane (berre brukt til å telja kor mange linjer det er).
#' @param hoyde høgda kvar linje tekst tar opp (i grafkoordinatar)
#' @export
flytt_opp = function(y, tekst, hoyde = .015) {
  tekst_ny = tekst[order(y)]
  y = y[order(y)]
  linjer = tekst_ny %>%
    stringr::str_split("\n") %>%
    sapply(length)
  nedre = y - linjer * hoyde / 2
  ovre = y + linjer * hoyde / 2
  for (i in 2:length(y)) {
    avs = nedre[i] - ovre[i - 1]
    if (avs < 0) {
      y[i] = y[i] - avs
      ovre[i] = ovre[i] - avs # Nedre treng me ikkje endra, sidan me ikkje brukar han
    }
  }
  y[match(tekst, tekst_ny)]
}

#' Lag linjegraf med 95 prosent konfidensintervall
#'
#' description
#' @param refline y-koordinaten til vassrett referanselinje
#' @param refline_df ev. dataramme med éi rad for kvart panel refline skal gjelda for
#' @param xlab tekst på x-aksen (standardverdi: "År")
#' @param ylab tekst på y-aksen (standardverdi: NULL (tom))
#' @param angle viss sann, vis verdiane på x-aksen på skrå (for å få plass til fleire)
#' @param konfint Legg til konfidensintervall på kvar punkt
#' @export
#' @examples
#' p = data.frame("År" = c(2016, 2017, 2018), "Andel" = c(0.25, 0.5, 0.6)) %>%
#'   ggplot(aes(x = År, y = Andel, ymin = c(0.2, 0.4, 0.5), ymax = c(0.3, 0.6, 0.7)))
#'
#' p + lag_fig_linje(konfint = FALSE, ylab = "Andel")
#' p + lag_fig_linje(refline = 0.55, konfint = FALSE, ylab = "Andel")
#' p + lag_fig_linje(refline = 0.55, konfint = TRUE, ylab = "Andel")
lag_fig_linje = function(refline = NULL, refline_df = NULL, xlab = "\uc5r", ylab = NULL,
                         angle = TRUE, konfint = TRUE, point_size = 2) {
  grafdel = list()
  colPrim = farger_kvalreg()$farger_hoved

  # Legg ev. til referanselinje(r)
  if (!is.null(refline)) {
    if (is.null(refline_df)) {
      grafdel = append(grafdel, list(ggplot2::geom_hline(yintercept = refline, col = colPrim[6], size = 2)))
    } else {
      grafdel = append(grafdel, list(ggplot2::geom_hline(
        data = refline_df,
        mapping = ggplot2::aes_string(yintercept = refline),
        col = colPrim[6], size = 2
      )))
    }
  }
  # Legg ev. til konfidensintervall (bak alt anna)
  if (konfint) {
    grafdel = append(grafdel, ggplot2::geom_linerange(size = .5, colour = colPrim[5]))
  }
  # Legg til resten
  grafdel = append(
    grafdel,
    list(
      ggplot2::geom_line(colour = colPrim[3], size = 1), # Linjer over tid
      ggplot2::geom_point(size = point_size, colour = colPrim[2]), # Punkt
      xlab(xlab),
      ylab(ylab),
      fjern_x = fjern_x()
    )
  )
  if (angle) {
    grafdel = append(grafdel, list(ggplot2::theme(
      axis.text.x =
        ggplot2::element_text(angle = 45, vjust = 0.5)
    )))
  }
  grafdel
}


# funksjon for å lage shewhart-diagram
# funksjonen krever et datasett (d) som inneholder teller (y) og en variabel for x.aksen (x) som
# ofte er en datovariabel, hvilket type shewhart-diagram det er er (velges "p" må også nevner, tilgjengelig i d, oppgis)
# gruppe er hvilken variabel man ønkser å dele opp et panel på, hvis ønskelig (default NULL),
# periode hvilket tidsrom (f.eks "month" eller "2 months", gjelder kun tidsvising)
# kan velge å legge til tittelen for plottet i tittel, x-aksenavn i x_navn og y-akse-navn i y-akse.
# krever pakkene tidyverse og qicharts2
lag_fig_shewhart = function(d, y, x, nevner = NULL, figtype, tittel = NULL,
                            gruppe = NULL, periode = NULL, x_navn = NULL, y_navn = NULL,
                            tidsvisning = TRUE, ...) {

  # definerer alle kolonner som skal være tilgjengelig inni datasettet (d)
  qic_x = enexpr(x)
  qic_y = enexpr(y)
  qic_n = enexpr(nevner)
  qic_facet = enexpr(gruppe)

  if (lubridate::is.Date(d[[qic_x]])) {
    skal_flippes = FALSE
  } else {
    skal_flippes = TRUE
  }

  # lager grunnplottet med alt som alle shewhart-diagram trenger + eventuelle tilleggsvalg, og ggplot2 tema
  plot = eval_bare(rlang::expr(qic(
    data = d, y = !!qic_y, n = maybe_missing(!!qic_n), x = !!qic_x, chart = figtype,
    title = tittel, xlab = x_navn, ylab = y_navn, show.labels = FALSE, x.period = periode, facets = ~ (!!qic_facet),
    flip = skal_flippes
  ))) +
    fjern_x() +
    fjern_y() +
    ggplot2::theme(legend.position = "none")

  # legger på ekstra tema under visse forhold
  if (figtype == "p") { # hvis det er p-chart ønsker vi norske prosenter fra funksjon i dette r-skriptet
    plot = plot + scale_y_continuous(labels = akse_prosent)
  }
  if (lubridate::is.Date(d[[qic_x]])) { # hvis det er en tidsvisning trenger vi en dot for punktene i linjediagrammet
    plot = plot + ggplot2::geom_point()
  }
  plot
}


#' Lag søylediagram
#'
#' @description Funksjon for å laga søylediagram.
#'
#' @param d Datasett som inkluderer dei variablane som skal brukast søylediagrammet.
#' @param x Variabel for x-aksen - Vanlegvis er dette ein kategorisk variabel.
#' @param y Variabel for y-aksen - Ein kontinuerleg variabel. Kan vere en prosent.
#' @param flip Logisk variabel som seier om diagrammet skal flippast.
#' @param facet Logisk variabel som seier om ein skal laga panel på ein variabel.
#' @param facet_gruppe Variabel som skal brukast for å laga panel. Brukast kun viss `facet` = TRUE.
#' @param facet_col Numerisk variabel som seier kor mange kolonnar med panel som skal lagast. Brukast kun viss `facet` = TRUE.
#' @param prosent Logisk variabel som seier om y-aksen er en prosent eller ikkje. Standard verdi FALSE.
#' @param farge Fargen på søylene. Standard er SKDE-blå. Kan endrast i andre sammenhengar.
#' @param ymax Maksverdi på y-aksen.
#' @param y_mellomrom Avstand mellom linjene på y-aksen.
#'
#' @details
#'
#' @return Eit søylediagram som ggplot-objekt.
#'
#' @export
#' @examples
#' d = tibble(gruppe = c("a", "b", "c"), verdi = c(2.6, 2.1, 3.2))
#' lag_fig_soyle(d, gruppe, verdi)
#' lag_fig_soyle(d, gruppe, verdi, flip = TRUE)
#' d = tibble(gruppe = c("a", "b", "c"), verdi = c(0.6, 0.5, 0.9))
#' lag_fig_soyle(d, gruppe, verdi, flip = TRUE, prosent = TRUE)
lag_fig_soyle = function(d, x, y, flip = FALSE, facet = FALSE, facet_gruppe = NULL, facet_col = NULL, prosent = FALSE,
                         farge = farger_kvalreg()$farger_hoved[3], ymax = NA, y_mellomrom = NULL, ...) {
  plott = ggplot(d, aes({{ x }}, {{ y }})) +
    geom_bar(stat = "identity", width = 2 / 3, fill = farge, ...) +
    xlab(NULL) +
    ylab(NULL)

  if (prosent) {
    plott = plott + scale_y_continuous(
      expand = expand_soyle(), labels = akse_prosent_format(0),
      limits = c(NA, 1), breaks = scales::breaks_width(0.1)
    )
  } else if (is.null(y_mellomrom)) {
    plott = plott + scale_y_continuous(expand = expand_soyle(), limits = c(0, ymax))
  } else {
    plott = plott + scale_y_continuous(expand = expand_soyle(), limits = c(0, ymax), breaks = scales::breaks_width(y_mellomrom))
  }

  if (facet) {
    facet_gruppe = enquo(facet_gruppe)

    plott = plott + facet_wrap(facet_gruppe, ncol = facet_col)
  }

  if (flip) {
    plott = plott + coord_flip() +
      fjern_y() + fjern_y_ticks()
  } else {
    plott = plott + fjern_x() +
      fjern_x_ticks()
  }
  plott
}

#' Lag histogram
#'
#' @description
#' Lager et ggplot2-basert histogram for valgt variabel,
#' med fornuftige standardverdier.
#'
#' @details
#' Tilsvarer et vanlig histogram laget med [ggplot2::ggplot()]
#' og [ggplot2::geom_histogram()],men med mer nyttige standardverdier
#' og enkelte visuelle forbedringer:
#'
#' - `boundary`-argumentet er som standard satt til 0, slik at
#'   søyler med heltallig `binwidth` alltid vil starte på et heltall.
#' - Søylene starter helt nede ved *x*-aksen (og har 5 % luft over seg).
#' - En kan enkelt velge avstanden som skal være mellom aksetallene
#'   (`aksetall_avstand`).
#' - Søylefargen er som standard lik standard årsrapportfarge.
#' - Vannrette akselinjer blir ikke vist.
#'
#' @param d Datasett som innholder variabelen som skal brukes i histogrammet.
#' @param x Variabelnavn (rånavn, ikke tekststreng) som skal brukes på *x*-aksen.
#' @param binwidth Søylebredde (som i [ggplot2::geom_histogram()]).
#' @param boundary Justeringsgrense for venstre søylekant
#'   (som i [ggplot2::geom_histogram()]). Bør vanligvis være 0.
#' @param fill Søylefarge (som i [ggplot2::geom_histogram()]).
#'   Hvis `NULL`, blir standard årsrapportfarge brukt.
#' @param aksetall_avstand Avstand/mellomrom som skal brukes mellom
#'   hvert aksetall. Hvis `NULL`, blir en fornuftig standardverdi brukt.
#' @param ... Eventuelle andre argument som skal videresendes til
#'   [ggplot2::geom_histogram()].
#'
#' @return
#' Et ggplot-objekt.
#'
#' @export
#' @examples
#' lag_fig_histogram(iris, Petal.Length, binwidth = .25, aksetall_avstand = .5)
#' lag_fig_histogram(iris, Petal.Width, binwidth = .2, aksetall_avstand = .4, fill = "#737373")
lag_fig_histogram = function(d, x, binwidth = 1, boundary = 0,
                             fill = NULL, aksetall_avstand = NULL, ...) {
  if (is.null(fill)) {
    fill = farger_kvalreg()$farger_hoved[3]
  }
  p = ggplot2::ggplot(d, aes(x = {{ x }})) +
    ggplot2::geom_histogram(binwidth = binwidth, boundary = boundary, fill = fill, ...) +
    scale_y_continuous(expand = expansion(mult = c(0.0, .05), add = 0)) +
    fjern_x()
  if (!is.null(aksetall_avstand)) {
    p = p + scale_x_continuous(breaks = scales::breaks_width(aksetall_avstand))
  }
  p
}
