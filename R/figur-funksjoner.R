# Dette skriptet inneholder en rekke funksjoner som er potensielt nyttige
# (og noen, uunnværlige) i registersammenheng og i andre sammenheng.

# Graffunksjoner ----------------------------------------------------------

#' Flytt-opp funksjon.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjon som flytter opp labels inni grafer hvis de kolliderer
#' @param y y-koordinat til (midten av) tekstane
#' @param tekst teksten i tekstane (berre brukt til å telja kor mange linjer det er).
#' @param hoyde høgda kvar linje tekst tar opp (i grafkoordinatar)
#' @export
flytt_opp = function(y, tekst, hoyde = 0.015) {
  tekst_ny = tekst[order(y)]
  y = sort(y, na.last = TRUE)
  linjer = tekst_ny |>
    str_split(stringr::fixed("\n")) |>
    lengths()
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
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' description
#' @param refline y-koordinaten til vassrett referanselinje
#' @param refline_df ev. dataramme med éi rad for kvart panel refline skal gjelda for
#' @param xlab tekst på x-aksen (standardverdi: "År")
#' @param ylab tekst på y-aksen (standardverdi: NULL (tom))
#' @param angle viss sann, vis verdiane på x-aksen på skrå (for å få plass til fleire)
#' @param konfint Legg til konfidensintervall på kvar punkt
#' @param point_size Storleik på punkt i grafen. Standardverdi er 2.
#' @export
#' @examples
#' library(ggplot2)
#' d = data.frame("År" = c(2016, 2017, 2018), "Andel" = c(0.25, 0.5, 0.6))
#' p = ggplot(d, aes(
#'   x = År,
#'   y = Andel,
#'   ymin = c(0.2, 0.4, 0.5),
#'   ymax = c(0.3, 0.6, 0.7)
#' ))
#'
#' p + lag_fig_linje(konfint = FALSE, ylab = "Andel")
#' p + lag_fig_linje(refline = 0.55, konfint = FALSE, ylab = "Andel")
#' p + lag_fig_linje(refline = 0.55, konfint = TRUE, ylab = "Andel")
lag_fig_linje = function(refline = NULL, refline_df = NULL, xlab = "\uc5r", ylab = NULL,
                         angle = TRUE, konfint = TRUE, point_size = 2) {
  grafdel = list()
  col_prim = farger_kvalreg()$farger_hoved

  # Legg ev. til referanselinje(r)
  if (!is.null(refline)) {
    if (is.null(refline_df)) {
      grafdel = append(grafdel, list(ggplot2::geom_hline(yintercept = refline, col = col_prim[6], linewidth = 2)))
    } else {
      grafdel = append(grafdel, list(ggplot2::geom_hline(
        data = refline_df,
        mapping = ggplot2::aes_string(yintercept = refline),
        col = col_prim[6], linewidth = 2
      )))
    }
  }
  # Legg ev. til konfidensintervall (bak alt anna)
  if (konfint) {
    grafdel = append(grafdel, ggplot2::geom_linerange(linewidth = 0.5, colour = col_prim[5]))
  }
  # Legg til resten
  grafdel = append(
    grafdel,
    list(
      ggplot2::geom_line(colour = col_prim[3], linewidth = 1), # Linjer over tid
      ggplot2::geom_point(size = point_size, colour = col_prim[2]), # Punkt
      xlab(xlab),
      ylab(ylab),
      fjern_x = fjern_x()
    )
  )
  if (angle) {
    grafdel = append(grafdel, list(theme(
      axis.text.x =
        element_text(angle = 45, vjust = 0.5)
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

#' Lag shewhart-diagram
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param d
#' Datasett som inkluderer dei variablane som skal brukast shewhart-diagrammet.
#' @param y
#' Vektor med tal som skal plottast på y-aksen (teljar).
#' @param x
#' Vektor med verdiar som skal plottast på x-aksen, ofte datoar.
#' @param nevner
#' Vektor med undergruppestorleikar (nemnar).
#' @param figtype
#' Tekstvektor med kva type diagram som skal lagast. Sjå [qicharts2::qic()]
#' for tilgjengelege typar.
#' @param tittel
#' Tekstvektor med tittel på plottet.
#' @param gruppe
#' Eventuell variabel det skal grupperast på.
#' @param periode
#' Tidsperiode brukt for å aggregera y-verdiar.
#' Til dømes `"month"` eller `"2 months"`.
#' @param x_navn
#' Tekststreng med namnet på x-aksen.
#' @param y_navn
#' Tekststreng med namnet på y-aksen.
#' @param ...
#' Eventuelle andre argument som vert gjeve vidare til [qicharts2::qic()].
#'
#' @return
#' Eit shewhart-diagram av typen `figtype`.
#' @export
#'
#' @examples
#' d = data.frame(
#'   dato = sample(
#'     seq.Date(
#'       from = as.Date("2021-01-01"),
#'       to = as.Date("2021-12-31"),
#'       by = "days"
#'     ),
#'     size = 1000,
#'     replace = TRUE
#'   ),
#'   avdeling = sample(
#'     x = c("HUS", "OUS", "UNN", "STA"),
#'     size = 1000, replace = TRUE
#'   ),
#'   teller = sample(
#'     x = c(TRUE, FALSE),
#'     size = 1000, replace = TRUE, prob = c(0.7, 0.3)
#'   ),
#'   nevner = sample(
#'     x = c(TRUE, FALSE),
#'     size = 1000, replace = TRUE, prob = c(0.9, 0.1)
#'   ),
#'   verdi = sample(100:250, size = 1000, replace = TRUE)
#' )
#'
#' # Setter teller til FALSE og verdi til NA i de radene som nevner er FALSE
#' # slik det ville vært for en ekte indikatorfunksjon.
#' d$teller[!d$nevner] = FALSE
#' d$verdi[!d$nevner] = NA
#'
#' lag_fig_shewhart(
#'   d = d, y = teller, x = dato, figtype = "p",
#'   nevner = nevner, gruppe = avdeling, periode = "2 months",
#'   x_navn = "Tidspunkt", y_navn = "Andel"
#' )
#'
#' lag_fig_shewhart(
#'   d = d, y = verdi, x = dato, figtype = "xbar",
#'   nevner = nevner, gruppe = avdeling, periode = "2 months",
#'   x_navn = "Tidspunkt", y_navn = "Verdi"
#' )
lag_fig_shewhart = function(d, y, x, figtype, nevner = NULL, tittel = NULL,
                            gruppe = NULL, periode = NULL, x_navn = NULL, y_navn = NULL,
                            ...) {
  # definerer alle kolonner som skal være tilgjengelig inni datasettet (d)
  qic_x = rlang::enexpr(x)
  qic_y = rlang::enexpr(y)
  qic_n = rlang::enexpr(nevner)
  qic_facet = rlang::enexpr(gruppe)

  if (lubridate::is.Date(d[[qic_x]])) {
    skal_flippes = FALSE
  } else {
    skal_flippes = TRUE
  }

  # lager grunnplottet med alt som alle shewhart-diagram trenger + eventuelle tilleggsvalg, og ggplot2 tema
  plot = rlang::eval_bare(rlang::expr(qicharts2::qic(
    data = d, y = !!qic_y, n = rlang::maybe_missing(!!qic_n), x = !!qic_x, chart = figtype,
    title = tittel, xlab = x_navn, ylab = y_navn, show.labels = FALSE, x.period = periode, facets = ~ (!!qic_facet),
    flip = skal_flippes, ...
  ))) +
    fjern_x() +
    fjern_y() +
    theme(legend.position = "none")

  # legger på ekstra tema under visse forhold
  if (figtype == "p") { # hvis det er p-chart ønsker vi norske prosenter fra funksjon i dette r-skriptet
    plot = plot + scale_y_continuous(labels = akse_prosent_format(0))
  }
  if (lubridate::is.Date(d[[qic_x]])) { # hvis det er en tidsvisning trenger vi en dot for punktene i linjediagrammet
    plot = plot + ggplot2::geom_point()
  }
  plot
}


#' Lag søylediagram
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjon for å laga søylediagram
#'
#' @param d
#' Datasett som inkluderer dei variablane som skal brukast søylediagrammet.
#' @param x
#' Variabel for x-aksen - Vanlegvis er dette ein kategorisk variabel.
#' @param y
#' Variabel for y-aksen - Ein kontinuerleg variabel.
#' For prosent, bruk [lag_fig_soyle_prosent()].
#' @param flip
#' Logisk variabel som seier om plottet skal flippast. Standard verdi = `TRUE`.
#' @param ...
#' Ekstra argument som vert gjeve vidare til [lag_fig_soyle_grunnplott()]
#' (og så vidare igjen til [ggplot2::geom_col()]).
#'
#' @return Eit søylediagram som ggplot-objekt.
#'
#' @export
#' @examples
#' library(tibble)
#'
#' d = tibble(gruppe = c("a", "b", "c"), verdi = c(2.6, 2.1, 3.2))
#' lag_fig_soyle(d, gruppe, verdi)
#' lag_fig_soyle(d, gruppe, verdi, flip = FALSE)
lag_fig_soyle = function(d, x, y, flip = TRUE, ...) {
  lag_fig_soyle_grunnplott(d, {{ x }}, {{ y }}, flip = flip, ...) +
    scale_y_continuous(
      expand = expand_soyle(),
      limits = c(0, NA)
    )
}

#' Lag søylediagram med prosent
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjon for å laga søylediagram med prosent
#'
#' @param d
#' Datasett som inkluderer dei variablane som skal brukast søylediagrammet.
#' @param x
#' Variabel for x-aksen - Vanlegvis er dette ein kategorisk variabel.
#' @param y
#' Variabel for y-aksen - Ein kontinuerleg variabel i prosent (andel).
#' @param flip
#' Logisk variabel som seier om plottet skal flippast. Standard verdi = `TRUE`.
#' @param ...
#' Ekstra argument som vert gjeve vidare til [lag_fig_soyle_grunnplott()]
#' (og så vidare igjen til [ggplot2::geom_col()]).
#'
#' @return Eit søylediagram som ggplot-objekt.
#'
#' @export
#' @examples
#' library(tibble)
#'
#' d = tibble(gruppe = c("a", "b", "c"), verdi = c(0.6, 0.1, 0.2))
#' lag_fig_soyle_prosent(d, gruppe, verdi)
#' lag_fig_soyle_prosent(d, gruppe, verdi, flip = FALSE)
lag_fig_soyle_prosent = function(d, x, y, flip = TRUE, ...) {
  lag_fig_soyle_grunnplott(d, {{ x }}, {{ y }}, flip = flip, ...) +
    scale_y_continuous(
      expand = expand_soyle(),
      labels = akse_prosent_format(0),
      limits = c(0, 1)
    )
}

#' Lag søylediagram-grunnplott
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Hjelpefunksjon for å laga søylediagram i [lag_fig_soyle()] og
#' [lag_fig_soyle_prosent()].
#'
#' @param d
#' Datasett som inkluderer dei variablane som skal brukast søylediagrammet.
#' @param x
#' Variabel for x-aksen - Vanlegvis er dette ein kategorisk variabel.
#' @param y
#' Variabel for y-aksen - Ein kontinuerleg variabel. Kan vera prosent.
#' @param flip
#' Logisk variabel som seier om plottet skal flippast. Standard verdi = `TRUE`.
#' @param ...
#' Ekstra argument som vert gjeve vidare til [ggplot2::geom_col()].
#'
#' @return Eit søylediagram som ggplot-objekt.
#'
#' @keywords internal
#'
#' @examples
#' library(tibble)
#'
#' d = tibble(gruppe = c("a", "b", "c"), verdi = c(2.6, 2.1, 3.2))
#' rapwhale:::lag_fig_soyle_grunnplott(d, gruppe, verdi)
#' rapwhale:::lag_fig_soyle_grunnplott(d, gruppe, verdi, flip = FALSE)
lag_fig_soyle_grunnplott = function(d, x, y, flip = TRUE, ...) {
  ggplot(d, aes({{ x }}, {{ y }})) +
    geom_col(width = 2 / 3, ...) +
    if (flip) {
      list(coord_flip(), fjern_y(), fjern_y_ticks())
    } else {
      list(fjern_x(), fjern_x_ticks())
    }
}

#' Lag histogram
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjon for å laga histogram.
#'
#' @param d
#' Datasett som inkluderer variabelen som skal brukast i histogrammet.
#' @param x
#' Variabelen som skal plottast.
#' @param binwidth
#' Søylebreidde (som i [ggplot2::geom_histogram()]). Standard er 1.
#' @param boundary
#' Justeringsgrense for venstre søylekant (som i [ggplot2::geom_histogram()]).
#' Bør vanlegvis vera 0, som òg er standardverdi.
#' @param breaks_width Avstand/mellomrom som skal brukast mellom kvart aksetal.
#' Viss `NULL`, vert ein fornuftig standardverdi brukt.
#' @param ...
#' Ekstra argument som vert gjeve vidare til [ggplot2::geom_histogram()].
#'
#' @details
#' Lagar eit vanleg histogram,
#' med [ggplot2::ggplot()] og [ggplot2::geom_histogram()],
#' men med meir nyttige standardverdiar og enkelte visuelle forbetringar:
#'
#' - `boundary`-argumentet er som standard sett til 0,
#' slik at søyler med heiltalig `binwidth` alltid vil starta på eit heiltal.
#' - Søylene starter heilt nede ved *x*-aksen, og har 5 % luft over seg.
#' - Ein kan enkelt velja avstanden som skal vera mellom aksetala
#' med `breaks_width`.
#' - Vassrette akselinjer vert ikkje vist.
#'
#' @return
#' Eit histogram som ggplot-objekt.
#'
#' @export
#' @examples
#' lag_fig_histogram(iris, Petal.Length, binwidth = .25, breaks_width = .5)
lag_fig_histogram = function(d, x,
                             binwidth = 1,
                             boundary = 0,
                             breaks_width = NULL,
                             ...) {
  p = ggplot(d, aes({{ x }})) +
    geom_histogram(binwidth = binwidth, boundary = boundary, ...) +
    scale_y_continuous(expand = expand_soyle()) +
    fjern_x()

  if (!is.null(breaks_width)) {
    p = p + scale_x_continuous(breaks = scales::breaks_width(breaks_width))
  }

  p
}
