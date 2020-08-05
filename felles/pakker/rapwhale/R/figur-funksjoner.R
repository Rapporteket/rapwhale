# Dette skriptet inneholder en rekke funksjoner som er potensielt nyttige
# (og noen, uunnværlige) i registersammenheng og i andre sammenheng.

#' @importFrom magrittr %>%
#' @importFrom colorspace coords
#' @importFrom rlang enexpr syms eval_bare maybe_missing
#' @importFrom lubridate is.Date
#' @importFrom qicharts2 qic
#' @import ggplot2


# Fargar og grafinnstillingar/-objekt -------------------------------------

# Dei offisielle fargene (som eg ikkje er så glad i)
# du mener, som INGEN liker.
colPrim = c("#000059", "#084594", "#2171b5", "#4292c6", "#6baed6", "#c6dbef") # Primærfarge (mørk til lys)
colNoyt = c("#4D4D4D", "#737373", "#A6A6A6", "#DADADA") # Nøytralfarge
colKontr = "#FF7260" # Kontrastfarge

# ggplot2-tema for figurar
if (!exists("skriftstorleik")) { # Skriftstorleik bør vera definert i kvar årsrapportfil
  skriftstorleik = 9
}
tema = theme_light(base_size = skriftstorleik)
tema$panel.grid.minor$colour = "white"
tema$strip.background$fill = "#f3f1ee"
tema$strip.background$colour = "#e4e0da"
tema$strip.text.x = element_text(colour = "black")
tema$panel.spacing = unit("13", "pt")
tema$panel.border$colour = tema$strip.background$colour
tema$panel.grid.major$colour = tema$strip.background$colour
tema$panel.grid.minor$colour = tema$strip.background$fill
tema$axis.title.y$angle = 0
tema$axis.title.y$margin = margin(r = 5)
tema$axis.title.x$margin = margin(t = 5)

# Fjern vannrette eller loddrette rutenett
fjern_x = theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)
fjern_y = theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank()
)

# Fjern strekmarkeringar for viste tal/kategoriar
# (tilsvarer «major breaks» på aksen).
# Dette er nyttig for søylediagram med kategoriske
# verdiar, der strekmarkeringane er unødvendige/stygge.
fjern_x_ticks = theme(axis.ticks.x = element_blank())
fjern_y_ticks = theme(axis.ticks.y = element_blank())

# Søyler skal i starta heilt inn til aksen, men ha litt luft
# over seg, altså asymmetriske expand-verdiar. Her er ein
# variabel som definerer dette, og som ein kan mata til
# expand-argumentet til skaladefinisjonar.
expand_soyle = expansion(mult = c(0.0, .05), add = 0)

# I noen tilfeller er det ikke tilstrekkelig plass for tekst-label i plot.
# Da kan vi bruke expand-argumentet under.
expand_soyle_str_fig = expansion(mult = c(0.0, .09), add = 0)

# Fjern luft til venstre for y-akseteksten og legg
# til ekstra luft til høgre for han, fjern luft under
# x-akseteksten og legg til ekstra luft over han,
# fjern nesten all luft rundt figurane (eventuell
# nødvendig luft legg me til via LaTeX).
#
# (Merk: Me set den ytre margen til 3 punkt
# opp og nede i staden for 0 punkt for å sikra at at
# kantane på alle bokstavane alltid vert med.)
tema$axis.title.y$margin = margin(r = tema$text$size / 2)
tema$axis.title.x$margin = margin(t = tema$text$size / 2)
tema$plot.margin = margin(3, 3, 3, 3)


# Graffunksjoner ----------------------------------------------------------

#' Funksjonal for breaks-argument i ggplot2
#'
#' Funksjon som lager funksjon som tar inn to tall
#' og lager aritmetisk tallfølge med valgfri
#' intervallbredde slik at alle tall i følgen er
#' multiplum av intervallbredden og de to tallene
#' er innenfor range() av følgen (puh!).
#' Eks. er breaks_bredde(5)(c(9,16)) lik c(5,10,15,20).
#' Nyttig til bruk i breaks- og minor-breaks-argument
#' i ggplot2, eks. breaks = breaks_bredde(10)
#' Viss min eller maks er definert, bruk dette
#' i stedet for verdiene fra lims
#' @param bredde Valgt bredde, standard er 5
#' @param min Minste mulige verdi
#' @param maks Største mulige verdi
#' @export
sett_avkutningspunkt_bredde = function(bredde = 5, min = NULL, maks = NULL) {
  function(lims) {
    lims = c(max(min, lims[1]), min(maks, lims[2]))
    seq(round_any(lims[1], bredde, floor),
      round_any(lims[2], bredde, ceiling),
      by = bredde
    )
  }
}


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
lag_fig_linje = function(refline = NULL, refline_df = NULL, xlab = "\uc5r", ylab = NULL,
                         angle = TRUE, konfint = TRUE, point_size = 2) {
  grafdel = list()
  # Legg ev. til referanselinje(r)
  if (!is.null(refline)) {
    if (is.null(refline_df)) {
      grafdel = append(grafdel, list(geom_hline(yintercept = refline, col = colPrim[6], size = 2)))
    } else {
      grafdel = append(grafdel, list(geom_hline(
        data = refline_df,
        mapping = aes_string(yintercept = refline),
        col = colPrim[6], size = 2
      )))
    }
  }
  # Legg ev. til konfidensintervall (bak alt anna)
  if (konfint) {
    grafdel = append(grafdel, geom_linerange(size = .5, colour = colPrim[5]))
  }
  # Legg til resten
  grafdel = append(
    grafdel,
    list(
      geom_line(colour = colPrim[3], size = 1), # Linjer over tid
      geom_point(size = point_size, colour = colPrim[2]), # Punkt
      xlab(xlab),
      ylab(ylab),
      fjern_x
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
lag_fig_shewhart = function(d, y, x, nevner = NULL, figtype, tittel = NULL,
                            gruppe = NULL, periode = NULL, x_navn = NULL, y_navn = NULL,
                            tidsvisning = TRUE, ...) {

  # definerer alle kolonner som skal være tilgjengelig inni datasettet (d)
  qic_x = enexpr(x)
  qic_y = enexpr(y)
  qic_n = enexpr(nevner)
  qic_facet = enexpr(gruppe)

  if (is.Date(d[[qic_x]])) {
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
    fjern_x +
    fjern_y +
    theme(legend.position = "none")

  # legger på ekstra tema under visse forhold
  if (figtype == "p") { # hvis det er p-chart ønsker vi norske prosenter fra funksjon i dette r-skriptet
    plot = plot + scale_y_continuous(labels = akse_prosent)
  }
  if (is.Date(d[[qic_x]])) { # hvis det er en tidsvisning trenger vi en dot for punktene i linjediagrammet
    plot = plot + geom_point()
  }
  plot
}

# Fargefunksjonar ---------------------------------------------------------

#' Lag mørkare/lysare fargar
#'
#' Gjer ein vektor med fargar mørkare.
#' Brukar CIELAB-fargerommet til utrekningar
#' (i staden for RGB-fargerommet), for
#' betre resultat (meir tilpassa korleis
#' synet vårt fungerer).
#' @param fargar en vektor med farger.
#' @param grad seier kor mykje mørkare fargen skal gjerast (so bruk negative verdiar for å gjera han lysare).
#' @export
farge_morkare = function(fargar, grad = 5) {
  farge_lab = as(colorspace::hex2RGB(fargar), "LAB")
  farge_lab@coords[, 1] = pmax(farge_lab@coords[, 1] - grad, 0)
  farge_rgb = as(farge_lab, "RGB")
  farge_rgb@coords[] = pmax(farge_rgb@coords, 0)
  farge_rgb@coords[] = pmin(farge_rgb@coords, 1)
  colorspace::hex(farge_rgb)
}


#' Lag Søylediagram
#'
#' Funksjon for å lage søylediagram.
#'
#' @param d Datasett som inkluderer de variablene man ønsker å bruke i søylediagrammet.
#' @param x Variabel for x-aksen - Vanligvis er dette en kategorisk variabel.
#' @param y Variabel for y-aksen - En kontinuerlig variabel. Kan være en prosent.
#' @param farge Fargen på søylene. Default er SKDE-blå. Kan endres i andre sammenhenger.
#' @param facet Boolsk variabel som bestemmer om man skal lage panel på en variabel. TRUE eller FALSE.
#' @param facet_gruppe Hvilken variabel som skal brukes for å dele på panel. Brukes kun hvis facet = TRUE.
#' @param prosent Om Y-aksen er en prosent (TRUE) eller ikke (FALSE). Er prosent som standard, siden disse er mest vanlig.
#' @export
lag_fig_soyle = function(d, x, y, farge = ColPrim[3], facet = FALSE, facet_gruppe = NULL, prosent = TRUE, ...) {
  x_var = syms(x)[[1]]
  y_var = syms(y)[[1]]

  plott = ggplot(d, aes(x = !!x_var, y = !!y_var)) +
    geom_barh(stat = "identity", fill = farge, width = 2 / 3) +
    xlab(NULL) +
    ylab(NULL) +
    fjern_y +
    scale_x_continuous(expand = expand_soyle) +
    fjern_y_ticks
  if (facet) {
    facet_gruppe = syms(facet_gruppe)[[1]]
    plott = plott + facet_wrap(vars(!!facet_gruppe))
    plott
  }
  if (prosent) {
    plott = plott +
      scale_x_continuous(labels = akse_prosent, limits = c(NA, 1), breaks = breaks_bredde(0.1), expand = expand_soyle)
    plott
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
#' lag_fig_histogram(iris, Petal.Length,
#'   binwidth = .25, aksetall_avstand = .5
#' )
lag_fig_histogram = function(d, x, binwidth = 1, boundary = 0,
                             fill = NULL, aksetall_avstand = NULL, ...) {
  if (is.null(fill)) {
    fill = colPrim[3] # fixme: Ikkje bruk ikkje-anonym global variabel!
  }
  p = ggplot2::ggplot(d, aes(x = {{ x }})) +
    geom_histogram(binwidth = binwidth, boundary = boundary, fill = fill, ...) +
    scale_y_continuous(expand = expansion(mult = c(0.0, .05), add = 0)) +
    fjern_x # fixme: Ikkje bruk ikkje-anonym global variabel!
  if (!is.null(aksetall_avstand)) {
    p = p + scale_x_continuous(breaks = scales::breaks_width(aksetall_avstand))
  }
  p
}
