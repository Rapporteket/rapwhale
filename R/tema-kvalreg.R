#' @importFrom ggplot2 theme_set update_geom_defaults
#' @importFrom scales colour_ramp gradient_n_pal
NULL
#' Slå på kvalregtema for ggplot2 og qicharts2
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Set aktivt tema til [tema_kvalreg()],
#' og oppdaterer andre standardverdiar som fargar og punkt-/linjestorleik.
#'
#' @return
#' Returnerer `NULL` usynleg.
#' @export
#'
#' @examples
#' # Pakke for å laga figurar
#' library(ggplot2)
#'
#' # Sølediagram før aktivering av tema
#' p = ggplot(mpg, aes(class)) +
#'   geom_bar()
#' p
#'
#' aktiver_kvalregtema()
#'
#' # Søylediagram etter aktivering av tema
#' p
aktiver_kvalregtema = function() {
  theme_set(tema_kvalreg())

  farger = farger_kvalreg()
  skde_bla = farger$farger_hoved[3]
  skde_morkebla = farger$farger_hoved[1]
  skde_lysebla = farger$farger_hoved[6]
  kontrastfarge = farger$farger_kontr

  update_geom_defaults("point", list(size = 2, colour = skde_bla))
  update_geom_defaults("line", list(linewidth = 1, colour = skde_bla))
  update_geom_defaults("linerange", list(linewidth = 0.5, colour = skde_bla))
  update_geom_defaults("hline", list(linewidth = 0.5, colour = skde_morkebla))
  update_geom_defaults("vline", list(linewidth = 0.5, colour = skde_morkebla))
  update_geom_defaults("bar", list(fill = skde_bla))
  update_geom_defaults("col", list(fill = skde_bla))

  options(
    ggplot2.discrete.colour = scale_colour_kvalreg,
    ggplot2.discrete.fill = scale_fill_kvalreg,
    ggplot2.continuous.colour = scale_colour_kvalreg_kont,
    ggplot2.continuous.fill = scale_fill_kvalreg_kont
  )

  options(
    qic.signalcol = kontrastfarge,
    qic.linecol = skde_bla,
    qic.targetcol = skde_lysebla
  )

  invisible(NULL)
}

#' Tema for bruk i figurer
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Inneholder temainnstillinger som skal brukes i figurer
#' i årsrapporter.
#'
#' @return Objektet `tema` som inneholder temainnstillinger.
#' @export
#'
#' @examples
#' # Pakke for å laga figurar
#' library(ggplot2)
#'
#' # Sølediagram utan tema_kvalreg()
#' p = ggplot(mpg, aes(class)) +
#'   geom_bar()
#' p
#'
#' # Søylediagram med tema_kvalreg()
#' p + tema_kvalreg()
tema_kvalreg = function() {

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

  tema
}

#' Temainstillinger for figurer
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Disse funksjonene brukes for å gjøre mindre endringer for panellinjer og
#' strekmarkeringer på aksene i figurer.
#' De krever ingen argumenter og legges til direkte i et ggplot-kall.
#'
#' @details
#' Funksjoner som er inkludert her:
#' - `fjern_x()`: Fjerner panellinjer på x-aksen.
#' - `fjern_y()`: Fjerner panellinjer på y-aksen.
#' - `fjern_x_ticks()`: Fjerner strekmarkeringer på x-aksen.
#' - `fjern_y_ticks()`: Fjerner strekmarkeringer på y-aksen.
#'
#' Fjerning av panellinjer kan være nyttig for å redusere støy i figurer
#' hvor det ikke er like stort behov for følgelinjer på begge aksene.
#' Fjerning av strekmarkeringer kan være nyttig for søylediagram med
#' kategoriske verdier, der strekmarkeringen er unødvendig.
#' Se eksempel under før hvordan figurene blir seende ut med de ulike
#' funksjonene.
#'
#' @return
#' Funksjonene returnerer et theme-objekt som kan brukes direkte i et ggplot-kall.
#'
#' @examples
#' # Pakke for å laga figurar
#' library(ggplot2)
#'
#' p = ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
#'   geom_point()
#'
#' # Eksempel på hva de ulike funksjonene gjør
#' p
#' p + fjern_x()
#' p + fjern_y()
#' p + fjern_x_ticks()
#' p + fjern_y_ticks()
#'
#' # Flere av funksjonene kan brukes samtidig
#' p + fjern_x() +
#'   fjern_y_ticks()
#' @export
fjern_x = function() {
  fjern_x = theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
}

#' @rdname fjern_x
#' @export
fjern_y = function() {
  fjern_y = theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
}

#' @rdname fjern_x
#' @export
fjern_x_ticks = function() {
  fjern_x_ticks = theme(axis.ticks.x = element_blank())
}

#' @rdname fjern_x
#' @export
fjern_y_ticks = function() {
  fjern_y_ticks = theme(axis.ticks.y = element_blank())
}

#' Juster søylediagram
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Disse funksjonene brukes for å justere plassering av søyler i søylediagram.
#' Søylene starter helt inntil aksen og får litt luft i enden av søylen.
#'
#' @details
#' - `expand_soyle()`: Brukes for å flytte start av søyle helt
#' inn til aksen og legger til litt luft i enden av søylen.
#' - `expand_soyle_str_fig()`: Brukes i de tilfeller der det ikke
#' er tilstrekkelig plass til tekst-label i plot. Gir litt større
#' plass i enden av søylene.
#'
#' @return Returnerer en tall-vektor som brukes som expand-argument i
#' scale_* funksjoner.
#'
#' @examples
#' # Pakke for å laga figurar
#' library(ggplot2)
#'
#' p = ggplot(mtcars, aes(x = cyl, y = mpg)) +
#'   geom_col()
#'
#' # Default plot har ikke start av søyle helt inntil aksen
#' p
#' p + scale_y_continuous(expand = expand_soyle())
#'
#' # Legge til mer rom i enden av søylene for tekst-labels
#' p = ggplot(mtcars, aes(x = cyl)) +
#'   geom_bar() +
#'   geom_text(stat = "count", aes(x = cyl, label = ..count..), vjust = -1)
#'
#' p
#' p + scale_y_continuous(expand = expand_soyle_str_fig())
#' @export
expand_soyle = function() {
  expand_soyle = ggplot2::expansion(mult = c(0.0, .05), add = 0)
}

# I noen tilfeller er det ikke tilstrekkelig plass for tekst-label i plot.
# Da kan vi bruke expand-argumentet under.
#' @rdname expand_soyle
#' @export
expand_soyle_str_fig = function() {
  expand_soyle_str_fig = ggplot2::expansion(mult = c(0.0, .09), add = 0)
}

#' Kvalreg-fargeskala
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Sekvensielle fargeskalaar for diskrete og kontinuerlege data
#' basert på dei blåe «SKDE-fargane».
#'
#' @return
#' Ein diskret eller kontinuerleg fargeskala.
#'
#' @export
#' @rdname scale_kvalreg
#'
#' @examples
#' library(ggplot2)
#'
#' # Diskrete data
#'
#' ggplot(diamonds, aes(x = price, fill = cut)) +
#'   geom_histogram(position = "dodge", binwidth = 1000) +
#'   scale_fill_kvalreg()
#'
#' ggplot(mpg, aes(cty, hwy)) +
#'   geom_point(aes(colour = drv)) +
#'   scale_colour_kvalreg()
#'
#' # Kontinuerlege data
#'
#' ggplot(faithfuld) +
#'   geom_tile(aes(waiting, eruptions, fill = density)) +
#'   scale_fill_kvalreg_kont()
#'
#' gplot(mpg, aes(cty, hwy)) +
#'   geom_point(aes(colour = displ)) +
#'   scale_colour_kvalreg_kont()
scale_fill_kvalreg = function() {
  fargar = farger_kvalreg()$farger_hoved
  fargerampe = colour_ramp(fargar)
  palett = function(n) {
    fargerampe(seq(0, 1, length.out = n))
  }
  discrete_scale("fill", scale_name = "skde", palette = palett)
}

#' @rdname scale_kvalreg
#' @export
scale_colour_kvalreg = function() {
  fargar = farger_kvalreg()$farger_hoved
  fargerampe = colour_ramp(fargar)
  palett = function(n) {
    fargerampe(seq(0, 1, length.out = n))
  }
  discrete_scale("colour", scale_name = "skde", palette = palett)
}

#' @rdname scale_kvalreg
#' @export
scale_fill_kvalreg_kont = function() {
  fargar = farger_kvalreg()$farger_hoved
  palett = gradient_n_pal(fargar)
  continuous_scale("fill",
    scale_name = "skde",
    palette = palett,
    guide = "colourbar"
  )
}

#' @rdname scale_kvalreg
#' @export
scale_colour_kvalreg_kont = function() {
  fargar = farger_kvalreg()$farger_hoved
  palett = gradient_n_pal(fargar)
  continuous_scale("colour",
    scale_name = "skde",
    palette = palett,
    guide = "colourbar"
  )
}
