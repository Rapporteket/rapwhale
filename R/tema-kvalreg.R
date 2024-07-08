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

  skde_bla_to = c(
    farger$farger_hoved[3],
    farger$farger_hoved[5]
  )
  skde_bla_tre = c(
    farger$farger_hoved[1],
    farger$farger_hoved[3],
    farger$farger_hoved[5]
  )
  skde_bla_fire = c(
    farger$farger_hoved[1],
    farger$farger_hoved[3],
    farger$farger_hoved[5],
    farger$farger_hoved[6]
  )
  skde_bla_fem = c(
    farger$farger_hoved[1],
    farger$farger_hoved[2],
    farger$farger_hoved[3],
    farger$farger_hoved[5],
    farger$farger_hoved[6]
  )
  skde_bla_seks = farger$farger_hoved
  skde_bla_liste = list(
    skde_bla_to,
    skde_bla_tre,
    skde_bla_fire,
    skde_bla_fem,
    skde_bla_seks
  )

  options(
    ggplot2.discrete.colour = skde_bla_liste,
    ggplot2.discrete.fill = skde_bla_liste
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
  tema$strip.background$fill = "#f3f1ee"
  tema$strip.background$colour = "#e4e0da"
  tema$strip.text.x = element_text(colour = "black")
  tema$panel.spacing = ggplot2::unit("13", "pt")
  tema$panel.border$colour = tema$strip.background$colour
  tema$panel.grid.major$colour = tema$strip.background$colour
  tema$panel.grid.minor$colour = tema$strip.background$fill
  tema$axis.title.y$angle = 0

  # Fjern luft til venstre for y-akseteksten og legg
  # til ekstra luft til høgre for han, fjern luft under
  # x-akseteksten og legg til ekstra luft over han,
  # fjern nesten all luft rundt figurane (eventuell
  # nødvendig luft legg me til via LaTeX).
  #
  # (Merk: Me set den ytre margen til 3 punkt
  # opp og nede i staden for 0 punkt for å sikra at at
  # kantane på alle bokstavane alltid vert med.)
  tema$axis.title.y$margin = ggplot2::margin(r = tema$text$size / 2)
  tema$axis.title.x$margin = ggplot2::margin(t = tema$text$size / 2)
  tema$plot.margin = ggplot2::margin(3, 3, 3, 3)

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
  expand_soyle = ggplot2::expansion(mult = c(0.0, 0.05), add = 0)
}

# I noen tilfeller er det ikke tilstrekkelig plass for tekst-label i plot.
# Da kan vi bruke expand-argumentet under.
#' @rdname expand_soyle
#' @export
expand_soyle_str_fig = function() {
  expand_soyle_str_fig = ggplot2::expansion(mult = c(0.0, 0.09), add = 0)
}
