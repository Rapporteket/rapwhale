#' Tema for bruk i figurer
#'
#' @description
#' Inneholder temainnstillinger som skal brukes i figurer
#' i årsrapporter.
#'
#' @return Objektet `tema` som inneholder temainnstillinger.
#' @export
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
