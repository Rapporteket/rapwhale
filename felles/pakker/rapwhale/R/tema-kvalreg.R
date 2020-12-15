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
