# Testar for aktiver_kvalregtema
aktiver_kvalregtema()

test_that("aktiver_kvalregtema() set tema_kvalreg() som ggplot2-tema", {
  expect_identical(ggplot2::theme_get(), tema_kvalreg())
})

test_that("aktiver_kvalregtema() set ynskja standardverdiar for ggplot2-geom", {
  geom_point_standard = ggplot2::get_geom_defaults("point")
  geom_line_standard = ggplot2::get_geom_defaults("line")
  geom_linerange_standard = ggplot2::get_geom_defaults("linerange")
  geom_hline_standard = ggplot2::get_geom_defaults("hline")
  geom_vline_standard = ggplot2::get_geom_defaults("vline")
  geom_bar_standard = ggplot2::get_geom_defaults("bar")
  geom_col_standard = ggplot2::get_geom_defaults("col")

  expect_identical(geom_point_standard$size, 2)
  expect_identical(geom_point_standard$colour, "#2171b5")
  expect_identical(geom_line_standard$linewidth, 1)
  expect_identical(geom_line_standard$colour, "#2171b5")
  expect_identical(geom_linerange_standard$linewidth, 0.5)
  expect_identical(geom_linerange_standard$colour, "#2171b5")
  expect_identical(geom_hline_standard$linewidth, 0.5)
  expect_identical(geom_hline_standard$colour, "#000059")
  expect_identical(geom_vline_standard$linewidth, 0.5)
  expect_identical(geom_vline_standard$colour, "#000059")
  expect_identical(geom_bar_standard$fill, "#2171b5")
  expect_identical(geom_col_standard$fill, "#2171b5")
})

test_that("aktiver_kvalregtema() set ynskja innstillingar for qicharts2", {
  expect_identical(getOption("qic.signalcol"), "#FF7260")
  expect_identical(getOption("qic.linecol"), "#2171b5")
  expect_identical(getOption("qic.targetcol"), "#c6dbef")
})

# Testar for tema_kvalreg()
test_that("Temaet får dei faktisk ynskja verdiane", {
  test_tema = tema_kvalreg()

  expect_identical(test_tema$strip.background$fill, "#f3f1ee")
  expect_identical(test_tema$strip.background$colour, "#e4e0da")
  expect_identical(test_tema$strip.text.x, element_text(colour = "black"))
  expect_identical(test_tema$panel.spacing, ggplot2::unit("13", "pt"))
  expect_identical(test_tema$panel.border$colour, "#e4e0da")
  expect_identical(test_tema$panel.grid.major$colour, "#e4e0da")
  expect_identical(test_tema$panel.grid.minor$colour, "#f3f1ee")
  expect_identical(test_tema$axis.title.y$angle, 0)
  expect_identical(test_tema$axis.title.y$margin, ggplot2::margin(r = 4.5))
  expect_identical(test_tema$axis.title.x$margin, ggplot2::margin(t = 4.5))
  expect_identical(test_tema$plot.margin, ggplot2::margin(3, 3, 3, 3))
})

test_that("Rett skriftstorleik vert brukt viss han finst frå før", {
  skriftstorleik = 12
  withr::with_environment(environment(), {
    test_tema_skriftstorleik = tema_kvalreg()
  })
  expect_identical(test_tema_skriftstorleik$text$size, skriftstorleik)
  expect_identical(test_tema_skriftstorleik$axis.title.y$margin,
    expected = ggplot2::margin(r = skriftstorleik / 2)
  )
  expect_identical(test_tema_skriftstorleik$axis.title.x$margin,
    expected = ggplot2::margin(t = skriftstorleik / 2)
  )
})

# Testar for fjern_*()
p = ggplot(mtcars, aes(wt, mpg)) +
  ggplot2::geom_point()

test_that("fjern_x() fjernar det han skal", {
  p_x = p + fjern_x()
  expect_identical(p_x$theme$panel.grid.major.x, element_blank())
  expect_identical(p_x$theme$panel.grid.minor.x, element_blank())
})

test_that("fjern_x_ticks() fjernar det han skal", {
  p_x_ticks = p + fjern_x_ticks()
  expect_identical(p_x_ticks$theme$axis.ticks.x, element_blank())
})

test_that("fjern_y() fjernar det han skal", {
  p_y = p + fjern_y()
  expect_identical(p_y$theme$panel.grid.major.y, element_blank())
  expect_identical(p_y$theme$panel.grid.minor.y, element_blank())
})

test_that("fjern_y_ticks() fjernar det han skal", {
  p_y_ticks = p + fjern_y_ticks()
  expect_identical(p_y_ticks$theme$axis.ticks.y, element_blank())
})

# Testing av expand_soyle() og expand_soyle_str_fig()

test_that("expand_soyle() gir ut riktig tallvektor", {
  riktig_tallvektor = c(0.00, 0.00, 0.05, 0.00)
  expect_identical(expand_soyle(), riktig_tallvektor)
})

test_that("expand_soyle_str_fig() gir ut riktig tallvektor", {
  riktig_tallvektor = c(0.00, 0.00, 0.09, 0.00)
  expect_identical(expand_soyle_str_fig(), riktig_tallvektor)
})
