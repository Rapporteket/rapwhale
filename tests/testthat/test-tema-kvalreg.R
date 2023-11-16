# Testar for aktiver_kvalregtema
aktiver_kvalregtema()

test_that("aktiver_kvalregtema() set tema_kvalreg() som ggplot2-tema", {
  expect_identical(ggplot2::theme_get(), tema_kvalreg())
})

test_that("aktiver_kvalregtema() set ynskja standardverdiar for ggplot2-geom", {
  geom_point_standard = ggplot2:::check_subclass("point", "Geom")$default_aes
  geom_line_standard = ggplot2:::check_subclass("line", "Geom")$default_aes
  geom_linerange_standard = ggplot2:::check_subclass("linerange",
    subclass = "Geom"
  )$default_aes
  geom_hline_standard = ggplot2:::check_subclass("hline", "Geom")$default_aes
  geom_vline_standard = ggplot2:::check_subclass("vline", "Geom")$default_aes
  geom_bar_standard = ggplot2:::check_subclass("bar", "Geom")$default_aes
  geom_col_standard = ggplot2:::check_subclass("col", "Geom")$default_aes

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

# Testing av expand_soyle() og expand_soyle_str_fig()

test_that("expand_soyle() gir ut riktig tallvektor", {
  riktig_tallvektor = c(0.00, 0.00, 0.05, 0.00)
  expect_identical(expand_soyle(), riktig_tallvektor)
})

test_that("expand_soyle_str_fig() gir ut riktig tallvektor", {
  riktig_tallvektor = c(0.00, 0.00, 0.09, 0.00)
  expect_identical(expand_soyle_str_fig(), riktig_tallvektor)
})
