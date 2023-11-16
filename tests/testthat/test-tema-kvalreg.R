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

# Testing av expand_soyle() og expand_soyle_str_fig()

test_that("expand_soyle() gir ut riktig tallvektor", {
  riktig_tallvektor = c(0.00, 0.00, 0.05, 0.00)
  expect_identical(expand_soyle(), riktig_tallvektor)
})

test_that("expand_soyle_str_fig() gir ut riktig tallvektor", {
  riktig_tallvektor = c(0.00, 0.00, 0.09, 0.00)
  expect_identical(expand_soyle_str_fig(), riktig_tallvektor)
})
