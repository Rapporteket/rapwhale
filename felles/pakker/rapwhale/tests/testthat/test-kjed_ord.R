# Eksempeldata
namn1 = c("Per")
namn2 = c("Per", "Kari")
namn3 = c("Per", "Kari", "Ola")

test_that("Gjev ut rett mengd element, og av rett type", {
  expect_length(kjed_ord(namn3), 1)
  expect_type(kjed_ord(namn3), "character")
})

test_that("Gjev ut rett resultat for vektor med 1, 2 og 3 element", {
  expect_identical(kjed_ord(namn1), "Per")
  expect_identical(kjed_ord(namn2), "Per og Kari")
  expect_identical(kjed_ord(namn3), "Per, Kari og Ola")
})

test_that("Gjev ut rett resultat med vald `skiljeteikn` og `og`", {
  expect_identical(kjed_ord(namn3, skiljeteikn = "/"), "Per/Kari og Ola")
  expect_identical(kjed_ord(namn2, og = " & "), "Per & Kari")
  expect_identical(kjed_ord(namn3, og = " & "), "Per, Kari & Ola")
})

test_that("Ved tom vektor inn skal tom tekstvektor gjevast ut", {
  expect_identical(kjed_ord(NULL), character(0))
})

test_that("Skal endra NA-verdiar til tekst", {
  expect_identical(kjed_ord(NA), "NA")
})
