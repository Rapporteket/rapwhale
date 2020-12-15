test_that("Gjev ut rett resultat for vektor med 1, 2, 3 eller 4/fleire element", {
  expect_identical(kjed_ord("Per"), "Per")
  expect_identical(kjed_ord(c("Per", "Kari")), "Per og Kari")
  expect_identical(kjed_ord(c("Per", "Kari", "Ola")), "Per, Kari og Ola")
  expect_identical(kjed_ord(c("Per", "Kari", "Ola", "Sakhwa")), "Per, Kari, Ola og Sakhwa")
})

test_that("Gjev ut rett resultat for vektor med tal", {
  expect_identical(kjed_ord(1:4), "1, 2, 3 og 4")
})

test_that("Gjev ut rett resultat for vektor med datoar", {
  expect_identical(kjed_ord(Sys.Date() + 1:4), paste0(Sys.Date() + 1, ", ", Sys.Date() + 2, ", ", Sys.Date() + 3, " og ", Sys.Date() + 4))
})

test_that("Gjev ut rett resultat med valt «skiljeteikn»-argument", {
  expect_identical(
    kjed_ord(c("Per", "Kari", "Ola", "Sakhwa"), skiljeteikn = "/"),
    "Per/Kari/Ola og Sakhwa"
  )
})

test_that("Gjev ut rett resultat med valt «og»-argument", {
  expect_identical(kjed_ord(c("Per", "Kari"), og = "/"), "Per/Kari")
  expect_identical(kjed_ord(c("Per", "Kari"), og = " & "), "Per & Kari")
  expect_identical(
    kjed_ord(c("Per", "Kari", "Ola", "Sakhwa"), og = " & "),
    "Per, Kari, Ola & Sakhwa"
  )
})

test_that("Ved tom vektor inn får ein tom tekstvektor ut", {
  expect_identical(kjed_ord(character()), character())
})

test_that("Gjev ut NA-verdiar som teksten «NA»", {
  expect_identical(kjed_ord(NA), "NA")
})
