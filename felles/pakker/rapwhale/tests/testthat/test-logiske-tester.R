# Testing av impl() og ekviv()

# Eksempeldata
d_gyldig_eks = tibble::tribble(
  ~pas_id, ~bosted_by, ~bosted_bydel, ~operert, ~komplikasjoner,
  1, "Bergen", "Åsane", "Ja", "Nei",
  2, "Bergen", "Landås", "Ja", "Nei",
  3, "Oslo", "Tjuvholmen", "Nei", NA,
  4, "Bergen", "Fana", "Nei", NA,
  5, "Oslo", "Haugenstua", "Ja", "Ja"
)

context("impl")

test_that("impl() gir kun ut TRUE ved gyldig datasett", {
  expect_identical(
    all(impl(
      d_gyldig_eks$bosted_bydel == "Tjuvholmen",
      d_gyldig_eks$bosted_by == "Oslo"
    )),
    TRUE
  )
})

test_that("impl() gir ut en eller flere FALSE ved ugyldig datasett", {
  d_ugyldig_eks = d_gyldig_eks
  d_ugyldig_eks$bosted_bydel[1] = "Tjuvholmen"
  expect_identical(
    all(impl(
      d_ugyldig_eks$bosted_bydel == "Tjuvholmen",
      d_ugyldig_eks$bosted_by == "Oslo"
    )),
    FALSE
  )
})

test_that("impl() gir ut en eller flere FALSE ved feil implikasjoner", {
  expect_identical(
    all(impl(
      d_gyldig_eks$bosted_bydel == "Tjuvholmen",
      d_gyldig_eks$bosted_by == "Bergen"
    )),
    FALSE
  )
})

test_that("impl() gir aldri ut NA", {
  d_ugyldig_na_eks = d_gyldig_eks
  d_ugyldig_na_eks[1, ] = NA
  expect_identical(
    all(impl(
      d_ugyldig_na_eks$bosted_bydel == "Tjuvholmen",
      d_ugyldig_na_eks$bosted_by == "Oslo"
    )),
    TRUE
  )
})

context("ekviv")

test_that("ekviv() gir kun ut TRUE ved gyldig datasett", {
  expect_identical(
    all(ekviv(
      d_gyldig_eks$operert == "Ja",
      !is.na(d_gyldig_eks$komplikasjoner)
    )),
    TRUE
  )
})

test_that("ekviv() gir ut en eller flere FALSE ved ugyldig datasett", {
  d_ugyldig_eks = d_gyldig_eks
  d_ugyldig_eks$komplikasjoner[1] = NA
  expect_identical(
    all(ekviv(
      d_ugyldig_eks$operert == "Ja",
      !is.na(d_ugyldig_eks$komplikasjoner)
    )),
    FALSE
  )
})

test_that("ekviv() gir ut en eller flere FALSE ved feil ekvivalering", {
  expect_identical(
    all(ekviv(
      d_gyldig_eks$bosted_bydel == "Tjuvholmen",
      d_gyldig_eks$bosted_by == "Oslo"
    )),
    FALSE
  )
})

test_that("ekviv() gir aldri ut NA", {
  d_ugyldig_na_eks = d_gyldig_eks
  d_ugyldig_na_eks[1, ] = NA
  expect_identical(
    all(ekviv(
      d_ugyldig_na_eks$operert == "Ja",
      !is.na(d_ugyldig_na_eks$komplikasjoner)
    )),
    TRUE
  )
})
