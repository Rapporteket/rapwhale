# Testing av impl() og ekviv()

# Eksempeldata
d_gyldig_eks = tribble(
  ~pas_id, ~bosted_by, ~bosted_bydel, ~operert, ~komplikasjoner,
  1, "Bergen", "Åsane", "Ja", "Nei",
  2, "Bergen", "Landås", "Ja", "Nei",
  3, "Oslo", "Tjuvholmen", "Nei", NA,
  4, "Bergen", "Fana", "Nei", NA,
  5, "Oslo", "Haugenstua", "Ja", "Ja"
)

# a og b som sammen har alle kombinasjoner av to av verdiene TRUE, FALSE og NA
a = rep(c(TRUE, FALSE, NA), each = 3)
b = rep(c(TRUE, FALSE, NA), 3)

test_that("impl() gir feilmelding hvis a eller b ikke er logiske vektorer", {
  feilmelding = "a og b må være logiske vektorer"
  expect_error(impl(1, TRUE), feilmelding)
  expect_error(impl(TRUE, 1), feilmelding)
  expect_error(impl(as.numeric(a), as.numeric(b)), feilmelding)
  expect_error(impl(as.character(a), as.character(b)), feilmelding)
})

test_that("impl() fungerer som forventet for inndata med lengde 0", {
  expect_identical(impl(logical(), logical()), logical())
  expect_identical(impl(logical(), TRUE), logical())
  expect_identical(impl(TRUE, logical()), logical())
})

test_that("impl() gir kun ut TRUE ved gyldig datasett", {
  expect_true(all(impl(
    d_gyldig_eks$bosted_bydel == "Tjuvholmen",
    d_gyldig_eks$bosted_by == "Oslo"
  )))
})

test_that("impl() gir ut en eller flere FALSE ved ugyldig datasett", {
  d_ugyldig_eks = d_gyldig_eks
  d_ugyldig_eks$bosted_bydel[1] = "Tjuvholmen"
  expect_false(all(impl(
    d_ugyldig_eks$bosted_bydel == "Tjuvholmen",
    d_ugyldig_eks$bosted_by == "Oslo"
  )))
})

test_that("impl() gir ut en eller flere FALSE ved feil implikasjoner", {
  expect_false(all(impl(
    d_gyldig_eks$bosted_bydel == "Tjuvholmen",
    d_gyldig_eks$bosted_by == "Bergen"
  )))
})

test_that("impl() gir aldri ut NA", {
  d_ugyldig_na_eks = d_gyldig_eks
  d_ugyldig_na_eks[1, ] = NA
  expect_true(all(impl(
    d_ugyldig_na_eks$bosted_bydel == "Tjuvholmen",
    d_ugyldig_na_eks$bosted_by == "Oslo"
  )))
})

test_that("impl() gir ut rett verdi for alle kombinasjoner av TRUE, FALSE og NA", {
  forventet = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  expect_identical(impl(a, b), forventet)
})

test_that("impl() fungerer som forventet for ulike lenger av a og b", {
  forventet_true = rep(c(TRUE, FALSE, FALSE), 3)
  forventet_false = !forventet_true
  expect_identical(impl(TRUE, b), forventet_true)
  expect_identical(impl(b, FALSE), forventet_false)

  forventet_ulik_lengde_a = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
  forventet_ulik_lengde_b = c(TRUE, FALSE, FALSE, rep(TRUE, 6))
  expect_identical(impl(c(TRUE, FALSE, NA), a), forventet_ulik_lengde_a)
  expect_identical(impl(a, c(TRUE, FALSE, NA)), forventet_ulik_lengde_b)
})

test_that("impl() gir advarsel hvis lengdene av a og b er ulike, og den ene ikke er et multiplum av den andre", {
  expect_warning(impl(c(TRUE, FALSE), rep(TRUE, 3)))
  expect_warning(impl(rep(TRUE, 3), c(TRUE, FALSE)))
})

test_that("Infiks-versjonen %impl% er identisk impl()", {
  expect_identical(`%impl%`, impl)
})


test_that("ekviv() gir feilmelding hvis a eller b ikke er logiske vektorer", {
  feilmelding = "a og b må være logiske vektorer"
  expect_error(ekviv(1, TRUE), feilmelding)
  expect_error(ekviv(TRUE, 1), feilmelding)
  expect_error(ekviv(as.numeric(a), as.numeric(b)), feilmelding)
  expect_error(ekviv(as.character(a), as.character(b)), feilmelding)
})

test_that("ekviv() fungerer som forventet for inndata med lengde 0", {
  expect_identical(ekviv(logical(), logical()), logical())
  expect_identical(ekviv(logical(), TRUE), logical())
  expect_identical(ekviv(TRUE, logical()), logical())
})

test_that("ekviv() gir kun ut TRUE ved gyldig datasett", {
  expect_true(all(ekviv(
    d_gyldig_eks$operert == "Ja",
    !is.na(d_gyldig_eks$komplikasjoner)
  )))
})

test_that("ekviv() gir ut en eller flere FALSE ved ugyldig datasett", {
  d_ugyldig_eks = d_gyldig_eks
  d_ugyldig_eks$komplikasjoner[1] = NA
  expect_false(all(ekviv(
    d_ugyldig_eks$operert == "Ja",
    !is.na(d_ugyldig_eks$komplikasjoner)
  )))
})

test_that("ekviv() gir ut en eller flere FALSE ved feil ekvivalering", {
  expect_false(all(ekviv(
    d_gyldig_eks$bosted_bydel == "Tjuvholmen",
    d_gyldig_eks$bosted_by == "Oslo"
  )))
})

test_that("ekviv() gir aldri ut NA", {
  d_ugyldig_na_eks = d_gyldig_eks
  d_ugyldig_na_eks[1, ] = NA
  expect_true(all(ekviv(
    d_ugyldig_na_eks$operert == "Ja",
    !is.na(d_ugyldig_na_eks$komplikasjoner)
  )))
})

test_that("ekviv() gir ut rett verdi for alle kombinasjoner av TRUE, FALSE og NA", {
  forventet = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
  expect_identical(ekviv(a, b), forventet)
})

test_that("ekviv() fungerer som forventet for ulike lenger av a og b", {
  forventet_true = rep(c(TRUE, FALSE, FALSE), 3)
  expect_identical(ekviv(TRUE, b), forventet_true)
  expect_identical(ekviv(b, TRUE), forventet_true)

  forventet_ulik_lengde = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
  expect_identical(ekviv(c(TRUE, FALSE, NA), a), forventet_ulik_lengde)
  expect_identical(ekviv(a, c(TRUE, FALSE, NA)), forventet_ulik_lengde)
})

test_that("ekviv() gir advarsel hvis lengdene av a og b er ulike, og den ene ikke er et multiplum av den andre", {
  ekviv(c(TRUE, FALSE), rep(TRUE, 3)) |>
    expect_warning(
      "longer object length is not a multiple of shorter object length"
    ) |>
    # Åtvaringa kjem to gonger, sidan ekviv() køyrer impl() to gonger,
    # og i testthat versjon 3 må alle åtvaringar testast eksplisitt
    expect_warning(
      "longer object length is not a multiple of shorter object length"
    )
  ekviv(rep(TRUE, 3), c(TRUE, FALSE)) |>
    expect_warning(
      "longer object length is not a multiple of shorter object length"
    ) |>
    # Åtvaringa kjem to gonger, sidan ekviv() køyrer impl() to gonger,
    # og i testthat versjon 3 må alle åtvaringar testast eksplisitt
    expect_warning(
      "longer object length is not a multiple of shorter object length"
    )
})

test_that("ekviv(a, b) gir samme resultat som ekviv(b, a)", {
  expect_identical(ekviv(a, b), ekviv(b, a))
})

test_that("Infiks-versjonen %ekviv% er identisk ekviv()", {
  expect_identical(`%ekviv%`, ekviv)
})
