test_that("Utdata skal seia (elementvis) om verdi1 er lik verdi2", {
  expect_identical(
    samanlikn_identisk(
      varnamn = rep("vekt", 5),
      verdi1 = c(74, 74, 74, NA, NA),
      verdi2 = c(74, 80, NA, 74, NA)
    ),
    c(TRUE, FALSE, FALSE, FALSE, TRUE)
  )

  expect_identical(
    samanlikn_identisk(
      varnamn = rep("dato", 5),
      verdi1 = c(74, 74, 74, NA, NA) + Sys.Date(),
      verdi2 = c(74, 80, NA, 74, NA) + Sys.Date()
    ),
    c(TRUE, FALSE, FALSE, FALSE, TRUE)
  )

  expect_identical(
    samanlikn_identisk(
      varnamn = rep("vekt", 7),
      verdi1 = c("foo", "foo", "", "", "NA", NA, NA),
      verdi2 = c("foo", "bar", NA, "", NA, "", NA)
    ),
    c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
  )

  expect_identical(
    samanlikn_identisk(
      varnamn = rep("janei", 8),
      verdi1 = c(TRUE, TRUE, FALSE, TRUE, FALSE, NA, NA, NA),
      verdi2 = c(TRUE, FALSE, TRUE, NA, NA, TRUE, FALSE, NA)
    ),
    c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
})

test_that("Skal handtera NA-verdiar", {
  expect_identical(
    samanlikn_identisk(
      varnamn = "dato_ut",
      verdi1 = NA,
      verdi2 = NA
    ),
    TRUE
  )

  expect_identical(
    samanlikn_identisk(
      varnamn = "vekt",
      verdi1 = c(75, 66),
      verdi2 = c(NA, 66)
    ),
    c(FALSE, TRUE)
  )
})
