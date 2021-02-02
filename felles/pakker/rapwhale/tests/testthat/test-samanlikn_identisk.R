test_that("Utdata skal seia (elementvis) om verdi1 er lik verdi2", {
  expect_identical(
    samanlikn_identisk(
      varnamn = "dato_ut",
      verdi1 = as.Date("2020-06-07"),
      verdi2 = as.Date("2020-06-07")
    ),
    TRUE
  )

  expect_identical(
    samanlikn_identisk(
      varnamn = rep("vekt", 4),
      verdi1 = c(74, 72, 50, 63),
      verdi2 = c(74, 72, 50, 63)
    ),
    rep(TRUE, 4)
  )

  expect_identical(
    samanlikn_identisk(
      varnamn = rep("diabetes", 4),
      verdi1 = c(TRUE, TRUE, FALSE, TRUE),
      verdi2 = c(TRUE, TRUE, FALSE, FALSE)
    ),
    c(TRUE, TRUE, TRUE, FALSE)
  )
})
