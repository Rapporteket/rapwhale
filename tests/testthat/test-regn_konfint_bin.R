# regn_ki_bin() -----------------------------------------------------------

test_that("Funksjonen gir konfindensintervall med øvre grense 1 og nedre nedre grense 0", {
  expect_false(any(regn_konfint_bin(1:1000, 1:1000)$upper > 1))
  expect_false(any(regn_konfint_bin(0, 1:1000)$lower < 0))
})
