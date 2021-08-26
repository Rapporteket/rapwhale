# ---------- Testar for er_syntaktisk_fnr() --------------------

test_that("er_syntaktisk_fnr() gjev forventa resultat", {
  nummer = c(
    "123456789", "123456789ab", "12345612345", "123456123456",
    "abcdefghijk"
  )
  forventa = c(FALSE, FALSE, TRUE, FALSE, FALSE)
  expect_identical(er_syntaktisk_fnr(nummer), forventa)
})
