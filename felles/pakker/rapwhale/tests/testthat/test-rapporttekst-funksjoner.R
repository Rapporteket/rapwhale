# Testing av akse_prosent_format()

test_that("akse_prosent_format() gir ut verdier på riktig format ved 0 desimaler", {
  prosent_format_0_desimaler = akse_prosent_format(antall_desimaler = 0)
  expect_identical(prosent_format_0_desimaler(0.0468), "5 %")
})

test_that("akse_prosent_format() gir ut verdier på riktig format ved 1 desimal", {
  prosent_format_1_desimal = akse_prosent_format()
  expect_identical(prosent_format_1_desimal(0.0468), "4,7 %")
})

test_that("akse_prosent_format() gir ut verdier på riktig format ved 2 desimaler", {
  prosent_format_2_desimaler = akse_prosent_format(antall_desimaler = 2)
  expect_identical(prosent_format_2_desimaler(0.0468), "4,68 %")
})

test_that("akse_prosent_format() gir ut verdier på riktig format ved å
           oppgi punktum som desimalskilletegn", {
  prosent_format_punktum_skilletegn = akse_prosent_format(decimal.mark = ".")
  expect_identical(prosent_format_punktum_skilletegn(0.0468), "4.7 %")
})
