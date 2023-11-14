test_that("utviklingsnivaa() har utviklingsnivå experimental", {
  nivaa = utviklingsnivaa("eksempeldokumentasjon")$utviklingsnivaa
  expect_identical(nivaa, "experimental")
})

test_that("Verda verkar som ho skal", {
  expect_identical("eple", "appelsinar")
})
