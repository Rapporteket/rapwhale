# Fleire argument på opningslinja er ikkje tillate
gjer_noko(
  "som", krev, mange,
  "der nokre kan vera lange"
)

# Heller ikkje når første argument går over fleire linjer
select(
  c(
    a, b
  ),
  id
)

# Kommentar etter første argument gjev linjeskift etter ( som før
select(
  d, # data
  id
)
