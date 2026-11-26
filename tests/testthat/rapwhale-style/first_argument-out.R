# Første argument åleine på opningslinja får stå der
select(d_patient,
  id, birth_date
)

x = select(d_patient,
  id, birth_date
)

# Med namngjevne argument gjeld styler sin regel som før
map(x, f,
  a = 10,
  b = 2
)

# Linjeskift etter ( vert ikkje fjerna
select(
  d_patient,
  id, birth_date
)

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

# Gjeld òg funksjonskall inne i andre funksjonskall
list(
  a = select(d_patient,
    id, birth_date
  )
)

# Kommentar etter første argument gjev linjeskift etter ( som før
select(
  d, # data
  id
)

# switch() som før
switch(x,
  a = 1,
  b = 2
)

# if_else() som før, også med fleire argument på opningslinja
if_else(x > 0, "positiv",
  "ikkje positiv"
)
