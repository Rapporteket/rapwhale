# Første argument åleine på opningslinja får stå der
select(d_patient,
  id, birth_date
)

x = select(d_patient,
  id, birth_date
)

# Gjeld òg funksjonskall inne i andre funksjonskall
list(
  a = select(d_patient,
    id, birth_date
  )
)
