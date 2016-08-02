# Forsøk på lesing av det elendige kodebokformatet til MRS :(


# Oppsett -----------------------------------------------------------------

# Ikkje gjer om tekst til faktorar automatisk
options(stringsAsFactors = FALSE)

# Nødvendige pakkar
library(readxl) # Lesing av Excel-filer
library(dplyr) # Datamassering
library(stringr) # Tekstmassering
library(magrittr) # Funksjonar som kan brukast med røyr-operatoren



# Datainnlesing -----------------------------------------------------------

# Les inn eksempeldata
mappe = "***FJERNA-ADRESSE***"
filnamn = "20160801_Kodebok_NorArtritt.xlsx"
adresse = paste0(mappe, filnamn)

# Les inn (ei fane i) Excel-kodeboka
d = read_excel(adresse, sheet = 5)

# Lag ny kodebok ----------------------------------------------------------

# Indeks til rader som startar ein ny variabel
ind_nyvar = which(!is.na(d$Feltnavn))
nvars = length(ind_nyvar) # Talet på variablar

# Kor mange kodar kvar variabel har
# (Merk at siste variabel ikkje vert
#  avløyst av ein ny variabel, og må
#  derfor handterast spesielt.)
var_nverd = diff(ind_nyvar) - 1
var_nverd[nvars] = nrow(d) - ind_nyvar[nvars]

# Lag dataramme med i utgangspunktet éi rad for kvar variabel
kodebok_utg = data_frame(
  var_id = d$Variabelnavn[ind_nyvar],
  var_type = d$Felttype[ind_nyvar],
  kode = NA_integer_,
  kode_tekst = NA_character_,
  forklaring = d$Feltnavn[ind_nyvar] # Berre forklaring for *enkelte* variablar, men …
)

# Kor mange gongar kvar variabel skal gjentakast i
# den nye kodeboka, dvs. kor mange rader han skal oppta
reps = pmax(var_nverd, 1)

# Utvid kodeboka slik at enum-variablane får fleire rader
kodebok = kodebok_utg[rep(1:nvars, times = reps), ]

# Hent ut kodane og tilhøyrande tekst til alle Enum-variablane
enums = d %>%
  filter(is.na(Felttype)) %>%
  extract2("Variabelnavn") %>%
  str_split_fixed(" = ", n = 2)

# Legg kodane inn i den nye kodeboka,
# med rett format (heiltal for kodar
# og tekst for kodetekst), og på rett plass
enum_ind = kodebok$var_type == "Enum"
kodebok$kode[enum_ind] = enums[, 1] %>%
  as.numeric() # Kodar
kodebok$kode_tekst[enum_ind] = enums[, 2] # Tilhøyrande tekst

# Sjå på den nye, flotte kodeboka
View(kodebok)
