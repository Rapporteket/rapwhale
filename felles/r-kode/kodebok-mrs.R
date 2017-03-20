# Forsøk på lesing av det elendige kodebokformatet til MRS :(


# Oppsett -----------------------------------------------------------------

# Ikkje gjer om tekst til faktorar automatisk
options(stringsAsFactors = FALSE)

# Nødvendige pakkar
library(readxl) # Lesing av Excel-filer
library(dplyr) # Datamassering
library(tibble) # Fornuftig datarammestruktur
library(stringr) # Tekstmassering
library(magrittr) # Funksjonar som kan brukast med røyr-operatoren



# Datainnlesing -----------------------------------------------------------

# Les inn eksempeldata
mappe = "***FJERNA-ADRESSE***"
filnamn = "20160801_Kodebok_NorArtritt.xlsx"
adresse = paste0(mappe, filnamn)

# Les inn (ei fane i) Excel-kodeboka
kb_mrs = read_excel(adresse, sheet = 8)



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

# Oversikt over variabeltypar i MRS og standardnamna me brukar
vartype_mrs_standard = tribble(
  ~type_mrs, ~type_standard,
  "Enum", "kategorisk",
  "Text", "tekst",
  "Avkrysning", "boolsk",
  "Dato/Tid", "dato_kl",
  "Id (Guid)", "tekst",
  "Numerisk (heltall)", "numerisk",
  "Numerisk (flyttall)", "numerisk"
)
nye_vartypar = na.omit(setdiff(d$Felttype, vartype_mrs_standard$type_mrs))
if (length(nye_vartypar) > 0) {
  stop("Kodeboka har variabeltypar me ikkje har standardnamn på: ", str_c(nye_vartypar, collapse = ", "))
}

# Lag dataramme med i utgangspunktet éi rad for kvar variabel
kodebok_utg = tibble(
  dd_id = d$DataDumpnavn[ind_nyvar], # Datadumpnamn (teit)
  variabel_id = d$Variabelnavn[ind_nyvar], # Variabel-ID (OK)
  variabeltype = vartype_mrs_standard$type_standard[match(d$Felttype[ind_nyvar], vartype_mrs_standard$type_mrs)],
  obligatorisk = str_to_lower(d$Obligatorisk[ind_nyvar]),
  # skjema = d$Skjema[ind_nyvar], #skjematype
  verdi = NA_integer_, # Føreset førbels at MRS-kodane alltid er tal (gjer om til tekst om dette ikkje stemmer)
  verdi_tekst = NA_character_,
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
enum_ind = (kodebok$variabeltype == "kategorisk")
kodebok$verdi[enum_ind] = enums[, 1] %>%
  as.numeric() # Kodar
kodebok$verdi_tekst[enum_ind] = enums[, 2] # Tilhøyrande tekst

# Sjå på den nye, flotte kodeboka
# View(kodebok)
