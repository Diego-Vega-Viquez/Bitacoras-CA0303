library(haven)
library(tidyverse)
#se carga la base de datos
enadis <- read_sav("data/ENADIS2023.sav")

#se seleccionan las variables de interes
enadis <- enadis %>% select(A5, A4, region, zona, Cap_grado, ING_PERCAPITA_HOGAR, condic_activi, B8a, B7)

# Solo factorizamos las columnas que son etiquetas (labelled) o caracteres (character)
enadis <- enadis %>% mutate(across(where(~ is.labelled(.x) || is.character(.x)), as_factor))

#se filtran por personas entre 18 y 65 años
enadis <- enadis %>% filter(A5 >= 18 & A5 <= 65)

