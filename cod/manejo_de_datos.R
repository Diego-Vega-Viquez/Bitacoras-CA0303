library(haven)
library(dplyr)
library(tidyr)
#se carga la base de datos
enadis <- read_sav("data/ENADIS2023.sav")

#se seleccionan las variables de interes
enadis <- enadis %>% select(A5, A4, region, zona, Cap_grado, ING_PERCAPITA_HOGAR, condic_activi, B8a, B7)

#se factorizan las variables, y se desfactorizan aquellas que sean numericas
enadis <- enadis %>% mutate(across(everything(), as_factor)) 
enadis$A5 <- as.numeric(enadis$A5)
enadis$ING_PERCAPITA_HOGAR <- as.numeric(enadis$ING_PERCAPITA_HOGAR)

#se filtran por personas entre 18 y 65 años
enadis <- enadis %>% filter(A5 >= 18 & A5 <= 65)

