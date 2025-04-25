library(haven)
library(tidyverse)
#se carga la base de datos
enadis <- read_sav("data/ENADIS2023.sav")

#se seleccionan las variables de interes
enadis <- enadis %>% select(A5, A4, region, zona, Cap_grado, ING_PERCAPITA_HOGAR, condic_activi, B8a, B7,
                            Dis_puntaje, Cap_puntaje, Des_puntaje, Cap_grado, Dis_grado, Grado_discap, Tiene_defi, Tiene_enfer, FD22_REC, FD23_REC, FD24_REC, FD25_REC, FD26_REC, FD27_REC, FD28_REC, FD29_REC, FD30_REC, FD31_REC, condic_activi, B2, B4_rec, B7, B8a, B8b, Posi_empleo, Cali_grup_ocup, B9_espacios, TOTAL_INGRESO_KISH, TOTAL_INGRESO_HOGAR, ING_PERCAPITA_HOGAR, QUINTIL_INGRESO, QUINTIL_ZONA, TOTAL_TRABAJO, Total_transf_B16, A15, Nivel_Instru, Escolaridad, A11Condi_aseg, A5, A4, region, zona, ASIS_PER, asis_int, USA_PRODUCTOS, PROD_ASIS, AUTONOMIA, ACTITUDES)

# Solo factorizamos las columnas que son etiquetas (labelled) o caracteres (character)
enadis <- enadis %>% mutate(across(where(~ is.labelled(.x) || is.character(.x)), as_factor))

#se filtran por personas entre 18 y 65 años
enadis <- enadis %>% filter(A5 >= 18 & A5 <= 65)

