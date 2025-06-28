library(haven)
library(tidyverse)
#se carga la base de datos
enadis_completa <- read_sav("data/ENADIS2023.sav")

#se seleccionan las variables de interes
enadis_completa <- enadis_completa %>% select(A5, A4, region, zona, Cap_grado, ING_PERCAPITA_HOGAR, condic_activi, B8a, B7, Nivel_Instru,
                            Dis_puntaje, Cap_puntaje, Des_puntaje, Cap_grado, Dis_grado, Grado_discap, Tiene_defi, Tiene_enfer, FD22_REC, FD23_REC, FD24_REC, FD25_REC, FD26_REC, FD27_REC, FD28_REC, FD29_REC, FD30_REC, FD31_REC, condic_activi, B2, B4_rec, B7, B8a, B8b, Posi_empleo, Cali_grup_ocup, B9_espacios, TOTAL_INGRESO_KISH, TOTAL_INGRESO_HOGAR, ING_PERCAPITA_HOGAR, QUINTIL_INGRESO, QUINTIL_ZONA, TOTAL_TRABAJO, Total_transf_B16, A15, Nivel_Instru, Escolaridad, A11Condi_aseg, A5, A4, region, zona, ASIS_PER, asis_int, USA_PRODUCTOS, PROD_ASIS, AUTONOMIA, ACTITUDES)

# Solo factorizamos las columnas que son etiquetas (labelled) o caracteres (character)
enadis_completa <- enadis_completa %>% mutate(across(where(~ is.labelled(.x) || is.character(.x)), as_factor))

#se filtran por personas entre 18 y 65 años
enadis_completa <- enadis_completa %>% filter(A5 >= 18 & A5 <= 65)

# enadis JOSE
enadis <- enadis_completa %>% select(A5,
                                     A4,
                                     region,
                                     zona,
                                     Cap_grado,
                                     ING_PERCAPITA_HOGAR,
                                     condic_activi,
                                     B8a,
                                     B7) %>%
  mutate(B8a = fct_collapse(B8a, "Empleado/a" = levels(B8a)[str_detect(levels(B8a), "Empleado/a")]))


#enadis Andrey
enadis_oc <- enadis_completa %>% filter(condic_activi == "Ocupados/as", !is.na(B7), !is.na(Cap_grado))

enadis_oc$horas <- case_when(
  enadis_oc$B7 == "Menos de 15 horas" ~ 7.5,
  enadis_oc$B7 == "De 15 a menos de 40 horas" ~ 27.5,
  enadis_oc$B7 == "De 40 a 48 horas" ~ 44,
  enadis_oc$B7 == "Más de 48 horas" ~ 55,
  TRUE ~ NA_real_
)

enadis_oc <- enadis_oc %>% filter(!is.na(ING_PERCAPITA_HOGAR))

# Guardado de Bases de Datos

# Recuerde actualizar esta cada vez que se cambie el código para que se guarde
# Guarda las base de datos en la carpeta data
saveRDS(obj = enadis, file = "data/enadis.rds") 
saveRDS(obj = enadis_completa, file = "data/enadis_completa.rds")
saveRDS(obj = enadis_oc, file = "data/enadis_oc.rds")







