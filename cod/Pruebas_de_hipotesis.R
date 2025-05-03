#recordar cargar la base de datos en manejo_de_datos.R

# Prueba 1: Ingreso per capita vs discapacidad

# H₀: Las distribuciones del ingreso per cápita son iguales entre los distintos grados de discapacidad.

# H₁: Al menos un grupo tiene una distribución distinta.

kruskal.test(ING_PERCAPITA_HOGAR ~ Cap_grado, data = enadis)

# Prueba 2: condicion actividad vs discapacidad

# H₀: No hay asociación entre condición de actividad y grado de discapacidad.

# H₁: Sí existe asociación.

chisq.test(table(enadis$Cap_grado, enadis$condic_activi))

# Prueba 3: horas trabajadas vs discapacidad

# H₀: No hay relación entre el grado de discapacidad y las horas trabajadas.

# H₁: Sí hay relación.

enadis_filtrado <- enadis %>%
  filter(!is.na(B7)) %>%
  mutate(B7 = droplevels(B7))

chisq.test(table(enadis_filtrado$Cap_grado, enadis_filtrado$B7))

# Prueba 4: puesto vs grado de discapacidad

#H₀: El tipo de puesto es independiente del grado de discapacidad.

#H₁: El tipo de puesto varía con el grado de discapacidad.

chisq.test(table(enadis$Cap_grado[enadis$condic_activi == "Ocupados/as"],
                 enadis$B8a[enadis$condic_activi == "Ocupados/as"]))

