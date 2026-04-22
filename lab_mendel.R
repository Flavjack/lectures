# -------------------------------------------------------------------------
# códigos -----------------------------------------------------------------
# -------------------------------------------------------------------------

#> líneas: ctrl + shift + R
#> ejecutar: ctrl + enter
#> asignar ( <- ): alt + -
#> pipe ( %>% ): ctrl + shift + M
#> ayuda: F1
#> limpiar la consola: ctrl + L

# -------------------------------------------------------------------------
# chi-cuadrado ------------------------------------------------------------
# -------------------------------------------------------------------------

# vector de los valores observados
obs <- c(315, 108, 101, 32)  

# vector de las proporciones (probabilities)
#> 9:3:3:1
prb <- c(9/16, 3/16, 3/16, 1/16) 

# test x2
chisq.test(x = obs, p = prb)

#> p-value = 0.9254 

# -------------------------------------------------------------------------

#> ¿Qué función usamos?
#> chisq.test()

#> ¿Cual es el valor de p-value?
#> 0.9254

#> ¿Se rechaza o se acepta la hipotesis?
#> p-value > 0.05 se acepta la H0



# chatGPT -----------------------------------------------------------------

# Valores observados
observed <- c(315, 108, 101, 32)

# Proporción mendeliana esperada
expected_ratios <- c(9, 3, 3, 1)

# Calcular las frecuencias esperadas basadas en la suma de las observaciones
total_observed <- sum(observed)
expected <- total_observed * expected_ratios / sum(expected_ratios)

# Realizar el test de chi-cuadrado de bondad de ajuste
chi_square_test <- chisq.test(x = observed, p = expected / total_observed)

# Mostrar los resultados del test
print(chi_square_test)







