# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Titulo:   Proyecto Final
# Autores:
#           - ALCIVAR MEDRANDA JOEL RICARDO
#           - CABRERA JIMENEZ ANDERSON ISAAC
#           - CHIRIBOGA CEDILLO SEBASTIAN ALEJANDRO
#           - MACIAS SANCHEZ EDUART SEBASTIAN 
# Fecha:    22 Agosto 2023
# Paralelo: 3

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LIBRERIAS A UTILIZAR
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------------------------------------

# OBJETIVO 1
datos <- read.csv("C:\\Users\\elsac\\Documents\\Estadistica\\Codigo proyecto\\loansdata.csv")

#ANALISIS DESCRIPTIVO
# Filtrado y selección de datos para propietarios e inquilinos con nivel "HIGH"
propietarios_alto_endeudamiento <- datos %>%
  filter(home_ownership == "OWN" & debt_to_income == "HIGH") %>%
  select(home_ownership, debt_to_income)

inquilinos_alto_endeudamiento <- datos %>%
  filter(home_ownership == "RENT" & debt_to_income == "HIGH")%>%
  select(home_ownership, debt_to_income)

# Combinar los dataframes
combined_data <- rbind(propietarios_alto_endeudamiento, inquilinos_alto_endeudamiento)
kable(table(combined_data$home_ownership),caption= "Tabla de Frecuencias entre prestatarios propietarios e inquilinos con nivel de endeudamiento alto")

grafico <- ggplot(data = combined_data, aes(x = home_ownership, fill = home_ownership)) +
  geom_bar(position = "dodge") +
  labs(caption = "Figura 1. Comparación de Nivel de Endeudamiento Alto entre Propietarios e Inquilinos",
       x = "Grupo", y = "Cantidad") +
  scale_fill_manual(values = c("OWN" = "blue", "RENT" = "red")) +
  theme_minimal() +
  geom_text(aes(label = paste(round((..count..)/sum(..count..)*100, 2), "%")), 
            stat = "count", position = position_dodge(width = 0.9), vjust = -0.5            )

print(grafico)

# Filtrado y selección de datos para propietarios e inquilinos con nivel "HIGH"
propietarios_alto_endeudamiento <- datos %>%
  filter(home_ownership == "OWN" & debt_to_income == "HIGH")

inquilinos_alto_endeudamiento <- datos %>%
  filter(home_ownership == "RENT" & debt_to_income == "HIGH")

#ANALISIS INFERENCIAL

# Hipótesis Nula (H0): La proporción de inquilinos con nivel de endeudamiento "ALTO" es igual a la proporción de propietarios con nivel de endeudamiento "ALTO".
# Hipótesis Alternativa (HA): La proporción de inquilinos con nivel de endeudamiento "ALTO" es diferente de la proporción de propietarios con nivel de endeudamiento "ALTO".

# Calcular el total de inquilinos y propietarios con nivel "ALTO"
total_inquilinos_propietarios <- nrow(inquilinos_alto_endeudamiento) + nrow(propietarios_alto_endeudamiento)

# Calcular las proporciones de inquilinos y propietarios con nivel "ALTO"
prop_inquilinos <- nrow(inquilinos_alto_endeudamiento) / total_inquilinos_propietarios
prop_propietarios <- nrow(propietarios_alto_endeudamiento) / total_inquilinos_propietarios

# Realizar la prueba de proporciones
prop_test_result <- prop.test(c(nrow(inquilinos_alto_endeudamiento), nrow(propietarios_alto_endeudamiento)),
                              c(total_inquilinos_propietarios, total_inquilinos_propietarios),
                              alternative = "two.sided")

# Imprimir el resultado de la prueba
print(prop_test_result)
# ------------------------------------------------------------------------------------------------

# OBJETIVO 2

#ANALISIS DESCRIPTIVO
datosbanco <- read.csv("C:\\Users\\elsac\\Documents\\Estadistica\\Codigo proyecto\\loansdata.csv")
septidato <- datosbanco %>%
  select(credit_score) %>%
  pull()
tablasepti <- table(septidato)
total_observaciones7 <- length(septidato)
porcentajes7 <- (tablasepti / total_observaciones7) * 100
grafico7 <- ggplot(as.data.frame(porcentajes7), aes(x = septidato, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(Freq, 2)), vjust = -0.5, color = "black") +  # Agrega los números como etiquetas
  labs(title = "Diagrama de Barras sobre el nivel de puntaje crediticio", caption= "Figura 2. Diagrama de Barras sobre el nivel de puntaje crediticio.",
       x = "Puntaje crediticio ",
       y = "Porcentaje que representan") 
print(grafico7)  

trecedato <- datosbanco %>%
  select(past_bankrupt) %>%
  pull()
tablatrece <- table(trecedato)
total_observaciones13 <- length(trecedato)
porcentajes13 <- (tablatrece / total_observaciones13) * 100
grafico13 <- ggplot(as.data.frame(porcentajes13), aes(x = trecedato, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(Freq, 2)), vjust = -0.5, color = "black") +  # Agrega los números como etiquetas
  labs(title = "Diagrama de Barras sobre si los clientes han estado en bancarrota", caption= "Figura 3. Diagrama de Barras sobre si los clientes han estado en bancarrota", 
       x = "Pasado en bancarrota ",
       y = "Porcentaje que representan") 
print(grafico13) 

# Filtrado y selección de datos para personas con un puntaje crediticio de nivel "HIGH"
altoingreso <- datosbanco %>%
  filter(credit_score == "HIGH") %>%
  select(credit_score,past_bankrupt)

kable(table(altoingreso), caption= "Tabla de Frecuencia entre prestatarios de puntaje crediticio alto que han estado en quiebra y aquellos que no ")
# Se crea una tabla con los usuarios con un puntaje crditicio "ALTO"
tablatotal=table(altoingreso$credit_score, altoingreso$past_bankrupt)

#ANALISIS INFERENCIAL
# Realizar la prueba de proporciones
prop_test_result <- prop.test(tablatotal,alternative="greater",conf.level=0.95)
# Imprimir el resultado de la prueba
print(prop_test_result)

# ------------------------------------------------------------------------------------------------

# OBJETIVO 3
loans <- read.csv("C:\\Users\\elsac\\Documents\\Estadistica\\Codigo proyecto\\loansdata.csv")
datos = loans

data_filtrada = filter(datos, datos$loan_amount == "HIGH")
data_filtrada = data.frame(data_filtrada)

#ANALISIS UNIVARIANTE
fa_tabla_PR = table(data_filtrada$bad_public_record)

fr_tabla_PR = prop.table(fa_tabla_PR) 

fq = data.frame(fa_tabla_PR, fr_tabla_PR)
names(fq) <- c("Record Publico", "Frecuencia Absoluta","Record Publico", "Frecunecia relativa")
fq = select(fq, "Record Publico","Frecuencia Absoluta","Frecunecia relativa")
fq

barplot(fa_tabla_PR, xlab = "Record Publico", ylab = "Frecuencia", axis.lty = 1,xlim = c(0,2) ,ylim = c(0, 12000),
        main = "Diagrama de barras: Registro publico nivel alto de credito", col = c("green", "red"), 
        cex.axis = 1, cex.lab = 1, cex.names = 1, cex.main = 0.8, 
        space = 0.2)

porcentaje = data_filtrada %>% 
  group_by(bad_public_record) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percentage=`n`/sum(`n`)*100) 

ggplot(porcentaje, aes(x=1, y=percentage, fill=bad_public_record)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = paste0(round(percentage,1),"%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + 
  theme_void() + scale_fill_manual(values = c("turquoise3","cornflowerblue"))+
  ggtitle("Porcentajes de los Record Publicos registrados con nivel alto de credito")

#ANALISIS DESCRIPTIVO
tabla_BR = table(data_filtrada$bad_public_record, data_filtrada$past_bankrupt)
tabla_FBR = prop.table(tabla_BR)
fq1 = data.frame(tabla_BR, tabla_FBR)
names(fq1) = c("Record Publico","Bancarrota", "Frecuencia Absoluta","Record Publico2","Bancarrota1", "Frecunecia relativa")
fq1 = select(fq1, "Record Publico","Bancarrota", "Frecuencia Absoluta","Frecunecia relativa")
fq1

data_YPR = filter(data_filtrada, data_filtrada$bad_public_record == "YES")
data_NPR = filter(data_filtrada, data_filtrada$bad_public_record == "NO")

tabla_NPR = table(data_NPR$income)
barplot(tabla_NPR, xlab = "Ingreso", ylab = "Frecuencia", axis.lty = 1, ylim = c(0, 6000),
        main = "Diagrama de barras: NO Registro Publico", col = c("blue", "red","cornflowerblue"), 
        cex.axis = 1.4, cex.lab = 1, cex.names = 1.5, cex.main = 1.5, 
        space = 0.5)

tabla_YPR = table(data_YPR$income)
barplot(tabla_YPR, xlab = "Ingreso", ylab = "Frecuencia", axis.lty = 1, ylim = c(0, 250),
        main = "Diagrama de barras: SI Registro Publico", col = c("blue", "red","cornflowerblue"), 
        cex.axis = 1.4, cex.lab = 1, cex.names = 1.5, cex.main = 1.5, 
        space = 0.5)

tabla_NPR2 = table(data_NPR$credit_score)
barplot(tabla_NPR2, xlab = "Puntuacion de Credito", ylab = "Frecuencia", axis.lty = 1, ylim = c(0, 8000),
        main = "Diagrama de barras: NO Record Publico", col = c("blue", "red","cornflowerblue"), 
        cex.axis = 1.4, cex.lab = 1, cex.names = 1.5, cex.main = 1.5, 
        space = 0.5)

tabla_YPR2 = table(data_YPR$credit_score)
barplot(tabla_YPR2, xlab = "Puntuacion de Credito", ylab = "Frecuencia", axis.lty = 1, ylim = c(0, 400),
        main = "Diagrama de barras: SI Registro Publico", col = c("blue", "red","cornflowerblue"), 
        cex.axis = 1.4, cex.lab = 1, cex.names = 1.5, cex.main = 1.5, 
        space = 0.5)

#ANALISIS INFERENCIAL
chisq.test(fa_tabla_PR)
td = table(datos$bad_public_record,datos$loan_amount)
mosaicplot(td, main = "Grafico Mosaico: Registro Publico", color = 2:6, xlab = "Registro Publico", ylab = "Nivel de credito")
total_PR <- nrow(data_YPR) + nrow(data_NPR)

prop_NPR <- nrow(data_NPR) / total_PR
prop_YPR <- nrow(data_YPR) / total_PR


prop.test(c(nrow(data_NPR), nrow(data_YPR)),
          c(total_PR, total_PR),
          alternative = "two.sided")



