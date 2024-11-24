###1.Asociar un nombre al archivo y examinarlo  
Datos_tratamiento <- read.table("datos-trabajoR.txt",header = TRUE)
#Emplear las funciones head() summary(), dim() y str()
head(Datos_tratamiento)
summary(Datos_tratamiento)
dim(Datos_tratamiento)
str(Datos_tratamiento)
#¿Cuántas variables hay? hay 4 variables 
#¿Cuántos tratamientos? hay 55 tratamientos


### 2. Hacer un Boxplot de 'variable' dividido por los grupos en 'condicion'
#Boxplot de los tratamientos 
boxplot(Datos_tratamiento$Tratamiento, col = "skyblue",main = "Boxplot de tratamiento", ylab = "Valores")
#Boxplot de los wildtype 
boxplot(Datos_tratamiento$Wildtype,col ="orange",main="Boxplot del wildtype", ylab = "valores")
#Boxplot de sequía 
boxplot(Datos_tratamiento$Sequia,col = "yellow",main= "Boxplot de Sequía", ylab = "valores")
#Boxplot de Exceso Riesgo 
boxplot(Datos_tratamiento$ExcesoRiego, col="green",main= "Boxplot de Exceso Riesgo", ylab= "valores ")

### 3.Hacer dos gráficos de dispersión 
#Gráfico de dispersión sequía vs Wildtype 
plot(Datos_tratamiento$Wildtype, Datos_tratamiento$Sequia, col= c("darkblue", "darkgreen", "aquamarine1", "azure1", "darkolivegreen1", "darkseagreen1"), main = "Gráfico de Dispersión sequía VS Wildtype", xlab = "Wildtype", ylab = "Sequía")

#Gráfico de dispersión exceso de riesgo vs Wildtype
plot(Datos_tratamiento$Wildtype, Datos_tratamiento$ExcesoRiego, col= c("darkblue", "darkgreen", "aquamarine1", "azure1", "darkolivegreen1", "darkseagreen1"), main = "Dispersion Graph sequía VS Exceso riesgo", xlab = "Wildtype", ylab = "Exceso de reisgo")

###4. Crear la leyenda para los gáficos de dispersión
legend("bottomright", legend=paste ("Tto", unique(Datos_tratamiento$Tratamiento)), col= c("darkblue", "darkgreen", "aquamarine1", "azure1", "darkolivegreen1", "darkseagreen1"), pch=16, cex=0.8, title="Tratamiento") 

###5. Histograma de cada variable 
# Histograma básico
#Histográma tratamientos 
hist(Datos_tratamiento$Tratamiento,col = "Skyblue", main = "Tratamientos", xlab = "Tratamientos", ylab = "Frecuencia")
#Histograma Wildtype 
hist(Datos_tratamiento$Wildtype,col = "orange", main = "Wildtype", xlab = "Wildtype", ylab = "Frecuencia")
#Histograma de sequía
hist(Datos_tratamiento$Sequia,col = "yellow", main = "Sequia", xlab = "Sequia", ylab = "Frecuencia")
#Histograma de Exceso de Riesgo 
hist(Datos_tratamiento$ExcesoRiego,col = "green",main = "Exceso de Riesgo",xlab = "Exceso Riesgo",ylab = "Frecuencia")


###6.Factor en la columna de tratamiento 
factor_tratamiento<-as.factor(Datos_tratamiento$Tratamiento)

###7.Calcular la media y la desviación estandar de cada tratamiento usando aggregate() 
#Calculo la media por tratamiento 
media_porTratamiento<-aggregate(Datos_tratamiento$Tratamiento~factor_tratamiento,data = Datos_tratamiento,FUN = mean)
#Cálculo la desviación estandar por tratamiento 
deviacionEst_porTratamiento<-aggregate(Datos_tratamiento$Tratamiento~factor_tratamiento,data = Datos_tratamiento,FUN = sd)

###8. Calcular cuantos elementos tiene cada tratamiento 
conteo_tratamiento<-table(Datos_tratamiento$Tratamiento)
print(conteo_tratamiento)
#Creo un data frame para ver la frecuencia de los datos en modo tabla  
conteo_df <- as.data.frame(table(Datos_tratamiento$Tratamiento))
colnames(conteo_df) <- c("Tratamiento", "Frecuencia")
print(conteo_df)

###9.Extraer los datos de el tt.o 1 y 4 y guardarlos en una variable diferente
tratamiento_1<-Datos_tratamiento[Datos_tratamiento$Tratamiento==1,]
tratamiento_4<-Datos_tratamiento[Datos_tratamiento$Tratamiento==4,]

###10 Comprobar que hay diferencias significativas para el tratamiento 1 y el tratamiento 5 entre Wildtype y Sequia, y entre Wildtype y ExcesoRiego.
#Primero vamos a extraer los datos del tt.o 5 por que no lo hemos sacado 
tratamiento_5<-Datos_tratamiento[Datos_tratamiento$Tratamiento==5,]
#Luego comprobamos la normalidad de el tratamieto 1 
shapiro_wildtype_t1<-shapiro.test(Datos_tratamiento$Wildtype)
shapiro_sequia_t1<-shapiro.test(tratamiento_1$Sequia)
shapiro_excesoriego_t1 <- shapiro.test(tratamiento_1$ExcesoRiego)

#Luego comprobamos la normalidad de el tratamieto 5 
shapiro_wildtype_t5 <-shapiro.test(tratamiento_5$Wildtype)
shapiro_sequia_t5<-shapiro.test(tratamiento_5$Sequia)
shapiro_excesoriego_t5<- shapiro.test(tratamiento_5$ExcesoRiego)

#Para ver los datos de tt.o 1 
shapiro_wildtype_t1 #No sigue distrubución normal 
shapiro_sequia_t1 #Sigue distrubución normal
shapiro_excesoriego_t1 #Sigue distrubución normal

#Para ver los datos de tt.o 5
shapiro_wildtype_t5 #No sigue distrubución normal
shapiro_sequia_t5  #No sigue distrubución normal 
shapiro_excesoriego_t5 #Sigue distrubución normal


#Ahora a comparar las variables de tratamiento 1 para ver si son iguales 
#de tt.1 Wt vs sequia
var_test_wildtype_sequia_t1<- var.test(tratamiento_1$Wildtype, tratamiento_1$Sequia)
#de tt.1 Wt vs exceso de riegso 
var_test_wildtype_excesoriesgo_t1<-var.test(tratamiento_1$Wildtype, tratamiento_1$ExcesoRiego)
#de tt.1 sequia  vs exceso de riegso 
var_test_sequia_excesoriego_t1 <- var.test(tratamiento_1$Sequia, tratamiento_1$ExcesoRiego)

#Ver los resultados de tt.o 1 
var_test_wildtype_sequia_t1 #variancias no iguales 
var_test_wildtype_excesoriesgo_t1  #variancias no iguales 
var_test_sequia_excesoriego_t1 #variancias iguales  


#Ahora a comparar las variables de tratamiento 5 para ver si son iguales 
#de tt.5 Wt vs sequia
var_test_wildtype_sequia_t5 <- var.test(tratamiento_5$Wildtype, tratamiento_5$Sequia)
#de tt.5 Wt vs exceso de riesgo 
var_test_wildtype_excesoriego_t5 <- var.test(tratamiento_5$Wildtype, tratamiento_5$ExcesoRiego)
#de tt.5 sequia vs exceso de riesgo 
var_test_sequia_excesoriego_t5 <- var.test(tratamiento_5$Sequia, tratamiento_5$ExcesoRiego)

#Ver los resultados de tt.o 5
var_test_wildtype_sequia_t5 #variancias no iguales  
var_test_wildtype_excesoriego_t5  #variancias no iguales 
var_test_sequia_excesoriego_t5 #variancias no iguales 

# Elegir y Realizar la Prueba Estadística
#Comparar Wildtype vs Sequia y Wildtype vs 
# He utilizado wilcox.test cuando uno o los dos no siguen la normalidad 
# He utilizado T-test cuando los dos siguen la normalidad 

#Tratamiento 1 
# del tt.o 1 Wt vs sequia
wilcox_wildtype_sequia_t1 <- wilcox.test(tratamiento_1$Wildtype, tratamiento_1$Sequia, exact = F)
# del tt.o 1 Wt vs exceso de riesgo 
wilcox_wildtype_excesoriesgo_t1 <- wilcox.test(tratamiento_1$Wildtype, tratamiento_1$ExcesoRiego, exact =  F)
# del tt.o 1 sequia vs exceso de riesgo 
t_test_sequia_excesoriesgo_t1 <- t.test(tratamiento_1$Sequia, tratamiento_1$ExcesoRiego, var.equal = TRUE)

#Tratamiento 5
# del tt.o 5 Wt vs sequia
wilcox_wildtype_sequia_t5<-wilcox.test(tratamiento_5$Wildtype, tratamiento_5$Sequia, exact = FALSE)
# del tt.o 5 Wt vs exceso de riesgo 
wilcox_wildtype_excesoriesgo_t5 <- wilcox.test(tratamiento_5$Wildtype, tratamiento_5$ExcesoRiego, exact =  F)
# del tt.o 5 exceso de riesgo vs sequia
wilcox_sequia_excesoriesgo_t5 <- wilcox.test(tratamiento_5$Wildtype, tratamiento_5$ExcesoRiego, exact =  F)


#RESULTADOS PRUEBA ESTADÍSTICA
#TT.O 1 
wilcox_wildtype_sequia_t1
wilcox_wildtype_excesoriesgo_t1
t_test_sequia_excesoriesgo_t1

#TT.O 2 
wilcox_wildtype_sequia_t5
wilcox_wildtype_excesoriesgo_t5
wilcox_sequia_excesoriesgo_t5 

#En función de los resultados de la prueba de normalidad, ¿qué test usarías para cada comparativa?
# En el caso de que sigua una distrubución normal -> Test paramétrico ( como ks.test() o  aov())
# En el caso de que no siga una distribución normal -> Test no paramétrico ( u.test(), wilcox.test() o kruskal.test())

###Pregunta 11
#Primero abro el archivo de anova 
datos_anova<- read.table("anova-datos.txt",header = TRUE)

# Primero voy a separar las condiciones en variables
wildtype_t1 <- tratamiento_1$Wildtype
sequia_t1 <- tratamiento_1$Sequia
excesoriego_t1 <- tratamiento_1$ExcesoRiego


#creamos el data frame del anova (una tabla igual que la de datos anova)

datos_anova <- data.frame(valor = c(tratamiento_1$Wildtype, tratamiento_1$Sequia, tratamiento_1$ExcesoRiego),
  condicion = rep(c("Wildtype", "Sequia", "ExcesoRiego"), each = nrow(tratamiento_1))
)

# Realizamos el anova (Para ver si hay diferencias significativas entre las condiciones)
anova_resultado <- aov(valor ~ condicion, data = datos_anova)

# Para ver los resultados del ANOVA
summary(anova_resultado)







