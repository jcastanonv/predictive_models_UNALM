#########################
library(readxl)
datos<-read_xlsx("coalition.xlsx",sheet = "coalition")

############ PREGUNTA 1 ################
# Pruebe la bondad de ajuste de la variable “duration” a la distribución gamma con el Package ‘goft’
modelo1<-glm(formula = datos$duration~datos$ciep12+datos$invest+datos$fract+datos$polar+datos$numst2+datos$crisis,family = Gamma(link = "inverse"),x = TRUE)
summary.glm(modelo1)

############ PREGUNTA 2 ################
# Calcule la matriz de correlaciones. Interprete. ¿Deben considerarse todas las variables?
# ¿Se pueden reducir la dimensionalidad?
cor(datos)
# De los resultados observamos que la correlación mas debil esta entre duration versus
# crisis con solo 0.036, con las otras variables la correlación esta entre 0.207 (numst2)
# hasta -0.72 con la variable ciep12.

############ PREGUNTA 3 ################
# Presente un análisis exploratorio de los datos. Tablas y gráficos
summary(datos)
hist(datos$duration)
# Para la variable duration el valor minimo es 0.5, el primer cuartil es 6.0, la mediana es 14.0, la media es 18.44, el tercer cuartil es 28.0 y el valor maximo es 59.0
# Para la variable ciep12 el valor minimo es 0.0, el primer cuartil es 1.0, la mediana es 1.0, la media es 0.86, el tercer cuartil es 1.0 y el valor maximo es 1.0
# Para la variable invest el valor minimo es 0.0, el primer cuartil es 0.0, la mediana es 0.0, la media es 0.45, el tercer cuartil es 1.0 y el valor maximo es 1.0
# Para la variable fract el valor minimo es 349, el primer cuartil es 677, la mediana es 719, la media es 718.8, el tercer cuartil es 788 y el valor maximo es 868
# Para la variable polar el valor minimo es 0.0, el primer cuartil es 3.0, la mediana es 14.5, la media es 15.29, el tercer cuartil es 25.0 y el valor maximo es 43.0
# Para la variable numst2 el valor minimo es 0.0, el primer cuartil es 0.0, la mediana es 1.0, la media es 0.63, el tercer cuartil es 1.0 y el valor maximo es 1.0
# Para la variable crisis el valor minimo es 0.0, el primer cuartil es 0.0, la mediana es 10.0, la media es 22.38, el tercer cuartil es 29.75 y el valor maximo es 274.0
hist(datos$fract)
hist(datos$polar)
hist(datos$crisis)
table(datos$ciep12)
table(datos$invest)
table(datos$numst2)
############ PREGUNTA 4 ################
# Ejecute la regresión gamma para “duration” con la data training. 
# Use glm 
modelo1<-glm(formula = datos$duration~datos$ciep12+datos$invest+datos$fract+datos$polar+datos$numst2+datos$crisis,family = Gamma(link = "inverse"),x = TRUE)
modelo1$coefficients
# Use glm.fit2
library(glm2)
modelo2<-glm.fit2(datos[,-1],datos$duration,family=Gamma("identity"))
modelo2$coefficients
# Vemos que en el modelo 1 existe el termino independiente 3.01 mientras que en el modelo 2 no existe.
# Para la variables ciep12 el valor en modelo1 es 3.31e-02 en el modelo2 es -11.78
# Para la variables invest el valor en modelo1 es 6.85e-03 en el modelo2 es 0.59
# Para la variables fract el valor en modelo1 es -1.12e-05 en el modelo2 es 0.04
# Para la variables polar el valor en modelo1 es 1.42e-03 en el modelo2 es -0.43
# Para la variables numst2 el valor en modelo1 es -9.88e-03 en el modelo2 es 4.80
# Para la variables crisis el valor en modelo1 es -9.77e-06 en el modelo2 es 0.08

############ PREGUNTA 5 ################
# Aplique 10-fold cross validation para calcular el valor de MSE con la data training.
View(datos)
library(caret)
set.seed(22222222)
inTraining <- createDataPartition(datos$duration, p = .80,list = FALSE)
training <- datos[inTraining,]
testing  <- datos[-inTraining,]
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
knn.datos <- train(duration~.,data = datos,method = 'knn',trControl = fitControl)

############ PREGUNTA 6 ################
# Presente la MSE con la data training
knn.datos
plot(knn.datos)
# El valor de la MSE es 13.78

############ PREGUNTA 7 ################
# ¿Qué variables son significativas? Ejecute la prueba de hipótesis respectiva.

summary.glm(modelo1)

# Para alpha=0.05
# Ho: El coeficiente no es significativo
# H1: El coeficiente es significativo

# Intercept: p=0.03<0.05 aceptamos H1, el coeficiente es significativo
# ciep12: p=0.0<0.05 aceptamos H1, el coeficiente es significativo
# invest: p=0.1591>0.05 aceptamos Ho, el coeficiente no es significativo
# fract: p=0.5912>0.05 aceptamos Ho, el coeficiente no es significativo
# polar: p=0.0<0.05 aceptamos H1, el coeficiente es significativo
# numst2: p=0.050>0.05 aceptamos Ho, el coeficiente no es significativo
# crisis: p=0.78>0.05 aceptamos Ho, el coeficiente no es significativo

############ PREGUNTA 8 ################
# Calcule y presente predicciones para los datos de la hoja “predicción”
# del archivo “coalition.xlsx
newdata1<-read_xlsx("coalition.xlsx",sheet = "predicción_gamma")
str(newdata1)
predict.glm(modelo1,newdata1,se.fit =T)

############ PREGUNTA 9 ################
# Presente la MSE del ajuste realizado con la data test.
deviance(modelo1)
# El MSE del ajuste realizado con la dataset es 208.1815






