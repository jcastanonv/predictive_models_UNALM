install.packages('tidyverse')
install.packages('hrbrthemes')
install.packages('viridis')
install.packages('corrplot')
install.packages('psych')
install.packages("MVN")
install.packages('BSDA')
install.packages('olsrr')

library(olsrr)
library(BSDA)
library(MVN)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(corrplot)
library(psych)
library(lmtest)
library(car)
library(tidymodels)

data(trees)
attach(trees)
head(trees)
par(mfrow=c(2,2))
boxplot(trees$Height, main='Height', col = 'red')
boxplot(trees$Girth, main='Girth', col = 'blue')
hist(trees$Height, main='Histogram of Height')
hist(trees$Girth, main='Histogram of Girth')
par(mfrow=c(1,1))



ggplot(trees, aes(x=Girth, y=Volume)) + geom_point(size=3) + geom_smooth(method = lm)
ggplot(trees, aes(x=Height, y=Volume)) + geom_point(size=3) + geom_smooth(method = lm)
ggplot(trees, aes(x=Height, y=Girth)) + geom_point(size=3)

cor(trees)

pairs.panels(trees,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             pch = 21,           # Símbolo pch
             lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             factor = 2,         # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             ci = TRUE)          # Si TRUE, añade intervalos



mvn(trees,mvnTest = c("mardia")) # NO
mvn(trees,mvnTest = c("hz"))    # NO
mvn(trees,mvnTest = c("royston")) # NO
mvn(trees,mvnTest = c("dh")) # NO
mvn(trees,mvnTest = c("energy")) # NO

# Por fines academicos se aplicara una Regresion Lineal Multivariada
cor(trees)
mixedCor(trees)

# Modelo de Regresion Lineal Multivarada

modelo <- lm(Volume~Girth + Height, data = trees)
summary(modelo)
# $Volume = -57.9877 + 4.7082*Girth + 0.3393*Height$
### por cada pulgada incrementada en la variable Girth, se reflejará como un aumento
### de 4.7082 pies cubicos en la variable respuesta "Volume". La variable es significativa ya que 
### tiene un valor p menor al valor de significancia de 0.05 (2e-16)
?trees
trees[,-3]

predicts <- cbind(predict(modelo, trees[,-3]))
originals <- c(trees$Volume)
se_table <- cbind(predicts, originals)
colnames(se_table) <- c('predicts', 'originals')
library(reshape2)
se_table_melt <- melt(se_table, id.vars='index', variable.name = 'series')
ggplot(se_table_melt, aes(Var1, value)) + geom_line(aes(colour = Var2))

# Calculo del SE
se_table <- data.frame(se_table)
se_table$SS <- ((se_table$originals-se_table$predicts)^2)
SE <- sqrt(sum(se_table$SS)/(length(se_table$originals)-3))

summary(modelo) # Residual standard error : 3.882 = SE


y <- mean(trees$Volume)

# promedio
tsum.test(mean.x=y, s.x=SE, n.x=length(se_table$originals), conf.level=0.95, alternative = 'two.sided')$conf

# valor en especifico
new <- data.frame(cbind(11.5, 73))
predict(modelo, newdata = new, interval = 'prediction', level = 0.95)

# Height significativa
anova(modelo)
2*pt(2.607, 28, lower.tail = FALSE)

# El modelo significativo
g <- summary(modelo)
g$fstatistic
pf(254.9723,3,28, lower.tail = FALSE)

summary(modelo)
anova(modelo)
r2 <-1 - (r2$`Sum Sq`[3]/(sum(r2$`Sum Sq`)))
r2_adj <- 1 - (((1-r2)*(length(trees$Volume)-1))/(length(trees$Volume)-length(trees[,-3])-1))

trees$predicted <- predict(modelo)
trees$residuals <- residuals(modelo)
ggplot(trees, aes(x = Height, y = Volume)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = Height, yend = predicts), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicts), shape = 1) +
  theme_bw()

ggplot(trees, aes(x = Girth, y = Volume)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = Girth, yend = predicts), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicts), shape = 1) +
  theme_bw()
# 11. Calcule residuales

predicts <- cbind(predict(modelo, trees[,-3]))
originals <- c(trees$Volume)
se_table <- cbind(predicts, originals)
colnames(se_table) <- c('predicts', 'originals')
se_table <- data.frame(se_table)
se_table$residuals <- residuals(modelo)

#no estandarizada
plot(modelo, which=1, col=c("red"))
# H0 = Los residuales son homocedasticos, varianza constante
# H1 = Los residuales no homocedasticos (Heterosedasticos)
bptest(modelo)
# Graficar los residuos frente a los valores ajustados por el modelo, 
# e identificar si existe un patrón cónico u otro patrón. 
# Idealmente deberían distribuirse de forma aleatoria en torno a 0. 
# También podemos recurrir al test de Breusch-Pagan como contraste de 
# homocedasaticidad. 
# La hipótesis nula de este test es que los residuos poseen una varianza constante.
bptest(modelo)
# valor p = 0.2911 > 0.05, no se rechaza la H0, es decir que los residuos tienen
# varianza constante


#12. 
#estandarizada y estudientizado
se_table$residuals_standard <- rstandard(modelo)
se_table$residuals_studentized <- rstudent(modelo)
# standarized graph
plot(modelo, which=2, col=c("red"))
ols_plot_resid_stand(modelo)

# studentized graph
hist(se_table$residuals_studentized, freq = FALSE)
qqPlot(modelo)
ols_plot_resid_stud(modelo)

# Lo que puede observar es que los valores residuales estandarizados y estudiantizados
# tienden a acercarse a la linea de 45º, por lo que, podemos decir
# que los valores de los residuales poseen una distribución normal

# 13.
ols_plot_cooksd_bar(modelo)
ols_plot_resid_lev(modelo)

cooksD <- cooks.distance(modelo)
se_table$cooksD <- cbind(cooksD)
head(se_table)
influential <- cooksD[(cooksD > (3*mean(cooksD, na.rm = TRUE)))]
influential

cooks_table <- se_table

Hmat <- hatvalues(modelo)
cooks_table$h <- c(Hmat)
data(trees)
attach(trees)
head(trees)
k <- length(trees[,-3])
cooks_table$cookD_calc <- ((cooks_table$residuals_standard^2)*cooks_table$h)/((k+1)*(1-cooks_table$h)) # k+1 => k: numero de variables predictoras
head(cooks_table, 5)
se_table <- cooks_table
# 14.
matrix_trees <- trees[, 1:2]
matrix_trees$r <- c(rep(1, 31))
matrix_trees <- matrix_trees[, c(3,1,2)]
matrix_trees <- as.matrix(matrix_trees)
vcov(modelo)
(SE^2)*solve(t(matrix_trees)%*%(matrix_trees))

# 15.
vif(modelo)
# El factor de inflación de varianza (VIF) muestra que tanto para la variable Girth como Height
# toma valores menores a 10, lo que significa que ambas variables entran al modelo

# 16.

division <- trees %>% initial_split(prop = 0.7, strata = Volume)
training_set <- division  %>%  training()
testing_set <- division  %>%  testing() 

length(training_set$Volume)
length(testing_set$Volume)

# 17. 
modelo_train <- lm(Volume ~ Girth + Height, data = training_set)

#18.
predicts <- cbind(predict(modelo, testing_set[, 1:2]))
predicts_table <- cbind(predicts, c(testing_set[,3]))
colnames(predicts_table) <- c('predicts', 'originals')
predicts_table <- as.data.frame(predicts_table)

#19.
install.packages('MLmetrics')
library(MLmetrics)
mean((predicts_table$original-predicts_table$predicts)^2)
MSE(predicts_table$predicts, predicts_table$original)

# 20. 

# Según los resultados del anova el MSE es 12.1 (es el resultado de dividir la suma de 
# de cuadrados del error entre n - k - 1) donde (k es numero de coeficientes = 3,
# n es el tamaño de la muestra = 21), mientras que el MSE calculado resultó 13.47 (es
# el resultado de la división entre n).

# 21.
library(plotly)
fig <- plot_ly(x = trees$Girth, y = trees$Height, z = trees$Volume)

# 22.
install.packages('multivator')
library(multivator)
beta_hat()



