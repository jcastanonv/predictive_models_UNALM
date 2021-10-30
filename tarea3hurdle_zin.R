install.packages('MixAll')
library(pacman)
library(MixAll)
library(MASS)
p_load(pscl)
p_load(sandwich)
library(car)
library(lmtest)

?DebTrivedi

data(DebTrivedi)
df <- DebTrivedi[, c(1, 6:8, 13, 15, 18)]
str(df)
plot(table(df$ofp))
plot(ofp ~ numchron, data = df)

clog <- function(x) log(x + 0.5)
cfac <- function(x, breaks = NULL) {
  if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
  x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
  levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
  c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
  sep = "")
  return(x)
}

plot(clog(ofp) ~ cfac(numchron), data = df)

par(mfrow = c(2,3))
plot(clog(ofp) ~ health, data = df, varwidth = TRUE, main = 'health', xlab = 'Self−perceived health status', ylab = 'Physician office visits (in clogs)')
plot(clog(ofp) ~ cfac(numchron), data = df, main = 'numchron', xlab = 'Number of chronic conditions', ylab = 'Physician office visits (in clogs)')
plot(clog(ofp) ~ privins, data = df, varwidth = TRUE, main = 'privins', xlab = 'Covered by private insurance', ylab = 'Physician office visits (in clogs)')
plot(clog(ofp) ~ cfac(hosp, c(0:2, 8)), data = df, main = 'hosp',  xlab = 'Number of hospital stays', ylab = 'Physician office visits (in clogs)')
plot(clog(ofp) ~ gender, data = df, varwidth = TRUE, main = 'gender', xlab = 'Gender', ylab = 'Physician office visits (in clogs)')
plot(cfac(ofp, c(0:2, 4, 6, 10, 100)) ~ school, data = df, breaks = 9, main = 'school', xlab = 'Number of years of education', ylab = 'Physician office visits (number of visits)')


hurdle_model1 <- hurdle(ofp ~ ., data = df, dist = "negbin")
hurdle_model2 <- hurdle(ofp ~ . | hosp + numchron + privins + school + gender, data = df, dist = "negbin")
summary(hurdle_model1)
summary(hurdle_model2)
waldtest(hurdle_model1, hurdle_model2)
lrtest(hurdle_model1, hurdle_model2)


zinb_reg1 <- zeroinfl(ofp ~ ., data = df, dist = "negbin")
zinb_reg2 <- zeroinfl(ofp ~ . | hosp + numchron + privins + school + gender, data = df, dist = "negbin")
waldtest(zinb_reg1, zinb_reg2)
lrtest(zinb_reg1, zinb_reg2)
summary(zinb_reg2)

fm <- list("Hurdle-NB" = hurdle_model2, "ZINB" = zinb_reg2)
sapply(fm, function(x) coef(x)[1:8])

cbind(sapply(fm[-1], function(x) sqrt(diag(vcov(x)))[1:8]))

rbind(logLik = sapply(fm, function(x) round(logLik(x), digits = 0)),
Df = sapply(fm, function(x) attr(logLik(x), "df")))

round(c("Obs" = sum(df$ofp < 1),
"NB-Hurdle" = sum(predict(hurdle_model2, type = "prob")[,1]),
"ZINB" = sum(predict(zinb_reg2, type = "prob")[,1])))

t(sapply(fm[1:2], function(x) round(x$coefficients$zero, digits = 3)))
