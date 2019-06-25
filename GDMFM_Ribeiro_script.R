install.packages("tidyverse")
library(tidyverse)
library(readr)

R.Version()

write_excel_csv(df2, "data.csv")

df2 <- read_dta(file = "bancounicoartigo.dta")

citation("mice")

df %>%
  print(n = 10, width = Inf)

#check specification for each vector
spec(df2)
View(df2)

df <- read_csv("bancounico.csv", col_types = cols(
  id = col_character(),
  sexo = col_character(),
  etnia = col_character(),
  vaginal = col_character(),
  forceps = col_character(),
  cesarea = col_character(),
  primigesta = col_character(),
  multipara = col_character(),
  aborto = col_character(),
  interultgest = col_character(),
  trabalho = col_character(),
  dmg = col_character()
))

is.character(df2$parity)
is.factor(df2$parity)
is.numeric(df2$ganhodepeso)

df2$dmg <- as.character(df2$dmg)
df2$male <- as.factor(df2$male)
df2$trabalho <- as.factor(df2$trabalho)
df2$parity <- as.factor(df2$parity)
df2$aborto <- as.character(df2$aborto)
df2$tipoparto <- as.factor(df2$tipoparto)


#check the sample size for a given variable
sum(is.na(df2$sexo))

#not normally distributed
shapiro.test(df2$renda)
shapiro.test(df2$idademae)
shapiro.test(df2$imcpre)
shapiro.test(df2$ganhodepeso)
shapiro.test(df2$pn)
shapiro.test(diabetes$FM)
shapiro.test(df2$igusint)
shapiro.test(df2$cabdominal)
shapiro.test(df2$index)

print(imputed$male)

qqnorm(df2$pn)
qqline(df2$pn, col = 2)


cor.test(df2$ganhodepeso, df2$FM, method = "spearman")


?wilcox.test

wilcox.test(ganhodepeso ~ male, data = df2)
median(NGT$FM, na.rm = TRUE)
IQR(NGT$FM, na.rm = TRUE)
t.test(ganhodepeso ~ male, data = df2)
sd(df2$FM, na.rm = TRUE)
cor.test(df2$imcpre, df2$ganhodepeso, method = "pearson")

sum(!is.na(df2.filtered$height))

ggplot(data = df2, mapping = aes(x = cabdominal)) +
  geom_histogram()

summary(df2$igusint)
filter(df2, igusint < 37)

summary(df2$cabdominal)
filter(df2, cabdominal > 38)
list(df2$cabdominal)

filter(df2, ganhodepeso > 30)
list(df2$ganhodepeso)


df2[101, 9] <- 1

list(df2$FM)

#outliers
#FM
df2[, 55][df2[, 55] == 1.1015] <- NA
df2[86, 55] <- 1.1015

NGT[, 60][NGT[, 60] == 1.1015] <- NA
NGT[86, 60] <- 1.1015


#birth weight or pn
df2[, 4][df2[, 4] == 5070] <- NA
df2[86, 4] <- 5070
#GWG
df2[251, 43] <- NA
df2[251, 43] <- 34.80
#renda
df2[248, 16] <- NA
df2[248, 16] <- 3750.0
df2[48, 16] <- NA
df2[48, 16] <- 2500.0
#gest age 
df2[54, 6] <- NA
df2[136, 6] <- NA
df2[54, 6] <- 36
df2[136, 6] <- 36
#abdomical circ
df2[177, 41] <- NA
df2[177, 41] <- 39.8



ggplot(data = df2, mapping = aes(x = FM)) +
  geom_histogram(data = subset(diabetes), fill = "green", alpha = 0.5) +
  geom_histogram(data = subset(NGT), fill = 'blue', alpha = 0.2)


?pwr


ggplot(df2, aes(pn, indexx, color = male)) +
  geom_point() +
  geom_smooth()

ggplot(df2, aes(dmg, indexx, color = dmg)) +
  geom_point()


cor.test(diabetes$imcpre, diabetes$FM, method = "pearson")


list(df$interultgest)
summary(diabetes$altura)
sd(diabetes$altura, na.rm = TRUE)

diabetesf <- filter(df2.filtered, dmg == 1)
NGTf <- filter(df2.filtered, dmg == 0)


?ifelse
df2$malesex <- ifelse(df2$sexo == "1", 1, 0)
imputed$malesex <- ifelse(df2$sexo == "1", 1, 0)
df2$parity <- ifelse(df2$parity == "1", 0, 1)

males <- filter(df2, male == 1)
females <- filter(df2, male == 0)


age <- filter(NGTf, idademae >= 36, idademae <= 45)
age <- filter(NGTf, idademae < 20)
ht <- filter(NGT, height > 1.70)
(10/112)*100


df2$height <- df2$altura/100

maleGDM <- filter(males, dmg == 1)
femGDM <- filter(females, dmg == 1)
maleNGT <- filter(males, dmg == 0)
femNGT <- filter(females, dmg == 0)

median(maleNGT$FM, na.rm = TRUE)
IQR(maleGDM$cabdominal, na.rm = TRUE)

mean(maleNGT$ctoracica, na.rm = TRUE)
sd(maleNGT$ctoracica, na.rm = TRUE)


more <- filter(df2.filtered, renda > 954.0, dmg == 1)
less <- filter(df2.filtered, renda < 954.0, dmg == 1)
(98/112)*100


less40 <- filter(diabetes, igusint >= 39, igusint <= 40)
forty <- filter(NGT, igusint > 40)
more40 <- filter(NGT, igusint == 42)
(26/208)*100

sum(is.na(diabetes$igusint))
(26/206)*100

mean(NGT$ffreemass)
sd(NGT$ffreemass)
list(df2$ffreemass)

df2$FM <- (parse_double(df2$fatmass))

?geom_point
ggplot(data = df) +
  geom_point(mapping = aes(x = dmg, y = pfat), position = "jitter")

t.test(df2$renda ~ df2$dmg)

filter(df2, lnrenda2 == 0.000000)
filter(df2, igusint <= 36)

summary(diabetes$renda)
sd(diabetes$renda, na.rm = TRUE)
list(diabetes$renda)
median(NGTf$idademae, na.rm = TRUE)
IQR(NGTf$idademae, na.rm = TRUE)

ggplot(data = df2, mapping = aes(x = ffreemass)) +
  geom_histogram()


ggplot(data = df2, mapping = aes(x = lnFM)) +
  geom_freqpoly(binwidth = 0.05)

df2$lnrenda2 <- log(df2$renda + 1)
list(df2$lnrenda2)


is.numeric(df$fatmass)
df$lnFM <- log(df$fatmass)
df$lnFFM <- log(df$ffreemass)


summary(lm(data = df, lnFM ~ lnFFM))
df$index <- df$fatmass/(df$ffreemass^1.4)
df$indexx <- (df$index)*1000

summary(lm(pfat ~ imcpre + ganhodepeso + male, data = NGT_df))
confint(lm(pfat ~ imcpre + ganhodepeso + male, data = NGT_df))


diabetes_df <- filter(df, dmg == 1)
NGT_df <- filter(df, dmg == 0)


df2$lnFM <- log(df2$FM)
df2$lnFFM <- log(df2$ffreemass)

mean(NGT$pn, na.rm = TRUE)
sd(NGT$pn, na.rm = TRUE)
prop.table(table(diabetes$pncat))
list(df2$pncat)
list(df2$pn)

fivethou <- filter(df2, pn >= 5000)

summary(lm(pffreemass ~ dmg, data = df))

tbl1 <- table(df2.filtered$male, df2.filtered$dmg)
tbl1
prop.table(tbl1,2)
chisq.test(tbl1)

tbl2 <- table(df2.filtered$parity, df2.filtered$dmg)
tbl2
chisq.test(tbl2)
(75/112)*100

tbl3 <- table(df2.filtered$tipoparto, df2.filtered$dmg)
tbl3
chisq.test(tbl3)
(33/68)*100

tbl4 <- table(df2$aborto, df2$dmg)
tbl4
chisq.test(tbl4)
(47/211)*100

tbl5 <- table(df2.filtered$imcprecat, df2.filtered$dmg)
tbl5
(55/112)*100

tbl6 <- table(df2.filtered$GWG, df2.filtered$dmg)
tbl6
(34/112)*100

tbl7 <- table(df2.filtered$trabalho, df2.filtered$dmg)
tbl7
chisq.test(tbl7)
(55/112)*100

table(df2.filtered$pncat, df2.filtered$dmg)
(13/112)*100

table(df2$classecono, df2$dmg)

summary(df$maeimc)
list(df2$renda)


t.test(renda ~ dmg, data = df2)



#eg 47 is the column number where you want to make change
df2[, 47][df2[, 47] == 5070] <- 4



list(df2$cesarea)
summary(df2$ganhodepeso)
sd(NGT$ganhodepeso, na.rm = TRUE)

list(df2$ganhodepeso)

df2$wtgain <- df2$ganhodepeso*2.20462
df2$rndwt <- (round(df2$wtgain, digits = 0))

df2$renda <- (round(df2$renda, digits = 1))

df2$BMI <- (round(df2$imcpre, digits = 0))

list(df2$rndwt)
list(df2$BMI)

df2$GWG <- c(1, 1, 2, 2, 1, 0, 1, 1, NA, 1, NA, 2, 0, 1, NA, 2, NA,
             1, 2, NA, 2, 1, 0, 0, 0, 1, 2, 2, 0, NA, 2, 1, NA, NA,
             1, NA, 1, 0, NA, NA, 2, NA, 1, NA, 0, 1, 2, 1, 0, 1, 1,
             2, 1, NA, NA, 2, 0, 1, 0, NA, NA, 1, 2, NA, 1, 1, 1, NA,
             NA, NA, 0, NA, 1, 1, 2, NA, 0, NA, 0, 1, 2, 0, 2, 2, NA,
             2, NA, 2, 1, 0, 0, NA, 2, 0, NA, 2, 1, NA, 2, 2, NA, 0,
             1, NA, 2, NA, 1, 2, NA, NA, NA, 0, 2, NA, 0, NA, 2, 1,
             1, NA, 0, NA, 2, 1, 1, 1, 0, 1, 0, NA, NA, 2, 2, 2, 2,
             NA, 2, 1, NA, 0, 2, NA, 0, NA, NA, NA, 0, 2, 1, 1, 1, NA,
             NA, 1, NA, 2, 1, 1, 0, NA, 0, 1, 1, 0, 1, 2, 1, 2, 0, 0,
             0, NA, 2, 0, 0, 0, 2, 1, 2, 1, 1, 1, 1, 1, 2, 2, 0, 0, 2,
             2, NA, NA, 0, 0, 0, 0, 1, 1, NA, 1, 0, 1, 1, NA, 1, 0, 2, 
             NA, 1, 2, 1, 2, 1, 2, 2, 2, 0, 1, 2, 1, 0, 1, 2, 1, 2, 2,
             2, 2, 2, 2, 1, 2, 2, 1, 2, 2, 2, 2, 2, 0, 0, 1, 2, 1, 1,
             2, 0, 1, 2, 0, 2, 1, 2, 2, 2, 2, 1, 0, 0, 1, 2, 1, 1, 2,
             2, 0, 1, 0, 1, 1, 2, 2, 0, 2, 2, 0, 1, 2, 0, 0, 2, 2, 0)


summary(lm(data = df2, lnFM ~ lnFFM))
df2$index <- df2$FM/(df2$ffreemass^1.4)

summary(lm(data = diabetes, lnFM ~ lnFFM))
diabetes$index <- diabetes$FM/(diabetes$ffreemass^0.24)
diabetes$indexx <- (diabetes$index)*1000

summary(lm(data = NGT, lnFM ~ lnFFM))
NGT$index <- NGT$FM/(NGT$ffreemass^1.94)
NGT$indexx <- (NGT$index)*1000



ggplot(df2, aes(ffreemass, index)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

ggplot(na.omit(df2), aes(x = parity, y = indexx, color = parity)) +
  geom_boxplot()

cor.test(df2$indexx, df2$igusint, method = "pearson")
t.test(df2$igusint ~ df2$dmg)
cohen.d(df2$FM, df2$dmg, na.rm = TRUE)


anova <- aov(index ~ tipoparto, data = df2)
summary(anova)
TukeyHSD(anova)


row.has.na <- apply(df2, 1, function(x){any(is.na(x))})
sum(row.has.na)
df2.filtered <- df2[!row.has.na,]
df2.filtered2 <- df2[row.has.na,]
table(df2.filtered$male, df2.filtered$dmg)

df3.filtered <- df3[row.has.na,]
df3.filtered.m <- filter(df3.filtered, male == 1)
df3.filtered.f <- filter(df3.filtered, male == 0)

males <- filter(df2, male == 1)
females <- filter(df2, male == 0)


?mice

sum(is.na(diabetes$renda))

filter(diabetes, renda <= 954)
filter(diabetes, renda >= 724)

filter(NGT, renda >= 954)
filter(NGT, renda >= 724)

summary(lm(indexx ~ idademae + ganhodepeso + imcpre + tipoparto +
             male + igusint, data = diabetes))

summary(lm(indexx ~ dmg + idademae + tipoparto + igusint + imcpre +
             ganhodepeso + male, data = df2))


summary(lm(pfat ~ idademae + imcpre + ganhodepeso + 
             + tipoparto + male + igusint, data = NGT))
confint(lm(pfat ~ idademae + imcpre + ganhodepeso + 
             + tipoparto + male + igusint, data = NGT))



summary(diabetes$pfat)
ggplot(data = NGT, mapping = aes(x = FM)) +
  geom_histogram()
ggplot(data = diabetes, mapping = aes(x = ffreemass, y = FM)) +
  geom_point()
shapiro.test(df2$lnpfat)
df2$lnpfat <- log(df2$pfat)

cor.test(diabetes$compaferido, diabetes$ffreemass, method = "pearson")
cor.test(NGT$compaferido, NGT$ffreemass, method = "pearson")


ggplot(data = df2, mapping = aes(x = FM)) +
  geom_histogram(data = subset(diabetes), fill = "red", alpha = 0.2) +
  geom_histogram(data = subset(NGT), fill = 'blue', alpha = 0.2)


ggplot(df2, aes(x = index))


df2$indexx <- (df2$index)*1000

cor.test(df2$igusint, df2$ indexx, method = "pearson")

contrast <- model.matrix(~tipoparto, data = df2)
head(contrast[, -1])


pwr.r.test(n = 283, sig.level = 0.05, power = .8)


lm1 <- (lm(indexx ~ dmg + male + idademae + imcpre + 
             ganhodepeso + tipoparto + igusint, data = df2))



pred_r_squared <- function(lm1) {
  lm.anova <- anova(lm1)
  tss <- sum(lm.anova$"Sum Sq")
  pred.r.squared <- 1 - PRESS(lm1)/(tss)
  return(pred.r.squared)
}

PRESS <- function(lm1) {
  pr <- residuals(lm1)/(1 - lm.influence(lm1)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}

pred.r.squared <- pred_r_squared(lm1)
pred.r.squared




u = length(coef(lm1))-1
u
anova(lm1)
EtaSQ = 6883/(6883+263847)
EtaSQ
f2 = EtaSQ/(1-EtaSQ)
f2
pwr.f2.test(u = 8, f2 = 0.25, sig.level = 0.05, power = .8)
pwr.f2.test(u = 2, f2 = 0.25, sig.level = 0.05, power = .8)

pwr.f2.test(u = 8, v = NULL, f2 = .02, sig.level = 0.05, power = .8)


summary(lm1)
hist(residuals(lm1))
boxplot(residuals(lm1))
shapiro.test(residuals(lm1))
confint(lm1)
vif(lm1)
naprint(na.action(lm1))

par(mfrow=c(2,2))
plot(lm1)


lm2 <- (lm(indexx ~ dmg + idademae + imcpre + ganhodepeso + tipoparto + 
             male + igusint, data = df2.filtered))
summary(lm2)
confint(lm2)


lm3 <- (lm(indexx ~ idademae + imcpre + ganhodepeso + tipoparto + 
             male + igusint, data = diabetes))
summary(lm3)
confint(lm3)
vif(lm3)
par(mfrow=c(2,2))
plot(lm3)


lm4 <- (lm(indexx ~ idademae + imcpre + ganhodepeso + tipoparto + 
             male + igusint, data = NGT))

lm4.1 <- (lm(indexx ~ male + imcpre + ganhodepeso, data = NGT))
summary(lm4.1)

summary(lm4)
confint(lm4)
vif(lm4)
par(mfrow=c(2,2))
plot(lm4)



library(AER)
library(mice)

list(df2$tipoparto)
sum(!is.na(df2$vaginal))


imp <- mice(df3, seed = 123, m = 20, print = FALSE)
fit <- with(imp, lm(indexx ~ dmg + idademae + imcpre + 
                      ganhodepeso + tipoparto + 
                      male + igusint))
summary(pool(fit))
coef(fit$analyses[[2]])


imp$loggedEvents

warnings()

library(magrittr)

est2 <- df2 %>%
  mice(seed = 123, print = FALSE) %>%
  with(lm(indexx ~ dmg + idademae + imcpre + 
            ganhodepeso + tipoparto + 
            male + igusint)) %>%
  pool()

warnings()

summary(est2)


est3 <- df3 %>%
  mice(seed = 123, print = FALSE) %>%
  with(lm(df2$indexx ~ df2$dmg + df2$idademae + df2$imcpre + 
            df2$ganhodepeso + df2$tipoparto + 
            df2$male + df2$igusint)) %>%
  pool()

summary(est3)
sapply(df3, function(x) sum(is.na(x)))
miss_plot <- aggr(df3, col = c('navyblue', 'yellow'),
                  numbers = TRUE, sortVars = TRUE,
                  labels = names(df3), cex.axis = 0.7,
                  gap = 3, ylab = c("Missing data", "Pattern"))


df3 <- select(df2, indexx, dmg, idademae, imcpre, ganhodepeso, 
              tipoparto, male, igusint, altura, height, parity,
              trabalho, renda, pn, FM, ffreemass, compaferido,
              ccefalica, ctoracica, cabdominal)

df3

imputed_df3 <- mice(df3, m = 5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_df3)
imputed_df3$imp$cabdominal
xyplot(imputed_df3, igusint ~ imcpre + ganhodepeso, pch = 18, cex = 1)
densityplot(imputed_df3)
fit <- with(data = imputed_df3, exp = lm(indexx ~ dmg + idademae +
                                         imcpre + ganhodepeso + tipoparto +
                                         male + igusint))
combine <- pool(fit)
summary(combine)

modelFit1 <- with(imputed_df3, lm(indexx ~ dmg + idademae + imcpre +
                                    ganhodepeso + tipoparto +
                                    male + igusint))
(pool(modelFit1))
summary(pool(modelFit1))

round(summary(pool(modelFit1), conf.int = TRUE), 3)
pool.r.squared(modelFit1)
pool.r.squared(modelFit1, adjusted = TRUE)


impdata <- mice(df3, m = 50, seed = 245435, print = FALSE)
modelFit2 <- with(impdata, lm(indexx ~ dmg + idademae + 
                                imcpre + ganhodepeso + tipoparto + 
                                male + igusint))
summary(pool(modelFit2))

summary(df2$igusint)


pwr.f2.test(u = 7, v = 275, sig.level = 0.05, power = 1 - 0.20)
?pwr.f2.test


ggplot(data = df2) +
  geom_point(mapping = aes(x = imcpre, y = indexx, color = dmg)) +
  geom_smooth(mapping = aes(x = imcpre, y = indexx, linetype = dmg))

ggplot(df2, aes(imcpre, indexx, color = dmg)) +
  geom_point() +
  geom_smooth(se = FALSE)



###########################
##most recent stuff
lm1 <- (lm(indexx ~ dmg + male + idademae + imcpre + 
             ganhodepeso + tipoparto + igusint, data = completeDF))
summary(lm1)


pred_r_squared <- function(lm2.red1) {
  lm.anova <- anova(lm2.red1)
  tss <- sum(lm.anova$"Sum Sq")
  pred.r.squared <- 1 - PRESS(lm2.red1)/(tss)
  return(pred.r.squared)
}

PRESS <- function(lm2.red1) {
  pr <- residuals(lm2.red1)/(1 - lm.influence(lm2.red1)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}

pred.r.squared <- pred_r_squared(lm2.red1)
pred.r.squared


lm1.full <- (lm(indexx ~ dmg + male + idademae + imcpre + 
                  ganhodepeso + tipoparto + igusint, data = df2))
summary(lm1.full)
confint(lm1.full)


#mother's age and gestational age removed
lm1.red1 <- (lm(indexx ~ dmg + imcpre + 
                 ganhodepeso + tipoparto + male, data = df2))
summary(lm1.red1)
confint(lm1.red1)

#type of delivery removed
lm1.red2 <- (lm(indexx ~ dmg + imcpre + 
                  ganhodepeso + male, data = df2))
summary(lm1.red2)
confint(lm1.red2)


#full GDM subgroup model
lm2.full <- (lm(indexx ~ idademae + imcpre + ganhodepeso + tipoparto + 
             male + igusint, data = diabetes))
summary(lm2.full)

#GDM subgroup model with mother's age, gest age and type of delivery removed
lm2.red1 <- (lm(indexx ~ imcpre + ganhodepeso + male, data = diabetes))
summary(lm2.red1)
confint(lm2.red1)

#GDM subgroup model with 'male' removed
lm2.red2 <- (lm(indexx ~ imcpre + ganhodepeso, data = diabetes))
summary(lm2.red2)
confint(lm2.red2)


#full NGT subgroup model 
lm3.full <- (lm(indexx ~ idademae + imcpre + ganhodepeso + tipoparto + 
                  male + igusint, data = NGT))
summary(lm3.full)
confint(lm3.full)

#NGT subgroup model with mother's age, gest age and type of delivery removed
lm3.red1 <- (lm(indexx ~ imcpre + ganhodepeso + male, data = NGT))
summary(lm3.red1)
confint(lm3.red1)



##multiple imputation###

##check the data for missing values
sapply(completeDF, function(x) sum(is.na(x)))
md.pattern(NGT)
marginplot(NGT[,c("ganhodepeso", "imcpre")], col = c("blue", "red", "orange"), cex = 1, cex.lab = 1.5, cex.numbers = 1.2, pch = 19)


##impute missing data using mice for df2 dataset
library(mice)
init = mice(df2, maxit = 100, print = FALSE)
meth = init$method
predM = init$predictorMatrix
predM[, c("id")] = 0
predM[, c("indexx")] = 0
predM[, c("dmg")] = 0
predM

set.seed(500)
imputed = mice(df2, method = meth, predictorMatrix = predM, m = 5, print = FALSE)
summary(imputed)

#check imputed data for specific variable
imputed$imp$ganhodepeso
#check imputation method for each variable
imputed$meth

completeDF <- complete(imputed, 2)
View(completeDF)


##plots to inspect the distribution of original and imputed data
xyplot(imputed, ganhodepeso ~ imcpre+cabdominal+renda, pch = 18, cex = 1)
densityplot(imputed)
stripplot(imputed, ganhodepeso+imcpre~.imp, pch = 20, cex = 1)
#to assess convergence
plot(imputed, y = c("ganhodepeso", "imcpre", "cabdominal"), layout = c(2,3))


##fitting models to each imputed dataset and pooling results 
mod1 <- with(imputed, lm(indexx ~ dmg + male + idademae + imcpre + 
                               ganhodepeso + tipoparto + igusint))
summary(pool(mod1))

mod2 <- with(imputed, lm(indexx ~ dmg + imcpre + 
                           ganhodepeso + tipoparto + male))
summary(pool(mod2))

mod3 <- with(imputed, lm(indexx ~ dmg + imcpre + 
                           ganhodepeso + male))
summary(pool(mod3))


#get confidence intervals
round(summary(pool(mod3), conf.int = TRUE), 3)

#get multiple and adjusted r-squared
pool.r.squared(mod3)
pool.r.squared(mod3, adjusted = TRUE)


NGT$male <- as.factor(NGT$male)
NGT$trabalho <- as.factor(NGT$trabalho)
NGT$parity <- as.factor(NGT$parity)
NGT$tipoparto <- as.factor(NGT$tipoparto)
##impute missing data using mice for NGT dataset
library(mice)
init = mice(NGT, maxit = 100, print = FALSE)
meth = init$method
predM = init$predictorMatrix
predM[, c("id")] = 0
predM[, c("indexx")] = 0
predM

set.seed(500)
imputed = mice(NGT, method = meth, predictorMatrix = predM, m = 5, print = FALSE)
summary(imputed)

#check imputed data for specific variable
imputed$imp$ganhodepeso
#check imputation method for each variable
imputed$meth


##plots to inspect the distribution of original and imputed data
xyplot(imputed, ganhodepeso ~ imcpre+cabdominal+renda, pch = 18, cex = 1)
densityplot(imputed)
stripplot(imputed, ganhodepeso+imcpre~.imp, pch = 20, cex = 1)
#to assess convergence
plot(imputed, y = c("ganhodepeso", "imcpre", "cabdominal"), layout = c(2,3))


#imputed models for NGT subgroup
mod4 <- with(imputed, lm(indexx ~ idademae + imcpre + ganhodepeso + 
                           tipoparto + male + igusint))
summary(pool(mod4))
round(summary(pool(mod4), conf.int = TRUE), 3)
pool.r.squared(mod4)
pool.r.squared(mod4, adjusted = TRUE)


mod5 <- with(imputed, lm(indexx ~ imcpre + ganhodepeso + male))
summary(pool(mod5))
round(summary(pool(mod5), conf.int = TRUE), 3)
pool.r.squared(mod5)
pool.r.squared(mod5, adjusted = TRUE)


