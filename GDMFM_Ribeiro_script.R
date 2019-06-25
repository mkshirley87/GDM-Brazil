install.packages("tidyverse")
library(tidyverse)
install.packages("mice")
library(mice)
install.packages("relaimpo")
library(relaimpo)

df2 <- read_dta(file = "bancounicoartigo.dta")


#not normally distributed
shapiro.test(df2$renda)
shapiro.test(df2$idademae)
shapiro.test(df2$imcpre)
shapiro.test(df2$ganhodepeso)
shapiro.test(df2$pn)
shapiro.test(df2$FM)
shapiro.test(df2$igusint)
shapiro.test(df2$cabdominal)

ggplot(data = df2, mapping = aes(x = FM)) +
  geom_histogram()


#Chi-squared, Mann-Whitney-Wilcoxon, independent sample t-test, 
#and one-way ANOVA with Tukey post-hoc adjustment  

tbl1 <- table(df2$male, df2$dmg)
tbl1
prop.table(tbl1,2)
chisq.test(tbl1)

tbl2 <- table(df2$parity, df2$dmg)
tbl2
chisq.test(tbl2)

tbl3 <- table(df2$tipoparto, df2$dmg)
tbl3
chisq.test(tbl3)

tbl4 <- table(df2$aborto, df2$dmg)
tbl4
chisq.test(tbl4)

tbl5 <- table(df2$imcprecat, df2$dmg)
tbl5

tbl6 <- table(df2$GWG, df2$dmg)
tbl6

tbl7 <- table(df2$trabalho, df2$dmg)
tbl7
chisq.test(tbl7)

wilcox.test(idademae ~ dmg, data = df2)
median(df2$idademae, na.rm = TRUE)
IQR(df2$idademae, na.rm = TRUE)

t.test(height ~ dmg, data = df2)
sd(NGT$height, na.rm = TRUE)

anova <- aov(dmg ~ tipoparto, data = df2)
summary(anova)
TukeyHSD(anova)


#derivation of FM/FFM^p index
summary(lm(data = df, lnFM ~ lnFFM))
df$index <- df$fatmass/(df$ffreemass^1.4)
df$indexx <- (df$index)*1000


##multiple regression models

#combined model (GDM + NGT groups)
lm1.full <- (lm(indexx ~ dmg + male + idademae + imcpre + 
                  ganhodepeso + tipoparto + igusint, data = df2))
summary(lm1.full)
confint(lm1.full)

#reduced model 1 - mother's age and gestational age removed
lm1.red1 <- (lm(indexx ~ dmg + imcpre + 
                 ganhodepeso + tipoparto + male, data = df2))
summary(lm1.red1)
confint(lm1.red1)

#reduced model 2 - type of delivery removed, best-fit model
lm1.red2 <- (lm(indexx ~ dmg + imcpre + 
                  ganhodepeso + male, data = df2))
summary(lm1.red2)
confint(lm1.red2)


#full GDM subgroup model
lm2.full <- (lm(indexx ~ idademae + imcpre + ganhodepeso + tipoparto + 
             male + igusint, data = diabetes))
summary(lm2.full)

#GDM subgroup model reduced 1 - mother's age, gest age and type of delivery removed
lm2.red1 <- (lm(indexx ~ imcpre + ganhodepeso + male, data = diabetes))
summary(lm2.red1)
confint(lm2.red1)

#GDM subgroup model reduced 2 - male removed, best-fit model
lm2.red2 <- (lm(indexx ~ imcpre + ganhodepeso, data = diabetes))
summary(lm2.red2)
confint(lm2.red2)


#full NGT subgroup model 
lm3.full <- (lm(indexx ~ idademae + imcpre + ganhodepeso + tipoparto + 
                  male + igusint, data = NGT))
summary(lm3.full)
confint(lm3.full)

#NGT subgroup model reduced 1 - mother's age, gest age and type of delivery removed, best-fit model
lm3.red1 <- (lm(indexx ~ imcpre + ganhodepeso + male, data = NGT))
summary(lm3.red1)
confint(lm3.red1)


##multiple imputation

#check the data for missing values
sapply(completeDF, function(x) sum(is.na(x)))
md.pattern(NGT)
marginplot(NGT[,c("ganhodepeso", "imcpre")], col = c("blue", "red", "orange"), 
           cex = 1, cex.lab = 1.5, cex.numbers = 1.2, pch = 19)


#impute missing data using mice for df2 dataset
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

#plots to inspect the distribution of original and imputed data
xyplot(imputed, ganhodepeso ~ imcpre+cabdominal+renda, pch = 18, cex = 1)
densityplot(imputed)
stripplot(imputed, ganhodepeso+imcpre~.imp, pch = 20, cex = 1)
#to assess convergence
plot(imputed, y = c("ganhodepeso", "imcpre", "cabdominal"), layout = c(2,3))

#fitting models to each imputed dataset and pooling results 
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

       
#impute missing data using mice for NGT dataset
init = mice(NGT, maxit = 100, print = FALSE)
meth = init$method
predM = init$predictorMatrix
predM[, c("id")] = 0
predM[, c("indexx")] = 0
predM

set.seed(500)
imputed = mice(NGT, method = meth, predictorMatrix = predM, m = 5, print = FALSE)
summary(imputed)

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

