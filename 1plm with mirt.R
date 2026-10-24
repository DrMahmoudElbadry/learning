library(mirt)
library(openxlsx)

df_1plm <- read.xlsx("rasch_data.xlsx")[,-1]

#############################
##Item parameter estimation##
#############################
spec <- 'F = 1-20
CONSTRAIN = (1-20, a1)'

model_1plm <- mirt(data = df_1plm, model = spec, itemtype = "2PL", SE = T)
model_1plm
extract.mirt(model_1plm, what = "converged")
extract.mirt(model_1plm, what = "secondordertest")

coef(model_1plm, IRTpars = T, simplify = T)
coef(model_1plm, IRTpars = T)
coef(model_1plm, IRTpars = T, printSE = T)

####################################################
##Person latent trait, or ability score estimation##
####################################################
fscores(model_1plm, full.scores.SE = T)

###################################
##ICC, TCC, and information plots##
###################################
itemplot(model_1plm, 5)
plot(model_1plm, type = "trace")
plot(model_1plm)

itemplot(model_1plm, 5, "info")
plot(model_1plm, type = "info")

#######################################
##Model-data fit and model comparison##
#######################################
M2(model_1plm)

itemfit(model_1plm)
itemfit(model_1plm, empirical.plot = 5)

personfit(model_1plm)

residuals(model_1plm, type = "LD")
residuals(model_1plm, type = "Q3")

model_2plm <- mirt(df_1plm, 1, "2PL")
anova(model_1plm, model_2plm)
