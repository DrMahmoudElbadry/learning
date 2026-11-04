library(mirt)
library(openxlsx)

df_3plm<- read.xlsx("3plm_dataset.xlsx", sheet = 1)

#############################
##Item parameter estimation##
#############################
spec <- 'F = 1-20
PRIOR = (1-20, g, norm, -1.1, 2)'
mirt(df_3plm, spec, "3PL", pars = "value")

spec <- 'F = 1-20
PRIOR = (1-20, g, norm, -1.1, 2), (1-20, a1, lnorm, 0, 0.5)'
mirt(df_3plm, spec, "3PL", pars = "value")

model_3plm<- mirt(df_3plm, spec, "3PL", SE = T)
model_3plm

extract.mirt(model_3plm, "converged")
extract.mirt(model_3plm, "secondordertest")

coef(model_3plm, IRTpars = T, simplify = T)
coef(model_3plm, simplify = T)
coef(model_3plm, IRTpars = T, printSE = T)
coef(model_3plm, printSE = T)

####################################################
##Person latent trait, or ability score estimation##
####################################################
fscores(model_3plm, full.scores.SE = T)

###################################
##ICC, TCC, and information plots##
###################################
itemplot(model_3plm, 15)
plot(model_3plm, type = "trace")
plot(model_3plm)

itemplot(model_3plm, 10, "info")
plot(model_3plm, type = "info")

##################
##Model-data fit##
##################
M2(model_3plm)

itemfit(model_3plm)
itemfit(model_3plm, empirical.plot = 1)

personfit(model_3plm)

p_fit<- cbind(id = 1:1500, df_3plm, personfit(model_3plm))
p_fit[order(p_fit[,26]),]

residuals(model_3plm, type = "LD")
residuals(model_3plm, type = "Q3")

####################
##model comparison##
####################
spec2 <- 'F = 1-20
PRIOR = (1-20, a1, lnorm, 0, 0.5'
model_2plm <- mirt(df_3plm, model = spec2, itemtype = "2PL", SE = T)

anova(model_2plm, model_3plm)
pchisq(2*(-16500.72 + 16550.31), 20, lower.tail = F)
