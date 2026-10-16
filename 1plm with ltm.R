install.packages("ltm")
library(ltm)
library(openxlsx)

data_1plm <- read.xlsx("rasch_data.xlsx")[,-1]

#############################
##Item parameter estimation##
#############################
model_1plm <- rasch(data_1plm [,c(-5)]) ##IRT.param = F
model_1plm$convergence
summary(model_1plm)
dfcoef <- coef(model_1plm)
write.xlsx(dfcoef, "dfcoef.xlsx", row.names = T)

####################################################
##Person latent trait, or ability score estimation##
####################################################
factor.scores(model_1plm, method = "EAP", resp.patterns = data_1plm)

############
##ICC plot##
############
plot(model_1plm)
plot(model_1plm, legend = T)
plot(model_1plm, items = 5)
plot(model_1plm, items = 5:9)
plot(model_1plm, items = c(1,4,9))

plot(model_1plm, type = "IIC")
plot(model_1plm, type = "IIC", legend = T)
plot(model_1plm, items = 5, type = "IIC")
plot(model_1plm, items = 5:9, type = "IIC")
plot(model_1plm, items = c(1,4,9), type = "IIC")
plot(model_1plm, items = 0, type = "IIC")

############
##Item fit##
############
item.fit(model_1plm) ## Pr(x2) > 0.05

##############
##Person fit##
##############
person.fit(model_1plm, resp.patterns = data_1plm [,-5]) ## Lz > -3 , Pr(Lz) > 0.001

options(max.print = 10000)

#######################################
##Model-data fit and model comparison##
#######################################
GoF.rasch(model_1plm, B = 100)

model_2plm <- ltm(data_1plm~z1)
model_1plm <- rasch(data_1plm)
anova(model_1plm, model_2plm)
