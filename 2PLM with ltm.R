install.packages("ltm")
install.packages("openxlsx")

library(ltm)
library(openxlsx)

data_2plm <- read.xlsx("rasch_data.xlsx")

#*************************#
#Item parameter estimation#
#*************************#
model_2plm<- ltm(data_2plm[c(-3,-62,-113),-1]~z1, IRT.param = T)
model_2plm$convergence
summary(model_2plm)
## d = - a * b
parm_model<- coef(model_2plm)
parm_model
write.xlsx(parm_model, "parm_model2plm.xlsx")

#************************************************#
#Person latent trait, or ability score estimation#
#************************************************#
factor.scores(model_2plm, method = "EAP")
factor.scores(model_2plm, method = "EAP", resp.patterns = data_2plm[c(-3,-62,-113),-1])

fs<-factor.scores(model_2plm, method = "EAP", resp.patterns = data_2plm[c(-3,-62,-113),-1])
max(fs$score.dat$z1)
min(fs$score.dat$z1)
mean(fs$score.dat$z1)
sd(fs$score.dat$z1)
fs$score.dat$z1 ## القدرة
fs$score.dat$se.z1 ## الخطأ المعياري

#********************#
#######ICC plot#######
#********************#
plot(model_2plm, legend = T)
plot(model_2plm)
plot(model_2plm, items = 6, legend = T)
plot(model_2plm, items = c(1,6,8,4))
plot(model_2plm, items = 2:7)

#*******************************#
#Item and test information plots#
#*******************************#
plot(model_2plm, type = "IIC", legend = T)
plot(model_2plm, type = "IIC")
plot(model_2plm, type = "IIC", items = 5)
plot(model_2plm, type = "IIC", items = c(1,9,5))

#TIC
plot(model_2plm, type = "IIC", items = 0)

#************************#
#########item fit#########
#************************#
item.fit(model_2plm)
item.fit(model_2plm, FUN = mean)

#**************************#
#########person fit#########
#**************************#
person.fit(model_2plm)
person.fit(model_2plm, resp.patterns = data_2plm[c(-3,-62,-113),-1])

#**************************#
#########comparison#########
#**************************#
model_1plm<-rasch(data_2plm[,-1])
model_2plm1<-ltm(data_2plm[,-1]~z1)
anova(model_1plm,model_2plm1) # p.value < 0.001 rejecting the null hypothesis
