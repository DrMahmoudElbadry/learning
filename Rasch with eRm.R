install.packages("openxlsx")
install.packages("haven")
install.packages("eRm")

#library(haven)
#read_sav("kgkdf.sav")

library(openxlsx)
df_rasch<-read.xlsx("rasch_data.xlsx")

dim(df_rasch)
names(df_rasch)
names(df_rasch[,-1])

library(eRm)
data(raschdat1)
raschdat1
head(raschdat1)

apply(raschdat1, 2 , table)

###############################
###Item parameter estimation###
###############################
mod_rasch<-RM(raschdat1)
mod_rasch$convergence
summary(mod_rasch)
eta<-round(cbind(defc = -mod_rasch$betapar,se =mod_rasch$se.beta),3)
eta<- as.data.frame(eta)
write.xlsx(eta, "rasch_item_parameters.xlsx", rowNames = TRUE)
round(cbind(low = -confint(mod_rasch)[,2], high = -confint(mod_rasch)[,1]),3)
round(sum(eta$defc),3)

mod_rasch0<-RM(raschdat1,sum0=F)
summary(mod_rasch0)

##########################################
###Item characteristic curve (ICC) plot###
##########################################
plotICC(mod_rasch,1)
plotjointICC(mod_rasch, item.subset = c(1,2,3,4,5))
plotjointICC(mod_rasch)
######################################################
###Person latent trait, or ability score estimation###
######################################################
pr_ability<-person.parameter(mod_rasch)
summary(pr_ability)
row_scor_ab<-print(pr_ability)
write.xlsx(row_scor_ab, "rasch_person_ability.xlsx", rowNames = TRUE)

write.xlsx(pr_ability$theta.table, "pr_ability_theta_table.xlsx", rowNames = TRUE)

plot(pr_ability)
as.data.frame(coef(pr_ability))
mean(coef(pr_ability))
sd(coef(pr_ability))
median(coef(pr_ability))
min(coef(pr_ability))
max(coef(pr_ability))

###################################
###Person-item map### Wright Map###
###################################
plotPImap(mod_rasch)

####################
###Model-data fit###
####################
lrtest<-LRtest(mod_rasch)
lrtest
Waldtest(mod_rasch)

MLoef(mod_rasch)

plotGOF(lrtest, conf = list())

itemfit(pr_ability) 
personfit(pr_ability)

#############################################################
###Item information curve and test information curve plots###
#############################################################
plotINFO(mod_rasch)
plotINFO(mod_rasch, type = "item")
plotINFO(mod_rasch, type = "test")
