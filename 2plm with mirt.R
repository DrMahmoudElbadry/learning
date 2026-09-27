install.packages("mirt")
library(mirt)
data <- expand.table(LSAT7)
itemstats(data)
names(data)<-paste("IT", 1:5, sep = "")

#**************************#
#Item parameter estimation #
#**************************#
model_2plm <- mirt(data[,c(-2)], 1, "2PL", SE = T)
model_2plm
extract.mirt(model_2plm, what = "converged")
extract.mirt(model_2plm, what = "secondordertest")

coef(model_2plm, IRTpars = T, simplify = T)
coef(model_2plm, IRTpars = T)
coef(model_2plm, IRTpars = T, printSE = T)

#************************************************#
#Person latent trait, or ability score estimation#
#************************************************#
fscores(model_2plm, method = "EAP", full.scores = T, full.scores.SE = T)

#*******************************#
#ICC, TCC, and information plots#
#*******************************#
itemplot(model_2plm, 3)
plot(model_2plm, type = "trace")
plot(model_2plm)
plot(model_2plm, type = "info")
itemplot(model_2plm, 3, type = "info")

#***************#
#Model-data fit #
#***************#
M2(model_2plm)

itemfit(model_2plm)
itemfit(model_2plm, empirical.plot = 1, empirical.CI = 0.95)


personfit(model_2plm, method = "EAP")

pf<- personfit(model_2plm, method = "EAP")
pf[abs(pf$Zh)>2,]

subset(as.data.frame(pf),infit > 1.5 | outfit > 1.5)
subset(as.data.frame(pf),infit > 2 | outfit > 2)

#***********#
#local indep#
#***********#
LD <- residuals(model_2plm, type = "LD")
Q3 <- residuals(model_2plm, type = "Q3")
install.packages("tidyverse")
library(tidyverse)
diag(LD)<-   NA
LD_x2 <- as.data.frame(as.table(as.matrix(LD))) |> ##تحوبل المصفوفة الي جدول يمكن التعامل معه
  filter(!is.na(Freq)) |> ##حذف القيم NA
  rename(Item1 = Var1, Item2 = Var2, LD = Freq) |> ##اعادةتسمية الاعمدة
  filter(as.integer(Item1) > as.integer(Item2)) |> ## اختيار المثلث السفلي
  arrange(desc(abs(LD)))

Cramer_Table <- as.data.frame(as.table(as.matrix(LD))) |> ##تحوبل المصفوفة الي جدول يمكن التعامل معه
  filter(!is.na(Freq)) |> ##حذف القيم NA
  rename(Item1 = Var1, Item2 = Var2, Cramer_V = Freq) |> ##اعادةتسمية الاعمدة
  filter(as.integer(Item1) < as.integer(Item2)) |> ## اختيار المثلث العلوي
  arrange(desc(abs(Cramer_V))) ## |Cramer_V| <= 0.174

diag(Q3)<- NA
Q3_Table <- as.data.frame(as.table(as.matrix(Q3))) |> 
  filter(!is.na(Freq)) |> 
  rename(Item1 = Var1, Item2 = Var2, Q3 = Freq) |> 
  filter(as.integer(Item1) < as.integer(Item2)) |> 
  arrange(desc(abs(Q3))) ## |Q3| < sqrt(0.05) |  |Q3| < 0.2236068

save.image("2plm_mirt.RData")
load("2plm_mirt.RData")
