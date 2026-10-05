install.packages("mirt")
library(mirt)
install.packages("openxlsx")
library(openxlsx)
data_2plm <- read.xlsx("rasch_data.xlsx")[,-1]

#**************************#
#Item parameter estimation #
#**************************#
model_2plm <- mirt(data_2plm[c(-51,-99),-1], 1, '2PL', SE = T)
model_2plm

extract.mirt(model_2plm, "converged")
extract.mirt(model_2plm, "secondordertest")

coef(model_2plm, IRTpars = T, simplify = T)
coef(model_2plm, IRTpars = T)
coef(model_2plm, IRTpars = T, printSE = T)

item.parm <- coef(model_2plm, IRTpars = T, simplify = T)
item.parm$items

mean(item.parm$items[,1])
sd(item.parm$items[,1])

mean(item.parm$items[,2])
sd(item.parm$items[,2])

summary(item.parm$items)

#************************************************#
#Person latent trait, or ability score estimation#
#************************************************#
person.abilaty <- fscores(model_2plm, full.scores.SE = T)
summary(person.abilaty)

sd(person.abilaty[,1])
apply(person.abilaty,2,sd)
apply(person.abilaty,2,mean)

#*******************************#
#ICC, TCC, and information plots#
#*******************************#
itemplot(model_2plm, 3)
plot(model_2plm, type = "trace")
plot(model_2plm)

itemplot(model_2plm, item =3,type = "info")
plot(model_2plm, type = "info")

#***************#
#Model-data fit #
#***************#
M2(model_2plm)

itemfit(model_2plm)
itemfit(model_2plm, empirical.plot = 10)

personfit(model_2plm)
pf <- personfit(model_2plm)

pf[pf$Zh > 2, ]

subset(pf, outfit > 1.5 | outfit < 0.5 | infit > 1.5 | infit < 0.5 )
subset(pf, outfit > 2 | outfit < 0.5 | infit > 2 | infit < 0.5 )

#***********#
#local indep#
#***********#
LD <- residuals(model_2plm, type = "LD")
Q3 <- residuals(model_2plm, type = "Q3")

install.packages("tidyverse")
library(tidyverse)
diag(LD)<- NA
LD_X2 <- as.data.frame(as.table(LD)) |> ##تحويل المصفوفة الي جدول يمكن التعامل معه
  filter(!is.na(Freq)) |> ##خذف قيم NA
  rename(Item1 = Var1, Item2 = Var2, LD_X2 = Freq) |> ## اعادة تسمية للاعمدة
  filter(as.integer(Item1) > as.integer(Item2)) |> ## اختيار المثلث السفلي
  arrange(desc(abs(LD_X2))) ## |LD_X2| <= 15.142

LD_X2

Cramer_table <- as.data.frame(as.table(LD)) |> ##تحويل المصفوفة الي جدول يمكن التعامل معه
  filter(!is.na(Freq)) |> ##خذف قيم NA
  rename(Item1 = Var1, Item2 = Var2, Carmer_V = Freq) |> ## اعادة تسمية للاعمدة
  filter(as.integer(Item1) < as.integer(Item2)) |> ## اختيار المثلث العلوي
  arrange(desc(abs(Carmer_V))) ## |Carmer_V| <= 0.174

Cramer_table

Q3_table <- as.data.frame(as.table(Q3)) |> ##تحويل المصفوفة الي جدول يمكن التعامل معه
  filter(!is.na(Freq)) |> ##خذف قيم NA
  rename(Item1 = Var1, Item2 = Var2, Q3 = Freq) |> ## اعادة تسمية للاعمدة
  filter(as.integer(Item1) < as.integer(Item2)) |> ## اختيار المثلث العلوي
  arrange(desc(abs(Q3))) ## |Q3| < sqrt(0.05) or |Q3| < 0.2236068

Q3_table

save.image("2plm_with_mirt_database.RData")
load("2plm_with_mirt_database.RData")
