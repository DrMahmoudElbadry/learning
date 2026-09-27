detach("package:eRm")
install.packages("mirt")
install.packages("openxlsx")
install.packages("tidyverse")
library(mirt)
library(openxlsx)
library(tidyverse)
df_rasch <- read.xlsx("rasch_data.xlsx")
model_rasch<- mirt(df_rasch[,-1], model = 1, itemtype = "Rasch", SE = T, SE.type = "Oakes") ##Oakes
model_rasch<- mirt(df_rasch[,-1], model = 1, itemtype = "Rasch", SE = T,SE.type = "crossprod") ##crossprod
model_rasch
extract.mirt(model_rasch, what = "converged")
extract.mirt(model_rasch, what = "secondordertest")
coef(model_rasch, IRTpars = T, printSE = T)

##person latenttrait scores
person_scores <- fscores(model_rasch, method = "EAP", full.scores.SE = TRUE)
person_scores

##ICC, TCC, and information Plots
# ICC
itemplot(model_rasch, 3)
# ICCs
plot(model_rasch, type = "trace")
# test characteristic curve (TCC)
plot(model_rasch, type = "info")
#item test information curve
itemplot(model_rasch, 20, type = "info")

#############Model-data fit
M2(model_rasch)
itemfit(model_rasch)
itemfit(model_rasch , empirical.plot = 1, empircal.CI = 0.95)

personfit(model_rasch)
LD<- residuals(model_rasch, type = "LD")
Q3<- residuals(model_rasch, type = "Q3")
diag(LD)<-NA ##لجعل محور المصفوفة NA
LD_Table<- as.data.frame(as.table(as.matrix(LD))) |> ## تحويل المصفوفة إلى جدول
filter(!is.na(Freq)) |> ## حذف القيم الفارغة
rename(Item1 = Var1, Item2 = Var2, LD_X2 = Freq) |> ## اعادة تسمية الأعمدة
filter(as.integer(Item1) > as.integer(Item2)) |> ## اختيار المثلث السفلي 
arrange (desc(abs(LD_X2))) ## ترتيب الجدول حسب القيمة المطلقة للـ LD_X2
LD_Table
## LD-X2 < |15.142|

Cramer_Table<- as.data.frame(as.table(as.matrix(LD))) |> ## تحويل المصفوفة إلى جدول
filter(!is.na(Freq)) |> ## حذف القيم الفارغة
rename(Item1 = Var1, Item2 = Var2, Cramer_V = Freq) |> ## اعادة تسمية الأعمدة
filter(as.integer(Item1) < as.integer(Item2)) |> ## اختيار المثلث العلوي
arrange (desc(abs(Cramer_V))) ## ترتيب الجدول حسب القيمة المطلقة للـ Cramer_V
Cramer_Table
## Cramer_V < |0.174|

diag(Q3)<-NA ##لجعل محور المصفوفة NA
Q3_Table<- as.data.frame(as.table(as.matrix(Q3))) |> ## تحويل المصفوفة إلى جدول
filter(!is.na(Freq)) |> ## حذف القيم الفارغة
rename(Item1 = Var1, Item2 = Var2, Q3 = Freq) |> ## اعادة تسمية الأعمدة
filter(as.integer(Item1) > as.integer(Item2)) |> ## اختيار المثلث السفلي 
arrange (desc(abs(Q3))) ## ترتيب الجدول حسب القيمة المطلقة للـ Q3
Q3_Table
#Q3 Value < sqrt(0.05) |0.2236|
