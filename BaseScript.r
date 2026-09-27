c(12,14,16,18,20,22,24,26,28,30)
"Hello, World!"
c("apple", "banana", "cherry","mah")
rep(1,10)
rep(2:10,3)
mah<- rnorm(n = 10, mean = 160, sd = 3)

mean(mah)
sd(mah)

round(mah, 0)
mah[mah>160]
mah[c(1,3,6)]

df<- data.frame(
  Name = c("Alice", "Bob", "Charlie", "David"),
  Age = c(25, 30, 35, 40,50),
  Score = c(85, 90, 95, 80)
)

df1<- data.frame(
  id = 1:10,
  group = c(rep(1,5), rep(2,5)),
  iq = round(rnorm(10, 100, 4),0),
  marf = sample(15:20, 10, replace = TRUE)
)

df1
df1[3:7,3:4]
df1$iq[2:3]

str(df)
str(df1)
dd<-c("1", "2", "3", "4", "5")
str(dd)
dd  <- as.integer(dd)

names(df) <- c("id", "hh", "ss")
names(df1) <- paste("col", 1:4, sep = "_")

dim(df)
dim(df1)

summary(df1)

apply(df1, 1, sum)

ms<-cbind(df1, sum = apply(df1, 1, sum))

rbind(ms, apply(ms, 2, mean))

save.image("mydata.RData")
load("mydata.RData")

# install.packages("openxlsx") # تعليق التثبيت لتجنب التكرار
library(openxlsx)
write.xlsx(df1, file = "mydata.xlsx")

rm(list = ls())  ##Remove all objects from the workspace

# rm(df1) # لا داعي لها هنا لأننا قمنا بمسح كل شيء في السطر السابق

df1<- read.xlsx("mydata.xlsx")

str(df1)

# استخدام file.choose بدلاً من choose.files ليعمل على كل الأنظمة
# df1<- read.xlsx(file.choose()) 

install.packages("mirt")



c(1,,1,2,2,3)






df1<- as.data.frame(df1)
save.image("mydata.RData")
