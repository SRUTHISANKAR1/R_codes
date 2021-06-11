Faltoons <- read.csv("C:/Users/server/Downloads/Faltoons (1).csv")
View(Faltoons)
head(Faltoons)
attach(Faltoons)
###400 0bservations and 2 variables
## both X and Y are discrete..so here we choose 2 proportion test
#Ho= Proportions of Male and Female are same
#Ha= Proportions of Male and Female are not same


#####2 proportion test
table1<-table(Weekdays,Weekend)
head(table1)

prop.test(x=c(120,47),n=c(287,113),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
#p value p-value = 0.9681>0.05   p high Ho fly.. accept Ho. So proportions are  equal
#prop.test(x=c(167,66),n=c(287,113),conf.level = 0.95,correct = FALSE,alternative = "two.sided")



