library(survival)
data(diabetic)
attach(diabetic)
trt <- as.factor(trt)

# 1a
print(names(diabetic))
#"id"     "laser"  "age"    "eye"    "trt"    "risk"   "time"   "status"
#https://www.mayo.edu/research/documents/diabeteshtml/DOC-10027460/
#ID：Il est utilisé pour distinguer chaque participant dans l'ensemble de données.
#laser：Il s'agit du type de traitement laser reçu. 1=xenon, 2=argon
#age: Il s'agit de l'âge auquel le diabète a été diagnostiqué chez le patient.
#eye: Il s'agit d'un facteur avec des niveau de gauche et de droit.
#trt: Il s'agit du group de traitement.0=no treatment,1=laser
#risk: Il s'agit d'une variable quantitative d'évaluation du risque utilisée pour classer les participants dans différents groupes de risque(les valeurs varient de 6 à 12).
#time: Il s'agit de la date de l'événement ou de la dernière visite de suivi.
#status:Il s'agit d'une variable binaire utilisée pour indiquer si une perte de vision s'est produite au cours de la période d'étude,où 0 indique qu'aucune perte de vision ne s'est produite à la fin
de la période d'observation et 1 indique qu'une perte de vision s'est produite.
#还需要再归纳一下这个实验来自哪个研究

#1bc
#Kaplan-Meier 
result.km <- survfit(Surv(time, status) ~ 1, conf.type="log-log")
plot(result.km, xlab = "Jours", ylab = "Probabilité de Survie", main = "Kaplan-Meier")
result.kmtrt0 <- survfit(Surv(time[trt == 0], status[trt == 0]) ~ 1, conf.type="log-log")
par(new=TRUE)
plot(result.kmtrt0, xlab = "Jours", ylab = "Probabilité de Survie", main = "Kaplan-Meier", col = "red")
result.kmtrt1 <- survfit(Surv(time[trt == 1], status[trt == 1]) ~ 1, conf.type="log-log")
par(new=TRUE)
plot(result.kmtrt1, xlab = "Jours", ylab = "Probabilité de Survie", main = "Kaplan-Meier", col = "blue")
legend("topright", legend=c("totale","trt = 0", "trt = 1"), col=c("black","red", "blue"), lty=1:5, cex=0.8)
#abline ( v = 43.7 , col = 'red' , lty =2)

#1d
#On concentre sur les patients trt = 0
#diabetic_controle <- diabetic[trt == 0,]
print(result.kmtrt0) #mediane = 43.7

#1e test de log-rank
result.logrank <- survdiff(Surv(time, status) ~ trt, data = diabetic)
print(result.logrank)
