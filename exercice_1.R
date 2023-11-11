library(survival)
data(diabetic)
attach(diabetic)

trt <- as.factor(trt)

# 1a
print(names(diabetic))
#"id"     "laser"  "age"    "eye"    "trt"    "risk"   "time"   "status"
#https://www.mayo.edu/research/documents/diabeteshtml/DOC-10027460/

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