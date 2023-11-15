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
subset_data <- subset(diabetic,trt == 0)
fit <- survfit(Surv(time,status) ~ 1,data=subset_data, conf.type="log-log")
result.km<-fit
print(result.km)
#Le délai médian jusqu'à la cécité pour les patients qui n'ont pas reçu de traitement au laser était de 43,7 mois. 
#L'intervalle de confiance à 95 % pour cette estimation était large, allant de 31,6 à 59,8 mois.

 #1e test de log-rank
result.logrank <- survdiff(Surv(time, status) ~ trt, data = diabetic)
print(result.logrank)
#Les résultats du test du chi-carré (Chisq=22,2, p=2e-06), dans lequel nous pouvons voir que la valeur p est bien inférieure à 0,05. 
#Cela suggère que la différence de temps avant la cécité entre les deux groupes de traitement est statistiquement significative. 
#En outre, dans ce cas, le nombre de cécités était significativement plus élevé que prévu dans le groupe qui n'a pas reçu de traitement au laser (trt=0), tandis que le nombre de cécités était plus faible que prévu dans le groupe qui a reçu un traitement au laser (trt=1).
#En conclusion, les résultats du test Log-Rank ont montré que le fait de recevoir ou non un traitement au laser avait un effet significatif sur la courbe de survie des patients. Cela implique que le traitement au laser peut être une intervention efficace pour retarder la perte de vision chez les patients diabétiques.

 #1f# 计算年龄的中位数，并创建年龄组变量
median_age <- median(diabetic$age, na.rm = TRUE)
diabetic$age_group <- ifelse(diabetic$age <= median_age, "younger", "older")
# 使用survfit函数根据年龄组计算Kaplan-Meier生存曲线
result.kmage <- survfit(Surv(time, status) ~ age_group, data = diabetic)
# 绘制Kaplan-Meier生存曲线
plot(result.kmage, main = 'Courbe de Kaplan-Meier', xlab = 'Temps en jours', ylab = 'Probabilite de survie', col = c("red", "blue"))
# 进行分层的Log-Rank测试
survdiff(Surv(time, status) ~ trt + strata(age), data = diabetic)
#Dans notre analyse approfondie de l'ensemble de données sur les diabetic, 
#nous avons adopté une stratégie basée sur la segmentation de l'âge médian afin de discerner l'effet des différents traitements sur les patients d'âges différents. 
#Cette segmentation a permis de classer les patients en deux groupes, “Younger“et “Older”, ce qui nous a permis d'explorer l'impact potentiel du facteur âge sur les résultats du traitement.
