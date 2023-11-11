library(survival)
data(rotterdam)
attach(rotterdam)

#2a

resultat.cox <- coxph(Surv(rtime, recur) ~ age + meno + size + grade + nodes + chemo, data = rotterdam)
print(summary(resultat.cox))