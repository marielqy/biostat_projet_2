library(survival)
data(rotterdam)
attach(rotterdam)

#print(rotterdam)
print(head(rotterdam))


#2a

#print(recur)

resultat.cox <- coxph(Surv(rtime, recur) ~ age + meno + size + grade + nodes + chemo, data = rotterdam)
print(summary(resultat.cox))