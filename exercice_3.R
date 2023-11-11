library(survival)
donnee <- read.delim("Chapters\\biostat_projet_2\\datasurv.txt", header = TRUE, sep = " ")
#donnee <- read.delim("datasurv.txt", header = TRUE)
attach(donnee)

#3a
#surv_objet <- Surv(time_days, event)
#print(grepl("\\+$", surv_objet[1]))

max_jour <- 500
t <- seq(max_jour)
n <- rep(0, max_jour)
d <- rep(0, max_jour)
q <- rep(0, max_jour)
S <- rep(0, max_jour)
n[1] <- sum(time_days >= 1)
S[1] <- 1

for (i in 2:max_jour) {
    n[i] <- sum(time_days >= i)
    if (i %in% time_days) {
        d[i] <- sum(event[which(time_days == i)])
    } 
    q[i] <- d[i] / n[i]
    S[i] <- S[i-1] * (1 - q[i])
}

tableau_a_remettre <- data.frame(
    cbind(t[sort(time_days[event == 1])], 
    n[sort(time_days[event == 1])], 
    d[sort(time_days[event == 1])], 
    q[sort(time_days[event == 1])],
    1 - q[sort(time_days[event == 1])],
    S[sort(time_days[event == 1])]))
names(tableau_a_remettre) <- c("t", "n", "d", "q", "1-q", "S")
print(tableau_a_remettre)

#3b 
library(ggplot2)

p <- ggplot(data.frame(t = t, q = q), aes(x = t, y = q)) +
  geom_line() +  labs(title = "Line Chart Example", x = "Time", y = "Value")
show(p)

#3d
debut <- 200
fin <- 260

p <- ggplot(data.frame(t = t[debut:fin], q = q[debut:fin]), aes(x = t, y = q)) +
  geom_line() +  labs(title = "Line Chart Example", x = "Time", y = "Value")
show(p)

epanechnikov <- function(u) {
    if (abs(u) <= 1) {
        return(0.75 * (1 - u^2))
    } else {
        return(0)
    }
}

b <- 5
D <- length(t[sort(time_days[event == 1])])
lisse <- rep(0, fin - debut)
for (i in debut:fin) {
    diff_seq <- (i - t[sort(time_days[event == 1])])/b
    epan <- rep(0, D)
    for (j in 1:D) {
        epan[j] <- epanechnikov(diff_seq[j])
    }
    lisse[i - debut + 1] <- (1/b)*sum(q[sort(time_days[event == 1])] * epan)
}

p <- ggplot(data.frame(t = t[debut:fin], lisse = lisse), aes(x = t, y = lisse)) +
  geom_line() +  labs(title = "Line Chart Example", x = "Time", y = "Value")
show(p)