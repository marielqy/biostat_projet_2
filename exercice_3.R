set.seed(1234)

donnee <- read.delim("Chapters\\biostat_projet_2\\datasurv.txt", header = TRUE)
#donnee <- read.delim("datasurv.txt", header = TRUE)

attach(donnee)
print(donnee)