#siec z dieta
sport <-read.csv(file.choose())
str(sport)
View(sport)
sport2<- sport[,-1]
View(sport2)
str(sport2)
library(bnlearn)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("graph", "RBGL", "Rgraphviz"))
a
install.packages("gRain", dependencies=TRUE)

siec <- hc(sport2)
str(sport2)
#zmieniamy na czynniki
sport2$A  <- as.factor(sport2$A)
sport2$B  <- as.factor(sport2$B)
sport2$C  <- as.factor(sport2$C)
sport2$D <- as.factor(sport2$D)
sport2$E  <- as.factor(sport2$E)
str(sport2)
View(sport2)
siechc <- hc(sport2)
graphviz.plot(siechc)


siec <- pc.stable(sport2)
graphviz.plot(siec)
#proba 0 (zgodnie z algorytmem hc)
siec0 <- set.arc(siec, "A", "B")
siec0 <- set.arc(siec0, "B", "E")
graphviz.plot(siec0)
score(siec0,data=sport2,type="bic")
#proba 1
siec1 <- set.arc(siec, "E", "B")
siec1 <- set.arc(siec1, "B", "A")
graphviz.plot(siec1)
score(siec1,data=sport2,type="bic")
#proba 2
siec2 <- set.arc(siec, "B", "E")
siec2 <- set.arc(siec2, "B", "A")
graphviz.plot(siec2)
score(siec2,data=sport2,type="bic")

#proba 3 - dodanie v-struktury obniza wartosc sieci
siec3 <- set.arc(siec, "E", "B")
siec3 <- set.arc(siec3, "A", "B")
graphviz.plot(siec3)
score(siec3,data=sport2,type="bic")

siechc <- hc(sport2)
graphviz.plot(siechc)
zmianakierunku <- set.arc(siechc, "D", "C")
graphviz.plot(zmianakierunku)

score(zmianakierunku,data=sport2,type="bic")

sieclista <- pc.stable(sport2, whitelist = matrix(c("E","D"),
                                                  ncol = 2, byrow = TRUE))
graphviz.plot(sieclista)           
sieclista <- set.arc(sieclista, "A", "B")
sieclista <- set.arc(sieclista, "B", "E")
graphviz.plot(sieclista)
score(sieclista,data=sport2,type="bic")
#jest gorzej niz na poczatku 

siec <- pc.stable(sport2)
graphviz.plot(siec)
siec1 <- set.arc(siec, "E", "B")
siec1 <- set.arc(siec1, "B", "A")
graphviz.plot(siec1)
score(siec1,data=sport2,type="bic")


ci.test("B","D",c("A"),test="x2", data=sport2)

ci.test("B","D",c("A"),test="x2", data=sport2)

ci.test("C","D",test="x2", data=sport2)
#male p-value - maly blad, czyli odrzucamy h0
#przyjmujemy h1: sa zalezne
#sprawdzamy sile wszystkich lukow
arc.strength(siec1,data=sport2,criterion = "x2")

dsep(siechc,x="A",y="E")
dsep(siechc,x="A",y="E",z="B")
dsep(siechc,x="A",y="C")
dsep(siechc,x="A",y="C",z="D")
#estymacja parametrow 
#zadanie - wyzanczyc rozklad zmiennej e z definicji

rozklade<- matrix(ncol=3, nrow=1)
rozklade[1,1]<- sum(sport2$E == "a")/nrow(sport2)
rozklade[1,2]<- sum(sport2$E == "b")/nrow(sport2)
rozklade[1,3]<- sum(sport2$E == "c")/nrow(sport2)
rozklade
ci.test("B","E",test="x2", data=sport2)
dsep(siechc,x="B",y="E")
rozkladb<- matrix(ncol=3, nrow=3)
rozkladb[1,1]<- sum(sport2$E == "a" & sport$B == "a")/sum(sport2$E == "a")
rozkladb[2,1]<- sum(sport2$E == "a" & sport$B == "b")/sum(sport2$E == "a")
rozkladb[3,1]<- sum(sport2$E == "a" & sport$B == "c")/sum(sport2$E == "a")
rozkladb[1,2]<- sum(sport2$E == "b" & sport$B == "a")/sum(sport2$E == "b")
rozkladb[2,2]<- sum(sport2$E == "b" & sport$B == "b")/sum(sport2$E == "b")
rozkladb[3,2]<- sum(sport2$E == "b" & sport$B == "c")/sum(sport2$E == "b")
rozkladb[1,3]<- sum(sport2$E == "c" & sport$B == "a")/sum(sport2$E == "c")
rozkladb[2,3]<- sum(sport2$E == "c" & sport$B == "b")/sum(sport2$E == "c")
rozkladb[3,3]<- sum(sport2$E == "c" & sport$B == "c")/sum(sport2$E == "c")
rozkladb
bn <- bn.fit(siec1, data=sport2)
library(lattice)
bn.fit.barchart(bn$B)
graphviz.chart(bn)

#zadanie - dyskretyzacja zmiennych 
mieszane <- read.csv(file.choose())
View(mieszane)
mieszane <- mieszane[,-1]
View(mieszane)
str(mieszane)
mieszane$A <- as.factor(mieszane$A)
mieszane$B <- as.factor(mieszane$B)
mieszane$C <- as.factor(mieszane$C)
mieszane$F <- as.factor(mieszane$F)
histogram(mieszane$D)

histogram(mieszane$E)
str(mieszane)
mieszane1 <-discretize(mieszane, 
                       method = "interval", 
                       breaks = 4)
View(mieszane1)
str(mieszane1)
mieszane2 <-discretize(mieszane, 
                       method = "interval", 
                       breaks = c(2,3,4,4,2,2,3,2))
View(mieszane2)
str(mieszane2)
abc<-hc(mieszane1)
graphviz.plot(abc)
score(abc, data=mieszane1, type="bic")
abc<-hc(mieszane2)
graphviz.plot(abc)
score(abc, data=mieszane2, type="bic")