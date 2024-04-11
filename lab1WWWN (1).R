#Zad.2

M <- 100
k <- 5
n <- 3

#P(A1|A2 i A3)
# sprawdzamy prawodopodbieństwo przegranej
pA1 = 95/100
pA2 = 94/99
pA3 = 93/98

wygrana <- 1-(pA1*pA2*pA3)
wygrana

#Zad.3
p_poz <- 0.6
p_neg <- 0.98
p_chor <- 0.001

#a
chory <- 0
chory
  
  library(lattice)


#analiza dla testu
Warunek = c(rep("bez warunku",2), rep("tak", 2), rep("nie",2))
Test = rep(c("pozytywny", "negatywny"), 3)
dane <- data.frame(Warunek = Warunek, Test = Test,
                   Prawdopodobienstwo = c(
                     0.6*0.001+0.02*0.999, 
                     1-(0.6*0.001+0.02*0.999), 
                     0.6, 0.4, 0.02, 0.98))

dane



barchart(Test ~ Prawdopodobienstwo | Warunek, data = dane,
         scales = list(alternating = 1, tck = c(1, 0)),
         strip = strip.custom(factor.levels =
                                c(expression(Pr(Test)),
                                  expression(Pr({Test} * " | " * {Choroba == NIE})),
                                  expression(Pr({Test} * " | " * {Choroba == TAK})))),
)




Warunek2 = c(rep("bez warunku",2), rep("pozytywny", 2), rep("negatywny",2))

choroba = rep(c("tak", "nie"), 3)

dane <- data.frame(Warunek = Warunek2, choroba = choroba,
                   
                   Prawdopodobienstwo = c(
                     0.001,1-0.001, (0.6*0.001)/(0.6*0.001+0.02*0.999),
                     1 - ((0.6*0.001)/(0.6*0.001+0.02*0.999)),
                     (0.4*0.001)/(0.4*0.001 + 0.98* 0.999),
                     1 - ((0.4*0.001)/(0.4*0.001 + 0.98* 0.999))
                   ))

#Prawdopodobienstwo ze ktos jest chory pod warunkiem wyniku testu
dane



barchart(choroba ~ Prawdopodobienstwo | Warunek2, data = dane,
         
         scales = list(alternating = 1, tck = c(1, 0)),
         
         strip = strip.custom(factor.levels =
                                
                                c(expression(Pr(Choroba)),
                                  
                                  expression(Pr({Choroba} * " | " * {Test == Pozytywny})),
                                  
                                  expression(Pr({Choroba} * " | " * {Test == Negatywny})))),
         
)


library(bnlearn)

dag <- empty.graph(nodes=c("smoker","covid","hospital"))

dag <- set.arc(dag,from="smoker", to ="hospital")
dag <- set.arc(dag,from="covid", to ="hospital")

plot(dag)

modelstring(dag)

nodes(dag)

arcs(dag)

smoker.lv <- c("true","false")
covid.lv <- c("true","false")
hospital.lv <- c("true","false")


smoker.prob <- array(c(0.25,0.75),dim=2,
                     dimnames = list(smoker=smoker.lv))
smoker.prob

covid.prob <- array(c(0.1,0.9),dim=2,
                    dimnames = list(covid=covid.lv))
covid.prob

hospital.prob <- array(c(0.9,0.1,0.1,0.9,0.9,0.1,0.01,0.99),
                       dim=c(2,2,2),
                       dimnames = list(hospital=hospital.lv, 
                                       covid=covid.lv,
                                       smoker=smoker.lv))
hospital.prob

cpt <- list(hospital=hospital.prob,covid=covid.prob,smoker=smoker.prob)
cpt

bn <- custom.fit(dag,cpt)
bn

library(lattice)
bn.fit.barchart(bn$hospital, main="Hospital")



# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(c("graph", "RBGL", "Rgraphviz"))
# a

graphviz.plot(dag,layout="dot", shape = "rectangle", 
              highlight = list(nodes = "hospital", 
                               col = "tomato", fill = "orange"))

graphviz.chart(bn, type = "barprob", grid = TRUE, 
               bar.col = "darkgreen", strip.bg = "lightskyblue")

# install.packages("gRain", dependencies=TRUE)

library(gRain)

junction <-compile(as.grain(bn))

querygrain(junction, nodes = "hospital")$hospital

querygrain(junction, nodes = c("hospital","covid","smoker"), type="joint")

warunek <- setEvidence(junction,nodes=c("covid"),states=c("true"))

querygrain(warunek, nodes = c("smoker","hospital"),type="joint")

querygrain(warunek, nodes = c("smoker","hospital"),type="marginal")




################################ LAB3 ################################
