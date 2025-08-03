#TUT 4 Übungen und Co.

install.packages("sjmisc")
install.packages("AER")
install.packages("hsm")

#benötigte packages laden
library("hsm")
library("AER")
library("sjmisc")
library("dplyr")




#Datensatz laden
data("TeachingRatings")


#Jeden Prof nur einmal betrachten
Teachers <- TeachingRatings %>% distinct(prof, .keep_all = TRUE)









#2a) Minorities unter Professor:innen
frq(Teachers$minority)
table(Teachers$minority)

#2b) Profs unter 38
Teachers %>% count(age <  38)
#oder
Teachers %>% filter(age < 38) %>% count()
#oder
count(Teachers, age < 38)

#3 age-Variable der Professor:innen

#a) häufigstes Alter
frq(Teachers$age)
#oder
which.max(table(Teachers$age))

#b) Durchschnittsalter
mean(Teachers$age)

#c) Median
median(Teachers$age)

#d) Varianz
var(Teachers$age)

#e) SD des Alters
sd(Teachers$age)

#4) z-Standardisierung für Evaluation
TeachingRatings %>% mutate(z_eval = scale(eval)) %>% select(eval, z_eval)

#5) Chi² Test
#b) Kontingenztabelle
table(GueW$NfC_1, GueW$NfC_4)

#c) Chi-Quadrat Test
chisq.test(GueW$NfC_4, GueW$NfC_1)

Crame
