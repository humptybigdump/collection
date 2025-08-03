# Beobachtungen
x = c(84,64,76,56,82,51,64,84,72,72,74,88,72,93,78,64,80,80,64,90,82,80,78,72,72,78,52,78,68,80,70,84,68,80,64,64,79,84,84,80,57,63,72,90,64,72,78,66,64,84,72,84,84,76,68,82,63,88)

# Geordnete Stichprobe
sort(x)

# St?ngel-Blatt-Diagramm
stem(x,scale = 0.5)
?stem

# Histogramm
hist(x, breaks=seq(from=50,to=100, by=5), right=TRUE, main = paste("Histogramm fuer Pulszahlen Klasse 11"), xlab="Pulszahlen Klasse 11", ylab="", col = "gray") #Klassenbreite 5, absolute Werte
hist(x, breaks=seq(from=50,to=100, by=10), right=TRUE, main = paste("Histogramm fuer Pulszahlen Klasse 11"), xlab="Pulszahlen Klasse 11", ylab="", col = "gray") #Klassenbreite 10, absolute Werte
?hist

hist(x, freq=FALSE, breaks=seq(from=50,to=100, by=5), right=TRUE, main = paste("Histogramm fuer Pulszahlen Klasse 11"), xlab="Pulszahlen Klasse 11", ylab="", col = "gray") #Klassenbreite 5, relative Werte
hist(x, freq=FALSE, breaks=seq(from=50,to=100, by=10), right=TRUE, main = paste("Histogramm fuer Pulszahlen Klasse 11"), xlab="Pulszahlen Klasse 11", ylab="", col = "gray") #Klassenbreite 10, relative Werte

#Boxplot
boxplot(x, xlab="Pulszahlen Klasse 11", main = paste("Boxplot fuer Pulszahlen Klasse 11")) #Boxplot Standard
boxplot(x, xlab="Pulszahlen Klasse 11", main = paste("Boxplot fuer Pulszahlen Klasse 11"), range = 1.5) #Boxplot mit anpassbaren Antennen, 1.5-facher Quartilsabstand ist Standard
?boxplot

median(x) #Median
quantile(x, probs=0.75, type=2) #Oberes Quartil
quantile(x, probs=0.25, type=2) #Unteres Quartil
quantile(x, probs=0.75, type=2)-quantile(x, probs=0.25, type=2) #Quartilsabstand



#Alternative Beobachtungen
# Stichprobe 11b 
x = c(84,64,76,56,82,51,64,84,72,72,74,88,72,93,78,64,80,80,64,90,82,80,78,72,72,78,52,78,68)

# Stichprobe 11c 
x = c(80,70,84,68,80,64,64,79,84,84,80,57,63,72,90,64,72,78,66,64,84,72,84,84,76,68,82,63,88)

