################
## Beispiel 1 ##
################

# Erstellung eines Dataframes
gummi = data.frame(dehnung=c(46,54,48,50,44,42,52),
                   weite=c(148,182,173,166,109,141,166))
gummi

# Ein Scatterplot (Streudiagramm) der y-Werte in Abhängigkeit von den x-Werten 
# wird folgendermaßen gezeichnet:
plot(gummi$dehnung, gummi$weite, pch=16)
abline( lm(gummi$weite ~ gummi$dehnung) )

