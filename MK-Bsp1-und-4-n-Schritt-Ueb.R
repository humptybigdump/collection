# install.packages("expm") # benoetigt fuer Matrixpotenz
library(expm, warn.conflicts=FALSE, quietly=TRUE)

# Bsp. B1 (Wettermodell)
a=0.2; b=0.6  
# Uebergangsmatrix
P = matrix( c(1-a,a,b,1-b), nrow=2, byrow=TRUE) 
P  
# 2-Schritt-Uebergangsmatrix
P%^%2  # oder P%*%P, aber nicht P*P
# n-Schritt-Uebergangsmatrizen
for (n in 1:5) {print(P%^%n); cat("\n")}

# Verteilung von X_n
# Start bei Sonne zur Zeit 0, also X_0 ~ (1,0)
nu = matrix(c(1,0), ncol=2) 
for (n in 1:5) {print( nu %*% (P%^%n) ); cat("\n")}

# Start mit X_0 ~ (0.5,0.5), also Sonne bzw. Regen mit gleicher W'
nu = matrix(c(1/2,1/2), ncol=2) 
for (n in 1:5) {print( nu %*% (P%^%n) );  cat("\n")}

#################################################################

# Bsp. B2 (Ehrenfest-Modell): 
P = matrix( c(0,1,0,0,0,0, 1/5,0,4/5,0,0,0, 0,2/5,0,3/5,0,0, 
              0,0,3/5,0,2/5,0, 0,0,0,4/5,0,1/5, 0,0,0,0,1,0), nrow=6, byrow=TRUE) 
P
# n-Schritt-Uebergangsmatrix
P%^%2  
P%^%10
P%^%40
P%^%41

# Verteilung von X_n
# Start mit 3 Teilchen in linker Kammer 
nu = matrix(c(0,0,0,1,0,0), ncol=6) 
for (n in 1:3) {print( nu %*% (P%^%n) ); cat("\n")}
nu %*% (P%^%40) 
nu %*% (P%^%41) 
