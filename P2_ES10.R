# Problem 2: Artificial data for Bayes classifier

# Generate data from a Gaussian mixture model - use this information for the Bayes-classifier
set.seed(100)
n1<-200
n2<-200
x11<-cbind(rnorm(n1,-1,0.3),rnorm(n1,1,0.3))
x12<-cbind(rnorm(n1,1,0.2),rnorm(n1,0.5,0.2))
z1<-rbinom(n1,1,0.98)
x1<-z1*x11+(1-z1)*x12 
x21<-cbind(rnorm(n2,1,0.3),rnorm(n2,-1,0.3))
x22<-cbind(rnorm(n2,-1,0.2),rnorm(n2,-0.5,0.2))
z2<-rbinom(n2,1,0.98)
x2<-z2*x21+(1-z2)*x22
x<-rbind(x1,x2)
y<-c(rep(0,n1),rep(1,n2))

# Bayes classification  
# Construct a grid (equal distance between two neighbouring points) covering the minimal and maximal value of each component.  


# Evaluate the pdf of both mixtures at all gridpoints. Use the information you have from the generation of the data.
d1<-     # pdf of X in case of y = 0 at x_1,x_2 (gridpoints)
d2<      # pdf of X in case of y = 1 at x_1,x_2 (gridpoints)
bayes<   # Bayes classifier (Which conditional probability is bigger?) Should be length(x1_seq) x length(x2_seq) matrix,
         #entry should be "TRUE" if d1(x_1,x_2) > d2(x_1,x_2), otherwise "FALSE"

# Plot results (no need for further implementation if you are happy with the layout)
contour(x=x1_seq,y=x2_seq,z=bayes,levels=c(0,1),nlevels=1,drawlabels=FALSE) 
points(x[1:n1,],col="chartreuse4", pch=17, cex=0.9,ylim=c(-1.5,1.5),xlim=c(-2,2)) # points that should be classified as 0
points(x[(n1+1):(n1+n2),],col="darkorchid4",pch=18) # points that should be classified as 1

