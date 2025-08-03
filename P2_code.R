install.packages('datasets')
library(datasets)
data(UCBAdmissions)

# (a)
# 3 different possibilities
dataset <- list(A = t(UCBAdmissions[,,'A']), B = t(UCBAdmissions[,,'B']),
                C = t(UCBAdmissions[,,'C']), D = t(UCBAdmissions[,,'D']),
                E = t(UCBAdmissions[,,'E']), F = t(UCBAdmissions[,,'F']))
###
dataset <- list()
for(i in 1:dim(UCBAdmissions)[3]){
  dataset[[i]] <- t(UCBAdmissions[,,i])
}
names(dataset) <- dimnames(UCBAdmissions)$Dept
###
dataset <- lapply(1:dim(UCBAdmissions)[3], function(x) t(UCBAdmissions[,,x]))
names(dataset) <- dimnames(UCBAdmissions)$Dept


# (b)
# 3 different possibilities
gend_on_adm <- matrix(data = rep(0, 4), nrow = 2)
for(i in 1:2){
  for(j in 1:2){
    for(k in 1:6){
      gend_on_adm[i, j] <- gend_on_adm[i, j] + dataset[[k]][i, j]
    }
  }
}
###
gend_on_adm <- dataset[[1]] + dataset[[2]] +
  dataset[[3]] + dataset[[4]] +
  dataset[[5]] + dataset[[6]]
###
gend_on_adm <- Reduce('+', dataset)


# (c)
fisher.test(gend_on_adm)

# (d)
# Either print the entire test output every time...
for(i in 1:length(dataset)){
  print(fisher.test(dataset[[i]]))
}
# ...or just save the p-values and the ossd ratios in two vectors.
p.values <- sapply(dataset, function(x){fisher.test(x)$p.value})
odds.ratios <- sapply(dataset, function(x){fisher.test(x)$estimate})


# (e)
# This is easiest to do using the initial data set since we not want to
# aggregate along different dimensions than before:
apply(UCBAdmissions, c('Gender', 'Dept'), sum)
chisq.test(apply(UCBAdmissions, c('Gender', 'Dept'), sum))
###
apply(UCBAdmissions, c('Dept', 'Admit'), sum)
chisq.test(apply(UCBAdmissions, c('Dept', 'Admit'), sum))


# (f)
# Open new plot window from 0 to 1 in both x- and y-axis with nothing in it
plot(c(0, 1), c(0, 1), type = 'n', axes = FALSE,
     xlab = "", ylab = "")
# Write the 3 variable names in the shape of a triangle
text(x = c(0.15, 0.5, 0.85), y = c(0, 1, 0),
     labels = c("Gender", "Department choice", "Admission"))
# Draw corresponding arrows between them
arrows(x0 = c(0.15, 0.55), y0 = c(0.1, 0.9),
       x1 = c(0.45, 0.85), y1 = c(0.9, 0.1))


# (g)
# Get the aggregated 2x2-matrix of just departments A and E
sum_a_e <- Reduce('+', dataset[c('A', 'E')])

# Get a new plot window that is large enaough with just the axes
plot(c(0, max(sum_a_e)), c(0, max(sum_a_e)), type = 'n',
     xlab = "# rejections",
     ylab = "# admissions")
# Draw the admitted-rejected-ratio-arrows for department A
arrows(x0 = c(0, 0), y0 = c(0, 0),
       x1 = c(dataset$A['Male', 'Rejected'],
              dataset$A['Female', 'Rejected']),
       y1 = c(dataset$A['Male', 'Admitted'],
              dataset$A['Female', 'Admitted']),
       length = 0.1, col = c('blue', 'red'), lwd = 2, lty = 2)
# Draw the same arrow shifted such that it ends up giving us a parallelogram
# to visualize the aggregation
arrows(x0 = c(dataset$A['Male', 'Rejected'],
              dataset$A['Female', 'Rejected']),
       y0 = c(dataset$A['Male', 'Admitted'],
              dataset$A['Female', 'Admitted']),
       x1 = c(sum_a_e['Male', 'Rejected'],
              sum_a_e['Female', 'Rejected']),
       y1 = c(sum_a_e['Male', 'Admitted'],
              sum_a_e['Female', 'Admitted']),
       length = 0.1, col = c('blue', 'red'), lwd = 2, lty = 3)
# Draw the admitted-rejected-ratio-arrows for department A
arrows(x0 = c(0, 0), y0 = c(0, 0),
       x1 = c(dataset$E['Male', 'Rejected'],
              dataset$E['Female', 'Rejected']),
       y1 = c(dataset$E['Male', 'Admitted'],
              dataset$E['Female', 'Admitted']),
       length = 0.1, col = c('blue', 'red'), lwd = 2, lty = 3)
# Draw the same arrow shifted such that it ends up giving us a parallelogram
# to visualize the aggregation
arrows(x0 = c(dataset$E['Male', 'Rejected'],
              dataset$E['Female', 'Rejected']),
       y0 = c(dataset$E['Male', 'Admitted'],
              dataset$E['Female', 'Admitted']),
       x1 = c(sum_a_e['Male', 'Rejected'],
              sum_a_e['Female', 'Rejected']),
       y1 = c(sum_a_e['Male', 'Admitted'],
              sum_a_e['Female', 'Admitted']),
       length = 0.1, col = c('blue', 'red'), lwd = 2, lty = 2)
# Draw the two arrows that visualize the admitted-rejected-ratio aggregated
# over the two departments
arrows(x0 = c(0, 0), y0 = c(0, 0),
       x1 = c(sum_a_e['Male', 'Rejected'],
              sum_a_e['Female', 'Rejected']),
       y1 = c(sum_a_e['Male', 'Admitted'],
              sum_a_e['Female', 'Admitted']),
       length = 0.1, col = c('blue', 'red'), lwd = 2)


# Same plot with the line of success frequencies drawn in:
plot(c(0, sum(sum_a_e['Male', ])),
     c(0, sum(sum_a_e['Male', ])), type = 'n',
     xlab = "# rejections",
     ylab = "# admissions")
arrows(x0 = c(0, 0), y0 = c(0, 0),
       x1 = c(dataset$A['Male', 'Rejected'],
              dataset$A['Female', 'Rejected']),
       y1 = c(dataset$A['Male', 'Admitted'],
              dataset$A['Female', 'Admitted']),
       length = 0.1, col = c('blue', 'red'), lwd = 2, lty = 2)
segments(x0 = c(0, 0), y0 = c(0, 0),
         x1 = c(dataset$A['Male', 'Rejected']*sum(sum_a_e['Male', ])/sum(dataset$A['Male', ]),
                dataset$A['Female', 'Rejected']*sum(sum_a_e['Male', ])/sum(dataset$A['Female', ])),
         y1 = c(dataset$A['Male', 'Admitted']*sum(sum_a_e['Male', ])/sum(dataset$A['Male', ]),
                dataset$A['Female', 'Admitted']*sum(sum_a_e['Male', ])/sum(dataset$A['Female', ])),
         col = c('blue', 'red'), lwd = 2, lty = 2)
arrows(x0 = c(dataset$A['Male', 'Rejected'],
              dataset$A['Female', 'Rejected']),
       y0 = c(dataset$A['Male', 'Admitted'],
              dataset$A['Female', 'Admitted']),
       x1 = c(sum_a_e['Male', 'Rejected'],
              sum_a_e['Female', 'Rejected']),
       y1 = c(sum_a_e['Male', 'Admitted'],
              sum_a_e['Female', 'Admitted']),
       length = 0.1, col = c('blue', 'red'), lwd = 2, lty = 3)
arrows(x0 = c(0, 0), y0 = c(0, 0),
       x1 = c(dataset$E['Male', 'Rejected'],
              dataset$E['Female', 'Rejected']),
       y1 = c(dataset$E['Male', 'Admitted'],
              dataset$E['Female', 'Admitted']),
       length = 0.1, col = c('blue', 'red'), lwd = 2, lty = 3)
segments(x0 = c(0, 0), y0 = c(0, 0),
       x1 = c(dataset$E['Male', 'Rejected']*sum(sum_a_e['Male', ])/sum(dataset$E['Male', ]),
              dataset$E['Female', 'Rejected']*sum(sum_a_e['Male', ])/sum(dataset$E['Female', ])),
       y1 = c(dataset$E['Male', 'Admitted']*sum(sum_a_e['Male', ])/sum(dataset$E['Male', ]),
              dataset$E['Female', 'Admitted']*sum(sum_a_e['Male', ])/sum(dataset$E['Female', ])),
       col = c('blue', 'red'), lwd = 2, lty = 3)
arrows(x0 = c(dataset$E['Male', 'Rejected'],
              dataset$E['Female', 'Rejected']),
       y0 = c(dataset$E['Male', 'Admitted'],
              dataset$E['Female', 'Admitted']),
       x1 = c(sum_a_e['Male', 'Rejected'],
              sum_a_e['Female', 'Rejected']),
       y1 = c(sum_a_e['Male', 'Admitted'],
              sum_a_e['Female', 'Admitted']),
       length = 0.1, col = c('blue', 'red'), lwd = 2, lty = 2)
arrows(x0 = c(0, 0), y0 = c(0, 0),
       x1 = c(sum_a_e['Male', 'Rejected'],
              sum_a_e['Female', 'Rejected']),
       y1 = c(sum_a_e['Male', 'Admitted'],
              sum_a_e['Female', 'Admitted']),
       length = 0.1, col = c('blue', 'red'), lwd = 2)
segments(x0 = c(0, 0), y0 = c(0, 0),
       x1 = c(sum_a_e['Male', 'Rejected'],
              sum_a_e['Female', 'Rejected']*sum(sum_a_e['Male', ])/sum(sum_a_e['Female', ])),
       y1 = c(sum_a_e['Male', 'Admitted'],
              sum_a_e['Female', 'Admitted']*sum(sum_a_e['Male', ])/sum(sum_a_e['Female', ])),
       col = c('blue', 'red'), lwd = 2)

segments(x0 = 0, y0 = sum(sum_a_e['Male', ]), x1 = sum(sum_a_e['Male', ]), y1 = 0)

text(x = dataset$A['Male', 'Rejected']*sum(sum_a_e['Male', ])/sum(dataset$A['Male', ])+95,
     y = dataset$A['Male', 'Admitted']*sum(sum_a_e['Male', ])/sum(dataset$A['Male', ]),
     labels = paste("A, Male:", 100*round(dataset$A['Male', 'Admitted']/sum(dataset$A['Male', ]), 3), "%"), col = "blue")
text(x = dataset$A['Female', 'Rejected']*sum(sum_a_e['Male', ])/sum(dataset$A['Female', ])+115,
     y = dataset$A['Female', 'Admitted']*sum(sum_a_e['Male', ])/sum(dataset$A['Female', ]),
     labels = paste("A, Female:", 100*round(dataset$A['Female', 'Admitted']/sum(dataset$A['Female', ]), 3), "%"), col = "red")
text(x = dataset$E['Male', 'Rejected']*sum(sum_a_e['Male', ])/sum(dataset$E['Male', ])+95,
     y = dataset$E['Male', 'Admitted']*sum(sum_a_e['Male', ])/sum(dataset$E['Male', ]),
     labels = paste("E, Male:", 100*round(dataset$E['Male', 'Admitted']/sum(dataset$E['Male', ]), 3), "%"), col = "blue")
text(x = dataset$E['Female', 'Rejected']*sum(sum_a_e['Male', ])/sum(dataset$E['Female', ])+115,
     y = dataset$E['Female', 'Admitted']*sum(sum_a_e['Male', ])/sum(dataset$E['Female', ]),
     labels = paste("E, Female:", 100*round(dataset$E['Female', 'Admitted']/sum(dataset$E['Female', ]), 3), "%"), col = "red")
text(x = sum_a_e['Male', 'Rejected']*sum(sum_a_e['Male', ])/sum(sum_a_e['Male', ])+130,
     y = sum_a_e['Male', 'Admitted']*sum(sum_a_e['Male', ])/sum(sum_a_e['Male', ]),
     labels = paste("overall, Male:", 100*round(sum_a_e['Male', 'Admitted']/sum(sum_a_e['Male', ]), 3), "%"), col = "blue")
text(x = sum_a_e['Female', 'Rejected']*sum(sum_a_e['Male', ])/sum(sum_a_e['Female', ])+150,
     y = sum_a_e['Female', 'Admitted']*sum(sum_a_e['Male', ])/sum(sum_a_e['Female', ]),
     labels = paste("overall, Female:", 100*round(sum_a_e['Female', 'Admitted']/sum(sum_a_e['Female', ]), 3), "%"), col = "red")



# Analogous plot for the potential outcomes framework instead of the
# observed outcomes framework:
po_dataset <- list()
po_dataset$A <- dataset$A*sum(dataset$A)/rowSums(dataset$A)
po_dataset$E <- dataset$E*sum(dataset$E)/rowSums(dataset$E)
po_dataset$sum <- po_dataset$A * sum(po_dataset$A)/(sum(po_dataset$A) + sum(po_dataset$E)) +
  po_dataset$E * sum(po_dataset$E)/(sum(po_dataset$A) + sum(po_dataset$E))
po_dataset$sum <- po_dataset$sum * sum(po_dataset$A)/sum(po_dataset$sum)

plot(c(0, sum(po_dataset$A['Female', ])),
     c(0, sum(po_dataset$A['Female', ])), type = 'n',
     xlab = "# rejections",
     ylab = "# admissions")
arrows(x0 = c(0, 0), y0 = c(0, 0),
       x1 = c(po_dataset$A['Male', 'Rejected'],
              po_dataset$A['Female', 'Rejected']),
       y1 = c(po_dataset$A['Male', 'Admitted'],
              po_dataset$A['Female', 'Admitted']),
       length = 0.1, col = c('blue', 'red'), lwd = 2, lty = 2)
# segments(x0 = c(0, 0), y0 = c(0, 0),
#          x1 = c(po_dataset$A['Male', 'Rejected']*sum(po_dataset$A['Female', ])/sum(po_dataset$A['Male', ]),
#                 po_dataset$A['Female', 'Rejected']*sum(po_dataset$A['Female', ])/sum(po_dataset$A['Female', ])),
#          y1 = c(po_dataset$A['Male', 'Admitted']*sum(po_dataset$A['Female', ])/sum(po_dataset$A['Male', ]),
#                 po_dataset$A['Female', 'Admitted']*sum(po_dataset$A['Female', ])/sum(po_dataset$A['Female', ])),
#          col = c('blue', 'red'), lwd = 2, lty = 2)
# arrows(x0 = c(po_dataset$A['Male', 'Rejected'],
#               po_dataset$A['Female', 'Rejected']),
#        y0 = c(po_dataset$A['Male', 'Admitted'],
#               po_dataset$A['Female', 'Admitted']),
#        x1 = c(po_dataset$sum['Male', 'Rejected'],
#               po_dataset$sum['Female', 'Rejected']),
#        y1 = c(po_dataset$sum['Male', 'Admitted'],
#               po_dataset$sum['Female', 'Admitted']),
#        length = 0.1, col = c('blue', 'red'), lwd = 2, lty = 3)
arrows(x0 = c(0, 0), y0 = c(0, 0),
       x1 = c(po_dataset$E['Male', 'Rejected'],
              po_dataset$E['Female', 'Rejected']),
       y1 = c(po_dataset$E['Male', 'Admitted'],
              po_dataset$E['Female', 'Admitted']),
       length = 0.1, col = c('blue', 'red'), lwd = 2, lty = 3)
segments(x0 = c(0, 0), y0 = c(0, 0),
         x1 = c(po_dataset$E['Male', 'Rejected']*sum(po_dataset$A['Female', ])/sum(po_dataset$E['Male', ]),
                po_dataset$E['Female', 'Rejected']*sum(po_dataset$A['Female', ])/sum(po_dataset$E['Female', ])),
         y1 = c(po_dataset$E['Male', 'Admitted']*sum(po_dataset$A['Female', ])/sum(po_dataset$E['Male', ]),
                po_dataset$E['Female', 'Admitted']*sum(po_dataset$A['Female', ])/sum(po_dataset$E['Female', ])),
         col = c('blue', 'red'), lwd = 2, lty = 3)
# arrows(x0 = c(po_dataset$E['Male', 'Rejected'],
#               po_dataset$E['Female', 'Rejected']),
#        y0 = c(po_dataset$E['Male', 'Admitted'],
#               po_dataset$E['Female', 'Admitted']),
#        x1 = c(po_dataset$sum['Male', 'Rejected'],
#               po_dataset$sum['Female', 'Rejected']),
#        y1 = c(po_dataset$sum['Male', 'Admitted'],
#               po_dataset$sum['Female', 'Admitted']),
#        length = 0.1, col = c('blue', 'red'), lwd = 2, lty = 2)
# arrows(x0 = c(0, 0), y0 = c(0, 0),
#        x1 = c(po_dataset$sum['Male', 'Rejected'],
#               po_dataset$sum['Female', 'Rejected']),
#        y1 = c(po_dataset$sum['Male', 'Admitted'],
#               po_dataset$sum['Female', 'Admitted']),
#        length = 0.1, col = c('blue', 'red'), lwd = 2)
# segments(x0 = c(0, 0), y0 = c(0, 0),
#          x1 = c(po_dataset$sum['Male', 'Rejected']*sqrt(sum(po_dataset$A['Female', ]^2))/sqrt(sum(po_dataset$sum['Male', ]^2)),
#                 po_dataset$sum['Female', 'Rejected']*sqrt(sum(po_dataset$A['Female', ]^2))/sqrt(sum(po_dataset$sum['Female', ]^2))),
#          y1 = c(po_dataset$sum['Male', 'Admitted']*sqrt(sum(po_dataset$A['Female', ]^2))/sqrt(sum(po_dataset$sum['Male', ]^2)),
#                 po_dataset$sum['Female', 'Admitted']*sqrt(sum(po_dataset$A['Female', ]^2))/sqrt(sum(po_dataset$sum['Female', ]^2))),
#          col = c('blue', 'red'), lwd = 2)

# radius = sqrt(sum(po_dataset$A['Female', ]^2))
# theta = seq(0, pi/2, length = 1001) # angles for drawing points around the circle
# lines(x = radius * cos(theta), y = radius * sin(theta))
segments(x0 = 0, y0 = sum(po_dataset$A['Female', ]), x1 = sum(po_dataset$A['Female', ]), y1 = 0)

segments(x0 = c(po_dataset$A['Male', 'Rejected'],
                po_dataset$A['Female', 'Rejected']),
         y0 = c(po_dataset$A['Male', 'Admitted'],
                po_dataset$A['Female', 'Admitted']),
         x1 = c(po_dataset$sum['Male', 'Rejected'],
                po_dataset$sum['Female', 'Rejected']),
         y1 = c(po_dataset$sum['Male', 'Admitted'],
                po_dataset$sum['Female', 'Admitted']),
         col = c('blue', 'red'), lwd = 2, lty = 3)
segments(x0 = c(po_dataset$E['Male', 'Rejected']*sum(po_dataset$A['Female', ])/sum(po_dataset$E['Male', ]),
                po_dataset$E['Female', 'Rejected']*sum(po_dataset$A['Female', ])/sum(po_dataset$E['Female', ])),
         y0 = c(po_dataset$E['Male', 'Admitted']*sum(po_dataset$A['Female', ])/sum(po_dataset$E['Male', ]),
                po_dataset$E['Female', 'Admitted']*sum(po_dataset$A['Female', ])/sum(po_dataset$E['Female', ])),
         x1 = c(po_dataset$sum['Male', 'Rejected'],
                po_dataset$sum['Female', 'Rejected']),
         y1 = c(po_dataset$sum['Male', 'Admitted'],
                po_dataset$sum['Female', 'Admitted']),
         col = c('blue', 'red'), lwd = 2, lty = 2)
segments(x0 = c(po_dataset$sum['Male', 'Rejected']-3,
                po_dataset$sum['Female', 'Rejected'])-8,
         y0 = c(po_dataset$sum['Male', 'Admitted']-3,
                po_dataset$sum['Female', 'Admitted'])-8,
         x1 = c(po_dataset$sum['Male', 'Rejected']+3,
                po_dataset$sum['Female', 'Rejected'])+8,
         y1 = c(po_dataset$sum['Male', 'Admitted']+3,
                po_dataset$sum['Female', 'Admitted'])+8,
         col = c('blue', 'red'), lwd = 2)

text(x = po_dataset$A['Male', 'Rejected']*sum(po_dataset$A['Female', ])/sum(po_dataset$A['Male', ])+95,
     y = po_dataset$A['Male', 'Admitted']*sum(po_dataset$A['Female', ])/sum(po_dataset$A['Male', ]),
     labels = paste("A, Male:", 100*round(po_dataset$A['Male', 'Admitted']/sum(po_dataset$A['Male', ]), 3), "%"), col = "blue")
text(x = po_dataset$A['Female', 'Rejected']*sum(po_dataset$A['Female', ])/sum(po_dataset$A['Female', ])+115,
     y = po_dataset$A['Female', 'Admitted']*sum(po_dataset$A['Female', ])/sum(po_dataset$A['Female', ]),
     labels = paste("A, Female:", 100*round(po_dataset$A['Female', 'Admitted']/sum(po_dataset$A['Female', ]), 3), "%"), col = "red")
text(x = po_dataset$E['Male', 'Rejected']*sum(po_dataset$A['Female', ])/sum(po_dataset$E['Male', ])+95,
     y = po_dataset$E['Male', 'Admitted']*sum(po_dataset$A['Female', ])/sum(po_dataset$E['Male', ]),
     labels = paste("E, Male:", 100*round(po_dataset$E['Male', 'Admitted']/sum(po_dataset$E['Male', ]), 3), "%"), col = "blue")
text(x = po_dataset$E['Female', 'Rejected']*sum(po_dataset$A['Female', ])/sum(po_dataset$E['Female', ])+115,
     y = po_dataset$E['Female', 'Admitted']*sum(po_dataset$A['Female', ])/sum(po_dataset$E['Female', ]),
     labels = paste("E, Female:", 100*round(po_dataset$E['Female', 'Admitted']/sum(po_dataset$E['Female', ]), 3), "%"), col = "red")
text(x = po_dataset$sum['Male', 'Rejected']*sum(po_dataset$A['Female', ])/sum(po_dataset$sum['Male', ])+130,
     y = po_dataset$sum['Male', 'Admitted']*sum(po_dataset$A['Female', ])/sum(po_dataset$sum['Male', ]),
     labels = paste("overall, Male:", 100*round(po_dataset$sum['Male', 'Admitted']/sum(po_dataset$sum['Male', ]), 3), "%"), col = "blue")
text(x = po_dataset$sum['Female', 'Rejected']*sum(po_dataset$A['Female', ])/sum(po_dataset$sum['Female', ])+150,
     y = po_dataset$sum['Female', 'Admitted']*sum(po_dataset$A['Female', ])/sum(po_dataset$sum['Female', ]),
     labels = paste("overall, Female:", 100*round(po_dataset$sum['Female', 'Admitted']/sum(po_dataset$sum['Female', ]), 3), "%"), col = "red")
