library(plyr)
library(ggplot2)


# Clear Environment
rm(list = ls())


##### Task 1 #####

#Read data
p003 <- read.csv2("PATH/p003.csv", na.strings = "")


#Task 1a)
p003$Proband <- "p003"


#Task 1b)
flag <- (p003$Anmerkungen == "Fragliche Fixation") %in% T
p003 <- p003[!flag, ]

flag <- (p003$blinks_both == "blink") %in% T
p003 <- p003[!flag, ]

p003 <- transform(p003, fixations_both = gsub("fixation fixation", "fixation", fixations_both))


#Task 1c)
flag <- with(p003, !is.na(Interactions.Right.Hand)|
                   !is.na(Rotations.Right.Hand)|
                   !is.na(Interactions.Left.Hand)|
                   !is.na(Rotations.Left.Hand)|
                   !is.na(Marker))
p003$fixations_both[flag] <- NA



#Task 1d)
starttime1 <- with(p003, Begin.Time[(Marker == "Start Presentation Matrix 2")%in% T])
starttime2 <- with(p003, Begin.Time[(Marker == "Start Presentation Matrix 3")%in% T])
p003 <- transform(p003, Task = aaply(Begin.Time, 1, function(time){
  if(time < starttime2){1}
  else{2}
}))



#Task 1e)
p003 <- transform(p003,
                  Begin.Time = aaply(cbind(Begin.Time, Task), 1, function(entry){
                    if(entry[2] == 1){entry[1]-starttime1}
                    else{entry[1]-starttime2}}),
                  End.Time = aaply(cbind(End.Time, Task), 1, function(entry){
                    if(entry[2] == 1){entry[1]-starttime1}
                    else{entry[1]-starttime2}}))



##### Task 2 #####

#Task 2a)

#load datasets
p005 <- read.csv2("PATH/p005.csv", sep = ";", na.strings = "")
p007 <- read.csv2("PATH/p007.csv", sep = ";", na.strings = "")
p009 <- read.csv2("PATH/p009.csv", sep = ";", na.strings = "")
p013 <- read.csv2("PATH/p013.csv", sep = ";", na.strings = "")

#combine datasets
df.all <- rbind(p003, p005, p007, p009, p013)



#Task 2b) 

df.allTask2b <- transform(df.all, Type = ifelse((fixations_both == "fixation") %in% T,
                                                ifelse(is.na(Product.IDs),
                                                       "Fixation Out Of AOI",
                                                       "Fixation On AOI"),
                                                ifelse(is.na(Marker),
                                                        "Interaction",
                                                        "Marker")))



#Task 2c) 

df.dwells <- ddply(df.allTask2b, .(Proband, Task), function(x){
  
  id <- x$Product.IDs[-nrow(x)]
  next.id <- x$Product.IDs[-1]
  f.samedwell <- (c(F, id == next.id) %in% T)
  
  new.x <- df.allTask2b[F,]
  next.index <- 0
  for(i in 1:nrow(x)){
    if(!f.samedwell[i]){
      next.index <- next.index + 1
      new.x[next.index, ] <- x[i, ]
    } else {
      new.x[next.index, "End.Time"] <- x[i, "End.Time"]
    }
  }
  new.x <- transform(new.x, Duration = End.Time - Begin.Time)
  new.x
})

#Rename entries of column "Types"
df.dwells <- transform(df.dwells, Type = gsub("Fixation On AOI", "Dwell On AOI", Type))
df.dwells <- transform(df.dwells, Type = gsub("Fixation Out Of AOI", "Dwell Out Of AOI", Type))

df.dwells <- df.dwells[,-c(4,5,12)]


# save as csv to start Task 3 without having done the first two tasks

write.csv2(df.dwells, file ="df.dwells_Shortcut.csv", na="")

df.dwellsShortCut <- read.csv2("PATH/df.dwells_Shortcut.csv", sep = ";", na.strings = "")



##### Task 3 #####

#Task 3 a-c)
df.final <- ddply(df.dwells, .(Proband, Task),function(x){     # or shortcut df.dwellsShortCut instead of df.dwells
  f.onAoi <- x$Type == "Dwell On AOI"
  n_dwells <- sum(f.onAoi)
  n_products <- length(unique(x[f.onAoi, "Product.IDs"])) #Don't count NA
  duration <- max(x$End.Time)
  
  data.frame(n_dwells = n_dwells, n_products = n_products, duration = duration)
})
df.final$Task <- as.factor(df.final$Task)


#Task 3 a-c) plots

ggplot(data=df.final, aes(x=Proband, y=n_dwells, fill=Task)) +
  geom_bar(stat="identity", position=position_dodge())

ggplot(data=df.final, aes(x=Proband, y=n_products, fill=Task)) +
  geom_bar(stat="identity", position=position_dodge())

ggplot(data=df.final, aes(x=Proband, y=duration, fill=Task)) +
  geom_bar(stat="identity", position=position_dodge())


#Task 3 d)

# Boxplot
fill <- "#4271AE"
line <- "#1F3552"

ggplot(df.final, aes(x=Task, y=duration)) +
  geom_boxplot(fill = fill, colour = line)



