library(mosaicData)
library(ggplot2)

desired <- with(SAT, frac >= 4 & frac <= 20)
subFrame <- subset(SAT,subset=desired)
selected  <- ifelse(desired,"selected","other")
SAT$selected <- factor(selected)
otherColour <- "blue"
selectColour <- "red"
myColours <- c(otherColour,selectColour)
ggplot(SAT,aes(x=salary,y=sat)) + geom_point(aes(colour=selected,size=selected,fill=selected)) +
  geom_smooth(method="lm",se=FALSE,colour=otherColour) +
  scale_fill_manual(values=myColours) +
  scale_colour_manual(values=myColours) +
  scale_size_manual(values=c(2,3)) +
  geom_smooth(data=subFrame,method="lm",se=FALSE,colour=selectColour)


currentNum <- 60
percentage <- 10
prop <- percentage/100
distances <- with(SAT,abs(currentNum-frac))
sorted <- SAT[order(distances),]
lastPick <- floor(nrow(sorted)*prop)
subFrame <- sorted[1:lastPick,]
selected  <- c(rep("selected",lastPick),rep("other",nrow(sorted)-lastPick))
sorted$selected <- factor(selected)
otherColour <- "blue"
selectColour <- "red"
myColours <- c(otherColour,selectColour)
ggplot(sorted,aes(x=salary,y=sat)) + geom_point(aes(colour=selected,size=selected,fill=selected)) +
  geom_smooth(method="lm",se=FALSE,colour=otherColour) +
  scale_fill_manual(values=myColours) +
  scale_colour_manual(values=myColours) +
  scale_size_manual(values=c(2,3)) +
  geom_smooth(data=subFrame,method="lm",se=FALSE,colour=selectColour)
