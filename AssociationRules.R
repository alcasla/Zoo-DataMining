require(arules)       #Association rules tools
require(arulesViz)    #Association rules visualization

#Data reading
zooData <- read.delim("~/R/DataMining/Data/zoo.data", header = F, sep=",", as.is=TRUE);
names(zooData) <- c("name", "hair", "feathers", "eggs", "milk", "airborne", "aquatic", "predator", "toothed", "backbone", 
                    "breathes", "venomous", "fins", "legs", "tail", "domestic", "catsize", "type")

head(zooData)
dim(zooData)
summary(zooData)

####### Prepare data #########
zoo <- zooData[,-1]     #Remove name colum, it´s useless

#Function convert 2 levels logic data (first argument, yes or no) to char label specifing positive lavel only
booleanToChar <- function(data, posLabel){
  negLabel = paste("no", posLabel)
  ifelse(data==1, posLabel, negLabel)
}

zoo$hair <- booleanToChar(zoo$hair, "hair")
zoo$feathers <- booleanToChar(zoo$feathers, "feather")
zoo$eggs <- booleanToChar(zoo$eggs, "eggs")
zoo$milk <- booleanToChar(zoo$milk, "milk")
zoo$airborne <- booleanToChar(zoo$airborne, "airborne")
zoo$aquatic <- booleanToChar(zoo$aquatic, "aquatic")
zoo$predator <- booleanToChar(zoo$predator, "predator")
zoo$toothed <- booleanToChar(zoo$toothed, "toothed")
zoo$backbone <- booleanToChar(zoo$backbone, "backbone")
zoo$breathes <- booleanToChar(zoo$breathes, "breathe")
zoo$venomous <- booleanToChar(zoo$venomous, "venomous")
zoo$fins <- booleanToChar(zoo$fins, "fin")
zoo$tail <- booleanToChar(zoo$tail, "tail")
zoo$domestic <- booleanToChar(zoo$domestic, "domestic")
zoo$catsize <- booleanToChar(zoo$catsize, "cat")

zoo$legs <- ifelse(zoo$legs>0, "legs", "no legs")
#work with number of legs (as factor)
#zoo$legs <- factor(zoo$legs, levels=c(0,2,4,5,6,8))
#zoo$type <- factor(zoo$type, levels=1:7)

zoo <- as.data.frame(apply(zoo, 2, as.factor))
#zoo <- zoo[order(zoo$type),]   #order dataset by animal type (1,2,...,7)

#######  #########
#Build trasaction set
Zoo <- as(zoo, "transactions")
summary(Zoo)
image(Zoo)

#Pintamos items más frecuentes
itemFrequencyPlot(Zoo, support=0.0, cex.names=0.7, main="Frecuency Plot from Zoo")
abline(h=0.75, col="darkred")

#Apriori 
apriori <- apriori(Zoo, parameter=list(support=0.1, target="frequent", maxlen=6))  #More "target" types in APparameter help
apriori <- sort(apriori, by="support")        #Order itemset by support

inspect(head(apriori, n=10))          #Show top 10 support itemsets
apriori           #set of 268401 itemsets

#plot distribution from itemsets size (color respect the mean), and mean line
barplot(table(size(apriori)), xlab="itemset size", ylab="count", main="Itemsets size distribution", 
        col=ifelse(as.integer(names(table(size(apriori))))>=floor(mean(size(apriori))), "darkred", "darkgrey"))
legend("topleft", fill=c("darkgrey", "darkred", "green"), 
       c("< mean(itemsets size)", "> mean(iteamsets size)", "mean (iteamsets size)"))
abline(v=mean(size(apriori)), col="green", lty=1, lwd=2.5)

#mayor itemsets
maximalZoo <- apriori[is.maximal(apriori)]        #set of 29681 itemsets
inspect(head(sort(maximalZoo, by="support"), n=5))        #show top5 support itemsets

#closed itemsets
closedZoo <- apriori[is.closed(apriori)]        #set of 32328 itemsets
inspect(head(sort(closedZoo, by="support"), n=5))        #show top5 support itemsets

#draw frecuent, maximal, and closed itemsets
barplot <- barplot( c(frequent=length(apriori), closed=length(closedZoo),
           maximal=length(maximalZoo)), ylab="count", xlab="itemsets", main="Frecuent vs Maximal vs Closed number od itemsets",
         col=c("darkred", "darkgrey", "grey"))
text(x=barplot, y=c(1,2,3), labels=c(length(apriori), length(closedZoo), length(maximalZoo)), pos=3, col="white")
abline(h=length(closedZoo), col="darkred")
abline(h=length(maximalZoo), col="darkred")

#get rules through apriori algorithm 
rulesZoo <- apriori(Zoo, parameter=list(support=0.2, confidence=0.4, minlen=2, maxlen=6))
summary(rulesZoo)
inspect(head(rulesZoo))     #rules build lhs->rhs, show rules and measures of quality
quality(head(rulesZoo))     #only quality measures from rules

#order rules for study
rulesSorted <- sort(rulesZoo, by="confidence")
inspect(head(rulesSorted))

#extract subset according to a condition
#rulesSubset <- subset(rulesSorted, subset=lhs %in% "type=4" & lift>5.9 & support>0.12)
rulesSubset <- subset(rulesSorted, subset=lhs %in% "legs=no legs" & support>0.15)
inspect(head(rulesSubset))

#remove duplicated rules
subsetMatrix <- is.subset(rulesSubset, rulesSubset)
subsetMatrix[lower.tri(subsetMatrix, diag=T)] <- NA
redundant <- colSums(subsetMatrix, na.rm=T) >= 1
rulesPruned <- rulesSubset[!redundant]

#check reduction number of rules
rulesSubset
rulesPruned

#aditional measures
??interestMeasure
mInteres <- interestMeasure(rulesPruned, measure=c("certainty"), transactions=Zoo)
quality(rulesPruned) <- cbind(quality(rulesPruned), mInteres)     #Algunas medidas, dependiendo de las reglas, puede fallar
inspect(rulesPruned)

####### Visualization rules #########
#Plot 2D usual
plot(rulesSubset)
#Plot grafo
plot(rulesSubset[1:8], method="graph", control=list(type="items"))
#interactive
try: plot(rulesSubset, method="grouped", interactive=TRUE)
#plot as vectors
plot(rulesSubset[1:8], method="paracoord", control=list(reorder=TRUE))


######### STUDY #########
#legless animals
subset <- subset(rulesZoo, subset=rhs %in% "legs=no legs")    #get specific rules
### Type 4 animals
subset <- subset(rulesZoo, subset=(lhs %in% "type=4" | rhs %in% "type=4"))
subset <- subset(subset, subset=lhs %in% "venomous=venomous" | rhs %in% "venomous=venomous")
#feather animals 
subset <- subset(rulesZoo, subset=(lhs %in% "feathers=feather" | rhs %in% "feathers=feather"))
#feather animals no airborne or domestic
subset <- subset(rulesZoo, subset=lhs %in% "type=2" | rhs %in% "type=2")
subset <- subset(subset, subset=lhs %in% "airborne=no airborne" | rhs %in% "airborne=no airborne")
subset <- subset(subset, subset=lhs %in% "domestic=domestic" | rhs %in% "domestic=domestic")
#airbone animals no bird
subset <- subset(rulesZoo, subset=lhs %in% "airborne=airborne" | rhs %in% "airborne=airborne")
subset <- subset(subset, subset=!(lhs %in% "type=2" | rhs %in% "type=2"))
  #mammal either
subset <- subset(subset, subset=lhs %in% "milk=milk" | rhs %in% "milk=milk")
#type 1 animals, apart from bats
subset <- subset(rulesZoo, subset=lhs %in% "type=1" | rhs %in% "type=1")
subset <- subset(subset, subset=!(lhs %in% "airborne=airborne" | rhs %in% "airborne=airborne"))
subset <- subset[1:15000]
  #oviparous mammal
subset <- subset(subset, subset=lhs %in% "eggs=eggs" | rhs %in% "eggs=eggs")
#negated items NO TAIL
subset <- subset(rulesZoo, subset=lhs %in% "tail=no tail")


#calculate certainty to subset and show
quality(subset) <- cbind(quality(subset),       #add certainty to quality measures
                               certainty = interestMeasure(subset, measure=c("certainty"), transactions=Zoo))
subset <- sort(subset, by="certainty")  #certainty, support
inspect(head(subset, n=100))


#remove duplicate rules
subsetMatrix <- is.subset(subset, subset)
subsetMatrix[lower.tri(subsetMatrix, diag=T)] <- NA
redundant <- colSums(subsetMatrix, na.rm=T) >= 1
subsetPruned <- subset[!redundant]


#calculate certainty to subsetPruned and show
quality(subsetPruned) <- cbind(quality(subsetPruned),       #add certainty to quality measures
                          certainty = interestMeasure(subsetPruned, measure=c("certainty"), transactions=Zoo))
subsetPruned <- sort(subsetPruned, by="certainty")
inspect(head(subsetPruned, n=100))


#check percent of coinciding items - legless animals
length(which(zooData$type==4))/dim(zooData)[1]
paste(round(length(which(zooData$legs==0 & zooData$type==4 & zooData$fins==1))/dim(zooData)[1], digits=4),"%")

#visualize feather animals
plot(subsetPruned, method="graph", control=list(type="items"), main="Feather animals graph")

#airborne and not of type 2
plot(subsetPruned, method="graph", control=list(type="items"))


#negative items study about no tail
zooNoTail <- zoo[which(zoo$tail=="no tail"),]   #select i´m interested
zooNoTail$tail <- NULL      #delete column have unique value

ZooNoTail <- as(zooNoTail, "transactions")
rulesZooNoTail <- apriori(ZooNoTail, parameter=list(support=0.2, confidence=0.4, minlen=2, maxlen=6))

quality(rulesZooNoTail) <- cbind(quality(rulesZooNoTail),       #add certainty to quality measures
                               certainty = interestMeasure(rulesZooNoTail, measure=c("certainty"), transactions=ZooNoTail))
rulesZooNoTail <- sort(rulesZooNoTail, by="certainty")
inspect(head(rulesZooNoTail, n=100))


#group of rules
zooMammal <- zoo[which(zoo$type=="1"),]   #select mammal (type 1) only
zooMammal$type <- NULL

ZooMammal <- as(zooMammal, "transactions")
rulesZooMammal <- apriori(ZooMammal, parameter=list(support=0.1, confidence=0.2, minlen=2, maxlen=6))

quality(rulesZooMammal) <- cbind(quality(rulesZooMammal),       #add certainty to quality measures
                                 certainty = interestMeasure(rulesZooMammal, measure=c("certainty"), transactions=ZooMammal))
rulesZooMammal <- sort(rulesZooMammal, by="certainty")

#remove bat(unique airborne) and platypus(unique eggs)
subset <- subset(rulesZooMammal, subset=!(lhs %in% "airborne=airborne" | rhs %in% "airborne=airborne"))
subset <- subset(subset, subset=!(lhs %in% "eggs=egss" | rhs %in% "eggs=egss"))

inspect(head(rulesZooMammal, n=100))

#examine in depth
subsetDeep2 <- subset(subset, subset=(lhs %in% "hair=hair"))
subsetDeep2 <- subset(subset, subset=(lhs %in% "venomous=no venomous"))
subsetDeep2 <- subset(subset, subset=(lhs %in% "backbone=backbone"))
subsetDeep2 <- subset(subset, subset=(lhs %in% "milk=milk"))
subsetDeep2 <- sort(subsetDeep2, by="support")
