# Script to generate summaries of statistics samples and inventory
# samples.
#

# Read in transformed statistics data for the first-tier groups
Order <- read.table( "StatSamples_Order_2019-08-22.csv", sep=";", header=TRUE )

# Read in the quantitative data from the inventory
F <- read.table( "../quantitative_data/quant-combined-data.tsv" )

# Aggregate totals for stat samples
Stat <- aggregate( Order$Specimens, by=list(Order$CollID), FUN=sum )

colnames(Stat) <- c( "EventID", "Total" )
Stat$CollID <- factor( Stat$EventID )

# Get additional data into Stat for the stat samples
E <- read.table("../misc_data/coll_event_data_2019-08-23.tsv")
Stat$Trapdays  <- E$Sampling.period..days.[ match( Stat$EventID, E$EventID ) ]
Stat$StartDate <- E$StartDate             [ match( Stat$EventID, E$EventID ) ]
Stat$TrapID    <- E$TrapID                [ match( Stat$EventID, E$EventID ) ]
Stat$Season <- rep("Nonsummer",length(Stat$Total))
Stat$Season[grep("-0[5678]",Stat$StartDate)] <- "Summer"
Stat$Season <- factor( Stat$Season )

G <- read.table("../misc_data/trap_data.tsv")
Stat$Latitude  <- G$Latitude              [ match( Stat$TrapID, G$TrapID ) ]

# Now collect relevant data for the entire catch into Sample
Sample <- E
Sample$Season <- rep("Nonsummer",length(Sample$EventID))
Sample$Season[grep("-0[5678]",Sample$StartDate)] <- "Summer"
Sample$Season <- factor( Sample$Season )
Sample$Latitude <- G$Latitude[ match( Sample$TrapID, G$TrapID ) ]
Sample$Counted <- rep("Uncounted", length(Sample$EventID) )
Sample$Counted[ match( Stat$EventID, Sample$EventID ) ] <- "Counted"
Sample$Counted <- factor( Sample$Counted )

# Test difference in fraction of summer samples
# Chi-square test
tbl = table(Sample$Season, Sample$Counted)
print(tbl)
print(chisq.test(tbl))
print( "Fraction of summer samples in total catch")
print( length(Sample$Season[Sample$Season=="Summer"]) / length(Sample$Season))
print( "Fraction of summer samples in counted samples")
print( length(Stat$Season[Stat$Season=="Summer"]) / length(Stat$Season))

# Test difference in latitude
print(t.test(Sample$Latitude[Sample$Counted=="Counted"],Sample$Latitude[Sample$Counted!="Counted"]))

# Test difference in trapdays
D <- Sample[ Sample$Sampling.period..days. > 0, ]
print(t.test(D$Sampling.period..days.[D$Counted=="Counted"],D$Sampling.period..days.[D$Counted!="Counted"]))

# Test explanatory variables
fit <- lm( Stat$Total~Stat$Trapdays+Stat$Latitude+Stat$Season )
print( summary( fit ) )

fit <- lm( Stat$Total/Stat$Trapdays~Stat$Latitude+Stat$Season )
print( summary( fit ) )

fit <- lm( log(Stat$Total)~Stat$Trapdays+Stat$Latitude+Stat$Season )
print( summary( fit ) )

fit <- lm( log(Stat$Total/Stat$Trapdays)~Stat$Latitude+Stat$Season )
print( summary( fit ) )

print( summary( Stat$Total ) )
print( summary( log(Stat$Total) ) )

pdf("Fig_X1.pdf")
hist( Stat$Total, breaks=8, main="", ylab="Frequency", xlab="No. specimens" )
dev.off()

pdf("Fig_X2A.pdf")
hist( log(Stat$Total), breaks=7, main="", ylab="Frequency", xlab="Log(no. specimens)" )
dev.off()
pdf("Fig_X2B.pdf")
qqnorm( log(Stat$Total), main="" )
dev.off()

pdf("Fig_X3A.pdf")
plot( Stat$Total~Stat$Trapdays, main="", ylab="No. specimens", xlab="Trap days" )
dev.off()
pdf("Fig_X3B.pdf")
Summer <- Stat[ Stat$Season=="Summer", ]
Winter <- Stat[ Stat$Season!="Summer", ]
plot( Total$Total~Total$Latitude, col="green", main="", ylab="No. specimens", xlab="Latitude" )
points( Winter$Total~Winter$Latitude, col="blue" )
dev.off()

