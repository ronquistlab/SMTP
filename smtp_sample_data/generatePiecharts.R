# Script to generate pie charts for the taxonomic composition of the
# catch.

# Read in transformed statistics data for the first-tier groups
Order <- read.table( "StatSamples_Order_2019-08-22.csv", sep=";", header=TRUE )

# Read in transformed statistics data for the second-tier Brachycera groups
Brachycera <- read.table( "StatSamples_Brachycera_2019-08-22.csv", sep=";", header=TRUE )

# Read in transformed statistics data for the second-tier Nematocera groups
Nematocera <- read.table( "StatSamples_Nematocera_2019-08-22.csv", sep=";", header=TRUE )

# Read in transformed statistics data for the second-tier Hymenoptera groups
Hymenoptera <- read.table( "StatSamples_Hymenoptera_2019-08-22.csv", sep=";", header=TRUE )

# Generate piechart for order-level groups
Order$Order <- as.character( Order$Taxon )
for ( i in 1:length(Order$Specimens) )
{
    t <- Order$Taxon[i]
    if ( t == "Auchenorrhyncha" )
        Order$Order[i] <- "Homoptera"
    else if ( t == "Sternorrhyncha" )
        Order$Order[i] <- "Homoptera"
    else if ( t == "Brachycera (excl. Phoridae)" )
        Order$Order[i] <- "Diptera"
    else if ( t == "Phoridae" )
        Order$Order[i] <- "Diptera"
    else if ( t == "Nematocera" )
        Order$Order[i] <- "Diptera"
}
Order$Order <- factor( Order$Order )

D <- read.table("../misc_data/coll_event_data_2019-08-23.tsv")
E <- read.table("../misc_data/trap_data.tsv")
Order$Latitude  <- E$Latitude[ match( Order$TrapID, E$TrapID ) ]
Order$StartDate <- D$StartDate[ match( Order$CollID, D$EventID ) ]
Order$StartDate <- factor( as.character( Order$StartDate ) )
Order$Season <- rep( "Nonsummer", times=length(Order$Specimens) )
Order$Season[ grep( "-0[5678]-", Order$StartDate ) ] <- "Summer"
Order$Season <- factor( Order$Season )

pdf( "Fig_Y1.pdf" )
A <- aggregate( Order$Specimens, by=list(Order$Order), FUN=sum )
colnames(A) <- c("Order", "Total" )
ord <- order(A$Total, decreasing=TRUE)
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Order,1,4), main="" )
cat("Sample size n=", length(unique(Order$CollID)),"\n")
A$Fraction <- A$Total / sum(A$Total)
print(A)
dev.off()

pdf( "Fig_Y2.pdf" )
par(mfrow=c(2,2))
B <- Order[ Order$Season == "Summer", ]
A <- aggregate( B$Specimens, by=list(B$Order), FUN=sum )
colnames(A) <- c("Order", "Total" )
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Order,1,4), main="Summer" )
A$Fraction <- A$Total / sum(A$Total)
print(A)

B <- Order[ Order$Season != "Summer", ]
A <- aggregate( B$Specimens, by=list(B$Order), FUN=sum )
colnames(A) <- c("Order", "Total" )
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Order,1,4), main="Nonsummer" )
A$Fraction <- A$Total / sum(A$Total)
print(A)

B <- Order[ Order$Latitude < 60, ]
A <- aggregate( B$Specimens, by=list(B$Order), FUN=sum )
colnames(A) <- c("Order", "Total" )
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Order,1,4), main="South" )
A$Fraction <- A$Total / sum(A$Total)
print(A)

B <- Order[ Order$Latitude > 60, ]
A <- aggregate( B$Specimens, by=list(B$Order), FUN=sum )
colnames(A) <- c("Order", "Total" )
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Order,1,4), main="North" )
A$Fraction <- A$Total / sum(A$Total)
print(A)
dev.off()


# Generate piecharts for Diptera groups
Nematocera <- Nematocera[ Nematocera$CollID %in% Brachycera$CollID, ]
Diptera <- rbind( Nematocera, Brachycera )
Diptera$CollID <- factor( Diptera$CollID )
Phoridae <- Order[ Order$Taxon == "Phoridae", ]
for ( i in 1:length( Diptera$Specimens ) )
{
    if ( Diptera$Taxon[i] == "Phoridae" )
        Diptera$Specimens[i] <- Phoridae$Specimens[ match(Diptera$CollID[i], Phoridae$CollID) ]
}

Diptera$Group <- Diptera$Taxon

D <- read.table("../misc_data/coll_event_data_2019-08-23.tsv")
E <- read.table("../misc_data/trap_data.tsv")
Diptera$Latitude  <- E$Latitude[ match( Diptera$TrapID, E$TrapID ) ]
Diptera$StartDate <- D$StartDate[ match( Diptera$CollID, D$EventID ) ]
Diptera$StartDate <- factor( Diptera$StartDate )
Diptera$Season <- rep( "Nonsummer", times=length(Diptera$Group) )
Diptera$Season[ grep( "-0[5678]-", Diptera$StartDate ) ] <- "Summer"
Diptera$Season <- factor( Diptera$Season )

pdf( "Fig_Y3.pdf" )
A <- aggregate( Diptera$Specimens, by=list(Diptera$Group), FUN=sum )
colnames(A) <- c("Group", "Total" )
A <- A[ order(A$Total, decreasing=TRUE), ]
pie( A$Total, labels=substr(A$Group,1,4), main="" )
A$Fraction <- A$Total / sum(A$Total)
print(A)
dev.off()

pdf( "Fig_Y4.pdf" )
par( mfrow=c(2,2) )
B <- Diptera[ Diptera$Season == "Summer", ]
A <- aggregate( B$Specimens, by=list(B$Group), FUN=sum )
colnames(A) <- c("Group", "Total" )
ord <- order(A$Total, decreasing=TRUE)
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Group,1,4), main="Summer" )
cat("Sample size n=", length(unique(Diptera$CollID)),"\n")
A$Fraction <- A$Total / sum(A$Total)
print(A)

B <- Diptera[ Diptera$Season != "Summer", ]
A <- aggregate( B$Specimens, by=list(B$Group), FUN=sum )
colnames(A) <- c("Group", "Total" )
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Group,1,4), main="Nonsummer" )
A$Fraction <- A$Total / sum(A$Total)
print(A)

B <- Diptera[ Diptera$Latitude < 60, ]
A <- aggregate( B$Specimens, by=list(B$Group), FUN=sum )
colnames(A) <- c("Group", "Total" )
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Group,1,4), main="South" )
A$Fraction <- A$Total / sum(A$Total)
print(A)

B <- Diptera[ Diptera$Latitude > 60, ]
A <- aggregate( B$Specimens, by=list(B$Group), FUN=sum )
colnames(A) <- c("Group", "Total" )
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Group,1,4), main="North" )
A$Fraction <- A$Total / sum(A$Total)
print(A)
dev.off()


# Generate piecharts for Hymenoptera groups
Hymenoptera$Group <- as.character( Hymenoptera$Taxon )
for ( i in 1:length(Hymenoptera$Specimens) )
{
    t <- Hymenoptera$Taxon[i]
    if ( t == "Diapriidae: Belytinae" )
        Hymenoptera$Group[i] <- "Diaprioidea"
    else if ( t == "Diapriidae: Diapriinae" )
        Hymenoptera$Group[i] <- "Diaprioidea"
    else if ( t == "Ismaridae" )
        Hymenoptera$Group[i] <- "Diaprioidea"
    else if ( t == "Diapriidae" )
        Hymenoptera$Group[i] <- "Diaprioidea"
    else if ( t == "Cynipidae" )
        Hymenoptera$Group[i] <- "Cynipoidea"
    else if ( t == "Figitidae" )
        Hymenoptera$Group[i] <- "Cynipoidea"
    else if ( t == "Figitidae; Charipinae" )
        Hymenoptera$Group[i] <- "Cynipoidea"
    else if ( t == "Eumeninae" )
        Hymenoptera$Group[i] <- "Vespidae s. lat."
    else if ( t == "Vespidae" )
        Hymenoptera$Group[i] <- "Vespidae s. lat."
    else if ( t == "Mymariforma" )
        Hymenoptera$Group[i] <- "Mymaridae"
    else if ( t == "Platygastridae: Scelioninae" )
        Hymenoptera$Group[i] <- "Platygastroidea"
    else if ( t == "Platygastridae" )
        Hymenoptera$Group[i] <- "Platygastroidea"
    else if ( t == "Scelionidae" )
        Hymenoptera$Group[i] <- "Platygastroidea"
    else if ( t == "Symphyta: Övrigt" )
        Hymenoptera$Group[i] <- "Symphyta excl. Tenthredinidae"
}
Hymenoptera$Group <- factor( Hymenoptera$Group )

D <- read.table("../misc_data/coll_event_data_2019-08-23.tsv")
E <- read.table("../misc_data/trap_data.tsv")
Hymenoptera$Latitude  <- E$Latitude[ match( Hymenoptera$TrapID, E$TrapID ) ]
Hymenoptera$StartDate <- D$StartDate[ match( Hymenoptera$CollID, D$EventID ) ]
Hymenoptera$StartDate <- factor( Hymenoptera$StartDate )
Hymenoptera$Season <- rep( "Nonsummer", times=length(Hymenoptera$Group) )
Hymenoptera$Season[ grep( "-0[5678]-", Hymenoptera$StartDate ) ] <- "Summer"
Hymenoptera$Season <- factor( Hymenoptera$Season )

pdf( "Fig_Y5.pdf" )
A <- aggregate( Hymenoptera$Specimens, by=list(Hymenoptera$Group), FUN=sum )
colnames(A) <- c("Group", "Total" )
ord <- order(A$Total, decreasing=TRUE)
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Group,1,4), main="" )
cat("Sample size n=", length(unique(Hymenoptera$CollID)),"\n")
A$Fraction <- A$Total / sum(A$Total)
print(A)
dev.off()

pdf( "Fig_Y6.pdf" )
par( mfrow=c(2,2) )
B <- Hymenoptera[ Hymenoptera$Season == "Summer", ]
A <- aggregate( B$Specimens, by=list(B$Group), FUN=sum )
colnames(A) <- c("Group", "Total" )
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Group,1,4), main="Summer" )
A$Fraction <- A$Total / sum(A$Total)
print(A)

B <- Hymenoptera[ Hymenoptera$Season != "Summer", ]
A <- aggregate( B$Specimens, by=list(B$Group), FUN=sum )
colnames(A) <- c("Group", "Total" )
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Group,1,4), main="Nonsummer" )
A$Fraction <- A$Total / sum(A$Total)
print(A)

B <- Hymenoptera[ Hymenoptera$Latitude < 60, ]
A <- aggregate( B$Specimens, by=list(B$Group), FUN=sum )
colnames(A) <- c("Group", "Total" )
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Group,1,4), main="South" )
A$Fraction <- A$Total / sum(A$Total)
print(A)

B <- Hymenoptera[ Hymenoptera$Latitude > 60, ]
A <- aggregate( B$Specimens, by=list(B$Group), FUN=sum )
colnames(A) <- c("Group", "Total" )
A <- A[ ord, ]
pie( A$Total, labels=substr(A$Group,1,4), main="North" )
A$Fraction <- A$Total / sum(A$Total)
print(A)
dev.off()



