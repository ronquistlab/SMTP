# Script to estimate total number of specimens in the inventory catch.
#
# The input data are samples for which all specimens of each taxon
# have been counted (in Excel file 'StatSamples_2019-08-22' and files
# derived therefrom), and the abundance data for the analysis groups.
#
# We use two methods. First, we simply compute the average number of
# specimens per counted sample and multiply with the total number of
# samples.
#
# We then use groups for which we have a fair amount of abundance data, 
# and that occur in a high proportion of samples. Specifically, we use
# Phoridae, Coleoptera, Trichoptera, Dolichopodidae and Drosophilidae.
#
# We try to predict the total number of specimens in the reference
# samples from the number of specimens of each of these groups. It turns
# out that a loglin model with zero intercept has the best fit and predictive
# power. An intuitive explanation is that large number of specimens of a
# particular group needs to be taken with some skepticism. It could mean
# that the sample contains a large total number of specimens, but it could
# also be due to an unusual abundance of the particular group we
# are analyzing. The loglin model gives a reasonble fit as indicated by
# the adjusted R-squared values, although there is a fair degree of variance,
# as one might expect.
#
# The results are summarized in the file 'result_summary.xlsx'.

# Read in transformed statistics data for the first-tier groups
Order <- read.table( "StatSamples_Order_2019-08-22.csv", sep=";", header=TRUE )

# Read in transformed statistics data for the second-tier Brachycera groups
Brachycera <- read.table( "StatSamples_Brachycera_2019-08-22.csv", sep=";", header=TRUE )

# Read in the quantitative data
F <- read.table( "../quantitative_data/quant-combined-data.tsv" )

# Aggregate totals from order counts
Total <- aggregate( Order$Specimens, by=list(Order$CollID), FUN=sum )

colnames(Total) <- c( "CollID", "Total" )
Total$CollID <- factor( Total$CollID )

# Get additional data into Total
E <- read.table("../misc_data/coll_event_data_2019-08-23.tsv")
Total$Trapdays  <- E$Sampling.period..days.[ match( Total$CollID, E$EventID ) ]
Total$StartDate <- E$StartDate             [ match( Total$CollID, E$EventID ) ]
Total$TrapID    <- E$TrapID                [ match( Total$CollID, E$EventID ) ]
Total$Season <- rep("Nonsummer",length(Total$Total))
Total$Season[grep("-0[5678]",Total$StartDate)] <- "Summer"
Total$Season <- factor( Total$Season )

G <- read.table("../misc_data/trap_data.tsv")
Total$Latitude  <- G$Latitude              [ match( Total$TrapID, G$TrapID ) ]


# Define estimates function
estimates <- function( D, taxon, sampleFraction )
{
    cat( "Estimates for taxon ", taxon, "\n" )

    # Get the determined data of the taxon
    A <- F[ F$AnalysisTaxon == taxon, ]
    A <- aggregate( A$Total, by=list(A$EventID), FUN=sum )
    colnames( A ) <- c( "CollID", "Specimens" )
    A$Trapdays  <- E$Sampling.period..days.[ match( A$CollID, E$EventID ) ]
    A$StartDate <- E$StartDate             [ match( A$CollID, E$EventID ) ]
    A$TrapID    <- E$TrapID                [ match( A$CollID, E$EventID ) ]
    A$Season <- rep("Nonsummer",length(A$CollID))
    A$Season[grep("-0[5678]",A$StartDate)] <- "Summer"
    A$Season <- factor( A$Season )
    A$Latitude  <- G$Latitude[ match( A$TrapID, G$TrapID ) ]

    # Get the statistic sample data
    B <- D[ D$Taxon == taxon, ]
    B <- merge( B, Total )
    B$Fraction <- B$Specimens / B$Total

    # Print some basic statistics
    cat( "Determined SMTP samples: ", length( A$CollID ), "\n" )
    cat( "Estimated fraction of SMTP samples: ", sampleFraction, "\n" )
    cat( "Summary of SMTP samples (no. specimens)\n" )
    print( summary( A$Specimens ) )
    cat( "Standard deviation of SMTP samples (no. specimens)\n" )
    print( sd( A$Specimens ) )

    cat( "Statistics samples: ", sum( B$Specimens > 0 ), "\n" )
    cat( "Fraction of statistics samples: ", sum( B$Specimens > 0 ) / length( B$Specimens ), "\n" )
    cat( "Summary of non-zero statistics samples\n" )
    print( summary( B$Specimens[ B$Specimens > 0 ] ) )
    cat( "Standard deviation of non-zero statistics samples\n" )
    print( sd( B$Specimens[ B$Specimens > 0 ] ) )
    cat( "\n" )

    # See if the samples are typical
    B1 <- B[ B$Specimens > 0, ]

    cat( "Wilcoxon test (no. specimens)\n" )
    print( wilcox.test( A$Specimens, B1$Specimens ) )

    cat( "T test (no. trapdays)\n" )
    print( t.test( A$Trapdays, B1$Trapdays ) )

    cat( "T test (latitude)\n" )
    print( t.test( A$Latitude, B1$Latitude ) )

    cat( "Chi-square test (fraction summer samples)\n" )
    x <- c( as.character(A$Season), as.character(B1$Season) )
    y <- c( rep( "Determined", times=length( A$Season ) ), rep( "Statistic", times=length( B1$Season ) ) )
    tbl <- table( x, y )
    print( tbl )
    print( chisq.test( tbl ) )

    # Plot lin-lin, log-lin prediction of total size
    x <- B$Specimens[ B$Specimens > 0 ]
    y <- B$Total[ B$Specimens > 0 ]
    fileName <- paste( "Fig_", taxon, "linlin.pdf", sep="" )
    pdf( fileName )
    plot( x, y, main=taxon )
    dev.off()
    fileName <- paste( "Fig_", taxon, "loglin.pdf", sep="" )
    pdf( fileName )
    plot( log(x+1), y, main=taxon )
    dev.off()

    # Fit a linlin model
    fit <- lm( y~x+0 )
    print( summary(fit) )
    k1 <- coefficients( fit ) [1]

    # Fit a loglin model
    fit <- lm( y~log(x + 1) + 0 )
    print( summary(fit) )
    k2 <- coefficients( fit ) [1]

    # Use the group to estimate the total SMTP size

    # Estimated catch based on this group
    cat( "Estimated catch of this group (using mean of processed samples and sample fraction) (millions)\n" )
    estimatedCatch <- mean( A$Specimens ) * 1919 * sampleFraction / 1000000
    print( estimatedCatch )
    cat( "Estimated total catch from this using mean fraction of the group in all statistics samples (millions)\n" )
    x <- estimatedCatch / mean( B$Fraction )
    print( x )
    cat( "\n" )
    cat( "Estimated total catch from this using mean fraction of the group in non-zero statistics\n")
    cat( "samples and estimated SMTP sample fraction (millions)\n" )
    x <- estimatedCatch / mean( B$Fraction[ B$Specimens > 0 ] ) / sampleFraction
    print( x )
    cat( "\n" )

    # Estimate catches; we use the fact that sum( k*A$Specimens ) = k*mean(A$Specimens) * n,
    # where n is the number of processed samples. This needs to be extrapolated to the total
    # sample, which is 1919, by multiplying by the factor (1919/n). Thus, the overall expression
    # simplifies to k*mean(A$Specimens) * n * (1919/n) = k*mean(A$Specimens) * 1919
    cat( "Estimated catch using linlin model\n" )
    x <- k1 * mean( A$Specimens ) * 1919 / 1000000
    print( x )
    cat( "\n" )
    cat( "Estimated catch using loglin model\n" )
    x <- k2 * mean( log( A$Specimens + 1 ) ) * 1919 / 1000000
    print( x )
    cat( "\n" )
}

# Use total number of specimens for statistics samples to compute total
cat( "Summary of stats samples\n" )
print( summary( Total$Total ) )

cat( "Estimate based on mean number of specimens in stats samples\n" )
x <- mean( Total$Total ) * 1919 / 1000000
y <- ( sd( Total$Total ) / sqrt( length( Total$Total ) - 1 ) ) * 1919 / 1000000
cat( "Estimate (million):", x, "\n" )
cat( "Sd (from standard error of the mean):", y, "\n\n" )

cat( "Estimate based on fitting a lognormal distribution to the stats samples\n" )
cat( "and drawing 1919 values from the estimated distribution. To capture\n" )
cat( "uncertainty, we draw the mean  and the standard deviation on the log scale\n" )
cat( "(meanlog and sdlog) of each of the lognormal distributions used in the\n" )
cat( "the simulations from a normal distribution that utilizes the standard error\n" )
cat( "of the estimates of the fitted distribution to capture uncertainty about the\n" )
cat( "exact values of those parameters.\n" )
library(MASS)
params <- fitdistr( Total$Total, "lognormal" )
meanlogs <- rnorm( 10000, mean=params$estimate[1], sd=params$sd[1] )
sdlogs   <- rnorm( 10000, mean=params$estimate[2], sd=params$sd[2] )
x <- numeric(10000)
for ( i in 1:10000 )
{
    x[i] <- sum( rlnorm( 1919, meanlog=meanlogs[i], sdlog=sdlogs[i] ) )
}
cat( "Mean estimate (millions): ", mean(x) / 1000000, "\n" )
cat( "Sd of estimate (millions): ", sd(x) / 1000000, "\n\n" )

# Use Phoridae from Order
estimates( Order, "Phoridae", 0.97 )

# Use Coleoptera from Order
estimates( Order, "Coleoptera", 0.94 )

# Use Trichoptera from Order
estimates( Order, "Trichoptera", 0.62 )

# Use Dolichopodidae from Brachycera
estimates( Brachycera, "Dolichopodidae", 0.77 )

# Use Drosophilidae from Brachycera
estimates( Brachycera, "Drosophilidae", 0.77 )


