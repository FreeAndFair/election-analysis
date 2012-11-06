

# Read in data
iowa = read.csv( "iowa.csv", as.is=TRUE )
head(iowa)

# Take the vote counts for each candidate from data.  Toss everything else.
votes = iowa[ c(3:11) ]
head(votes)

# compute total votes in each precinct
votes$tot = apply(votes, 1, sum )

# order precints from small to large
votes = votes[ order( votes$tot), ]
head(votes)
tail(votes)

# drop all precincts with missing values
sum( is.na( votes$tot ) )
votes[ is.na(votes$tot), ]
votes = subset( votes, !is.na( tot ) & tot > 0 )

# number of precints
n = nrow(votes)
n


plot( votes$tot, main="We see many small precincts and a few large ones", type="l")


# Calc percent for Romney
votes$per.Romney = votes$Romney/votes$tot

# Re-implement their method
votes$cum.Romney = cumsum(votes$Romney)
votes$cum.tot = cumsum(votes$tot )
votes$cum.per = votes$cum.Romney / votes$cum.tot

# a version with percent precincts counted on x-axis
plot( seq(0,1, length.out=nrow(votes)), votes$cum.per, type="l", xlab="percent precincts counted")

# a version with vote totals on x-axis
plot( votes$cum.tot, votes$cum.per, type="l", xlab="Total votes counted", ylab="Percent for Romney" )


#
# A view on why this happens:
#


# Cumulative sum based on percent precincts.  Only 20% of vote is in the smallest 50% of precincts.
# Only 50% in the bottom 80% of precincts
plot(  seq(0,1, length.out=nrow(votes)), votes$cum.tot/sum(votes$tot), type="l", xlab="percent precincts counted" )
abline( h=0.5 )
abline( v=0.5 )
quantile( votes$cum.tot, c(0.01,0.5,0.8,0.99,1) ) / sum(votes$tot)


# what the 20th percentile precinct looks like
votes[ round(0.2*n), ]


# making a scatterplot of precinct size and percent 
plot( jitter(per.Romney,factor=10) ~ jitter(tot,factor=10), data=votes, 
				xlab="Total Votes in Precincts", ylab="Percent for Romney", 
				main="Plot of all Precincts",
				pch=19, cex=0.2 )
lines( lowess(votes$tot, votes$per.Romney) )				
abline( h=sum(votes$Romney)/sum(votes$tot ) )
text( 400, 0.1, labels=cor(votes$per.Romney,votes$tot) )
abline( lm( per.Romney ~ tot, data=votes ) ) 




# why the strips are there.  Discrete number of votes in small precincts
plot( per.Romney ~ tot, xlim=c(0,25), data=votes, cex=0.2  )
plot( function(x) { 2/x } )
plot( function(x) { 2/x }, xlim=c(1,25), add=TRUE )
plot( Romney ~ tot, data=votes, cex=0.2, xlim=c()  )
