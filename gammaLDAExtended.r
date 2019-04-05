gammaLDAExtended <- function(dataset1, dataset2) {	
	library(tm)
	library(topicmodels)

	Encoding(dataset1$'prepReview')  <- "UTF-8"

	corp <- Corpus(VectorSource(c(dataset1$'prepReview') ))
	dtm <- DocumentTermMatrix(corp)
	#convert rownames to filenames
	rownames(dtm) <- dataset1$'InternalId'

	#Number of topics
	k <- dataset2[[1]]
	
	#Set parameters for Gibbs sampling
	burnin <- 4000
	iter <- 2000
	thin <- 500
	seed <-list(2003,5,63,100001,765)
	nstart <- 5
	best <- TRUE

	#Run LDA using Gibbs sampling
	ldaOut <-LDA(dtm,k, method="Gibbs",  control=list(nstart=nstart, 
	seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

	ldaOutTerms <- as.data.frame(terms(ldaOut,10))
	gammaDF <- as.data.frame(ldaOut@gamma)
	names(gammaDF) <- names (ldaOutTerms)

	# Now for each doc, find just the top-ranked topic   
	toptopics <- data.frame(cbind(document = rownames(dtm), 
	  topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))

	toptopics$topic <- sapply(toptopics$topic,function (x) paste(unlist(x), collapse = ';'))

	toptopics$document <- sapply(toptopics$document,function (x) unlist(x)[1])
	
	#bind gamma distributions to documents
	gammaDF$document <- rownames(dtm)
	
	return (list(ldaOutTerms = ldaOutTerms, toptopics = toptopics, gamma = gammaDF))
}