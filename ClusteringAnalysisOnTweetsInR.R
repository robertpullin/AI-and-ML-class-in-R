# Notes from a Data Science, AI, and ML Course  (Source: LearningTree)

# Load the twitteR and tm libraries
library(twitteR)
library(tm)

# We will not be downloading a livefeed from Twitter, but take some time to view the required commands to do so:
# load("twitteR_credentials")
# registerTwitterOAuth(twitCred)
# s <- searchTwitter('Search String Goes Here', cainfo="cacert.pem", n=500)
# df<-do.call("rbind", lapply(s, as.data.frame))
# write.csv(df, file = "DSTweets3.csv")

# Load a pre-downloaded data set of tweets into a data frame called tweets
tweets <- read.csv("DSTweets3.csv")

# Convert the tweets to a corpus
documents <- Corpus(VectorSource(tweets$text))

# Transform the data to lowercase
documents <- tm_map(documents,content_transformer(tolower))

# Remove punctuation
documents <- tm_map(documents,content_transformer(removePunctuation))

# Remove numbers
documents <- tm_map(documents,content_transformer(removeNumbers))

# Remove stop words, adding the words "via" and "amp" to the standard stop words
stopWords <- c(stopwords("english"),"via","amp")
documents <- tm_map(documents,removeWords,stopWords)

# View the first five tweets from your transformed corpus
inspect(documents[1:5])

# Create a function called removeURL, to remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*","",x)

# Remove the URLs from the corpus
documents <- tm_map(documents,content_transformer(removeURL))

# Convert the corpus into a term document matrix
termDocMat <- TermDocumentMatrix(documents,control = list(minWordLength=1))

# Remove sparse terms
termDocMat_sparseRemoved <- removeSparseTerms(termDocMat,sparse = 0.95)

# Convert to a matrix
mat <- as.matrix(termDocMat_sparseRemoved)

# Scale the terms and calculate the distances between terms with the dist() function
distanceMatrix <- dist(scale(mat))

# Cluster the tweets using hierarchical clustering and the ward agglomeration method
fit <- hclust(distanceMatrix,method = "ward.D2")

# Plot the dendrogram
plot(fit)

# Plot the dendrogram into four clusters
rect.hclust(fit,k=4)

# For K-Means clustering, transpose the matrix (i.e., convert it from a term document 
matrixTranspose = t(mat)


# Set a seed for replicable results
set.seed(123)

# Set the clustering value to 4
k <- 4

# Perform K-Means clustering on the document term matrix
kmeansResult <- kmeans(matrixTranspose,k)

# Print the top five words in every cluster to see what the clusters are about
for (i in 1:k) {
  cat(paste("cluster ",i,": ",sep=""))
  s <- sort(kmeansResult$centers[i,],decreasing = T)
  cat(names(s)[1:5],"\n")
  #print(tweets[which(kmeansResult$cluster==i), ]$text)
}


