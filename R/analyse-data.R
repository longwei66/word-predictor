## DTM matrix
dtm <- DocumentTermMatrix(en)
dtm

inspect(dtm[1:2,1000:1005])

freq <- colSums(as.matrix(dtm))
length(freq)

ord <- order(freq,decreasing=TRUE)

freq[head(ord)]
freq[tail(ord)]

#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freq),freq,min.freq=70,colors=brewer.pal(6,"Dark2"))

options(mc.cores=1)
BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
txtTdmBi <- TermDocumentMatrix(en, control = list(tokenize = BigramTokenizer))




################
tdm <- TermDocumentMatrix(en, control=list(tokenize = NGramTokenizer))

findFreqTerms(tdm, lowfreq = 10)
