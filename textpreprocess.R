getwd()
path <- 'F:/Study/SAS-Statistical Business Analytics/HW3'
setwd(path)
library('tm')
cname <- file.path(path)
length(dir(cname))
dir(cname)

docs <- Corpus(DirSource(path))
docs
class(docs)
class(docs[[1]]) #picking 1st document
summary(docs)

inspect(docs[1])
writeLines(as.character(docs[[1]])) #writing 1st document

getTransformations() #functions transformation

docs_nonum <- tm_map(docs, removeNumbers)
writeLines(as.character(docs_nonum[[2]])) #writing 2nd document

docs_nopun <- tm_map(docs_nonum, removePunctuation)
writeLines(as.character(docs_nopun[[2]])) #writing 2nd document
 
#remove certain words
docs_remove <- tm_map(docs_nopun, removeWords, c('Lake'))
writeLines(as.character(docs_remove[[2]])) #writing 2nd document

#remove stopwords
docs_stopwrd <- tm_map(docs_remove, removeWords, stopwords('english'))
writeLines(as.character(docs_stopwrd[[2]])) #writing 2nd document
