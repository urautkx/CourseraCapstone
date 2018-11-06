
require(tm)
require(RWeka)
require(data.table)
require(RMySQL)
require(stringi)
#options(java.parameters = "-Xmx1024m")


##Define some utility functions:
##-----------------------------------------------------------------


##This function takes the raw text as character string, cleans it and retunrs it
##This function uses Corpora and clean up functions associated with Corpora

cleanCorpus<-function(corpus1) {
    
    ##remove non ascii characters
    
    corpus1 <- tm_map(corpus1, content_transformer(function(x) iconv(x, "Latin1", "ASCII", sub="")))
    
    ##remove non-alpha numeric characters 
    
    removeSpecialChar<-content_transformer(function(x,pattern) gsub(pattern," ",x))
    
    specialChar <- "[\\@\\#\\$\\%\\^\\&\\*\\'\\(\\)\\{\\}\\[\\]\\,\\.\\;\\:]"
    
    corpus1<-tm_map(corpus1,removeSpecialChar,specialChar)
    
    ##remove punctuations
    
    corpus1<-tm_map(corpus1,removePunctuation)
    
    ##remove numbers
    
    corpus1<-tm_map(corpus1,removeNumbers)
    
    ##remove stop words
    ##corpus1<-tm_map(corpus1,removeWords,stopwords("english"))
    
    ##remove extra white spaces
    corpus1<-tm_map(corpus1,stripWhitespace)
    
    ##covert all characters to lower case
    corpus1<-tm_map(corpus1,content_transformer(tolower))
    
    return(corpus1)
}



extractPreTerm<-function(Term,n){
    
    preTerm<-paste(unlist(strsplit(Term,split='\ '))[1:n-1],collapse = ' ')
}

extractLastTerm<-function(Term,n){

        lastTerm<-unlist(strsplit(Term,split='\ '))[n]
}


saveData <- function(databaseName, table, data) {
    # Connect to the database
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    
    dbWriteTable(db,table,data,overwrite=T, rownames=FALSE)
    
    dbDisconnect(db)
}

readData <- function(databaseName, table) {
    # Connect to the database
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    
    dataDF<-dbReadTable(db,table)
    dataDT<-data.table(dataDF)
    
    dbDisconnect(db)
    
    return (dataDT)
}

readCleanInsert<- function(uniDT,biDTNew,triDTNew,quadDTNew,pentaDTNew){
    
    ##########################################################################*
    ## Read the documents, clean them and insert in the database 
    ###########################################################################
    
    
    ##Create corpus object with all the files in the directory
    ##-----------------------------------------------------------------------
    
    filePath<-"C:/Users/Kamal/Projects/DataScientistsToolbox/R/Capstone Project/Coursera-SwiftKey/final/en_US"
    
    corpusObj<-Corpus(DirSource(filePath))
    
    #corpusObj<-Corpus(DirSource("./Docs"))
    
    set.seed(1234)
    
    corpusObj[[1]]$content<-sample(corpusObj[[1]]$content,size=.05*length(corpusObj[[1]]$content))
    corpusObj[[2]]$content<-sample(corpusObj[[2]]$content,size=.05*length(corpusObj[[2]]$content))
    corpusObj[[3]]$content<-sample(corpusObj[[3]]$content,size=.05*length(corpusObj[[3]]$content))
    
    corpusObj<-cleanCorpus(corpusObj)
    
    ##---------------------------------UniGrams---------------------------
    
    uniGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
    uniGramMatrix <- DocumentTermMatrix(corpusObj,control = list(tokenize = uniGramTokenizer,wordLengths=c(1,Inf)))
    
    uniDT<-data.table(Terms=uniGramMatrix$dimnames$Terms,Freqs=colSums(as.matrix(uniGramMatrix)))
    
    rm(uniGramMatrix)
    
    uniDT<-uniDT[Freqs>3]
    
    setkey(uniDT,Terms)

    
    ##---------------------------------biGrams----------------------------
    
    biGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    biGramMatrix <- DocumentTermMatrix(corpusObj,control = list(tokenize = biGramTokenizer,wordLengths=c(1,Inf)))
    
    biDT<-data.table(Terms=biGramMatrix$dimnames$Terms,Freqs=colSums(as.matrix(biGramMatrix)))
    
    rm(biGramMatrix)
    
    preTerms<-sapply(biDT[,Terms,],extractPreTerm,2)
    
    lastTerms<-sapply(biDT[,Terms,],extractLastTerm,2)
    
    biDTNew<-data.table(preTerms,lastTerms,Freqs=biDT[,Freqs,])
    
    biDTNew<-biDTNew[Freqs>2]
    
    setkey(biDTNew,preTerms)
    
    rm(lastTerms)
    rm(preTerms)
    rm(biDT)
    
    
    ##--------------------------------TriGrams-----------------------------------
    
    triGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
    triGramMatrix <- DocumentTermMatrix(corpusObj,control = list(tokenize = triGramTokenizer,wordLengths=c(1,Inf)))
    
    triDT<-data.table(Terms=triGramMatrix$dimnames$Terms,Freqs=colSums(as.matrix(triGramMatrix)))
    
    rm(triGramMatrix)
    
    preTerms<-sapply(triDT[,Terms,],extractPreTerm,3)
    
    lastTerms<-sapply(triDT[,Terms,],extractLastTerm,3)
    
    triDTNew<-data.table(preTerms,lastTerms,Freqs=triDT[,Freqs,])
    
    triDTNew<-triDTNew[Freqs>1]
    
    setkey(triDTNew,preTerms)
    
    rm(lastTerms)
    rm(preTerms)
    rm(triDT)
    
    
    
    ##--------------------------------QuadGrams-----------------------------------
    
    quadGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
    quadGramMatrix <- DocumentTermMatrix(corpusObj,control = list(tokenize = quadGramTokenizer,wordLengths=c(1,Inf)))
    
    quadDT<-data.table(Terms=quadGramMatrix$dimnames$Terms,Freqs=colSums(as.matrix(quadGramMatrix)))
    
    rm(quadGramMatrix)
    
    preTerms<-sapply(quadDT[,Terms,],extractPreTerm,4)
    
    lastTerms<-sapply(quadDT[,Terms,],extractLastTerm,4)
    
    quadDTNew<-data.table(preTerms,lastTerms,Freqs=quadDT[,Freqs,])
    
    quadDTNew<-quadDTNew[Freqs>1]
    
    setkey(quadDTNew,preTerms)
    
    rm(lastTerms)
    rm(preTerms)
    rm(quadDT)
    
    
    
    ##--------------------------------PentaGrams-----------------------------------
    
    pentaGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
    pentaGramMatrix <- DocumentTermMatrix(corpusObj,control = list(tokenize = pentaGramTokenizer,wordLengths=c(1,Inf)))
    
    pentaDT<-data.table(Terms=pentaGramMatrix$dimnames$Terms,Freqs=colSums(as.matrix(pentaGramMatrix)))
    
    rm(pentaGramMatrix)
    
    preTerms<-sapply(pentaDT[,Terms,],extractPreTerm,5)
    
    lastTerms<-sapply(pentaDT[,Terms,],extractLastTerm,5)
    
    pentaDTNew<-data.table(preTerms,lastTerms,Freqs=pentaDT[,Freqs,])
    
    pentaDTNew<-pentaDTNew[Freqs>1]
    
    setkey(pentaDTNew,preTerms)
    
    rm(lastTerms)
    rm(preTerms)
    rm(pentaDT)
    rm(corpusObj)
        
    ##---------------------------Save the n-grams in the database---------------------
    
    saveData(databaseName,"penta_gram",pentaDTNew)
    
    saveData(databaseName,"quad_gram",quadDTNew)
    
    saveData(databaseName,"tri_gram",triDTNew)
    
    saveData(databaseName,"bi_gram",biDTNew)
    
    saveData(databaseName,"uni_gram",uniDT)
    
    return(list(uniDT,biDTNew,triDTNew,quadDTNew,pentaDTNew))
}

options(mysql=list(
    "host"="mydbinstance.czom2wnmp4yn.us-west-1.rds.amazonaws.com",
    "port"=3306,
    "user"="root",
    "password"="rootuser123"
    
))

databaseName<-"predictword"

refreshDatabase<-FALSE
pentaDTNew<-NULL

##-------Read the n-grams from the database, if no need to insert new data-------

if(!refreshDatabase){
    pentaDTNew<-readData(databaseName,"penta_gram")
    pentaDTNew<-pentaDTNew[Freqs>1]
    quadDTNew<-readData(databaseName,"quad_gram")
    quadDTNew<-quadDTNew[Freqs>1]
    triDTNew<-readData(databaseName,"tri_gram")
    triDTNew<-triDTNew[Freqs>1]
    biDTNew<-readData(databaseName,"bi_gram")
    biDTNew<-biDTNew[Freqs>1]
    uniDT<-readData(databaseName,"uni_gram")
    uniDT<-uniDT[Freqs>1]
    setkey(pentaDTNew,preTerms)
    setkey(quadDTNew,preTerms)
    setkey(triDTNew,preTerms)
    setkey(biDTNew,preTerms)
    setkey(uniDT,Terms)
}

##---------------------------Read, Clean, Insert if need be--------------------

if (refreshDatabase | is.null(pentaDTNew) ) {
    
    ngrams<-readCleanInsert()
    uniDT<-ngrams[[1]]
    biDTNew<-ngrams[[2]]
    triDTNew<-ngrams[[3]]
    quadDTNew<-ngrams[[4]]
    pentaDTNew<-ngrams[[5]]
    
}

##---------------------------Stupid Backoff Model---------------------------------------------

predictNextWord <- function(inputText){
    
    inputText<-tolower(inputText)

    inputWords<-unlist(stri_extract_all_words(inputText))
    
    done <- FALSE
    
    if(length(inputWords)>=4){
        
        inputWords<-inputWords[(length(inputWords)-3):length(inputWords)]
        
        input4<-paste(inputWords[1],inputWords[2],inputWords[3],inputWords[4])
        
        denom<-as.numeric(unlist(quadDTNew[paste(preTerms,lastTerms)==input4,.(sum(Freqs)),]))
        
        if (length(denom)>0){
            matches<-pentaDTNew[preTerms==input4,.(lastTerms=lastTerms,scores=Freqs/denom),]
            if (nrow(matches) >0) { done = TRUE}
        }
        
    } 
    
    if(!done & length(inputWords)>=3){
    
        inputWords<-inputWords[(length(inputWords)-2):length(inputWords)]
    
        input3<-paste(inputWords[1],inputWords[2],inputWords[3])
        
        denom<-as.numeric(unlist(triDTNew[paste(preTerms,lastTerms)==input3,.(sum(Freqs)),]))
        
        if (length(denom)>0){
            matches<-quadDTNew[preTerms==input3,.(lastTerms=lastTerms,scores=Freqs/denom),]
            if (nrow(matches) >0) { done = TRUE}
        }
        
    } 
    
    if (!done & length(inputWords)>=2){
        
        inputWords<-inputWords[(length(inputWords)-1):length(inputWords)]
        
        input2<-paste(inputWords[1],inputWords[2])
        
        denom<-as.numeric(unlist(biDTNew[paste(preTerms,lastTerms)==input2,.(sum(Freqs)),]))
        
        if (length(denom)>0){
            matches<-triDTNew[preTerms==input2,.(lastTerms=lastTerms,scores=Freqs/denom),]
            if (nrow(matches) >0) { done = TRUE}
        }
        
    } 
    
    if (!done & length(inputWords)>=1){
        
        inputWords<-inputWords[(length(inputWords)-0):length(inputWords)]
        
        input1<-inputWords[1]
         
        denom<-as.numeric(unlist(uniDT[Terms==input1,.(sum(Freqs)),]))
        
        if (length(denom)>0){
            matches<-biDTNew[preTerms==input1,.(lastTerms=lastTerms,scores=Freqs/denom),]
            if (nrow(matches) >0) { done = TRUE}
        }   
    } 
    
    if(!done){    
        denom<-as.numeric(unlist(uniDT[,.(sum(Freqs)),]))
        matches<-uniDT[,.(lastTerms=Terms,scores=Freqs/denom),]
    }
    
    matches<-matches[order(-scores)]
    return(na.omit(matches[1:3]$lastTerms))
}
