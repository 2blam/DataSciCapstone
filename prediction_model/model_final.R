library(tm)
library(stringi)
library(data.table)
library(doSNOW)

getSampleDoc = function(docs, samplingRate=1.0){
  #sampling
  if (samplingRate < 1.0){
    print("Sampling...")
    #for each document
    for (i in 1:length(docs)){
      #get the total number of line
      totalNumLine = length(docs[[i]]$content)
      #generate totalNumLine tosses with biased coin
      tosses = rbinom(totalNumLine, size=1, p=samplingRate)
      extractIdx = which(tosses == 1)
      #sampling
      docs[[i]]$content = docs[[i]]$content[extractIdx]      
    }
    print("Done!")
  }
  
  #replace ` and curly quote with '
  replaceQuote = content_transformer(function(x) {x = stri_replace_all_regex(x,"\u2019|`","'")})
  #remove non printable, except ' - and space
  removeNonletter =  content_transformer(function(x) stri_replace_all_regex(x,"[^\\p{L}\\s']+",""))
  
  #get alpha only
  getAlpha =  content_transformer(function(x) stri_replace_all_regex(x, "[^[A-Za-z ']]+", ""))
  #remove signal quote, except quote used in words e.g. didn't, we've, ours'
  removeQuote = content_transformer(function(x) stri_replace_all_regex(x, "[^A-Za-z]'+[^A-Za-z]", ""))
  #convert all to lowercase
  toLowercase     = content_transformer (function(x) stri_trans_tolower (x))
  #remove leading and trailing space
  removeLTSpace  = content_transformer(function(x) stri_replace_all_regex(x,"^\\s+|\\s+$",""))
  
  print("Cleaning...")
  #remove non printable, except '
  docs = tm_map(docs, removeNonletter)
  #
  docs = tm_map(docs, getAlpha)
  docs = tm_map(docs, removeQuote)
  
  #conversion to lowercase
  docs = tm_map(docs, toLowercase)
  #eliminating extra whitespace, leading and trailing space
  docs = tm_map(docs, stripWhitespace)
  docs = tm_map(docs, removeLTSpace)
  print("Done!")  
  docs
}

getToken = function(docs){  
  tokens = list()
  
  #splitWhiteSpace = function(x){c("<S>", strsplit(x, " "), "</S>")}
  splitWhiteSpace = function(x){strsplit(x, " ")}
  
  #for each document  
  for (i in 1:length(docs)){  
    result = lapply(docs[[i]]$content, splitWhiteSpace)      
    tokens = append(tokens, result)          
  }  
  
  unlist(tokens)
}

getTokenInIdx = function(docs, mapping){  
  splitWhiteSpace = function(x){strsplit(x, " ")}
  tokenInIdx = list()  
  #for each document  
  for (i in 1:length(docs)){ 
    print(paste("Processing", i , " out of", length(docs)))
    result = lapply(docs[[i]]$content, splitWhiteSpace)
    result = unlist(result)
    
    idx = lapply(result, function(x){
      searchStr = paste("^", x, "$", sep="")
      wIdx = mapping[w %like% searchStr, wIdx]
    })
    
    tokenInIdx = append(tokenInIdx, unlist(idx))
   
  }  
  tokenInIdx
}


set.seed(1407)
cl = makeCluster(3)
registerDoSNOW(cl)

setwd("C:/[DATA]/CapStone")

#suppose all the text file stored in <working directory>/corpus/txt
txtPath = paste(getwd(), "/corpus/txt" ,sep="")
docs = Corpus(DirSource(txtPath, encoding="UTF-8"),readerControl = list(reader=readPlain, language="en_US"))
docs = getSampleDoc(docs, 0.01)
tokens = getToken(docs)

#remove docs to save the memory space
rm(txtPath)
rm(cl)

#sort the order
unigram = sort(table(tokens), decreasing=TRUE)
unigram = as.data.frame(unigram)
names(unigram) = "count"

#create mapping table
mapping = as.data.frame(rownames(unigram))
names(mapping) = "w"

#convert the mapping to data.table
mapping = data.table(mapping)
mapping$wIdx = as.integer(rownames(mapping))

rm(unigram)

#convert tokens words with number
tokensInIdx = data.frame(tokens)
tokensInIdx$wIdx = 0 #all wIdx is ZERO

mapping$w <- as.character(mapping$w)   
mapping$wIdx <- as.integer(mapping$wIdx)

library(hash)
dic <- hash(c(mapping$w), c(mapping$wIdx))  # build a word to index hash map
f <- function(word) dic[[word]]
tokensInIdx$wIdx <- sapply(tokensInIdx$tokens, f)

mapping$wIdx = as.integer(rownames(mapping))
setkey(mapping, w)
save(tokensInIdx, file="tokensInIdx.RData")

tokensInIdx = data.table(tokensInIdx)
unigramDT <- copy(tokensInIdx)
setnames(unigramDT, "wIdx", "curr")
setnames(unigramDT, "tokens", "w")
unigramDT = unigramDT[, count := .N, by=list(curr)]
unigramDT = unique(unigramDT) #collapse

bigramDT <- copy(tokensInIdx)
bigramDT = bigramDT[1:(nrow(bigramDT)-1), ] #remove the last row
setnames(bigramDT, "wIdx", "prev1")
l = nrow(tokensInIdx)
bigramDT$curr = tokensInIdx[2:l]$wIdx
bigramDT = bigramDT[, tokens:=NULL]

#prepare trigramDT
trigramDT <- copy(bigramDT)
trigramDT = trigramDT[1:(nrow(trigramDT)-1), ]
setnames(trigramDT, "curr", "prev2")
trigramDT$curr = tokensInIdx[3:l]$wIdx

#group by and then count
trigramDT = trigramDT[, count := .N, by=list(prev1, prev2, curr)]

#collapse - must be perform at the end
bigramDT = bigramDT[, count := .N, by=list(prev1, curr)]
bigramDT = unique(bigramDT)
trigramDT = unique(trigramDT)

#load data
save(mapping, file="mapping.RData")
save(unigramDT, file="unigramDT.RData")
save(bigramDT, file="bigramDT.RData")
save(trigramDT, file="trigramDT.RData")

# load("mapping.RData")
# load("unigramDT.RData")
# load("bigramDT.RData")
# load("trigramDT.RData")

getCount = function(ngram, c1, p1, p2=""){
  if (p2 == ""){
    ngram[ngram$prev1 == p1 & ngram$curr == c1, count]
  }else{
    ngram[ngram$prev1 == p1 & ngram$prev2 == p2 & ngram$curr == c1, count]
   }
}


getDiscount = function(discount, curr_count){
  if (curr_count == 1){
    discount[1]
  }else if (curr_count == 2){
    discount[2]
  }else{
    discount[3]
  }
}

getCountOfHistory = function(ngram, p1, p2=""){
  if (p2 == ""){
    sum(ngram[ngram$prev1 == p1, count]) 
  }else{
    sum(ngram[ngram$prev1 == p1 & ngram$prev2 == p2, count])
  }
}

getCountOfExtendedHistory = function(ngram, p1, p2=""){
  #get the number of words follows the prev
  if(p2 == ""){
    idx = which(ngram$prev1 == p1)
  }else{
    idx = which(ngram$prev1 == p1 & ngram$prev2 == p2)
  }
  
  #check how many of them occurs 1, 2, or 3+ times
  counts = ngram[idx, count]
  
  N1 = sum(counts == 1)
  N2 = sum(counts == 2)
  N3p = sum(counts >= 3)
  
  c(N1, N2, N3p)
}

calc_discount = function(ngram){  
  #ngram - ngram for calculating the discount, it can be trigram / bigram / unigram
  #print(paste("nrow:", nrow(ngram)))
  #N_c - the counts of n-grams with exactly count c
  N_1 = sum(ngram$count == 1)
  N_2 = sum(ngram$count == 2)
  N_3 = sum(ngram$count == 3)
  N_4 = sum(ngram$count == 4)
  #   
  #calculate the Y value
  Y = N_1 / (N_1 + 2 * N_2)
  
  #calculate D_c - the optimal discounting parameters
  D_1 = 1 - (2 * Y *N_2 / N_1)
  D_2 = 2 - (3 * Y *N_3 / N_2)
  D_3p = 3 - (4 * Y *N_4 / N_3)
  
  c(D_1, D_2, D_3p)
}


getProb_recur = function(trigramDT, bigramDT, unigramDT, stepNum=3, w_n, p1, p2=""){
  if (stepNum > 1){
    if (stepNum == 3){
      #print("use trigramDT")
      ngram = trigramDT
    }else{
      #print("use bigramDT")
      ngram = bigramDT
    }
    
    
    
    discount = calc_discount(ngram) #get D1, D2, D3p

    c = getCount(ngram, w_n, p1, p2)
    
    D = getDiscount(discount, c)
    
    c_hist = getCountOfHistory(ngram, p1, p2)
    
    #prob of this token
    prob = (c-D) / c_hist
    
    #gamma
    N = getCountOfExtendedHistory(ngram, p1, p2)
    gamma = sum(discount * N)/c_hist
    
    if (stepNum == 3){
      p1 = p2
      p2 = ""
    }else{
      p1 = NA
    }
    
    stepNum = stepNum - 1
    
    prob = prob + gamma * getProb_recur(trigramDT, bigramDT, unigramDT, stepNum, w_n, p1, p2)
    
    prob
  }else{
    #numerator - number of distint words that precedes the possible word
    numerator = sum(bigramDT$curr == w_n)
    
    #denumerator - the sum of distint words that using different end
    denumerator = nrow(bigramDT)
    
    numerator / denumerator
  }
}

getProb = function(trigramDT, bigramDT, unigramDT, stepNum=1, p1="", p2=""){
  if (stepNum> 1){
    # step:3 ==> trigram ---> get last 2 tokens
    # step:2 ==> bigram ----> get last 1 tokens
    
    #check if there is any possible match    
    if (stepNum == 3){
      #check if there is enough tokens
      if (p1 == "" || p2 == ""){ 
        return (NA) 
      }
      ngram = trigramDT
      possible = ngram[(ngram$prev1 == p1 & ngram$prev2 == p2), ]      
    }else{
      ngram = bigramDT     
      possible = ngram[(ngram$prev1 == p1), ]      
    }
    
    # too much possible may delay the response time, limit to top 30 only
    possible = possible[order(count, decreasing=TRUE), ] #sort wrt to count
    possible = head(possible, 10) #get top 10 only

    if (nrow(possible) == 0){
      NA #cannot find any match, just return 0
    }else{
      #for each of the possible match, calculate the prob            
      possible = possible$curr
      
      
      probability = unlist(lapply(possible, function(x){
        getProb_recur(trigramDT, bigramDT, unigramDT, stepNum, x, p1, p2)
      }))
      data.table(wIdx=possible, prob=probability)
    }      
  }
}


getMatches= function(mapping, prob, top=5){
  prob = prob[order(prob, decreasing=TRUE), ]
  
  idx = head(prob, top)$wIdx
  #only need the last token
  mapping[idx]
}


#trigram -> bigram -> unigram
#####################
curr_time = proc.time()
phrase = "merry christmas" #get possible next token, calculate the prob
tokens = unlist(strsplit(phrase, " "))
tokens = tail(tokens, 2) #just get the last 2 only
#get the idx mapping value
if (length(tokens) == 2){
  prev1 = which(mapping$w == tokens[1])
  prev2 = which(mapping$w == tokens[2])
}else{
  prev1 = which(mapping$w == tokens[1])
  prev2 = ""
}
if (identical(prev1, integer(0))==TRUE){
  prev1 = ""
}
if (identical(prev2, integer(0))==TRUE){
  prev2 = ""
}

prob = getProb(trigramDT, bigramDT, unigramDT, 3, prev1, prev2)

if (!any(is.na(prob))){
  matches= getMatches(mapping, prob, 5)
}else{
  #backoff - bigram
  print("backoff - bigram")
  prob = getProb(trigramDT, bigramDT, unigramDT, 2, prev2, "")
}

if (!any(is.na(prob))){
  matches= getMatches(mapping, prob, 5)
}else{
  #cannot find any related words in trigram and bigram
  # find the most common word in unigram and return
  print("backoff - unigram")
  
  unigramDT = unigramDT[order(count, decreasing=TRUE), ]
  matches = mapping[head(unigramDT)$curr]
}

matches


proc.time() - curr_time

format(object.size(unigramDT), "MB")
format(object.size(bigramDT), "MB")
format(object.size(trigramDT), "MB")
format(object.size(unigramDT)+object.size(bigramDT)+object.size(trigramDT), "MB")
