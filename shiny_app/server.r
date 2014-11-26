library(shiny)
library(data.table)

load("mapping.RData")
load("unigramDT.RData")
load("bigramDT.RData")
load("trigramDT.RData")



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
  #N_c - the counts of n-grams with exactly count c
  N_1 = sum(ngram$count == 1)
  N_2 = sum(ngram$count == 2)
  N_3 = sum(ngram$count == 3)
  N_4 = sum(ngram$count == 4)
  
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
      ngram = trigramDT
    }else{
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
      NA #cannot find any match, just return NA
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
  mapping[idx]
}


tokenization = function(phrase){
  library(stringi)
  #replace ` and curly quote with '
  phrase = stri_replace_all_regex(phrase,"\u2019|`","'")
  #remove non printable, except ' - and space
  phrase = stri_replace_all_regex(phrase,"[^\\p{L}\\s']+","")
  
  #get alpha only
  phrase = stri_replace_all_regex(phrase, "[^[A-Za-z ']]+", "")
  #remove signal quote, except quote used in words e.g. didn't, we've, ours'
  phrase = stri_replace_all_regex(phrase, "[^A-Za-z]'+[^A-Za-z]", "")
  #convert all to lowercase
  phrase = stri_trans_tolower (phrase)
  #remove leading and trailing space
  phrase = stri_replace_all_regex(phrase,"^\\s+|\\s+$","")
  
  tokens = unlist(strsplit(phrase, " "))
  
  tokens  
}

shinyServer(
  function(input, output){
    values = reactiveValues(phrase="")
    
    observe({
      if(input$submit > 0) {
        
        values$phrase <- isolate(input$phrase)
        
      }
    })
    
    
    output$result = renderText({
      #preprocess & tokenization
      tokens = tokenization(values$phrase) 
      
      #we support 3-gram only, extract the last 2 token
      tokens = tail(tokens, 2)
      
      #predict
      if (!identical(tokens, character(0)) && length(tokens) > 0){
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
          matches= getMatches(mapping, prob, 10)
        }else{
          #backoff - bigram
          prob = getProb(trigramDT, bigramDT, unigramDT, 2, prev2, "")
        }
        
        if (!any(is.na(prob))){
          matches= getMatches(mapping, prob, 10)
        }else{
          #cannot find any related words in trigram and bigram
          # find the most common word in unigram and return
          unigramDT = unigramDT[order(count, decreasing=TRUE), ]
          matches = mapping[head(unigramDT)$curr]
        }
        
        
        result = paste("<li>", as.character(matches$w), collapse="")        
        result = paste("<ol>", result, "</ol>")
        #print(result)
        result
      }
        
    }) 
  }
)