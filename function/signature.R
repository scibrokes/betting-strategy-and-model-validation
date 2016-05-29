signature <- function(x){
  
  ## http://www.r-bloggers.com/merging-data-sets-based-on-partially-matched-data-elements/
  ## (1) Here's where the algorithm starts...
  ## I'm going to generate a signature from country names to reduce some of the minor differences between strings
  ## In this case, convert all characters to lower case, sort the words alphabetically, and then concatenate them with no spaces.
  ## So for example, United Kingdom would become kingdomunited
  ## We might also remove stopwords such as 'the' and 'of'.
  
  sign <- paste(sort(unlist(strsplit(tolower(x), " "))), collapse='')
  return(sign)
  }


