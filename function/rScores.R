rScores <- function(mbase, type = 'option10', print.loop = FALSE) {
  ## type = 'option1' until type = 'option9' which will simulate the 
  ##   soccer scores result for Kelly model proceed further and eventaully get the P&L from 
  ##   investment.
  ## print.loop = TRUE or print.loop = FALSE.
  ## mbase is the dataset which contain home and away scores.
  
  ## two multivariate normal distribution functions from 'mvtnorm' and 'MASS' packages
  ## There will be endless looping from below while loops.
  
  ## --------------------- Load packages --------------------------------
  options(warn = -1)
  suppressMessages(library('plyr'))
  suppressMessages(library('tidyverse'))
  suppressMessages(library('MASS'))
  suppressMessages(library('tmvtnorm'))
  suppressMessages(library('BBmisc'))
  
  ## --------------------- Start Multivariate Analysis --------------------------------
  ## I tried to apply multivariate normal distribution via MASS package but due to zero 
  ##   inflated issue, the variance is very high. mvtnorm package might workable as well.
  ## 
  scores <- mbase[c('FTHG', 'FTAG')]
  
  org <- mvrnorm(n = nrow(scores), mu = colMeans(scores), Sigma = as.matrix(var(scores))) %>% 
    data.frame %>% tbl_df
  
  prepois <- list(mean = org %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                    mutate(dif = pre / obs), 
                  var = data.frame(var(scores), var(org)))
  
  # return value of original observation.
  opt0 <- list(pred = org, pstpois = prepois)
  
  if(type == 'option1') {
    ## --------------------- option 1 ---------------------------------------------------
    
    ## adjusted the mean value of negative figures from rmvnorm() and mvrnorm and then plus.
    bvs <- org
    hgadj <- bvs$FTHG[bvs$FTHG <= 0] %>% sum(.)/length(bvs$FTHG[bvs$FTHG > 0]) %>% abs
    agadj <- bvs$FTAG[bvs$FTAG <= 0] %>% sum(.)/length(bvs$FTAG[bvs$FTAG > 0]) %>% abs
    
    # add the range of negative figure to the positive figure to raise the R^2.
    bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG + hgadj), 
                    FTAG = ifelse(FTAG <= 0, 0, FTAG + agadj))
    rm(hgadj, agadj)
    prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> prepois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.497899 1.0006059
    #2 1.154427 1.150775 0.9968366
    #
    #$var
    #            FTHG        FTAG      FTHG.1      FTAG.1
    #FTHG  1.63115357 -0.06298846  1.28908689 -0.04326059
    #FTAG -0.06298846  1.27395462 -0.04326059  0.91651577
    
    # rpois the scores
    bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                    FTAG = rpois(n = nrow(bvs), lambda = FTAG)) %>% 
      mutate(FTHG = ifelse(is.na(FTHG), 0, FTHG), FTAG = ifelse(is.na(FTAG), 0, FTAG))
    pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> pstpois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.488028 0.9940123
    #2 1.154427 1.149044 0.9953371
    #
    #$var
    #            FTHG        FTAG      FTHG.1      FTAG.1
    #FTHG  1.63115357 -0.06298846  2.74238481 -0.04838128
    #FTAG -0.06298846  1.27395462 -0.04838128  2.05570712
    
    #> summary(scores)
    #         FTHG            FTAG      
    #Min.   :0.000   Min.   :0.000  
    #1st Qu.:1.000   1st Qu.:0.000  
    #Median :1.000   Median :1.000  
    #Mean   :1.497   Mean   :1.154  
    #3rd Qu.:2.000   3rd Qu.:2.000  
    #Max.   :9.000   Max.   :8.000  
    #
    #> summary(bvs)
    #          FTHG             FTAG       
    #Min.   : 0.000   Min.   : 0.000  
    #1st Qu.: 0.000   1st Qu.: 0.000  
    #Median : 1.000   Median : 1.000  
    #Mean   : 1.488   Mean   : 1.149  
    #3rd Qu.: 2.000   3rd Qu.: 2.000  
    #Max.   :13.000   Max.   :12.000
    
  } else if(type == 'option2') {
    ## --------------------- option 2 ---------------------------------------------------
    
    ## adjusted the mean value of negative figures from rmvnorm() and mvrnorm and then minus.
    bvs <- org
    hgadj <- bvs$FTHG[bvs$FTHG <= 0] %>% sum(.)/length(bvs$FTHG[bvs$FTHG > 0]) %>% abs
    agadj <- bvs$FTAG[bvs$FTAG <= 0] %>% sum(.)/length(bvs$FTAG[bvs$FTAG > 0]) %>% abs
    
    # add the range of negative figure to the positive figure to raise the R^2.
    bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG - hgadj), 
                    FTAG = ifelse(FTAG <= 0, 0, FTAG - agadj)) %>% 
      mutate(FTHG =  FTHG * (colMeans(scores)[1] / mean(FTHG)), 
             FTAG = FTAG * (colMeans(scores)[2]/mean(FTAG)))
    rm(hgadj, agadj)
    prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #$mean
    #       obs      pre dif
    #1 1.496992 1.496992   1
    #2 1.154427 1.154427   1
    #
    #$var
    #            FTHG        FTAG      FTHG.1      FTAG.1
    #FTHG  1.63115357 -0.06298846  1.10551888 -0.03529094
    #FTAG -0.06298846  1.27395462 -0.03529094  0.74877360
    
    # rpois the scores
    bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                    FTAG = rpois(n = nrow(bvs), lambda = FTAG)) %>% 
      mutate(FTHG = ifelse(is.na(FTHG), 0, FTHG), FTAG = ifelse(is.na(FTAG), 0, FTAG))
    pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> pstpois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.492291 0.9968597
    #2 1.154427 1.158154 1.0032282
    #
    #$var
    #            FTHG        FTAG      FTHG.1      FTAG.1
    #FTHG  1.63115357 -0.06298846  2.59422492 -0.03118927
    #FTAG -0.06298846  1.27395462 -0.03118927  1.90110849
    
    #> summary(scores)
    #         FTHG            FTAG      
    #Min.   :0.000   Min.   :0.000  
    #1st Qu.:1.000   1st Qu.:0.000  
    #Median :1.000   Median :1.000  
    #Mean   :1.497   Mean   :1.154  
    #3rd Qu.:2.000   3rd Qu.:2.000  
    #Max.   :9.000   Max.   :8.000  
    #
    #> summary(bvs)
    #          FTHG             FTAG       
    #Min.   : 0.000   Min.   : 0.000  
    #1st Qu.: 0.000   1st Qu.: 0.000  
    #Median : 1.000   Median : 1.000  
    #Mean   : 1.492   Mean   : 1.158  
    #3rd Qu.: 2.000   3rd Qu.: 2.000  
    #Max.   :13.000   Max.   :10.000
    
  } else if(type == 'option3') {
    ## --------------------- option 3 ---------------------------------------------------
    
    ## adjusted the mean value of negative figures from rmvnorm() and mvrnorm and then apply rnorm.
    bvs <- org %>% mutate(No = seq(nrow(.)))
    
    # start looping
    while(any(bvs$FTHG < 0)|any(bvs$FTAG < 0)) {
      hgadj0 <- bvs[c('No', 'FTHG')] %>% filter(FTHG <= 0)
      hgadj1 <- bvs[c('No', 'FTHG')] %>% filter(FTHG > 0) %>% 
        mutate(FTHG = rnorm(n = nrow(.), mean = mean(hgadj0$FTHG), sd = sd(hgadj0$FTHG)))
      agadj0 <- bvs[c('No', 'FTAG')] %>% filter(FTAG <= 0)
      agadj1 <- bvs[c('No', 'FTAG')] %>% filter(FTAG > 0) %>% 
        mutate(FTAG = rnorm(n = nrow(.), mean = mean(agadj0$FTAG), sd = sd(agadj0$FTAG)))
      hgadj <- suppressMessages(join(bvs[c('No')], hgadj1)) %>% tbl_df %>% 
        mutate(FTHG = ifelse(is.na(FTHG), 0, FTHG))
      agadj <- suppressMessages(join(bvs[c('No')], agadj1)) %>% tbl_df %>% 
        mutate(FTAG = ifelse(is.na(FTAG), 0, FTAG))
      rm(hgadj0, hgadj1, agadj0, agadj1)
      
      # add the range of negative figure to the positive figure to raise the R^2.
      bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG - hgadj$FTHG), 
                      FTAG = ifelse(FTAG <= 0, 0, FTAG - agadj$FTAG))
    }
    hgadj %<>% .[c('FTHG')]
    agadj %<>% .[c('FTAG')]
    bvs %<>% .[c('FTHG', 'FTAG')]
    
    rm(hgadj, agadj)
    prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> prepois
    #$mean
    #       obs      pre      dif
    #1 1.496992 2.128443 1.421813
    #2 1.154427 1.737807 1.505342
    #
    #$var
    #            FTHG        FTAG     FTHG.1     FTAG.1
    #FTHG  1.63115357 -0.06298846  1.8465079 -0.0498577
    #FTAG -0.06298846  1.27395462 -0.0498577  1.4409946
    
    bvs %<>% mutate(FTHG = FTHG * colMeans(scores[,1])/colMeans(bvs[,1]), 
                    FTAG = FTAG * colMeans(scores[,2])/colMeans(bvs[,2]))
    
    # rpois the scores
    bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                    FTAG = rpois(n = nrow(bvs), lambda = FTAG))
    pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> pstpois
    #$mean
    #       obs      pre      dif
    #1 1.496992 1.506077 1.006069
    #2 1.154427 1.154744 1.000274
    #
    #$var
    #            FTHG        FTAG      FTHG.1      FTAG.1
    #FTHG  1.63115357 -0.06298846  2.43377585 -0.02901322
    #FTAG -0.06298846  1.27395462 -0.02901322  1.79441501
    
    #> summary(scores)
    #         FTHG            FTAG      
    #Min.   :0.000   Min.   :0.000  
    #1st Qu.:1.000   1st Qu.:0.000  
    #Median :1.000   Median :1.000  
    #Mean   :1.497   Mean   :1.154  
    #3rd Qu.:2.000   3rd Qu.:2.000  
    #Max.   :9.000   Max.   :8.000  
    #
    #> summary(bvs)
    #          FTHG             FTAG       
    #Min.   : 0.000   Min.   : 0.000  
    #1st Qu.: 0.000   1st Qu.: 0.000  
    #Median : 1.000   Median : 1.000  
    #Mean   : 1.506   Mean   : 1.155  
    #3rd Qu.: 2.000   3rd Qu.: 2.000  
    #Max.   :12.000   Max.   :12.000
    
  } else if(type == 'option4') {
    ## --------------------- option 4 ---------------------------------------------------
    
    ## apply min and max as adjusted which is similar with rmvnorm() and mvrnorm. Test the difference.
    bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                    lower = apply(scores, 2 , min), upper = apply(scores, 2 , max),  #adjuster
                    algorithm = 'gibbs') %>% 
      matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
    prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> prepois
    #$mean
    #       obs      pre      dif
    #1 1.496992 1.781963 1.190362
    #2 1.154427 1.466103 1.269984
    #
    #$var
    #            FTHG        FTAG      FTHG.1      FTAG.1
    #FTHG  1.63115357 -0.06298846  1.09590894 -0.03011711
    #FTAG -0.06298846  1.27395462 -0.03011711  0.80920880
    
    bvs %<>% mutate(FTHG = FTHG * colMeans(scores[,1])/colMeans(bvs[,1]), 
                    FTAG = FTAG * colMeans(scores[,2])/colMeans(bvs[,2]))
    
    # rpois the scores
    bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                    FTAG = rpois(n = nrow(bvs), lambda = FTAG))
    pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> pstpois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.494045 0.9980312
    #2 1.154427 1.148654 0.9949995
    #
    #$var
    #            FTHG        FTAG     FTHG.1     FTAG.1
    #FTHG  1.63115357 -0.06298846  2.2115334 -0.0314745
    #FTAG -0.06298846  1.27395462 -0.0314745  1.6512341
    
    #> summary(scores)
    #         FTHG            FTAG      
    #Min.   :0.000   Min.   :0.000  
    #1st Qu.:1.000   1st Qu.:0.000  
    #Median :1.000   Median :1.000  
    #Mean   :1.497   Mean   :1.154  
    #3rd Qu.:2.000   3rd Qu.:2.000  
    #Max.   :9.000   Max.   :8.000  
    #
    #> summary(bvs)
    #          FTHG            FTAG      
    #Min.   : 0.000   Min.   :0.000  
    #1st Qu.: 0.000   1st Qu.:0.000  
    #Median : 1.000   Median :1.000  
    #Mean   : 1.494   Mean   :1.149  
    #3rd Qu.: 2.000   3rd Qu.:2.000  
    #Max.   :13.000   Max.   :9.000
    
  } else if(type == 'option5') {
    ## --------------------- option 5 ---------------------------------------------------
    
    ## manual adjusted lower and upper but option 8 will be more accurate by apply while loops.
    bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                    lower = apply(scores, 2 , min), upper = c(3, 2.3), #adjuster
                    algorithm = 'gibbs') %>% 
      matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
    prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> prepois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.495693 0.9991323
    #2 1.154427 1.149879 0.9960601
    #
    #$var
    #            FTHG        FTAG       FTHG.1       FTAG.1
    #FTHG  1.63115357 -0.06298846  0.618832035 -0.005146472
    #FTAG -0.06298846  1.27395462 -0.005146472  0.385158974
    
    # rpois the scores
    bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                    FTAG = rpois(n = nrow(bvs), lambda = FTAG))
    pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> pstpois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.499306 1.0015457
    #2 1.154427 1.149872 0.9960544
    #
    #$var
    #            FTHG        FTAG      FTHG.1      FTAG.1
    #FTHG  1.63115357 -0.06298846  2.13026088 -0.02692134
    #FTAG -0.06298846  1.27395462 -0.02692134  1.55075844
    
    #> summary(scores)
    #FTHG            FTAG      
    #Min.   :0.000   Min.   :0.000  
    #1st Qu.:1.000   1st Qu.:0.000  
    #Median :1.000   Median :1.000  
    #Mean   :1.497   Mean   :1.154  
    #3rd Qu.:2.000   3rd Qu.:2.000  
    #Max.   :9.000   Max.   :8.000  
    #
    #> summary(bvs)
    #FTHG             FTAG      
    #Min.   : 0.000   Min.   : 0.00  
    #1st Qu.: 0.000   1st Qu.: 0.00  
    #Median : 1.000   Median : 1.00  
    #Mean   : 1.499   Mean   : 1.15  
    #3rd Qu.: 2.000   3rd Qu.: 2.00  
    #Max.   :11.000   Max.   :10.00
    
  } else if(type == 'option6') {
    ## --------------------- option 6 ---------------------------------------------------
    
    ## apply quantile 1st and 3rd as lower and upper.
    bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                    lower = c(1, 0), upper = c(2, 2.3), 
                    algorithm = 'gibbs') %>% 
      matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
    prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> prepois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.501266 1.0028552
    #2 1.154427 1.150499 0.9965974
    #
    #$var
    #            FTHG        FTAG       FTHG.1       FTAG.1
    #FTHG  1.63115357 -0.06298846  0.081854062 -0.001981379
    #FTAG -0.06298846  1.27395462 -0.001981379  0.383968266
    
    # rpois the scores
    bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                    FTAG = rpois(n = nrow(bvs), lambda = FTAG))
    pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> pstpois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.498916 1.0012854
    #2 1.154427 1.154086 0.9997046
    #
    #$var
    #            FTHG        FTAG       FTHG.1       FTAG.1
    #FTHG  1.63115357 -0.06298846  1.587414180 -0.005630223
    #FTAG -0.06298846  1.27395462 -0.005630223  1.543558535
    
    #> summary(scores)
    #         FTHG            FTAG      
    #Min.   :0.000   Min.   :0.000  
    #1st Qu.:1.000   1st Qu.:0.000  
    #Median :1.000   Median :1.000  
    #Mean   :1.497   Mean   :1.154  
    #3rd Qu.:2.000   3rd Qu.:2.000  
    #Max.   :9.000   Max.   :8.000  
    #
    #> summary(bvs)
    #          FTHG            FTAG      
    #Min.   : 0.000   Min.   :0.000  
    #1st Qu.: 1.000   1st Qu.:0.000  
    #Median : 1.000   Median :1.000  
    #Mean   : 1.499   Mean   :1.154  
    #3rd Qu.: 2.000   3rd Qu.:2.000  
    #Max.   :11.000   Max.   :9.000
    
  } else if(type == 'option7') {
    ## --------------------- option 7 ---------------------------------------------------
    
    ## apply quantile 1st and 3rd as lower and upper.
    bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                    lower = apply(scores, 2 , quantile)[2,], upper = apply(scores, 2 , quantile)[4,], 
                    algorithm = 'gibbs') %>% 
      matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
    prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> prepois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.501468 1.0029902
    #2 1.154427 1.035222 0.8967411
    #
    #$var
    #            FTHG        FTAG        FTHG.1        FTAG.1
    #FTHG  1.63115357 -0.06298846  8.212482e-02 -7.639482e-06
    #FTAG -0.06298846  1.27395462 -7.639482e-06  3.006181e-01
    
    bvs %<>% mutate(FTHG = FTHG * colMeans(scores[,1])/colMeans(bvs[,1]), 
                    FTAG = FTAG * colMeans(scores[,2])/colMeans(bvs[,2]))
    
    # rpois the scores
    bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                    FTAG = rpois(n = nrow(bvs), lambda = FTAG))
    pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> pstpois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.515406 1.0123009
    #2 1.154427 1.037584 0.8987868
    #
    #$var
    #            FTHG        FTAG       FTHG.1       FTAG.1
    #FTHG  1.63115357 -0.06298846  1.596141804 -0.009871674
    #FTAG -0.06298846  1.27395462 -0.009871674  1.330467392
    
    #> summary(scores)
    #         FTHG            FTAG      
    #Min.   :0.000   Min.   :0.000  
    #1st Qu.:1.000   1st Qu.:0.000  
    #Median :1.000   Median :1.000  
    #Mean   :1.497   Mean   :1.154  
    #3rd Qu.:2.000   3rd Qu.:2.000  
    #Max.   :9.000   Max.   :8.000  
    #
    #> summary(bvs)
    #         FTHG             FTAG       
    #Min.   :0.000   Min.   : 0.000  
    #1st Qu.:1.000   1st Qu.: 0.000  
    #Median :1.000   Median : 1.000  
    #Mean   :1.515   Mean   : 1.038  
    #3rd Qu.:2.000   3rd Qu.: 2.000  
    #Max.   :9.000   Max.   :11.000
    
  } else if(type == 'option8') {
    ## --------------------- option 8 ---------------------------------------------------
    
    ## set min value as lower which is 0 but adjust the upper from mean value.
    xy <- colMeans(scores)
    iteration <- 0
    x <- xy[1]
    y <- xy[2]
    
    bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                    lower = apply(scores, 2 , min), upper = c(x, y), 
                    algorithm = 'gibbs') %>% 
      matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
    
    prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> prepois
    #$mean
    #       obs       pre       dif
    #1 1.496992 0.8350581 0.5578241
    #2 1.154427 0.6282839 0.5442387
    #
    #$var
    #            FTHG        FTAG       FTHG.1       FTAG.1
    #FTHG  1.63115357 -0.06298846  0.174421944 -0.000497472
    #FTAG -0.06298846  1.27395462 -0.000497472  0.105868278
    
    # start looping
    while((prepois$mean$dif[1] < 0.9999) | (prepois$mean$dif[2] < 0.9999)) {
      
      bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                      lower = apply(scores, 2 , min), upper = c(x, y), 
                      algorithm = 'gibbs') %>% 
        matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
      
      bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG), FTAG = ifelse(FTAG <= 0, 0, FTAG))
      
      bvs %<>% mutate(FTHG = FTHG * colMeans(scores[,1])/colMeans(bvs[,1]), 
                      FTAG = FTAG * colMeans(scores[,2])/colMeans(bvs[,2]))
      
      prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      iteration = iteration + 1
      if(prepois$mean$dif[1] < 0.9999) {
        x <- x + 0.0001
      }
      if(prepois$mean$dif[2] < 0.9999) {
        y <- y + 0.0001
      }
      
      if(print.loop == TRUE) {
        cat('iteration :', iteration, '; x = ', x, 'and y = ', y, '\n')
        print(prepois)
      }
    }
    
    # rpois the scores
    bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                    FTAG = rpois(n = nrow(bvs), lambda = FTAG))
    
    pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> pstpois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.488662 0.9944353
    #2 1.154427 1.160930 1.0056335
    #
    #$var
    #            FTHG        FTAG      FTHG.1      FTAG.1
    #FTHG  1.63115357 -0.06298846 2.105920786 0.005929097
    #FTAG -0.06298846  1.27395462 0.005929097 1.542206179
    
    #> summary(scores)
    #         FTHG            FTAG      
    #Min.   :0.000   Min.   :0.000  
    #1st Qu.:1.000   1st Qu.:0.000  
    #Median :1.000   Median :1.000  
    #Mean   :1.497   Mean   :1.154  
    #3rd Qu.:2.000   3rd Qu.:2.000  
    #Max.   :9.000   Max.   :8.000  
    #
    #> summary(bvs)
    #          FTHG             FTAG       
    #Min.   : 0.000   Min.   : 0.000  
    #1st Qu.: 0.000   1st Qu.: 0.000  
    #Median : 1.000   Median : 1.000  
    #Mean   : 1.489   Mean   : 1.161  
    #3rd Qu.: 2.000   3rd Qu.: 2.000  
    #Max.   :10.000   Max.   :10.000
    
  } else if(type == 'option9') {
    ## --------------------- option 9 ---------------------------------------------------
    
    ## Range within 1st quantile and 3rd quantile of scores set as the upper interval of 
    ##   rtmvnorm().
    ## Adjust the both lower and also upper from quantile which will be more centralize.
    xy <- apply(scores, 2, quantile)
    # A tibble: 5 × 2
    #   FTHG  FTAG
    #   <dbl> <dbl>
    #1     0     0
    #2     1     0
    #3     1     1
    #4     2     2
    #5     9     8
    
    # I choose iteration start expand from quantile 1 and quantile 3 will be more accurate 
    #   compare to start from min & max.
    x <- xy[,1]
    y <- xy[,2]
    
    iteration <- 0
    x2 <- x[2]; x4 <- x[4]
    y2 <- y[2]; y4 <- y[4]
    
    bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                    lower = c(x2, y2), upper = c(x4, y4), 
                    algorithm = 'gibbs') %>% 
      matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
    
    prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> prepois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.499980 1.0019963
    #2 1.154427 1.032095 0.8940325
    #
    #$var
    #            FTHG        FTAG        FTHG.1        FTAG.1
    #FTHG  1.63115357 -0.06298846  0.0816319511 -0.0008102165
    #FTAG -0.06298846  1.27395462 -0.0008102165  0.2980400245
    
    # start looping
    while((prepois$mean$dif[1] > 1.0001) | (prepois$mean$dif[2] < 0.9999)) {
      bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                      lower = c(x2, y2), upper = c(x4, y4), 
                      algorithm = 'gibbs') %>% 
        matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
      
      bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG), FTAG = ifelse(FTAG <= 0, 0, FTAG))
      
      prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      iteration = iteration + 1
      if(findInterval(prepois$mean$dif[1], c(0.9999, 1.0001)) == FALSE) {
        x2 <- x2 - 0.0001
        x4 <- x4 + 0.0001
      }
      if(findInterval(prepois$mean$dif[2], c(0.9999, 1.0001)) == FALSE) {
        y2 <- y2 - 0.0001
        y4 <- y4 + 0.0001
      }
      if(print.loop == TRUE) {
        cat('iteration :', iteration, '; x.low = ', x2, 'and y.low = ', y2, 
            '; x.up = ', x4, 'and y.up = ', y4, '\n')
        print(prepois)
      }
    }
    
    # rpois the scores
    bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                    FTAG = rpois(n = nrow(bvs), lambda = FTAG))
    
    pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> pstpois
    #$mean
    #       obs      pre      dif
    #1 1.496992 1.497528 1.000358
    #2 1.154427 1.155012 1.000506
    #
    #$var
    #            FTHG        FTAG       FTHG.1       FTAG.1
    #FTHG  1.63115357 -0.06298846 1.5826837595 0.0007242577
    #FTAG -0.06298846  1.27395462 0.0007242577 1.8468725671
    
    #> summary(scores)
    #         FTHG            FTAG      
    #Min.   :0.000   Min.   :0.000  
    #1st Qu.:1.000   1st Qu.:0.000  
    #Median :1.000   Median :1.000  
    #Mean   :1.497   Mean   :1.154  
    #3rd Qu.:2.000   3rd Qu.:2.000  
    #Max.   :9.000   Max.   :8.000  
    #
    #> summary(bvs)
    #         FTHG             FTAG       
    #Min.   :0.000   Min.   : 0.000  
    #1st Qu.:1.000   1st Qu.: 0.000  
    #Median :1.000   Median : 1.000  
    #Mean   :1.498   Mean   : 1.155  
    #3rd Qu.:2.000   3rd Qu.: 2.000  
    #Max.   :9.000   Max.   :13.000
    
  } else if(type == 'option10') {
    ## --------------------- option 10 ---------------------------------------------------
    
    ## Range within 1st quantile and 3rd quantile of scores set as the upper interval of 
    ##   rtmvnorm().
    ## Adjust the both lower and also upper from quantile which will be more centralize.
    ## set median value as lower and mean value as upper and expand both lower and upper intervals.
    
    xy1 <- apply(scores, 2, quantile)
    xy2 <- colMeans(scores)
    
    iteration <- 0
    x2 <- xy1[3]; x4 <- xy2[1]
    y2 <- xy1[3]; y4 <- xy2[2]
    
    bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                    lower = c(x2, y2), upper = c(x4, y4), 
                    algorithm = 'gibbs') %>% 
      matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
    
    prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> prepois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.250631 0.8354293
    #2 1.154427 1.077607 0.9334561
    #
    #$var
    #            FTHG        FTAG        FTHG.1        FTAG.1
    #FTHG  1.63115357 -0.06298846  2.053851e-02 -1.873448e-06
    #FTAG -0.06298846  1.27395462 -1.873448e-06  1.989772e-03
    
    # start looping
    while((prepois$mean$dif[1] > 1.0001) | (prepois$mean$dif[2] < 0.9999)) {
      bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                      lower = c(x2, y2), upper = c(x4, y4), 
                      algorithm = 'gibbs') %>% 
        matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
      
      bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG), FTAG = ifelse(FTAG <= 0, 0, FTAG))
      
      bvs %<>% mutate(FTHG = FTHG * colMeans(scores[,1])/colMeans(bvs[,1]), 
                      FTAG = FTAG * colMeans(scores[,2])/colMeans(bvs[,2]))
      
      prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      iteration = iteration + 1
      if(findInterval(prepois$mean$dif[1], c(0.9999, 1.0001)) == FALSE) {
        x2 <- x2 - 0.0001
        x4 <- x4 + 0.0001
      }
      if(findInterval(prepois$mean$dif[2], c(0.9999, 1.0001)) == FALSE) {
        y2 <- y2 - 0.0001
        y4 <- y4 + 0.0001
      }
      if(print.loop == TRUE) {
        cat('iteration :', iteration, '; x.low = ', x2, 'and y.low = ', y2, 
            '; x.up = ', x4, 'and y.up = ', y4, '\n')
        print(prepois)
      }
    }
    
    # rpois the scores
    bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                    FTAG = rpois(n = nrow(bvs), lambda = FTAG))
    
    pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                      mutate(dif = pre / obs), 
                    var = data.frame(var(scores), var(bvs)))
    #> pstpois
    #$mean
    #       obs      pre       dif
    #1 1.496992 1.386823 0.9264062
    #2 1.154427 1.159396 1.0043043
    #
    #$var
    #            FTHG        FTAG      FTHG.1      FTAG.1
    #FTHG  1.63115357 -0.06298846  2.14746117 -0.01808269
    #FTAG -0.06298846  1.27395462 -0.01808269  1.76433266
    
    #> summary(scores)
    #         FTHG            FTAG      
    #Min.   :0.000   Min.   :0.000  
    #1st Qu.:1.000   1st Qu.:0.000  
    #Median :1.000   Median :1.000  
    #Mean   :1.497   Mean   :1.154  
    #3rd Qu.:2.000   3rd Qu.:2.000  
    #Max.   :9.000   Max.   :8.000  
    #
    #> summary(bvs)
    #          FTHG             FTAG       
    #Min.   : 0.000   Min.   : 0.000  
    #1st Qu.: 0.000   1st Qu.: 0.000  
    #Median : 1.000   Median : 1.000  
    #Mean   : 1.387   Mean   : 1.159  
    #3rd Qu.: 2.000   3rd Qu.: 2.000  
    #Max.   :13.000   Max.   :11.000
    
  } else if(type == 'all') {
    ## --------------------- option all ---------------------------------------------------
    
      ## --------------------- option 1 ---------------------------------------------------
      ## adjusted the mean value of negative figures from rmvnorm() and mvrnorm and then plus.
      bvs <- org
      hgadj <- bvs$FTHG[bvs$FTHG <= 0] %>% sum(.)/length(bvs$FTHG[bvs$FTHG > 0]) %>% abs
      agadj <- bvs$FTAG[bvs$FTAG <= 0] %>% sum(.)/length(bvs$FTAG[bvs$FTAG > 0]) %>% abs
      
      # add the range of negative figure to the positive figure to raise the R^2.
      bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG + hgadj), 
                      FTAG = ifelse(FTAG <= 0, 0, FTAG + agadj))
      rm(hgadj, agadj)
      
      prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # rpois the scores
      bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                      FTAG = rpois(n = nrow(bvs), lambda = FTAG)) %>% 
        mutate(FTHG = ifelse(is.na(FTHG), 0, FTHG), FTAG = ifelse(is.na(FTAG), 0, FTAG))
      pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # return value of option 1
      opt1 <- list(pred = bvs, pstpois = pstpois)
      
      ## --------------------- option 2 ---------------------------------------------------
      ## adjusted the mean value of negative figures from rmvnorm() and mvrnorm and then minus.
      bvs <- org
      hgadj <- bvs$FTHG[bvs$FTHG <= 0] %>% sum(.)/length(bvs$FTHG[bvs$FTHG > 0]) %>% abs
      agadj <- bvs$FTAG[bvs$FTAG <= 0] %>% sum(.)/length(bvs$FTAG[bvs$FTAG > 0]) %>% abs
      
      # add the range of negative figure to the positive figure to raise the R^2.
      bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG - hgadj), 
                      FTAG = ifelse(FTAG <= 0, 0, FTAG - agadj)) %>% 
        mutate(FTHG =  FTHG * (colMeans(scores)[1] / mean(FTHG)), 
               FTAG = FTAG * (colMeans(scores)[2]/mean(FTAG)))
      rm(hgadj, agadj)
      
      prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # rpois the scores
      bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                      FTAG = rpois(n = nrow(bvs), lambda = FTAG)) %>% 
        mutate(FTHG = ifelse(is.na(FTHG), 0, FTHG), FTAG = ifelse(is.na(FTAG), 0, FTAG))
      pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # return value of option 2
      opt2 <- list(pred = bvs, pstpois = pstpois)
      
      ## --------------------- option 3 ---------------------------------------------------
      ## adjusted the mean value of negative figures from rmvnorm() and mvrnorm and then apply rnorm.
      bvs <- org %>% mutate(No = seq(nrow(.)))
      
      # start looping
      while(any(bvs$FTHG < 0)|any(bvs$FTAG < 0)) {
        hgadj0 <- bvs[c('No', 'FTHG')] %>% filter(FTHG <= 0)
        hgadj1 <- bvs[c('No', 'FTHG')] %>% filter(FTHG > 0) %>% 
          mutate(FTHG = rnorm(n = nrow(.), mean = mean(hgadj0$FTHG), sd = sd(hgadj0$FTHG)))
        agadj0 <- bvs[c('No', 'FTAG')] %>% filter(FTAG <= 0)
        agadj1 <- bvs[c('No', 'FTAG')] %>% filter(FTAG > 0) %>% 
          mutate(FTAG = rnorm(n = nrow(.), mean = mean(agadj0$FTAG), sd = sd(agadj0$FTAG)))
        hgadj <- suppressMessages(join(bvs[c('No')], hgadj1)) %>% tbl_df %>% 
          mutate(FTHG = ifelse(is.na(FTHG), 0, FTHG))
        agadj <- suppressMessages(join(bvs[c('No')], agadj1)) %>% tbl_df %>% 
          mutate(FTAG = ifelse(is.na(FTAG), 0, FTAG))
        rm(hgadj0, hgadj1, agadj0, agadj1)
        
        # add the range of negative figure to the positive figure to raise the R^2.
        bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG - hgadj$FTHG), 
                        FTAG = ifelse(FTAG <= 0, 0, FTAG - agadj$FTAG))
      }
      hgadj %<>% .[c('FTHG')]
      agadj %<>% .[c('FTAG')]
      bvs %<>% .[c('FTHG', 'FTAG')]
      
      rm(hgadj, agadj)
      prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      bvs %<>% mutate(FTHG = FTHG * colMeans(scores[,1])/colMeans(bvs[,1]), 
                      FTAG = FTAG * colMeans(scores[,2])/colMeans(bvs[,2]))
      
      # rpois the scores
      bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                      FTAG = rpois(n = nrow(bvs), lambda = FTAG))
      pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # return value of option 3
      opt3 <- list(pred = bvs, pstpois = pstpois)
      
      ## --------------------- option 4 ---------------------------------------------------
      ## apply min and max as adjusted which is similar with rmvnorm() and mvrnorm. Test the difference.
      bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                      lower = apply(scores, 2 , min), upper = apply(scores, 2 , max),  #adjuster
                      algorithm = 'gibbs') %>% 
        matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
      prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      bvs %<>% mutate(FTHG = FTHG * colMeans(scores[,1])/colMeans(bvs[,1]), 
                      FTAG = FTAG * colMeans(scores[,2])/colMeans(bvs[,2]))
      
      # rpois the scores
      bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                      FTAG = rpois(n = nrow(bvs), lambda = FTAG))
      pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # return value of option 4
      opt4 <- list(pred = bvs, pstpois = pstpois)
      
      ## --------------------- option 5 ---------------------------------------------------
      ## manual adjusted lower and upper but option 8 will be more accurate by apply while loops.
      bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                      lower = apply(scores, 2 , min), upper = c(3, 2.3), #adjuster
                      algorithm = 'gibbs') %>% 
        matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
      prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # rpois the scores
      bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                      FTAG = rpois(n = nrow(bvs), lambda = FTAG))
      pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # return value of option 5
      opt5 <- list(pred = bvs, pstpois = pstpois)
      
      ## --------------------- option 6 ---------------------------------------------------
      ## apply quantile 1st and 3rd as lower and upper.
      bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                      lower = c(1, 0), upper = c(2, 2.3), 
                      algorithm = 'gibbs') %>% 
        matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
      prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # rpois the scores
      bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                      FTAG = rpois(n = nrow(bvs), lambda = FTAG))
      pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # return value of option 6
      opt6 <- list(pred = bvs, pstpois = pstpois)
      
      ## --------------------- option 7 ---------------------------------------------------
      ## apply quantile 1st and 3rd as lower and upper.
      bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                      lower = apply(scores, 2 , quantile)[2,], upper = apply(scores, 2 , quantile)[4,], 
                      algorithm = 'gibbs') %>% 
        matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
      prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      bvs %<>% mutate(FTHG = FTHG * colMeans(scores[,1])/colMeans(bvs[,1]), 
                      FTAG = FTAG * colMeans(scores[,2])/colMeans(bvs[,2]))
      
      # rpois the scores
      bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                      FTAG = rpois(n = nrow(bvs), lambda = FTAG))
      pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # return value of option 7
      opt7 <- list(pred = bvs, pstpois = pstpois)
      
      ## --------------------- option 8 ---------------------------------------------------
      ## set min value as lower which is 0 but adjust the upper from mean value.
      xy <- colMeans(scores)
      iteration <- 0
      x <- xy[1]
      y <- xy[2]
      
      bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                      lower = apply(scores, 2 , min), upper = c(x, y), 
                      algorithm = 'gibbs') %>% 
        matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
      
      prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # start looping
      while((prepois$mean$dif[1] < 0.9999) | (prepois$mean$dif[2] < 0.9999)) {
        
        bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                        lower = apply(scores, 2 , min), upper = c(x, y), 
                        algorithm = 'gibbs') %>% 
          matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
        
        bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG), FTAG = ifelse(FTAG <= 0, 0, FTAG))
        
        bvs %<>% mutate(FTHG = FTHG * colMeans(scores[,1])/colMeans(bvs[,1]), 
                        FTAG = FTAG * colMeans(scores[,2])/colMeans(bvs[,2]))
        
        prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                          mutate(dif = pre / obs), 
                        var = data.frame(var(scores), var(bvs)))
        
        iteration = iteration + 1
        if(prepois$mean$dif[1] < 0.9999) {
          x <- x + 0.0001
        }
        if(prepois$mean$dif[2] < 0.9999) {
          y <- y + 0.0001
        }
        
        if(print.loop == TRUE) {
          cat('iteration :', iteration, '; x = ', x, 'and y = ', y, '\n')
          print(prepois)
        }
      }
      
      # rpois the scores
      bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                      FTAG = rpois(n = nrow(bvs), lambda = FTAG))
      
      pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # return value of option 8
      opt8 <- list(pred = bvs, pstpois = pstpois)
      
      ## --------------------- option 9 ---------------------------------------------------
      ## Range within 1st quantile and 3rd quantile of scores set as the upper interval of 
      ##   rtmvnorm().
      ## Adjust the both lower and also upper from quantile which will be more centralize.
      xy <- apply(scores, 2, quantile)

      # I choose iteration start expand from quantile 1 and quantile 3 will be more accurate 
      #   compare to start from min & max.
      x <- xy[,1]
      y <- xy[,2]
      
      iteration <- 0
      x2 <- x[2]; x4 <- x[4]
      y2 <- y[2]; y4 <- y[4]
      
      bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                      lower = c(x2, y2), upper = c(x4, y4), 
                      algorithm = 'gibbs') %>% 
        matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
      
      prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # start looping
      while((prepois$mean$dif[1] > 1.0001) | (prepois$mean$dif[2] < 0.9999)) {
        bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                        lower = c(x2, y2), upper = c(x4, y4), 
                        algorithm = 'gibbs') %>% 
          matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
        
        bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG), FTAG = ifelse(FTAG <= 0, 0, FTAG))
        
        prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                          mutate(dif = pre / obs), 
                        var = data.frame(var(scores), var(bvs)))
        
        iteration = iteration + 1
        if(findInterval(prepois$mean$dif[1], c(0.9999, 1.0001)) == FALSE) {
          x2 <- x2 - 0.0001
          x4 <- x4 + 0.0001
        }
        if(findInterval(prepois$mean$dif[2], c(0.9999, 1.0001)) == FALSE) {
          y2 <- y2 - 0.0001
          y4 <- y4 + 0.0001
        }
        if(print.loop == TRUE) {
          cat('iteration :', iteration, '; x.low = ', x2, 'and y.low = ', y2, 
              '; x.up = ', x4, 'and y.up = ', y4, '\n')
          print(prepois)
        }
      }
      
      # rpois the scores
      bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                      FTAG = rpois(n = nrow(bvs), lambda = FTAG))
      
      pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # return value of option 9
      opt9 <- list(pred = bvs, pstpois = pstpois)
      
      ## --------------------- option 10 ---------------------------------------------------
      ## Range within 1st quantile and 3rd quantile of scores set as the upper interval of 
      ##   rtmvnorm().
      ## Adjust the both lower and also upper from quantile which will be more centralize.
      ## set median value as lower and mean value as upper and expand both lower and upper intervals.
      
      xy1 <- apply(scores, 2, quantile)
      xy2 <- colMeans(scores)
      
      iteration <- 0
      x2 <- xy1[3]; x4 <- xy2[1]
      y2 <- xy1[3]; y4 <- xy2[2]
      
      bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                      lower = c(x2, y2), upper = c(x4, y4), 
                      algorithm = 'gibbs') %>% 
        matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
      
      prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # start looping
      while((prepois$mean$dif[1] > 1.0001) | (prepois$mean$dif[2] < 0.9999)) {
        bvs <- rtmvnorm(n = nrow(scores), mean = colMeans(scores), sigma = var(scores), 
                        lower = c(x2, y2), upper = c(x4, y4), 
                        algorithm = 'gibbs') %>% 
          matrix(ncol = 2, dimnames = list(NULL, c('FTHG', 'FTAG'))) %>% data.frame %>% tbl_df
        
        bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG), FTAG = ifelse(FTAG <= 0, 0, FTAG))
        
        bvs %<>% mutate(FTHG = FTHG * colMeans(scores[,1])/colMeans(bvs[,1]), 
                        FTAG = FTAG * colMeans(scores[,2])/colMeans(bvs[,2]))
        
        prepois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                          mutate(dif = pre / obs), 
                        var = data.frame(var(scores), var(bvs)))
        
        iteration = iteration + 1
        if(findInterval(prepois$mean$dif[1], c(0.9999, 1.0001)) == FALSE) {
          x2 <- x2 - 0.0001
          x4 <- x4 + 0.0001
        }
        if(findInterval(prepois$mean$dif[2], c(0.9999, 1.0001)) == FALSE) {
          y2 <- y2 - 0.0001
          y4 <- y4 + 0.0001
        }
        if(print.loop == TRUE) {
          cat('iteration :', iteration, '; x.low = ', x2, 'and y.low = ', y2, 
              '; x.up = ', x4, 'and y.up = ', y4, '\n')
          print(prepois)
        }
      }
      
      # rpois the scores
      bvs %<>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                      FTAG = rpois(n = nrow(bvs), lambda = FTAG))
      
      pstpois <- list(mean = bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
                        mutate(dif = pre / obs), 
                      var = data.frame(var(scores), var(bvs)))
      
      # return value of option 10
      opt10 <- list(pred = bvs, pstpois = pstpois)
      
      opts = list(opt0 = opt0, opt1 = opt1, opt2 = opt2, 
                  opt3 = opt3, opt4 = opt4, opt5 = opt5,
                  opt6 = opt6, opt7 = opt7, opt8 = opt8,
                  opt9 = opt9, opt10 = opt10)
      
      tmp <- list(data = scores, opts = opts)
      return(tmp)
      
  } else {
    
    stop('Kindly choose type from type = "option1" to type = "option10" or type = "all".')
  }
  
  ## --------------------- skip Poison models ------------------------------------------
  ## <s> I simply use the bivpois package to conduct a Monte Carlo simulation on the lambda 
  ##     values for scores modelling. </s>
  ## 
  ## Here I skip the Poisson model due to it measure the lambda value of every single team while 
  ##   we only need to randomize the scores. The offence and defence index of teams are not in 
  ##   consideration in this paper. The purpose of the research is based on how to follow the bets 
  ##   and generates more profit than the sportsbook consultancy company in this paper as mentioned 
  ##   in the paper couple times.
  ## 
  ## http://www.stat-athens.aueb.gr/~jbn/papers/paper14.htm
  #'@ if(file.exists('./function/bivpois.tar.gz') == FALSE) {
  #'@   download.file(url = 'https://cran.r-project.org/src/contrib/Archive/bivpois/bivpois_0.50-3.tar.gz', 
  #'@                 destfile = './function/bivpois.tar.gz', quiet = TRUE)
  #'@   untar('./function/bivpois.tar.gz', exdir = './function')
  #'@   file.remove('./function/bivpois.tar.gz')
  #'@ }
  #'@ suppressMessages(source('./function/bivpois/R/lm.dibp.R', local = TRUE))
  
  ## --------------------- Return Function ----------------------------------------
  
  tmp <- list(data = scores, pred = bvs, pstpois = pstpois)
  return(tmp)
}


