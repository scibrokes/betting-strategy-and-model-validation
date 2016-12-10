simulateKelly <- function(mbase, lPmean = 'runif', type = 'vKelly', type = 'flat', 
                          weight.stakes = 1, weight = 1, maxit = 100, parallel = FALSE) {
  ## Comparison of various fractional Kelly models
  ## mbase = a converted data frame by using readfirmData() and arrfirmData()s.
  ## type = 'flat', 'weight' or 'dynamic' for static or dynamic which is simulate the process 
  ##   to get the optimal weight  parameter.
  ## weight.stakes = a numeric weight parameter in single or vector format. Manual weight 
  ##   only work on 'flat' type.
  ## weight = a numeric weight parameter in single or vector format. Manual weight only work 
  ##   on 'flat' type.
  ## maxit = maximum iteration for the dynamic process. Only work on 'dynamic' type.
  ##   Once choose 'dynamic'.
  ## type = 'flat' : both 'weight.stakes' and 'weight' will only usable for 'flat' type.
  ## type = 'weight' : Once you choose type = 'weight', both 'weight.stakes' and 'weight' 
  ##   will auto ignore all input value but using previous year data to get a constant 
  ##   weight parameter.
  ## type = 'dynamic' : Once you choose 'dynamic' type, both 'weight.stakes' and 'weight' 
  ##   will auto ignore all input value but using data from previous until latest staked 
  ##   match to generates a vector of weighted parameters.
  ## type = 'vKelly' or type = 'vKelly2' in order to simulate the vKelly() or vKelly2(). By 
  ##   the way, please select if the mean value of the league risk profile resampling by 
  ##   `runif` of `rnorm`.
  ## lPmean = 'runif' or lPmean = 'rnorm'.
  
  ## --------------------- Load packages --------------------------------
  suppressMessages(library('formattable'))
  suppressMessages(library('plyr'))
  suppressMessages(library('tidyverse'))
  suppressMessages(library('BBmisc'))
  suppressMessages(source('./function/vKelly.R', local = TRUE))
  suppressMessages(source('./function/vKelly2.R', local = TRUE))
  suppressMessages(source('./function/leagueRiskProf.R', local = TRUE))
  
  ## --------------------- Start Multivariate Analysis --------------------------------
  ## I tried to apply multivariate normal distribution via MASS package but due to zero 
  ##   inflated issue, the variance is very high. mvtnorm package might workable as well.
  ## 
  library('MASS')
  scores <- mbase[c('FTHG', 'FTAG')]
  
  #'@ mvrnorm(n = nrow(scores), mu = colMeans(scores), Sigma = as.matrix(var(scores))) %>% 
  #'@   data.frame %>% tbl_df %>% mutate(FTHG = rpois(n = length(FTHG), lambda = FTHG), 
  #'@                                    FTAG = rpois(n = length(FTAG), lambda = FTAG)) %>% 
  #'@   mutate(FTHG = ifelse(is.na(FTHG), 0, FTHG), FTAG = ifelse(is.na(FTAG), 0, FTAG)) %>% 
  #'@   colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% mutate(dif = pre - obs)
  #
  #       obs      pre        dif
  #1 1.496992 1.577956 0.08096456
  #2 1.154427 1.246085 0.09165753
  #
  #       obs      pre        dif
  #1 1.496992 1.554841 0.05784923
  #2 1.154427 1.227719 0.07329193
  #
  #       obs      pre        dif
  #1 1.496992 1.573158 0.07616612
  #2 1.154427 1.239678 0.08525149
  
  #'@ bvs <- mvrnorm(n = nrow(scores), mu = colMeans(scores), 
  #'@                Sigma = as.matrix(var(scores))) %>% data.frame %>% tbl_df
  #'@ hgadj <- bvs$FTHG[bvs$FTHG <= 0] %>% sum(.)/length(bvs$FTHG[bvs$FTHG > 0]) %>% abs
  #'@ agadj <- bvs$FTAG[bvs$FTAG <= 0] %>% sum(.)/length(bvs$FTAG[bvs$FTAG > 0]) %>% abs
  # times the range of negative figure to the positive figure to raise the R^2.
  #'@ bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG * (1 + hgadj)), 
  #'@                 FTAG = ifelse(FTAG <= 0, 0, FTAG * (1 + agadj)))
  #'@ rm(hgadj, agadj)
  #'@ bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% 
  #'@   mutate(dif = pre - obs)
  #'@ data.frame(var(scores), var(bvs))
  #       obs      pre         dif
  #1 1.496992 1.437422 -0.05956946
  #2 1.154427 1.111828 -0.04259867
  #
  #            FTHG        FTAG     FTHG.1     FTAG.1
  #FTHG  1.63115357 -0.06298846  1.1089662 -0.0421898
  #FTAG -0.06298846  1.27395462 -0.0421898  0.7705333
  
  ## option 1
  bvs <- mvrnorm(n = nrow(scores), mu = colMeans(scores), Sigma = as.matrix(var(scores))) %>% 
    data.frame %>% tbl_df
  hgadj <- bvs$FTHG[bvs$FTHG <= 0] %>% sum(.)/length(bvs$FTHG[bvs$FTHG > 0]) %>% abs
  agadj <- bvs$FTAG[bvs$FTAG <= 0] %>% sum(.)/length(bvs$FTAG[bvs$FTAG > 0]) %>% abs
  
  # add the range of negative figure to the positive figure to raise the R^2.
  bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG + hgadj), 
                  FTAG = ifelse(FTAG <= 0, 0, FTAG + agadj))
  rm(hgadj, agadj)
  bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% mutate(dif = pre - obs)
  data.frame(var(scores), var(bvs))
  
  bvs <- bvs %>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                        FTAG = rpois(n = nrow(bvs), lambda = FTAG)) %>% 
    mutate(FTHG = ifelse(is.na(FTHG), 0, FTHG), FTAG = ifelse(is.na(FTAG), 0, FTAG))
  bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% mutate(dif = pre - obs)
  data.frame(var(scores), var(bvs))
  
  ## option 2
  bvs <- mvrnorm(n = nrow(scores), mu = colMeans(scores), Sigma = as.matrix(var(scores))) %>% 
    data.frame %>% tbl_df
  hgadj <- bvs$FTHG[bvs$FTHG <= 0] %>% sum(.)/length(bvs$FTHG[bvs$FTHG > 0]) %>% abs
  agadj <- bvs$FTAG[bvs$FTAG <= 0] %>% sum(.)/length(bvs$FTAG[bvs$FTAG > 0]) %>% abs
  
  # add the range of negative figure to the positive figure to raise the R^2.
  bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG - hgadj), 
                  FTAG = ifelse(FTAG <= 0, 0, FTAG - agadj)) %>% 
    mutate(FTHG =  FTHG * (colMeans(scores)[1] / mean(FTHG)), 
           FTAG = FTAG * (colMeans(scores)[2]/mean(FTAG)))
  rm(hgadj, agadj)
  bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% mutate(dif = pre - obs)
  data.frame(var(scores), var(bvs))
  
  bvs <- bvs %>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                        FTAG = rpois(n = nrow(bvs), lambda = FTAG)) %>% 
    mutate(FTHG = ifelse(is.na(FTHG), 0, FTHG), FTAG = ifelse(is.na(FTAG), 0, FTAG))
  bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% mutate(dif = pre - obs)
  data.frame(var(scores), var(bvs))
  
  ## option 3
  bvs <- mvrnorm(n = nrow(scores), mu = colMeans(scores), Sigma = as.matrix(var(scores))) %>% 
    data.frame(No = seq(nrow(.)), .) %>% tbl_df
  
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
  bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% mutate(dif = pre - obs)
  data.frame(var(scores), var(bvs))
  
  bvs %<>% mutate(FTHG = FTHG * colMeans(scores[,1])/colMeans(bvs[,1]), 
                 FTAG = FTAG * colMeans(scores[,2])/colMeans(bvs[,2]))
  
  bvs <- bvs %>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), 
                        FTAG = rpois(n = nrow(bvs), lambda = FTAG))
  bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% mutate(dif = pre - obs)
  data.frame(var(scores), var(bvs))
  
  ## option 4
  bvs <- mvrnorm(n = nrow(scores), mu = colMeans(scores), Sigma = as.matrix(var(scores))) %>% 
    data.frame(No = seq(nrow(.)), .) %>% tbl_df
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
  hgadj %<>% .[c('FTHG')]
  agadj %<>% .[c('FTAG')]
  bvs %<>% .[c('FTHG', 'FTAG')]
  
  # add the range of negative figure to the positive figure to raise the R^2.
  bvs %<>% mutate(FTHG = ifelse(FTHG <= 0, 0, FTHG - hgadj), FTAG = ifelse(FTAG <= 0, 0, FTAG - agadj)) %>% mutate(FTHG =  FTHG * (colMeans(scores)[1] / mean(FTHG)), FTAG = FTAG * (colMeans(scores)[2]/mean(FTAG)))
  rm(hgadj, agadj)
  bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% mutate(dif = pre - obs)
  data.frame(var(scores), var(bvs))
  #obs      pre dif
  #1 1.496992 1.496992   0
  #2 1.154427 1.154427   0
  #FTHG        FTAG      FTHG.1      FTAG.1
  #FTHG  1.63115357 -0.06298846  0.90239003 -0.02104512
  #FTAG -0.06298846  1.27395462 -0.02104512  0.62889288
  bvs <- bvs %>% mutate(FTHG = rpois(n = nrow(bvs), lambda = FTHG), FTAG = rpois(n = nrow(bvs), lambda = FTAG)) %>% mutate(FTHG = ifelse(is.na(FTHG), 0, FTHG), FTAG = ifelse(is.na(FTAG), 0, FTAG))
  bvs %>% colMeans %>% data.frame(obs = colMeans(scores), pre = .) %>% mutate(dif = pre - obs)
  data.frame(var(scores), var(bvs))
  
  ## I simply use the bivpois package to conduct a Monte Carlo simulation on the lambda values for scores modelling.
  ## http://www.stat-athens.aueb.gr/~jbn/papers/paper14.htm
  if(file.exists('./function/bivpois.tar.gz') == FALSE) {
    download.file(url = 'https://cran.r-project.org/src/contrib/Archive/bivpois/bivpois_0.50-3.tar.gz', 
                  destfile = './function/bivpois.tar.gz', quiet = TRUE)
    untar('./function/bivpois.tar.gz', exdir = './function')
    file.remove('./function/bivpois.tar.gz')
  }
  suppressMessages(source('./function/bivpois/R/lm.dibp.R', local = TRUE))
  ## --------------------- End Multivariate Analysis --------------------------------
  
  
  ## --------------------- Data validation -------------------------------- 
  if(!is.data.frame(mbase)) stop('Kindly apply the readfirmData() and arrfirmData() 
                                 in order to turn the data into a fittable data frame.')
  
  if(!is.data.frame(leagueProf)) stop('Kindly insert a data frame of league risk profile which list the min, median, sd and max stakes.')
  if(!is.logical(parallel)) parallel <- as.logical(as.numeric(parallel))
  
  #'@ mbase <- mbase[order(mbase$TimeUS, mbase$No.x, decreasing = TRUE),]
  attr(mbase$Home,'levels') <- levels(factor(mbase$Home))
  attr(mbase$Away,'levels') <- levels(factor(mbase$Away))
  attr(mbase$Home,'contrasts') <- contrasts(C(factor(mbase$Home),sum))
  attr(mbase$Away,'contrasts') <- contrasts(C(factor(mbase$Away),sum))
  
  if(type != 'flat' & type != 'weight' & type != 'dynamic1') {
    
    stop('Kindly choose "flat", "weight" or "dynamic" for parameter named "type". You can choose 
         fit the "weight" parameter controller for both models.')
    
  } else {
    if(type == 'dynamic') {
      
      wt <- data_frame(No = seq(nrow(mbase)))
      
      if(is.null(weight.stakes)) {
        wt$weight.stakes <- 1
      } else {
        if(!is.vector(weight.stakes)) {
          stop('Kindly insert a range of vector or single numeric value as weight.stakes parameter.')
        } else {
          if(is.vector(weight.stakes)) wt$weight.stakes <- weight.stakes
        }
      }
      
      if(is.null(weight)) {
        wt$weight <- 1
      } else {
        if(!is.vector(weight)) {
          stop('Kindly insert a range of vector or single numeric value as weight parameter.')
        } else {
          if(is.vector(weight)) wt$weight <- weight
        }
      }
      
      if(is.null(maxit)) {
        maxit <- 1
      } else {
        if(!is.numeric(maxit)) {
          stop('Kindly insert a numeric value as maximum iteration parameter.')
        } else {
        maxit <- maxit
        }
      }
    } else {
      wt <- data_frame(No = seq(nrow(mbase)))
      
      if(is.null(weight.stakes)) {
        wt$weight.stakes <- 1
      } else {
        if(!is.vector(weight.stakes)) {
          stop('Kindly insert a range of vector or single numeric value as weight.stakes parameter.')
        } else {
          if(is.vector(weight.stakes)) wt$weight.stakes <- weight.stakes
        }
      }
      
      if(is.null(weight)) {
        wt$weight <- 1
      } else {
        if(!is.vector(weight)) {
          stop('Kindly insert a range of vector or single numeric value as weight parameter.')
        } else {
          if(is.vector(weight)) wt$weight <- weight
        }
      }
      
      if(is.null(maxit)) {
        maxit <- 1
      } else {
        if(!is.vector(maxit)) stop('Kindly insert a range of vector or single numeric value as maximum iteration parameter.')
      }
    }
  }
  
  if(type != 'vKelly' & type != 'vKelly2') {
    stop('Kindly choose type = "vKelly" or type = "vKelly2" for simulation.')
  } else {
    
    ## risk management
    ## dynamic variance staking risk management similar with idea from bollinger bands.
    #'@ $$ = k^2 * r^2 * f^2$$
    ## leave it as next study in [Application of Kelly Criterion model in Sportsbook Investment - Part II](https://github.com/scibrokes/kelly-criterion)
    
    K <- list()
    for(i in maxit) {
      
      ## wrong... bivariate normal distribution required.
      ## chrome-extension://oemmndcbldboiebfnladdacbdfmadadm/https://cran.r-project.org/web/packages/mvtnorm/mvtnorm.pdf
      ## chrome-extension://oemmndcbldboiebfnladdacbdfmadadm/https://cran.r-project.org/web/packages/mnormt/mnormt.pdf
      ##
      ## Remarks :
      ## I directly apply the model8 as refer to below paper to simulate a final scores of a soccer match.
      ## chrome-extension://oemmndcbldboiebfnladdacbdfmadadm/http://tolstoy.newcastle.edu.au/R/e8/help/att-6544/karlisntzuofras03.pdf
      
      mbase %>% mutate(FTHG = rpois(length(FTHG), rnorm(mean(FTHG), sd(FTHG))), 
                        FTAG = rpois(length(FTAG), rnorm(mean(FTAG), sd(FTAG))))
      ## --------------- start wrong ------------------------
      > dat %>% mutate(FTHG = rpois(length(FTHG), rnorm(mean(FTHG), sd(FTHG))), 
                       FTAG = rpois(length(FTAG), rnorm(mean(FTAG), sd(FTAG))), DIFF = FTHG - FTAG) %>% select(FTHG, FTAG, DIFF) %>% apply(2, mean, na.rm = TRUE)
      
      ## ------------- end wrong ----------------------
      
      lProf <- leagueProf %>% 
        mutate(mean = ifelse(lPmean == 'runif', runif(mean, min, max), 
                      ifelse(lPmean == 'rnorm', rnorm(mean, sd), 0)))
      mb <- join(mbase, lProf) %>% mutate(Stakes = mean)
      
      if(Kelly == 'stakes') K[[i]] <- vKelly(mb, weight.stakes = weight.stakes, weight = weight)
      if(Kelly == 'prob') K[[i]] <- vKelly2(mb, weight.stakes = weight.stakes, weight = weight)
    }
  }
  return(list(K, leagueProf = leagueProf, lPmean = lPmean, Kelly = Kelly, maxit = maxit))
}