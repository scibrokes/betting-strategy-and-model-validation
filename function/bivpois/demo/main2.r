cat('\n')
cat('\n')
cat('\n')
cat ('---------------------------------------------', '\n')
cat ('Available Examples in DEMO of bivpois Package', '\n')
cat ('---------------------------------------------', '\n')
cat('\n')
cat('\n')
cat('\n')
choice1 <- menu(c(
'Example 1 (Simulated Data - Bivariate Poisson Models)',
'Example 2 (Simulated Data - Diagonal Inflated Poisson Models)',
'Example 3 (Health Data)',
'Example 4 (Italian Serie A data)',) , title = "")



file<-paste('ex', '.r', sep=as.character(choice1) )
file<-system.file('demo', file, package='bivpois')

if (choice1==0) cat('*** EXITING BIVPOIS DEMO *** \n') else source(file,echo=TRUE)