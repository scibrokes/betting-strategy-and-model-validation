# Betting Strategy and Model Validation

## 1. Summary

  Betting Strategy and Model Validation, I analyse the staking model of a sportsbook agency which follow bets from consultancy firm A.
  
  The `rebates` value inside the dataset doesn't count in `Return` and `PL` since it is not includes in win/loss profit but only awarded upon hit a certain amount of stakes. `Rebates` is a marketing strategy for bankers and also agent to fight for revenue and occupy the market shares.
  
  - Normally `rebates` only offers by credit market sportsbook makers.
  - The maximum stakes per bet of `cash market` normally only up to HKD10,000 while credit market can up to millions HKD1,000,000.
  - Normally only high volume agents, small bookmakers and also high volume sportbook consultancy firms will able to get the `rebates`.

## 2. Case Study

### 2.1 Section A

  - [Natural Language Analysis](http://rpubs.com/englianhu/natural-language-analysis) : Applied NLP to filter team name.

![AI - Soccer team name filtering](figure/20160918_171322.gif)

### 2.2 Section B

  - [Betting Strategy and Model Validation - Part I (*Sep-2016*)](https://englianhu.github.io/2016/09/Betting%20Strategy%20and%20Model%20Validation/Betting_Strategy_and_Model_Validation_-_Part_01.html), ([Alternative Link](http://rpubs.com/englianhu/208637)) : Analyse the staking breakdown.

![Data analysis on soccer betting](figure/20160918_172444.gif)

### 2.3 Section C

  - [Betting Strategy and Model Validation - Part II](https://englianhu.github.io/2017/10/Betting_Strategy_and_Model_Validation_-_Part_02/) (Notes : *The latest Rmd files lost but only html file available*)
    
    + Kindly click on [*regressionApps*](https://beta.rstudioconnect.com/content/1807/) to use the ShinyApp
    + ![regressionApps](figure/20160928_021252.gif)
    + Kindly click on [*KellyApps*](https://beta.rstudioconnect.com/content/2311/) to use the ShinyApp
    + <iframe width="560" height="315" src="https://www.youtube.com/embed/42NOxuYjOQo" frameborder="0" allowfullscreen></iframe>
    + <iframe width="560" height="315" src="https://www.youtube.com/embed/NDUkg4jHmiA" frameborder="0" allowfullscreen></iframe>
    + <s>Kindly refer to [English Soccer League] for analysis on only English soccer which is pretest on my personal research.</s>
    + You are feel free to browse over the summary of Kelly investment fund [BRSum](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.csv) ([BRSum.Op](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.Op.csv), [BRSum.Hi](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.Hi.csv), [BRSum.Lo](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.Lo.csv), [BRSum.Cl](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.Cl.csv), [BRSum.Vo](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.Vo.csv), [BRSum.Ad](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.Ad.csv)).
    + [Extention]() : summarise the comparison of Kelly fund due to the BSMV II heavily loading...
    
## 3. Old File

  I splited below file to be **two part** as list above since there are a lot of gragh and high volume dataset. The `Return` and `PL` columns suddenly noticed wrong figure and cause the profit of below files are not accurate. Secondly, the old files are not completed as it only stop at section 3 which just a breakdown display and summary of staking data but section 4 and so fort analyse the dataset.

  Kindly refer to new files in section [2. Case Study] to know the profit and return of firm A from agent A.

  - [Betting Strategy and Model Validation (Old)](http://rpubs.com/englianhu/betting-strategy-and-model-validation) (*Remarks: Initially one file, but due to file size and finally seperate to few sections and files.*)
  - [Betting Strategy and Model Validation - Part I (*Aug-2016*)](http://englianhu.github.io/2016/08/Betting%20Strategy%20and%20Model%20Validation/Betting_Strategy_and_Model_Validation_-_Part_01.html)
  - Miscellaneous : [Introduction of R Packages](http://rpubs.com/englianhu/introduction-of-r-packages)
  - Miscellaneous : [Introduction of R Packages (Shiny)](https://beta.rstudioconnect.com/content/2291/)
  - Miscellaneous : [Unable display highchart at html format](http://rpubs.com/englianhu/highcharter-issue)
  - [Betting Strategy and Model Validation - Part II (*Sep-2017*)](http://rpubs.com/englianhu/240073), ([Alternative Link](http://englianhu.github.io/2016/09/Betting%20Strategy%20and%20Model%20Validation/Betting_Strategy_and_Model_Validation_-_Part_02.html))
  - Due to **KellyApps** shiny app heavily loading, below video is the original file.
  
<iframe width="560" height="315" src="https://www.youtube.com/embed/TEhmN-Of--Y" frameborder="0" allowfullscreen></iframe>

## 4. Future Work

 - Part II in [Application of Kelly Criterion model in Sportsbook Investment](https://github.com/scibrokes/kelly-criterion) which apply shinyApp.
 - Algorithmic scrapping team name and filtering, matching team name.
 - Writing a website to apply the staking model which is similar with <http://www.matchodds.org/>.

## 5. Reference

1. [**Odds Modelling and Testing Inefficiency of Sports Bookmakers : Rmodel** by ®γσ, Eng Lian Hu (2016)](https://github.com/scibrokes/odds-modelling-and-testing-inefficiency-of-sports-bookmakers)
2. [**Apply Kelly-Criterion on English Soccer 2011/12 to 2012/13** *by ®γσ, Eng Lian Hu (2014)*](https://github.com/scibrokes/kelly-criterion)
3. [**Creating a Profitable Betting Strategy for Football by Using Statistical Modelling** *by Niko Marttinen (2006)*](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/references/Creating%20a%20Profitable%20Betting%20Strategy%20for%20Football%20by%20Using%20Statistical%20Modelling.pdf)

---

**Powered by - Copyright© Intellectual Property Rights of <img src='figure/oda-army.jpg' width='24'> [Scibrokes®](http://www.scibrokes.com)**
