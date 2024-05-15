# 投注策略|投资战略和计数|机数造物鉴别

在此愚生从[7M](http://www.7msport.com)和[NowGoal.com](http://www.nowgoal.com)赔率资讯网上採撷赔率数据，欲知更多详情请查阅[WebDriver-DynamicWebpage-Scrapping.](https://github.com/scibrokes/webdriver-dynamicwebpage-scrapping)并且使用凯利标准计数|机数尤物，从模拟与回测中可以获利超过三成。

- [「猫城」在足彩投注模式|投资战略中，使用凯利标准计数|机数尤物（英）](https://github.com/scibrokes/kelly-criterion)
  + [「鄀客栈」在英超二零二一/二零二二年赛季中，使用凯利标准计数|机数尤物（英）](http://rpubs.com/englianhu/kelly_eng1112)
  + [「鄀客栈」在英超二零二二/二零二三年赛季中，使用凯利标准计数|机数尤物（英）](http://rpubs.com/englianhu/kelly_eng1213)
- [「猫城」投注策略|投资战略和计数|机数造物鉴别（英）](https://github.com/scibrokes/betting-strategy-and-model-validation)

## 一、简介

投注策略|投资战略和计数|机数造物鉴别，愚生尝试分析一家高频量化对冲基金（在此称之为”公司甲“）的投注策略并尝试跟单。
  
数据中`Return`（注单结算金额）和`PL`（盈利）并没有分类并扣除抽佣（佣金、价差、回馈、返水）`rebates`，佣金的计算方式是根据投注金额（并没有计算赢和输）达到一定的门槛后就会有指定比率的金额回馈。根据投注金额的多寡门槛表来支付佣金，是博彩庄和代理商之间合作占据市场份额的营销策略。

- 一般上，佣金`rebates`只出现在信用网市场亚洲博彩庄。
- 一般上，单注最高投注金额在现金网市场大概是一万港币，而信用网单注金额可以高达一百万港币。
- 一般上，只有那些出货量（也就是投注金额）多的总代理、中型或大型博彩庄且有信誉（结算后可以支付贸易来往）才会有高频量化对冲基金看上，因为高频量化对冲基金的投注金额非常多。

## 二、科研案例

### 第二章第一节、第一部

- [「猫城」自然语言分析（英）](http://rpubs.com/englianhu/natural-language-analysis)：Applied NLP to filter team name.

![AI - Soccer team name filtering](figure/20160918_171322.gif)

### 第二章第二节、第二部

- [Betting Strategy and Model Validation - Part I (*Sep-2016*)](https://englianhu.github.io/2016/09/Betting%20Strategy%20and%20Model%20Validation/Betting_Strategy_and_Model_Validation_-_Part_01.html), ([Alternative Link](http://rpubs.com/englianhu/208637)) : Analyse the staking breakdown.

![Data analysis on soccer betting](figure/20160918_172444.gif)

### 第二章第三节、第三部

- [Betting Strategy and Model Validation - Part II](https://englianhu.github.io/2017/10/Betting_Strategy_and_Model_Validation_-_Part_02/) (Notes : *The latest Rmd files lost but only html file available*)
    
  + Kindly click on [*regressionApps*](https://beta.rstudioconnect.com/content/1807/) to use the ShinyApp
  + ![regressionApps](figure/20160928_021252.gif)
  + Kindly click on [*KellyApps*](https://beta.rstudioconnect.com/content/2311/) to use the ShinyApp
  + <iframe width="560" height="315" src="https://www.youtube.com/embed/42NOxuYjOQo" frameborder="0" allowfullscreen></iframe>
  + <iframe width="560" height="315" src="https://www.youtube.com/embed/NDUkg4jHmiA" frameborder="0" allowfullscreen></iframe>
  + <s>Kindly refer to [English Soccer League] for analysis on only English soccer which is pretest on my personal research.</s>
  + You are feel free to browse over the summary of Kelly investment fund [BRSum](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.csv) ([BRSum.Op](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.Op.csv), [BRSum.Hi](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.Hi.csv), [BRSum.Lo](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.Lo.csv), [BRSum.Cl](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.Cl.csv), [BRSum.Vo](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.Vo.csv), [BRSum.Ad](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/data/BRSum.Ad.csv)).
  + [Extention]() : summarise the comparison of Kelly fund due to the BSMV II heavily loading...

## 三、备份旧著作

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

## 四、科研前瞻

- Part II in [「猫城」Application of Kelly Criterion model in Sportsbook Investment](https://github.com/scibrokes/kelly-criterion) which apply shinyApp.
- Algorithmic scrapping team name and filtering, matching team name.
- Writing a website to apply the staking model which is similar with <http://www.matchodds.org/>.

## 五、参考文献

一、[**Odds Modelling and Testing Inefficiency of Sports Bookmakers : Rmodel** by ®γσ, Eng Lian Hu (2016)](https://github.com/scibrokes/odds-modelling-and-testing-inefficiency-of-sports-bookmakers)
二、[**Apply Kelly-Criterion on English Soccer 2011/12 to 2012/13** *by ®γσ, Eng Lian Hu (2014)*](https://github.com/scibrokes/kelly-criterion)
三、[**Creating a Profitable Betting Strategy for Football by Using Statistical Modelling** *by Niko Marttinen (2006)*](https://github.com/scibrokes/betting-strategy-and-model-validation/blob/master/references/Creating%20a%20Profitable%20Betting%20Strategy%20for%20Football%20by%20Using%20Statistical%20Modelling.pdf)

---
[![](诸子百家考工记/世博量化.png){height=14} Sςιβrοκεrs Trαdιηg®](http://www.scibrokes.com)<br>
[![](诸子百家考工记/世博量化.png){height=14} 世博量化®](http://www.scibrokes.com)企业知识产权及版权所有，盗版必究。**]{style="color:RoyalBlue"}
