## ----setup, echo=FALSE, include=FALSE--------------------------------------------
library(knitr) #to produce dynamic reports, allowing the embedding of R code, its results, and text into a single document
# knitr::opts_chunk$set(echo = TRUE)
library(haven) #to import and export data in SPSS, Stata, and SAS formats, enhancing interoperability with other statistical software
library(rmarkdown) #is used to convert files that contain text and R code into documents of various formats, such as HTML, PDF, and Word
library(writexl) #to export tabular data from R to Excel files
library(Hmisc) #provides a collection of functions for data manipulation, statistical analysis, and graph creation, facilitating various stages of the analytical process
library(naniar) #exploring and visualizing missing data in the dataset
library(here)


# Define working directory
rm(list = ls())
here()
getwd()






## ----Libraries & Data, echo=FALSE,results='hide',message=FALSE , include=FALSE----
library(tidyverse) #provides a set of tools for data manipulation, visualization, and modeling
library(readxl) # allows us to read Excel files (.xls or .xlsx) directly into R. 
library(visdat) #to explore our dataset through visualizations.
library(stargazer) #to produce beautiful LaTeX or HTML tables and descriptive statistics from R statistical output
library(GGally) #to extend ggplot2 functionality to create a scatterplot matrix
library(car)
library(lmtest)
library(sandwich)
library(margins)
library(caret)
library(randomForest)


## --------------------------------------------------------------------------------
nlswork <- as.data.frame(read_excel("nlswork.xlsx"))



## --------------------------------------------------------------------------------
nlswork_no_na <- drop_na(nlswork)



## ----include=FALSE---------------------------------------------------------------
#Select a subset of variables 
  db_ols<- nlswork_no_na %>% 
 select(age, ln_wage, collgrad, union, hours)



## ----include=FALSE---------------------------------------------------------------
# Model 1
    ols1 <- lm(data = db_ols, ln_wage ~ age)
        summary(ols1)



## ----include=FALSE---------------------------------------------------------------
# Model 2
    ols2 <- lm(data = db_ols, ln_wage ~ .)

#Alternative ols2<-lm(data = db_ols, ln_wage ~ age + collgrad + union+ hours)
    

      summary(ols2)



## ----echo=FALSE, warning=FALSE, message=FALSE------------------------------------
library(caret)
importance <- varImp(ols2, scale = TRUE)
print(importance)



## ----echo=FALSE, warning=FALSE, message=FALSE------------------------------------

stepwise.model <- step(ols2, direction = "backward")

summary(stepwise.model)


## ----Regressions, echo=FALSE, warning=FALSE, results='asis'----------------------
      stargazer(ols1,ols2,title = "Regression analysis", 
                model.numbers = FALSE,
                column.labels = c("Model (1)","Model (2)"),
                label = "regressions",
                table.placement = "!ht",
                notes.append = TRUE,
                notes.align="l",
                notes="Standard errors in parentheses.",
                header = FALSE,
                no.space = TRUE,
                #covariate.labels = c("Grade","Experience","Experienced sqrd."),
                omit = c("Constant"),
                omit.stat = c("adj.rsq","f","ser"),
                digits = 3,
                digits.extra = 6,
                omit.yes.no = c("Constant",""),
                dep.var.caption="",
                dep.var.labels.include = FALSE,
                type = "latex",
                style = "qje"
                )



## ----echo=FALSE, message=FALSE,include=TRUE, warning=FALSE-----------------------
    
      #linearHypothesis(ols2,c("age=0"))

      linearHypothesis(ols2, "union = collgrad")
      
      #test if union is equal to collgrad and if they are equal to zero
      
      #linearHypothesis(ols2, c("collgrad = 0", "union = 0"))


## ----echo=FALSE, message=FALSE,include=TRUE, warning=FALSE-----------------------
    
      AIC(ols1)
      BIC(ols1)
    
      AIC(ols2)
      BIC(ols2)



## ----echo=FALSE, message=FALSE,include=TRUE, warning=FALSE-----------------------
    
      car::vif(ols2)



## ----echo=FALSE, message=FALSE,include=TRUE, warning=FALSE-----------------------
      
plot(ols2$fitted.values, resid(ols2))
abline(h=0, col="red")



## ----echo=FALSE, message=FALSE,include=TRUE, warning=FALSE-----------------------
          bptest(ols2)



## --------------------------------------------------------------------------------

robust_ols2 <- coeftest(ols2, vcov. = vcovHC(ols2, type="HC1"))
print(robust_ols2)


## ----Specification issues, echo=FALSE, warning=FALSE, results='asis'-------------

stargazer(ols2, robust_ols2,
          se = list(NULL, sqrt(diag(vcovHC(ols2, type = "HC1")))),
          title = "Comparison of OLS and Robust Regression Models",
          header = FALSE, 
          model.names = FALSE,
         # robust = TRUE,
          column.labels = c("OLS", "Robust OLS"),
          intercept.bottom = FALSE,
       #   intercept.top = TRUE,
          type = "latex",
          style = "qje")



## ----echo=FALSE------------------------------------------------------------------
# Probit
probit <- glm(union ~ ., data = db_ols, family = binomial(link = "probit"))
summary(probit)



## ----echo=FALSE, include=FALSE---------------------------------------------------
# Logit
logit <- glm(union ~ ., data=db_ols, family = binomial(link = "logit"))
summary(logit)



## ----echo=FALSE, warning=FALSE, results='asis'-----------------------------------
#export results 2 models

stargazer(logit, probit, type = "latex",
          title = "Comparative Table of Logit and Probit Models",
          style = "qje")


## ----echo=FALSE, include=TRUE----------------------------------------------------
m2 <- margins(probit)
summary(m2)



## ----echo=FALSE, include=TRUE----------------------------------------------------
m3 <- margins(logit)
summary(m3)


## ----include=FALSE---------------------------------------------------------------
#CONVERSÃO DO CÓDIGO RMARKDOWN PARA O CÓDIGO R

#options(knitr.duplicate.label = "allow")

#knitr::purl(input= "RIntro2.Rmd", output = "RIntro2.R")



## ----warning=FALSE---------------------------------------------------------------
card<-as.data.frame(read_excel("card.xlsx"))


## ----warning=FALSE---------------------------------------------------------------
#BEGIN SOLUTION

card_no_na <- na.omit(card)


#END SOLUTION


## ----warning=FALSE---------------------------------------------------------------
#BEGIN SOLUTION


#END SOLUTION


## ----warning=FALSE---------------------------------------------------------------
#BEGIN SOLUTION


#END SOLUTION


## ----warning=FALSE---------------------------------------------------------------
#BEGIN SOLUTION


#END SOLUTION


## ----warning=FALSE---------------------------------------------------------------
#BEGIN SOLUTION

#END SOLUTION


## ----warning=FALSE---------------------------------------------------------------
average_wage <- mean(card_no_na$wage)

# Create a binary variable (high wage): 1 if wage is above average, 0 otherwise
card_no_na <- card_no_na %>%
  mutate(high_wage = ifelse(wage > average_wage, 1, 0))



## ----warning=FALSE---------------------------------------------------------------
#BEGIN SOLUTION

#END SOLUTION


## ----warning=FALSE---------------------------------------------------------------
#BEGIN SOLUTION

#END SOLUTION


## ----warning=FALSE---------------------------------------------------------------
#BEGIN SOLUTION

#END SOLUTION

