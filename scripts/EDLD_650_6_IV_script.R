######################################################
## Script Name: EDLD_650_6_IV_script.R
## Project Title: EDLD 650 Winter 2022
## Author: David Liebowitz
## Created: 2/4/22
## Last update: 2/4/222
## Purpose: This script imports the Angrist et al data and conducts randomization assumption checks. It then conducts 
## initial analysis of the endogenous relationship between 8th grade completion and voucher use. Then, it proceeds through 
## some visualization and estimation of ITT estimates before implementing an IV estimator to return the causal effect of 
## using a voucher on 8th grade completion
## Inputs: ch11_PACES.csv 
######################################################

# Define graphing colors
red_pink <- "#e64173"
turquoise <- "#20B2AA"
orange <- "#FFA500"
red <- "#fb6107"
blue <- "#3b3b9a"
green <- "#8bb174"
grey_light <- "grey70"
grey_mid <- "grey50"
grey_dark <- "grey20"
purple <- "#6A5ACD"
slate <- "#314f4f"

library(pacman)

# These are the packages you will need for the analyses 
p_load(here, tidyverse, DT, ggplot2, xaringan, knitr, kableExtra, modelsummary, stargazer, xaringanthemer, ggthemes, fixest, haven, arsenal)

# You will want to have created a folder for the course and an R project within that folder
# Then, create a folder within your course folder entitled "code"
# Finally, save this R script within the code folder

# This command tells R where your script is and allows you to point it to other folders within the directory
i_am("code/EDLD_650_6_IV_script.R")

####################################
## The Angrist et al PACES data
#####################################

# Read the data in; here in csv format
paces <- read.csv(here("./data/ch11_PACES.csv"))

# Can view the structure of the data
DT::datatable(paces[,c(1:7)], fillContainer = FALSE, options = 
                list(pageLength = 5))


# Examine summary statistics
summary(paces)

# Do some light data prettifying

paces$lottery <- factor(paces$won_lottry,
                        levels = c(0,1),
                        labels = c("No voucher offer", "Received voucher offer"))

##################################
## Balance checks
###################################

# We can examine the variable-by-variable balance using the `tableby` command from the `arsenal` package
# There are, of course, many other approaches to doing the same

random <- arsenal::tableby(won_lottry ~ male + base_age, paces)
summary(random)

# More customization, could do even more...
treatment <- tableby(won_lottry ~ male + base_age, 
                     numeric.stats=c("meansd"), cat.stats=c("N", "countpct"), 
                     digits=2, data=paces)
  #create labels
  mylabels <- list(male = "Male", base_age="Starting Age")

  summary(treatment, 
          labelTranslations = mylabels, 
          title='Descriptive statistics by assigned treatment')


# In large data, given the likelihood that by idiosyncratic chance we will reject the null for some covariates,
# many researchers encourage the use of a single omnibus F-test for balance

summary(lm(won_lottry ~ male + base_age, data=paces))


####################################
## Naive OLS estimates of the outcome on endogenous predictor
####################################

ols1 <- lm(finish8th ~ use_fin_aid, data=paces)
ols2 <- lm(finish8th ~ use_fin_aid + base_age + male, data=paces)

# A no-fuss table to view these results
stargazer(ols1, ols2, type='html', omit.stat = c("ser", "adj.rsq", "f"), 
          dep.var.caption="", dep.var.labels.include=F, omit=c("Constant"), 
          star.cutoffs=c(0.05, 0.01, 0.001), notes.align="l")


###############################
## Visualize the ITT effects
###############################

# Note that I'm using a factor version of won_lottry to make the labeling more attractive, but this is not necessary

mean <- paces %>% 
                group_by(lottery) %>% 
                                    summarize(mean8th = mean(finish8th))

ggplot(data=mean, aes(x=lottery, y=mean8th)) + 
          geom_col(fill=red_pink, alpha=0.4) + 
          theme_pander(base_size = 18) +
          xlab("Assigned treatment status") + scale_y_continuous("Finish 8th grade")

# Note that you would use a different geometry if the outcome were continuous rather than dichotomous (probably a box- or violin-plot)

#################################
## A simple t-test
#################################

# If randomization is done correctly, and we are interested only in examining the effects of winning/losing randomization...
# the analysis is exceedingly simple...

ttest <- t.test(finish8th ~ won_lottry, data=paces)
ttest


###################################
## Intent-to-Treat (ITT) estimates
###################################

# Estimate the models
itt1 <- lm(finish8th ~ won_lottry, data=paces)
itt2 <- lm(finish8th ~ won_lottry + base_age + male, data=paces)
itt3 <- lm(finish8th ~ won_lottry + base_age + male + 
             as.factor(school), data=paces)

# Create a decent-looking table

# Create a row indicating FEs
row <- tribble(~term,          ~'1',  ~'2', ~'3', 
                 'School Fixed Effects', 'No', 'No', 'Yes')
attr(row, 'position') <- c(7)
  
# Produce the table; can export to markdown, tex, etc. by changing the type
modelsummary(list(itt1, itt2, itt3), 
               title = "Table 1. Intent-to-Treat Estimates of Winning the PACES lottery on 8th Grade Completion",
               stars=c('*' = 0.05, '**' = 0.01, '***' = 0.001),
               coef_omit = "(Intercept)|as.factor",
               coef_rename = c("won_lottry" = "Won Lottery", "base_age" = "Starting Age", "male" = "Male"),
               estimate = "{estimate}{stars}",
               gof_omit= "Adj|Pseudo|Log|Within|AIC|BIC|FE|Std|R2|F",
               add_rows = row,
               threeparttable= T,
               notes = c("Notes: The table displays coefficients from Equation X and standard errors in parentheses."),
               type='html')


#########################################
## 2SLS estimates of the TOT
#########################################

######## The structure of the IV command using `fixest` is as follows

## feols(outcome ~ covariates | FE (if any, otherwise leave blank) | endogenous_predictor ~ instrument, data)



# Instrument with no covariates
# With only instrumented predictor and no covariates, 
# need to include a "1" in 2nd stage
tot1 <- feols(finish8th ~ 1 | use_fin_aid ~ won_lottry, data=paces)

# Instrument with covariates
# Note that these are automatically included in 1st stage
# Can include multiple instruments and multiple
# endogenous predictors
tot2 <- feols(finish8th ~ base_age + male | 
                use_fin_aid ~ won_lottry, data=paces)


# See only first stage results

summary(tot2, stage = 1)

# See only second stage results (or both stages, with mostly 2nd stage results displayed)

summary(tot2, stage = 2)
summary(tot2)

###  A taxonomy of IV models

# Include school fixed effects
tot3 <- feols(finish8th ~ base_age + male | as.factor(school) | 
                use_fin_aid ~ won_lottry, 
              vcov = "iid",  data=paces)

# Cluster-robust standard errors
tot4 <- feols(finish8th ~ base_age + male | as.factor(school) | 
                use_fin_aid ~ won_lottry, 
              vcov = ~ school, data=paces)

# Note that for pedagogical purposes, I'm specifying the standard error structure in the regression command
# This can also be done post-estimation. Just don't include the vcov, it defaults to whatever is in the fixed effect line.
# You can then specify different SEs in the summary or modelsummary/etable commands

# Build a table of IV estimates


mods <- list()
mods[['(1)']] <- tot1
mods[['(2)']] <- tot2
mods[['(3)']] <- tot3
mods[['(4)']] <- tot4

row <- tribble(~term,          ~'(1)',  ~'(2)', ~'(3)', ~'(4)',
               'School FE', 'No', 'No', 'Yes', 'Yes')
attr(row, 'position') <- c(7)

modelsummary(mods,
             title = "Table 2. Instrumental variable estimates of using financial aid to attend private school due to winning the PACES lottery on 8th grade completion",
             stars=c('*' = 0.05, '**' = 0.01, '***' = 0.001),
             coef_omit = "Int",
             gof_omit= "Adj|Pseudo|Log|Within|AIC|BIC|FE|Std|R2|F|Int",
             coef_rename = c("fit_use_fin_aid" = "Use Fin. Aid", "base_age" = "Starting Age", "male" = "Male"),
             add_rows = row,
             threeparttable= T,
             notes = c("The table displays coefficients from Equation X and standard errors in parentheses. Model 4 uses cluster-robust standard errors at school level.")
              ) 

# Build a table comparing OLS, ITT (reduced form) and IV estimates


mod2 <- list()
mod2[['(1)']] <- ols2
mod2[['(2)']] <- itt2
mod2[['(3)']] <- tot1
mod2[['(4)']] <- tot2
mod2[['(5)']] <- tot4

row2 <- tribble(~term,          ~'(1)',  ~'(2)', ~'(3)', ~'(4)', ~'(5)',
                ' ', 'OLS', 'ITT', 'TOT', 'TOT', 'TOT',
                'School FE', 'No', 'No', 'No', 'No', 'Yes',
                'Student Chars.', 'Yes', 'Yes', 'No', 'Yes', 'Yes',
                'Clust. SEs', 'No', 'No', 'No', 'No', 'Yes'
                )  

attr(row2, 'position') <- c(1, 6, 7, 8)

modelsummary(mod2,
             title = "Table 3. Comparison of OLS, ITT and IV estimates of using financial aid to attend private school due to winning the PACES lottery",
             stars=c('*' = 0.05, '**' = 0.01, '***' = 0.001),
             coef_omit = "Int|male|base",
             gof_omit= "Adj|Pseudo|Log|Within|AIC|BIC|FE|Std|R2|F|Int",
             coef_rename = c("won_lottry" = "Win Lottery", "use_fin_aid" = "Use Fin. Aid", "fit_use_fin_aid" = "Use Fin. Aid"),
             add_rows = row2,
             threeparttable= T,
             notes = c("The table displays coefficients from Equation X and standard errors in parentheses.")
              ) 


