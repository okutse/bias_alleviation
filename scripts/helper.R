######################################################
# THESIS DATA ANALYSIS
######################################################

#load the required packages
library(kableExtra) ## nicely format tables in rmd files
library(tidyverse)
library(data.table)
library(latex2exp) ## use latex code in images
library(gridExtra) ## arrange graphs in grids on page
library(gtsummary) ## create nice tables for descriptive statistics
library(stargazer) ## nicely formatted latex regression tables
library(ggpubr)     ## publication ready graphics
library(ggsci)      ## scientific journal color themes
library(lme4)       ## multilevel data analysis






####################################################
## IMPLEMENTATION OF ALGORITHMS: Hyperparameter tuning results
####################################################
# (1) Random Forest
options(knitr.kable.NA = "")
dfff <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/rfhyper.csv", header = TRUE)
xx = kable(dfff, format = "latex", 
           #longtable = TRUE,
           label = "rf-hyper",
           caption = "Hyperparameter tuning results for the random forest regression model",
           col.names = c("Full", "n = 500, SD = 1", "n = 500, SD = 45", "n = 2000, SD = 1", "n = 2000, SD = 45"),
           booktabs = TRUE,
           align = c("l", "c", "c", "c", "c"),
           escape = TRUE) %>% 
  kable_styling(latex_options = c("hold_position", "scale_down")) %>% 
  row_spec(c(0), bold = TRUE) %>% 
  row_spec(c(2,3), hline_after = TRUE) %>% 
  add_header_above(header = c("Dataset" = 1, "Case" = 4), bold = TRUE) %>% 
  row_spec(c(3), bold = TRUE, hline_after = FALSE) %>% 
  footnote(general = c("SD: Standard deviation;", 
                       "mtry: the number of predictors that will be randomly sampled at each split;", 
                       "min\\\\_n: the maximum node size;", 
                       "All tuning parameters are based on models fit using $n = 1000$ trees."), 
           threeparttable = FALSE, escape = FALSE, general_title = "Notes:", footnote_as_chunk = FALSE)
xx

## (2) XGBoost
options(knitr.kable.NA = "")
xgb <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/xgboost.csv", header = TRUE)
xg = kable(xgb, format = "latex", 
           caption = "Hyperparameter tuning results for the eXtreme gradient boosted tree ensemble (XGBoost) model",
           col.names = c("Full", "n = 500, SD = 1", "n = 500, SD = 45", "n = 2000, SD = 1", "n = 2000, SD = 45"),
           booktabs = TRUE,
           align = c("l", "c", "c", "c", "c"), escape = TRUE, label = "xgboost-hyper") %>% 
  kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE) %>% 
  row_spec(c(0), bold = TRUE) %>% 
  add_header_above(header = c("Dataset" = 1, "Case" = 4), bold = TRUE) %>% 
  row_spec(c(5), bold = TRUE) %>% 
  row_spec(c(4, 5), hline_after = TRUE) %>% 
  footnote(general = c("SD: Standard deviation;", 
                       "mtry: the number of predictors randomly sampled at each split during tree creation;", 
                       "min\\\\_n: the maximum size of a node before further splitting;", 
                       "tree\\\\_depth: the maximum depth of a tree;", 
                       "learn\\\\_rate: the step-size, shrinkage parameter, or the learning rate;", 
                       "loss\\\\_reduction: the desired loss at a node before being split further;",
                       "All tuning parameters were based on models fit using $n = 1000$ trees"), 
           threeparttable = FALSE, escape = FALSE, general_title = "Notes:", footnote_as_chunk = FALSE)
xg

#############################################################################################################
# NON INFORMATIVE MISSING DATA
################################################################################
## Exploratory Analysis
################################################################################

## load the simulated data sets for description in main text [use the X's in analyses]
    ## n = 500, sd = 1
load("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/df_one.RData")
    ## n = 500, sd = 45
load("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/df_two.RData")
    ## n = 2000, sd = 1
load("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/df_three.RData")
    ## n = 2000, sd = 45
load("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/df_four.RData")

################################################################################
## load the correct outcome generating variables                              
################################################################################
load("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/dt_one.RData")
## n = 500, sd = 45
load("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/dt_two.RData")
## n = 2000, sd = 1
load("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/dt_three.RData")
## n = 2000, sd = 45
load("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/dt_four.RData")


  ## observed response rates
    ## response and non-response rate (n = 500, sd = 1)
(table(df_one$R)/length(df_one$R))*100
    ## response and non-response rate (n = 500, sd = 45)
(table(df_two$R)/length(df_two$R))*100
    ## response and non-response rate (n = 2000, sd = 1)
(table(df_three$R)/length(df_three$R))*100
    ## response and non-response rate (n = 2000, sd = 45)
(table(df_four$R)/length(df_four$R))*100

  ## average expected outcome (y = 234. 5195)
mean(df_one$y); mean(df_two$y); mean(df_three$y); mean(df_four$y)
(mean(df_one$y) + mean(df_two$y) + mean(df_three$y) + mean(df_four$y))/4

  ## average expected outcome by treatment group (y(1) = 260.8149; y(0) = 213.8771)
mean(df_one$y[df_one$A == 1]); mean(df_one$y[df_one$A == 0])
mean(df_two$y[df_two$A == 1]); mean(df_two$y[df_two$A == 0])
mean(df_three$y[df_three$A == 1]); mean(df_three$y[df_three$A == 0])
mean(df_four$y[df_four$A == 1]); mean(df_four$y[df_four$A == 0])

(mean(df_one$y[df_one$A == 1]) + mean(df_two$y[df_two$A == 1]) + mean(df_three$y[df_three$A == 1]) +  mean(df_four$y[df_four$A == 1]))/4  # (1) = 259.0909
(mean(df_one$y[df_one$A == 0]) + mean(df_two$y[df_two$A == 0]) + mean(df_three$y[df_three$A == 0]) + mean(df_four$y[df_four$A == 0]))/4 # (0) = 209.948

  ## average expected difference in potential outcomes (y(1) - y(0) = 46.9377)
mean(df_one$y[df_one$A == 1]) - mean(df_one$y[df_one$A == 0])



  ## selection bias [difference between E(Y/R = 1) and E(Y) should be less than 1/4 of the population standard deviation sqrt(var(Y))]
    ##n = 500, sd = 1
1/4*(sd(df_one$y)) # 10.709
mean(df_one$y) - mean(df_one$y[df_one$R == 1]) # 11.03
    ##n = 500, sd = 45
1/4*sd(df_two$y) ## 15.53
mean(df_two$y) - mean(df_two$y[df_two$R == 1]) # 6.12
    ##n = 2000, sd = 1
0.25*sd(df_three$y) # 11.058
mean(df_three$y) - mean(df_three$y[df_three$R == 1]) # 11.59112
    ##n = 2000, sd = 45
0.25*sd(df_four$y) # 15.86
mean(df_four$y) - mean(df_four$y[df_four$R == 1]) # 10.38


  ## Association between the observed variables (Xs) and the outcome (Y) for n = 500, sd = 1 and sd = 45 
    ## n = 500, sd = 1

onea <- ggplot(df_one, aes(x = x1, y = y)) + geom_point(alpha = .2) + geom_smooth(formula = "y ~ x", method = "loess") + ylab(TeX(r'($y$)')) + xlab(TeX(r"($x_1$)")) + ggpubr::stat_cor(r.digits = 2, size = 3.0, label.y = 400, aes(label = after_stat(r.label))) + theme_bw()
oneb <- ggplot(df_one, aes(x = x2, y = y)) + geom_point(alpha = .2) + geom_smooth(formula = "y ~ x", method = "loess") + theme_bw() + ylab(TeX(r'($y$)')) + xlab(TeX(r'($x_2$)')) + ggpubr::stat_cor(r.digits = 2, p.accuracy = 0.01, size = 3.0, label.y = 400, aes(label = after_stat(r.label)))
onec <- ggplot(df_one, aes(x=x3, y=y))+geom_point(alpha = .2) + geom_smooth(formula = "y~x", method = "loess") + theme_bw() + ylab(TeX(r'($y$)')) + xlab(TeX(r'($x_3$)')) + ggpubr::stat_cor(r.digits = 2, p.accuracy = 0.01, size = 3.0, label.y = 400, aes(label = after_stat(r.label)))
oned <- ggplot(df_one, aes(x=x4, y=y)) + geom_point(alpha = .2) + geom_smooth(formula = "y~x", method = "loess") + theme_bw() + ylab(TeX(r'($y$)')) + xlab(TeX(r"($x_4$)")) + ggpubr::stat_cor(r.digits = 2, p.accuracy = 0.01, size = 3.0, label.y = 400, aes(label = after_stat(r.label)))

    ## n = 500, sd = 45
twoa <- ggplot(df_two, aes(x = x1, y = y)) + geom_point() + geom_smooth(formula = "y ~ x", method = "loess") + theme_bw() + ylab(TeX(r'($y$)')) + xlab(TeX(r"($x_1$)")) + ggpubr::stat_cor(r.digits = 2, p.accuracy = 0.01, size = 3.0, label.y = 400)
twob <- ggplot(df_two, aes(x = x2, y = y)) + geom_point() + geom_smooth(formula = "y ~ x", method = "loess") + theme_bw() + ylab(TeX(r'($y$)')) + xlab(TeX(r'($x_2$)')) + ggpubr::stat_cor(r.digits = 2, p.accuracy = 0.01, size = 3.0, label.y = 400)
twoc <- ggplot(df_two, aes(x=x3, y=y))+geom_point() + geom_smooth(formula = "y~x", method = "loess") + theme_bw() + ylab(TeX(r'($y$)')) + xlab(TeX(r'($x_3$)')) + ggpubr::stat_cor(r.digits = 2, p.accuracy = 0.01, size = 3.0, label.y = 400)
twod <- ggplot(df_two, aes(x=x4, y=y)) + geom_point() + geom_smooth(formula = "y~x", method = "loess") + theme_bw() + ylab(TeX(r'($y$)')) + xlab(TeX(r"($x_4$)")) + ggpubr::stat_cor(r.digits = 2, p.accuracy = 0.01, size = 3.0, label.y = 400)

#grid.arrange(onea, oneb, onec, oned, ncol=2)
#grid.arrange(twoa, twob, twoc, twod, ncol=2)

    ## n = 2000, sd = 1
threea <- ggplot(df_three, aes(x = x1, y = y)) + geom_point( size = 1.5) + geom_smooth(formula = "y ~ x", method = "loess") + theme_classic() + ylab(TeX(r'($y$)')) + xlab(TeX(r"($x_1$)")) + ggpubr::stat_cor(r.digits = 3, p.accuracy = 0.01, size = 3.0, label.y = 400)
threeb <- ggplot(df_three, aes(x = x2, y = y)) + geom_point() + geom_smooth(formula = "y ~ x", method = "loess") + theme_classic() + ylab(TeX(r'($y$)')) + xlab(TeX(r'($x_2$)')) + ggpubr::stat_cor(r.digits = 3, p.accuracy = 0.01, size = 3.0, label.y = 400)
threec <- ggplot(df_three, aes(x=x3, y=y))+geom_point() + geom_smooth(formula = "y~x", method = "loess") + theme_classic() + ylab(TeX(r'($y$)')) + xlab(TeX(r'($x_3$)')) + ggpubr::stat_cor(r.digits = 3, p.accuracy = 0.01, size = 3.0, label.y = 400)
threed <- ggplot(df_three, aes(x=x4, y=y)) + geom_point() + geom_smooth(formula = "y~x", method = "loess") + theme_classic() + ylab(TeX(r'($y$)')) + xlab(TeX(r"($x_4$)")) + ggpubr::stat_cor(r.digits = 3, p.accuracy = 0.01, size = 3.0, label.y = 400)
grid.arrange(threea, threeb, threec, threed)

    ## n = 2000, sd = 45
foura <- ggplot(df_four, aes(x = x1, y = y)) + geom_point( size = 1.5) + geom_smooth(formula = "y ~ x", method = "loess") + theme_classic() + ylab(TeX(r'($y$)')) + xlab(TeX(r"($x_1$)")) + ggpubr::stat_cor(r.digits = 3, p.accuracy = 0.01, size = 3.0, label.y = 400)
fourb <- ggplot(df_four, aes(x = x2, y = y)) + geom_point() + geom_smooth(formula = "y ~ x", method = "loess") + theme_classic() + ylab(TeX(r'($y$)')) + xlab(TeX(r'($x_2$)')) + ggpubr::stat_cor(r.digits = 3, p.accuracy = 0.01, size = 3.0, label.y = 400)
fourc <- ggplot(df_four, aes(x=x3, y=y))+geom_point() + geom_smooth(formula = "y~x", method = "loess") + theme_classic() + ylab(TeX(r'($y$)')) + xlab(TeX(r'($x_3$)')) + ggpubr::stat_cor(r.digits = 3, p.accuracy = 0.01, size = 3.0, label.y = 400)
fourd <- ggplot(df_four, aes(x=x4, y=y)) + geom_point() + geom_smooth(formula = "y~x", method = "loess") + theme_classic() + ylab(TeX(r'($y$)')) + xlab(TeX(r"($x_4$)")) + ggpubr::stat_cor(r.digits = 3, p.accuracy = 0.01, size = 3.0, label.y = 400)
grid.arrange(foura, fourb, fourc, fourd)

## 3D interaction plots between the observed variables, X and the outcome, Y
    ## consider all two-way interactions between the variables

library(scatterplot3d)  ## an r package for 3D plots
par(mfrow = c(1, 3))
scatterplot3d::scatterplot3d(x = df_one$x1, y = df_one$y, z = df_one$x2, xlab = "x1", ylab = "y", zlab = "x2")
scatterplot3d::scatterplot3d(x = df_one$x1, y = df_one$y, z = df_one$x3, xlab = "x1", ylab = "y", zlab = "x3")
scatterplot3d::scatterplot3d(x = df_one$x1, y = df_one$y, z = df_one$x4, xlab = "x1", ylab = "y", zlab = "x4")

#rgl::plot3d(x = df_one$x1, y = df_one$y, z = df_one$x2, xlab = "x1", ylab = "y", zlab = "x2", type = "s")
#aws::smooth3D(x = df_one$x1, y = df_one$y, z = df_one$x2, xlab = "x1", ylab = "y", zlab = "x2")


#summary(lm(y~A*x1 + A*x2 + A*x3 + A*x4, data = df_one))



## Table of descriptive statistics factored by whether outcome is missing or observed

    ### n= 500; sd = 1
five_hundreda <- df_one %>% 
  gtsummary::tbl_summary(by = R,
                         type = list(A ~ "categorical"),
                         statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  bold_labels() %>% 
  add_overall(last = TRUE) %>% 
  add_p() %>% 
  modify_header(label = "**Variable**")
    ### n= 500; variance = 45
five_hundredb <- df_two %>% 
  gtsummary::tbl_summary(by = R,
                         type = list(A ~ "categorical"),
                         statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  bold_labels() %>% 
  add_overall(last = TRUE) %>% 
  add_p() %>% 
  modify_header(label = "**Variable**")
    ## n = 2000, sd = 1
twoka <- df_three %>% 
  gtsummary::tbl_summary(by = R,
                         type = list(A ~ "categorical"),
                         statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  bold_labels() %>% 
  add_overall(last = TRUE) %>% 
  add_p() %>% 
  #add_header_above(header = c(" " = 1, "Data generating mechanism (n = 2000, sd = 1)" = 4), bold = TRUE, line = TRUE, line_sep = 0) %>% 
  modify_header(label = "**Variable**")
    ## n = 2000, sd = 45
twokb <- df_four %>% 
  gtsummary::tbl_summary(by = R,
                         type = list(A ~ "categorical"),
                         statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  bold_labels() %>% 
  add_overall(last = TRUE) %>% 
  add_p() %>% 
  modify_header(label = "**Variable**")

## combined table of descriptive statistics
    ## merge table when n = 500 and 2000 and sd = 1
tbl_one <- gtsummary::tbl_merge(tbls = list(five_hundreda, twoka), tab_spanner =  c("n = 500", "n = 2000")) %>% bold_labels()
tbl_two<- gtsummary::tbl_merge(tbls = list(five_hundredb, twokb), tab_spanner = c("n = 500", "n = 2000")) 

full_desc = tbl_stack(tbls = list(tbl_one, tbl_two), group_header = c("SD = 1", "SD = 45"), quiet = TRUE) %>% bold_labels()

tbl_full <- full_desc %>% 
  as_kable_extra(format = "latex", caption = "Group comparisons of the descriptive statistics between the observed (R = 1) and unobserved (R = 0) participant groups stratified by the sample size and the residual standard deviation used in the data generating mechanism under the correct outcome model.", booktabs = TRUE, escape = TRUE, label = "table-one") %>% 
  row_spec(c(0), bold = TRUE) %>% 
  kable_styling(latex_options = c("scale_down")) %>%
  row_spec(8, hline_after = TRUE)
tbl_full

## Boxplots of the distribution of the observed covariates by whether missing or observed

    ## plots for when n = 500 and sd = 1 
oneaa <- ggplot2::ggplot(data = df_one, aes(x = as.factor(R), y = x1, group = R, color = as.factor(R))) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(TeX(r'($R$)'))+
  ylab(TeX(r'($X_1$)'))+scale_color_npg(guide = "none")
onebb <- ggplot2::ggplot(data = df_one, aes(x = as.factor(R), y = x2, group = R, color = as.factor(R))) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(TeX(r'($R$)'))+
  ylab(TeX(r'($X_2$)'))+scale_color_npg(guide = "none")
onecc <- ggplot2::ggplot(data = df_one, aes(x = as.factor(R), y = x3, group = R, color = as.factor(R))) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(TeX(r'($R$)'))+
  ylab(TeX(r'($X_3$)'))+scale_color_npg(guide = "none")
onedd <- ggplot2::ggplot(data = df_one, aes(x = as.factor(R), y = x4, group = R, color = as.factor(R))) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(TeX(r'($R$)'))+
  ylab(TeX(r'($X_4$)'))+scale_color_npg(guide = "none")





## Exploratory regression models 
    ## (A) The correct outcome generating covariates (Z's)
      ## n = 500, sd = 1
model_onea <- lm(y ~ A + z1 + z2 + z3 + z4, data = dt_one)
summary(model_onea)
      ## n = 2000, sd = 1
model_oneb <- lm(y ~ A + z1 + z2 + z3 + z4, data = dt_three)
summary(model_oneb)
      ## n = 500, sd = 45
model_onec <- lm(y ~ A + z1 + z2 + z3 + z4, data = dt_two)
summary(model_onec)
      ## n = 2000, sd = 45
model_oned <- lm(y ~ A + z1 + z2 + z3 + z4, data = dt_four)
summary(model_oned)

    ## (B) The observed covariates (X's)
      ## n = 500, sd = 1
model_one <- lm(y ~ A + x1 + x2 + x3 + x4, data = df_one)
summary(model_one)
      ## n = 500, sd = 45
model_two <- lm(y ~ A + x1 + x2 + x3 + x4, data = df_two)
summary(model_two)
      ## n = 2000, sd = 1
model_three <- lm(y ~ A + x1 + x2 + x3 + x4, data = df_three)
summary(model_three)
      ## n = 2000, sd = 45
model_four <- lm(y ~ A + x1 + x2 + x3 + x4, data = df_four)
summary(model_four)


## merge the regression tables by the sample sizes and standard errors and stack

stargazer::stargazer(list(model_one, model_three, model_two, model_four
                          ), header = FALSE, type = "latex",
                     column.labels = c("n = 500, sd = 1", "n = 2000, sd = 1", 
                                       "n = 500, sd = 45", "n = 2000, sd = 45"),
                     style = "ajps",
                     digits = 4,
                     #label = "table-two",
                     single.row = TRUE,
                     no.space = TRUE, 
                     font.size = "normalsize",
                     #column.sep.width = "2pt",
                     dep.var.labels = "Y",
                     title = "Summary exploratory regression model results adjusting for the observed covariates for n = 500 and n = 2000 and varying the residual standard error as 1 and 45, respectively.")

## the edited model as an excel file [for the paper]
t_two <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/exploratory_reg.csv", header = TRUE)
table_two = t_two %>% 
  kable(format = "latex", digits = 2, col.names = c("Variable", "Model 1", "Model 2", "Model 3", "Model 4"),
        caption = "Summary exploratory regression results adjusting for the observed covariates for n = 500 and n = 2000 and varying the residual standard error as 1 and 45, respectively.",
        align = c("lcccc"),
        label = "tab:table_two", booktabs = TRUE) %>% 
  add_header_above(header = c(" " = 1, "n = 500, SD = 1" = 1, "n = 2000, SD = 1" = 1, "n = 500, SD = 45" = 1, "n = 2000, SD = 45"), bold = TRUE, align = "c") %>% 
  row_spec(0, bold = TRUE) %>% 
  column_spec(1, bold = TRUE) %>% 
  footnote(general = c("SD denotes the standard deviation of the correct outcome data generating model",  "Residual standard error is based on the linear regression adjustment model with the observed covariates, X", "Cell values denote the estimated linear regression coefficients and their associated standard errors."), general_title = "Notes:",  footnote_as_chunk = FALSE, threeparttable = FALSE)



## Regression model diagnostics
    ## n = 500, sd = 1
par(mfrow = c(2, 2))
plot(model_one)
    ## n = 500, sd = 45
par(mfrow = c(2, 2))
plot(model_two)
    ## n = 2000, sd = 1
par(mfrow = c(2, 2))
plot(model_three)
    ## n = 2000, sd = 45
par(mfrow = c(2, 2))
plot(model_four)



## correlation between fitted values under correct and misspecified models
    ## n = 500, sd = 1
ggplot() + geom_point(aes(x = fitted(model_onea), y = fitted(model_one))) +
  xlab(TeX(r'(Correct $y$ model)')) + ylab(TeX(r'(Misspecified $y$ model)')) + theme_classic() +
  ggpubr::stat_cor(mapping = aes(x = fitted(model_onea), y = fitted(model_one)), p.accuracy = .001, r.digits = 3)
cor.test(x = fitted(model_onea), y = fitted(model_one), alternative = "two.sided") 
    ## n = 2000, sd = 1
cor.test(x = fitted(model_oneb), y = fitted(model_three), alternative = "two.sided") 
    ## n = 500, sd = 45
cor.test(x = fitted(model_onec), y = fitted(model_two), alternative = "two.sided") 
    ## n = 2000, sd = 45
cor.test(x = fitted(model_oned), y = fitted(model_four), alternative = "two.sided") 
 #- because these have no issues, an analyst would have no interest in pursuing other models

####################################
## NON-INFORMATIVE MISSING DATA
####################################

##------------------------------------------------------------------------------
## Comparisons of ML models under complete data
##------------------------------------------------------------------------------
# (1) Graphics




# (2) Tables
## Precision and bias under complete data
##------------------------------------------------------------------------------
options(knitr.kable.NA = ' ')
precision_biasa <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/full_data_results.csv", header = TRUE)
tb_a <- precision_biasa %>% kable(format = "latex", 
                                  digits = 3, 
                                  col.names = c("DGP", "$n$", 
                                                "Correct model[note]", "Unadj[note]", "MLR[note]", 
                                                "MLR[note]", "RF[note]", "XGBoost[note]", "BART[note]",
                                                "SL[note]"),
                                  caption = "Comparing bias of the average treatment effect estimates using machine learning (ML) covariate adjustment, multiple linear regression (MLR) and the unadjusted estimator under complete data and model misspecification across 1000 simulated datasets.",
                                  booktabs = TRUE,
                                  align = c("cccccccccc"),
                                  linesep = '', label = "complete-data", escape = FALSE) %>% 
  kableExtra::group_rows(group_label = "Effect size (SE)", start_row = 1, end_row = 4, latex_align = "l", bold = TRUE) %>% 
  kableExtra::group_rows(group_label = "Bias", start_row = 5, end_row = 8, latex_align = "l", bold = TRUE, hline_before = TRUE, hline_after = TRUE) %>% 
  row_spec(0, bold = TRUE) %>% 
  column_spec(1, width = "2cm") %>% 
  column_spec(c(2, 3), width = "1cm") %>% 
  column_spec(c(4, 5, 6, 7, 8, 9, 10), width = "1.5cm") %>% ## originally 2.5 cm
  kable_styling(latex_options = "scale_down") %>% 
  add_footnote(c("Main effects linear regression of $Y_i$ on the $Z_{ij}$'s", 
                 "Unadjusted estimator, $E(Y|A = 1) - E(Y|A = 0)$", 
                 "Regression of $Y_i$ on $X_{ij}$ with interaction terms for $A$ and each $X_{ij}$",
                 "Main effects linear regression of $Y_i$ on $X_{ij}$", "Random forest tuned for minimal node size and number of predictors randomly sampled at a node", "XGBoost tuned for tree depth, learning rate, minimal node size, and loss reduction", "Bayesian Additive Regression Trees", "Weighted linear combination of RF and XGBoost."), notation = "alphabet", escape = FALSE) %>% 
  footnote(general = c("DGP = Data generating mechanism; $n$ = actual analyzed sample."), general_title = "Notes:", footnote_as_chunk = FALSE, escape = FALSE, threeparttable = FALSE) %>% 
  row_spec(c(3, 4, 7, 8), background = "lightgray")
tb_a

## Relative efficiency under complete data and adjustment model misspecification
##------------------------------------------------------------------------------
  ## comparing ML adjustment to the unadjusted estimator under misspecification
re.dt <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/full_analysis_resultsb.csv")
comp.re.dt <- re.dt %>% 
  dplyr::filter(analysis_type == "Full") %>% 
  dplyr::select(-c(effect_size, bias, variance_explained))
      # convert the complete data results to wide format to have each estimate from a method in the column
re_one <- tidyr::pivot_wider(comp.re.dt, names_from = "estimator", values_from = "std_error") %>% 
  dplyr::mutate(re_U = (U/U)^2 ,
                re_IMLR = (IMLR/U)^2,
                re_MLR = (MLR/U)^2,
                re_urf = (urf/U)^2,
                re_trf = (RF/U)^2,
                re_XGBoost = (XGBoost/U)^2,
                re_BART = (BART/U)^2,
                re_SL = (SL/U)^2,
                re_CM = (CM/U)^2) 

re1 = re_one %>% 
  tidyr::pivot_longer(names_to = c("estimator"), cols = 4:21, cols_vary = "slowest", 
                      values_to = c("std_error")) %>% 
  #filter(estimator %like% "re_" & std_error != 0) %>%
  filter(estimator %like% "re_") %>% 
  rename(relative_efficiency1 = std_error)

  ## comparing ML adjustment to linear (LM) regression adjustment under misspecification {% inc in precision using LM relative to ML; -ve denote improved efficiency using ML}

re_two <- tidyr::pivot_wider(comp.re.dt, names_from = "estimator", values_from = "std_error") %>% 
  dplyr::mutate(re_U = (U/MLR)^2,
                re_IMLR = (IMLR/MLR)^2,
                re_MLR = (MLR/MLR)^2,
                re_urf = (urf/MLR)^2,
                re_trf = (RF/MLR)^2,
                re_XGBoost = (XGBoost/MLR)^2,
                re_BART = (BART/MLR)^2,
                re_SL = (SL/MLR)^2,
                re_CM = (CM/MLR)^2) 
re2 = re_two %>% 
  tidyr::pivot_longer(names_to = c("estimator"), cols = 4:21, cols_vary = "slowest", 
                      values_to = c("std_error")) %>% 
  filter(estimator %like% "re_") %>% 
  rename(relative_efficiency2 = std_error)

# % increase in precision in using covariate and ML adjustment versus no adjustment and misspecified linear model
efficiency_table <- merge(re1, re2, sort = FALSE, by = c("dgp", "analysis_type", "analyzed_sample", "estimator")) 

eff_tab1 <- efficiency_table %>% 
  dplyr::filter(!(dgp == "n = 500, SD = 23" | dgp == "n = 500, SD = 68" | dgp == "n = 2000, SD = 23"|dgp == "n = 2000, SD = 68"))

# filter out cases when the proportion variance explained is low
lowvar500 <- eff_tab1 %>% dplyr::filter(dgp == "n = 500, SD = 45") %>% 
  dplyr::rename("rel-eff1" = relative_efficiency1, "rel-eff2" = relative_efficiency2)
lowvar500$estimator <- c("Unadjusted estimator", "Interacted Multiple Linear Regression", "Multiple Linear Regression", "Untuned Random Forest", "Tuned Random Forest", "eXtreme Gradient Boosted Trees", "Bayesian Additive Regression Trees", "SuperLearner", "Correct model")
lowvar2000 <- eff_tab1 %>% dplyr::filter(dgp == "n = 2000, SD = 45") %>% 
  dplyr::rename("rel-eff1" = relative_efficiency1, "rel-eff2" = relative_efficiency2)
lowvar2000$estimator <- c("Unadjusted estimator", "Interacted Multiple Linear Regression", "Multiple Linear Regression", "Untuned Random Forest", "Tuned Random Forest", "eXtreme Gradient Boosted Trees", "Bayesian Additive Regression Trees", "SuperLearner", "Correct model")
low <- rbind(lowvar500, lowvar2000)

# filter out cases when the proportion variance explained is high
highvar500 <- eff_tab1 %>% dplyr::filter(dgp == "n = 500, SD = 1") %>% 
  dplyr::rename("rel-eff3" = relative_efficiency1, "rel-eff4" = relative_efficiency2)
highvar500$estimator <- c("Unadjusted estimator", "Interacted Multiple Linear Regression", "Multiple Linear Regression", "Untuned Random Forest", "Tuned Random Forest", "eXtreme Gradient Boosted Trees", "Bayesian Additive Regression Trees", "SuperLearner", "Correct model")
highvar2000 <- eff_tab1 %>% dplyr::filter(dgp == "n = 2000, SD = 1") %>% 
  dplyr::rename("rel-eff3" = relative_efficiency1, "rel-eff4" = relative_efficiency2)
highvar2000$estimator <- c("Unadjusted estimator", "Interacted Multiple Linear Regression", "Multiple Linear Regression", "Untuned Random Forest", "Tuned Random Forest", "eXtreme Gradient Boosted Trees", "Bayesian Additive Regression Trees", "SuperLearner", "Correct model")
high <- rbind(highvar500, highvar2000)
comp_eff <- cbind(low, high)[, c(4, 3, 5, 6, 11, 12)] 


comp_efficiency <- comp_eff %>% 
  kable(format = "latex",
        caption = "Relative efficiency using machine learning (ML) adjustment relative to the unadjusted estimator and multiple linear regression (MLR) under model misspecification with complete data. Results are grouped by the sample size and stratified by the proportion of variance explained by covariates in the correct outcome model.",
        col.names = c("Estimator", "Analyzed sample", "Relative efficiency[note]", "Relative efficiency[note]", "Relative efficiency[note3]", "Relative efficiency[note4]"),
        booktabs = TRUE,
        align = c("lcccc"),
        digits = 2, 
        linesep = '',
        label = "complete-efficiency") %>% 
  add_header_above(header = c(" " = 2, "$\\R^2 = 0.44$[note]" = 2, "$\\R^2 = 0.85$[note]" = 2), line = TRUE, line_sep = 0, bold = TRUE, escape = FALSE) %>% 
  column_spec(c(2, 3, 4, 5, 6), width = "2.5cm") %>% 
  row_spec(c(1:9), background = "lightgray") 
comp_efficiency <- base::gsub("1.000", "--", comp_efficiency) %>% 
  #group_rows(group_label = "n = 500", start_row = 1, end_row = 9)
  kable_styling(bootstrap_options = ".table-hover", latex_options = c("scale_down")) %>% 
  row_spec(0, bold = TRUE) %>% 
  add_footnote(c("Residual standard deviation = 45", "Residual standard deviation = 1", "Estimated as ratio of the variance of the estimator under consideration to the variance of the unadjusted estimator.", "Estimated as ratio of the variance of the estimator under consideration to the variance of multiple linear regression."), notation = "symbol", escape = FALSE) %>% 
  footnote(general = c("Relative efficiency values less than 1 suggest poor performance of the base model."), general_title = "Note:", footnote_as_chunk = FALSE, escape = FALSE, threeparttable = FALSE)
comp_efficiency



## Figure: Relative efficiency vs proportion variance explained for selected models 
    # (BART, SL, MLR, Correct model, Unadjusted estimator)
##------------------------------------------------------------------------------

## load the full analysis (c) data that has all required results
# (a: relative efficiency on complete data)
fig_dt <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/all.csv")
sdt <- fig_dt %>% dplyr::filter(estimator %in% c("BART", "SL", "CM", "MLR", "U"))
#jpeg(filename = "../figures/efficiency_nyu.jpg", width = 8, height = 8, units = 'in', res = 300)
repvar_one <- ggplot(data = subset(sdt, analysis_type == "Full"))+
  geom_smooth(aes(x = variance_explained, y = relative_efficiency, group = estimator, color = estimator), 
              span = 0.9, se = FALSE, method = NULL) +
  theme_bw() + 
  xlab(TeX(r'(Proportion variance explained, $R^2$)'))+
  ylab(TeX(r'(Relative efficiency, $\frac{\Var(\theta_A)}{\Var(\theta_B)}$)'))+
  scale_color_npg(name = "Estimator")
repvar_one
#dev.off()

# (b: relative efficiency on complete data) ## same figure as above
repvar_oneb <- ggplot(data = subset(sdt, analysis_type == "Observed modified"))+
  geom_smooth(aes(x = variance_explained, y = relative_efficiency, group = estimator, color = estimator), 
              span = 0.9, se = FALSE, method = NULL) +
  theme_bw() + 
  xlab(TeX(r'(Proportion variance explained, $R^2$)'))+
  ylab(TeX(r'(Relative efficiency, $\frac{\Var(\theta_A)}{\Var(\theta_B)}$)'))+
  scale_color_npg(name = "Estimator")
repvar_oneb

# (c: bias and variance explained on complete data)
biasa <- ggplot(data = subset(sdt, analysis_type == "Full"))+
  geom_smooth(aes(x = variance_explained, y = bias, group = estimator, color = estimator), 
              span = 1.0, se = FALSE, method = NULL) +
  theme_bw() + 
  xlab(TeX(r'(Proportion variance explained, $R^2$)'))+
  ylab(TeX(r'(Bias, $\E(\hat{\theta}) - \theta$)'))+
  scale_color_npg(name = "Estimator")
biasa  
  
  
  
  
##------------------------------------------------------------------------------
## Comparisons of ML models under incomplete/missing data
##------------------------------------------------------------------------------
# (1) Graphics




# (2) Tables
## Precision and bias under misspecification and incomplete data
##------------------------------------------------------------------------------
options(knitr.kable.NA = ' ') ## precision of the estimated treatment effects
precision_biasb <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/missing_data_results.csv")
 
tb_b <- precision_biasb %>% kable(format = "latex", 
                                  digits = 3, 
                                  col.names = c("DGP", "Analysis Type", "$n$", 
                                                paste0("Correct model", footnote_marker_alphabet(1, "latex")), 
                                                paste0("Unadj", footnote_marker_alphabet(2, "latex")), 
                                                paste0("MLR", footnote_marker_alphabet(3, "latex")), 
                                                paste0("MLR", footnote_marker_alphabet(4, "latex")), 
                                                paste0("RF", footnote_marker_alphabet(5, "latex")), 
                                                paste0("XGBoost", footnote_marker_alphabet(6, "latex")), 
                                                paste0("BART", footnote_marker_alphabet(7, "latex")),
                                                paste0("SL", footnote_marker_alphabet(8, "latex"))),
                                  caption = "Precision of the average treatment effect estimates using machine learning (ML), multiple linear regression (MLR), and the unadjusted estimator under observed and observed modified analysis and model misspecification. Effect stimates are averaged across $1000$ simulated datasets.",
                                  booktabs = TRUE,
                                  align = c("lllccccccc"),
                                  linesep = '', escape = FALSE,
                                  label = "missing-data") %>% 
  kable_styling(bootstrap_options = ".table-hover", font_size = 10) %>% 
  row_spec(0, bold = TRUE) %>% 
  column_spec(c(1, 2, 9), width = "1.5cm") %>% 
  column_spec(c(3, 4, 5, 6, 7, 8, 10, 11), width = "1cm") %>% #can change to 1.5cm
  #column_spec(1, bold = TRUE) %>% 
  row_spec(c(3, 4, 7, 8, 12, 13, 16, 17), background = "lightgray") %>% 
  row_spec(c(8,9)) %>% 
  #add_footnote(c("Main effects linear regression of $Y_i$ on the $Z_{ij}$'s.", 
   #              "$E(Y|A = 1) - E(Y|A = 0).$", 
  #               "Linear regression of $Y_i$ on $X_{ij}$ with interacted terms for $A$ and each of the observed covariates, $X_{ij}$.",
  #               "Main effects linear regression of $Y_i$ on $X_{ij}$", "Model tuned using data-specific parameters for the minimal node size and the number of predictors to be randomly sampled at a node", "eXtreme gradient boosted tree ensemble (XBoost) using data-specific parameters for the tree depth, the learning rate, the minimal node size, and the loss reduction.", "Bayesian Additive Regression Trees", "Weighted linear combination of RF and the XGBoost algorithms"), notation = "alphabet", escape = FALSE) %>%
  footnote(general = c("DGP = Data generating mechanism; n = actual analyzed sample."), general_title = "Notes:", 
           alphabet = c("Main effects linear regression of $Y_i$ on the $Z_{ij}$'s",
                        "Unadjusted estimator, $E(Y|A = 1) - E(Y|A = 0)$",
                        "Regression of $Y_i$ on $X_{ij}$ with interaction terms for $A$ and each $X_{ij}$",
                        "Main effects regression of $Y_i$ on $X_{ij}$",
                        "Model tuned for minimal node size and the number of predictors to be randomly sampled at a node",
                        "XBoost tuned for tree depth, learning rate, minimal node size, and loss reduction",
                        "Bayesian Additive Regression Tree",
                        "Weighted linear combination of RF and the XGBoost."),
           footnote_as_chunk = FALSE, escape = FALSE, threeparttable = FALSE) #%>% 
  #landscape()
tb_b



## missing data bias table [derived from the above]
bias_c <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/missing_data_bias.csv")
tb_c <- bias_c %>% kable(format = "latex", 
          digits = 3, 
          col.names = c("DGP", "Analysis Type", "$n$", 
                        paste0("Correct model", footnote_marker_alphabet(1, "latex")), 
                        paste0("Unadj", footnote_marker_alphabet(2, "latex")), 
                        paste0("MLR", footnote_marker_alphabet(3, "latex")), 
                        paste0("MLR", footnote_marker_alphabet(4, "latex")), 
                        paste0("RF", footnote_marker_alphabet(5, "latex")), 
                        paste0("XGBoost", footnote_marker_alphabet(6, "latex")), 
                        paste0("BART", footnote_marker_alphabet(7, "latex")),
                        paste0("SL", footnote_marker_alphabet(8, "latex"))),
          caption = "Bias of the estimated effects using machine learning (ML), multiple linear regression (MLR), and the unadjusted estimator under observed and observed modified analysis and model misspecification. Results are averaged across $1000$ simulated datasets.",
          booktabs = TRUE,
          align = c("lllccccccc"),
          linesep = '', escape = FALSE,
          label = "bias-missing-data") %>% 
  kable_styling(bootstrap_options = ".table-hover", font_size = 10) %>% 
  row_spec(0, bold = TRUE) %>% 
  column_spec(c(1, 2, 9), width = "1.5cm") %>% 
  column_spec(c(3, 4, 5, 6, 7, 8, 10, 11), width = "1cm") %>% #can change to 1.5cm
  #column_spec(1, bold = TRUE) %>% 
  row_spec(c(3, 4, 7, 8, 12, 13, 16, 17), background = "lightgray") %>% 
  row_spec(c(8,9)) %>% 
  footnote(general = c("DGP = Data generating mechanism; n = actual analyzed sample."), general_title = "Notes:", 
           alphabet = c("Main effects linear regression of $Y_i$ on the $Z_{ij}$'s",
                        "Unadjusted estimator, $E(Y|A = 1) - E(Y|A = 0)$",
                        "Regression of $Y_i$ on $X_{ij}$ with interaction terms for $A$ and each $X_{ij}$",
                        "Main effects regression of $Y_i$ on $X_{ij}$",
                        "Model tuned for minimal node size and the number of predictors to be randomly sampled at a node",
                        "XBoost tuned for tree depth, learning rate, minimal node size, and loss reduction",
                        "Bayesian Additive Regression Tree",
                        "Weighted linear combination of RF and the XGBoost."),
           footnote_as_chunk = FALSE, escape = FALSE, threeparttable = FALSE)



## Relative efficiency under incomplete/missing data and adjustment model misspecification
##------------------------------------------------------------------------------
## (% increase in precision using one model relative to another under model misspecification and missing data)
## comparing ML adjustment to the unadjusted estimator under misspecification
comp_dtt <- re.dt %>% 
  dplyr::filter(analysis_type != "Full") %>% 
  dplyr::select(-c(effect_size, bias, variance_explained))
# convert the observed data analysis results to wide format to have each estimate from a method in the column
re_three <- comp_dtt %>% 
  tidyr::pivot_wider(names_from = "estimator", values_from = "std_error") 
re_three[c(11, 12), 12] <- re_three[c(17, 18), 12]
re_three <- re_three[-c(17, 18), ]
re_three <- re_three %>% dplyr::mutate(re_U = (U/U)^2 ,
                re_IMLR = (IMLR/U)^2,
                re_MLR = (MLR/U)^2,
                re_urf = (urf/U)^2,
                re_RF = (RF/U)^2,
                re_XGBoost = (XGBoost/U)^2,
                re_BART = (BART/U)^2,
                re_SL = (SL/U)^2,
                re_CM = (CM/U)^2) 


## save re_three to confirm problem and correct it hopefully! file saved before adding in extra columns with ratio of variances.
#write.csv(re_three, file = "../data/re_three_check.csv", row.names = FALSE)
re3 = re_three %>% 
  tidyr::pivot_longer(names_to = c("estimator"), cols = 4:21, cols_vary = "slowest", 
                      values_to = c("std_error")) %>% 
  #filter(estimator %like% "re_" & std_error != 0) %>%
  filter(estimator %like% "re_") %>% 
  rename(relative_efficiency1 = std_error)

## comparing ML adjustment to linear (LM) regression adjustment under misspecification {% inc in precision using LM relative to ML; -ve denote improved efficiency using ML}

re_four <- tidyr::pivot_wider(comp_dtt, names_from = "estimator", values_from = "std_error") 
re_four[c(11, 12), 12] <- re_four[c(17, 18), 12]
re_four <- re_four[-c(17, 18), ]
re_four <- re_four %>% 
  dplyr::mutate(re_U = (U/MLR)^2,
                re_IMLR = (IMLR/MLR)^2,
                re_MLR = (MLR/MLR)^2,
                re_urf = (urf/MLR)^2,
                re_RF = (RF/MLR)^2,
                re_XGBoost = (XGBoost/MLR)^2,
                re_BART = (BART/MLR)^2,
                re_SL = (SL/MLR)^2,
                re_CM = (CM/MLR)^2) 

re4 = re_four %>% 
  tidyr::pivot_longer(names_to = c("estimator"), cols = 4:21, cols_vary = "slowest", 
                      values_to = c("std_error")) %>% 
  filter(estimator %like% "re_") %>% 
  rename(relative_efficiency2 = std_error)

## % increase in precision in using covariate and ML adjustment versus no adjustment and misspecified linear model
efficiency_table2 <- merge(re3, re4, sort = FALSE, by = c("dgp", "analysis_type", "analyzed_sample", "estimator")) 
# write.csv(efficiency_table2, file = "../data/efficiency_incompleteb.csv", row.names = FALSE) ## file to include in full analysis results [save the full analysis results as full_analysis_results(c)]


eff_tab2 <- efficiency_table2 %>% 
  dplyr::filter(!(dgp == "n = 500, SD = 23" | dgp == "n = 500, SD = 68" | dgp == "n = 2000, SD = 23"| dgp == "n = 2000, SD = 68"))
## Prepare the tables
obs_analysis <- eff_tab2 %>% 
  dplyr::filter(analysis_type == "Observed")
obs_m_analysis <- eff_tab2 %>% 
  dplyr::filter(analysis_type == "Observed modified")
incomp_re <- cbind(obs_analysis, obs_m_analysis)[, c(4, 1, 3, 5, 6, 11, 12)]

# write.csv(incomp_re, file = "../data/incomp_re[b].csv", row.names = FALSE)
## load the saved relative efficiency file comparing efficiency gains using observed and 
## observed modified analysis types under incomplete data and missing outcome data on some observations
## and using incorrectly specified adjustment models

incomp_re2 <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/incomp_re[b].csv")
observed_data_efficiency <- incomp_re2 %>% 
  kable(format = "latex", row.names = FALSE, align = c("lllcccc"),
        col.names = c("Estimator","DGP", "$n$", "RE[note]", 
                      "RE[note]", "RE[note3]", "RE[note4]"),
        caption = "Relative efficiency using the unadjusted estimator and linear regression to machine learning covariate adjustment under missing outcome data and misspecification of the adjustment model. The missing outcome data mechanism is MAR with $R \\sim \\text{Ber}(\\pi)$ where $\\pi = \\text{expit}(-z_{i1}+0.5z_{i2}- 0.25z_{i3}-0.1z_{i4})$.",
        booktabs = TRUE, linesep = '', label = "missing-data-efficiency", digits = 2, escape = FALSE) 
observed_data_efficiency <- base::gsub("1.00", "--", observed_data_efficiency)

observed_data_efficiency<- observed_data_efficiency %>% 
  add_header_above(header = c(" " = 3, "Observed analysis[note]" = 2, "Modified analysis[note]" = 2), 
                   escape = FALSE, bold = TRUE, line = TRUE, line_sep = 0) %>% 
  #kable_styling(bootstrap_options = ".table-hover") %>% 
  row_spec(0, bold = TRUE) %>% 
  row_spec(row = c(3, 4, 7, 8, 11, 12, 15, 16, 19, 20, 23, 24, 27, 28, 31, 32, 35, 36), background = "lightgray", extra_css = "line-height: 7px") %>% 
  add_footnote(label = c("Analysis restricted to individuals with observed outcomes.", "Analysis restricted to individuals with observed outcomes and predicted potential outcomes for everyone in each study arm.", "Relative efficiency comparing the unadjusted estimator to linear regression and machine learning.", "Relative efficiency comparing misspecified linear regression to machine learning adjustment."), notation = "symbol") %>% 
  footnote(general = c("DGP = data generating mechanism; n = actual analyzed sample; RE = relative efficiency."), general_title = "Notes:", footnote_as_chunk = FALSE, escape = FALSE, threeparttable = FALSE)
observed_data_efficiency

## Figure for bias in the estimated treatment effects under missing outcome data analysis in main paper
##------------------------------------------------------------------------------

df <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/full_analysis_results.csv")
## jpeg(filename = "../figures/bias11_nyu.jpg", width = 8, height = 8, units = 'in', res = 300)
#new = c("n = 2000, SD = 1", "n = 2000, SD = 45", "n = 500, SD = 1", "n = 500, SD = 45")
dx = df %>% dplyr::filter(!estimator %in% c("IMLR", "urf", "unadj"))
dx$dgp <- factor(dx$dgp, labels = c("n = 1003, SD = 1", "n = 1003, SD = 45", "n = 244, SD = 1", "n = 244, SD = 45"))
bias_correction = ggplot2::ggplot(filter(dx, analysis_type == "Observed"), aes(x = reorder(as.factor(estimator), std_error), 
                                                             y = bias, color = dgp,
                                                             group = dgp))+
  geom_pointrange(aes(ymin = bias - std_error, ymax = bias + std_error), position = position_dodge(.9))+ theme_bw()+
  geom_hline(yintercept = 0.00, linetype = "dashed", color = "black")+
  labs(x = "Estimator", y = "Bias")+
  facet_grid(~dgp) + theme(axis.text.x = element_text(angle = 90))+
  theme(panel.spacing = unit(2, "lines"),
        legend.position = "top",
        text = element_text(size = 11),
        strip.text.x = element_text(face = "bold"))+
  scale_color_npg(name = "Residual error", label = c("SD = 1", "SD = 45", "SD = 1", "SD = 45"))+ 
  scale_x_discrete(labels = c("Correct", "BART", "SL", "XGBoost", "MLR", "RF"))+
  ylab(TeX(r'(Bias, $\E(\hat{\theta}) - \theta$)'))
bias_correction


################################################################################
# INFORMATIVE MISSING DATA
################################################################################

##----------------------------------------------------
## Simulation setting 3
##----------------------------------------------------
### Precision of the treatment effect estimates
mna_five <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/s3_trt_prec.csv", header = FALSE)[ -1, ]
rownames(mna_five) <- NULL
names(mna_five) <- NULL
sim_3a <- kable(mna_five,
      format = "latex",
      caption = "Treatment effect estimates and their standard errors averaged over 1000 simulated datasets comparing full and incomplete specifications of adjustment models based on the observed covariates $X$ by analysis type. This setting induced a treatment-by-covariate interaction with $\\text{R}\\sim \\text{Ber}(\\pi)$ where $\\pi = \\text{expit}(A_i + (A_i z_{i1})-z_{i1}+0.5z_{i2}-0.25z_{i3}-0.1z_{i4}).$",
      booktabs = TRUE,
      align = c("lllccccccccccccc"),
      linesep = '',
      escape = FALSE,
      label = "mna-fivea") %>% 
  add_header_above(header = c(" " = 3, "Correct" = 2, "MLR" = 2, "IMLR" = 2, "BART" = 2, "SL" = 2, "XGBoost" = 2, "RF" = 2), line_sep = 0, bold = TRUE) %>% 
  row_spec(c(0, 1), bold = TRUE, hline_after = TRUE) %>% 
  kable_styling(latex_options = "scale_down") %>% 
  column_spec(1, bold = TRUE) %>% 
  row_spec(c(5, 6, 10, 11, 15, 16), background = "lightgray") %>% 
  column_spec(c(1, 3:17), width = "0.7cm") %>% 
  column_spec(2, width = "1.5cm") %>% 
  footnote(general = c("DGP: Data generating process; $n$: actual analyzed sample;", "Full: full specification of the misspecified adjustment model; Miss.: Incomplete specification of the adjustment model;", "MLR: Multiple Linear Regression; IMLR: Interacted Multiple Linear Regression;", "BART: Bayesian Additive Regression Trees; SL: SuperLearner;", "XGBoost: eXtreme Gradient Boosted Regression Trees; RF:Random Forest."), general_title = "Notes:", escape = FALSE)
sim_3a


### Bias of the estimated treatment effect estimates
### under informative/induced missing data
###--------------------------------------------------
mna_six <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/s3_trt_bias.csv", header = FALSE)[ -1, ]
rownames(mna_six) <- NULL
names(mna_six) <- NULL


sim_3b <- kable(mna_six,
      format = "latex",
      caption = "Bias of the treatment effect estimates averaged over 1000 simulated datasets comparing full and incomplete specifications of the misspecified adjustment models by analysis type. We induced a treatment-by-covariate interaction with $\\text{R}\\sim \\text{Ber}(\\pi)$ where $\\pi = \\text{expit}(A_i + (A_i z_{i1})-z_{i1}+0.5z_{i2}-0.25z_{i3}-0.1z_{i4}).$",
      booktabs = TRUE,
      align = c("llcccccccccccccc"),
      linesep = '',
      escape = FALSE,
      label = "mna-fiveb", digits = 2) %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  add_header_above(header = c(" " = 3, "Correct" = 2, "MLR" = 2, "IMLR" = 2, "BART" = 2, "SL" = 2, "XGBoost" = 2, "RF" = 2), line_sep = 0, bold = TRUE) %>% 
  row_spec(c(0,1), bold = TRUE) %>% 
  column_spec(1, bold = TRUE) %>% 
  column_spec(2, width = "1.5cm") %>% 
  row_spec(c(5, 6, 10, 11, 15, 16), background = "lightgray") %>% 
  column_spec(c(1, 3:17), width = "0.8cm") %>% 
  footnote(general = c("DGP: Data generating process; n: actual analyzed sample;", "Full: full specification of the misspecified adjustment model; Miss.: Incomplete specification of the adjustment model;", "MLR: Multiple Linear Regression; IMLR: Interacted Multiple Linear Regression;", "BART: Bayesian Additive Regression Trees; SL: SuperLearner;",  "XGBoost: eXtreme Gradient Boosted Regression Trees; RF:Random Forest.")) 
sim_3b


################################################################################
## APPLICATION TO THE BIGPIC STUDY
################################################################################


## Baseline covariates table

load("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/ds.RData")
dim(ds) ## 2890 obs on 133 variables
##--------------------------------------------------------------
names(ds)
## total of missing data in the data frame
sum(is.na(ds))
sum(is.na(ds))/prod(dim(ds))*100 # 60% missing values in the whole data frame

dsx = subset(ds, arm.refUC2 == "UC")

## missing data on variables in the primary outcome analysis
table_one <- apply(dsx, 2, function(x) sum(is.na(x))/nrow(dsx)*100)
table_one #sbp.change has 7.14% and cluster specific baseline sbp has 0.10% missing data. Other variables have no missing values.


dsx2 = subset(ds, arm.refUC2 == "UCMF")
table_one <- apply(dsx2, 2, function(x) sum(is.na(x))/nrow(dsx2)*100)
table_one


dsx3 = subset(ds, arm.refUC2 == "UCMF")
table_one <- apply(dsx2, 2, function(x) sum(is.na(x))/nrow(dsx2)*100)
table_one
sum(is.na(ds$sbp.change)) ## 413 observations missing outcome data

bpic.df <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/bart_mlr.csv")[, -c(5,6)]
bpic.l <- bpic.df %>%  
  dplyr::filter(Treatment %in% c("MF-UC", "GMV-UC", "GMVMF-UC")) %>% 
  tidyr::pivot_wider(names_from = "Estimator", values_from = c("Estimate", "Std.Error"))
bpic.l = bpic.l[, c(1, 2, 4, 3, 5)]





## BIGPIC TABLE
##------------------------------------------------------------------------------
## Print the table of the results from BIGPIC study comparing LME to MLR and ML models
bpic_table <- kable(bpic.l, format = "latex", caption = "Primary outcome results comparing MLR adjustment and BART under complete-case analysis. Treatment effects and the standard errors are estimated as an average across n = 1000 bootstrap samples for BART.",
                    col.names = c("Treatment arm", "Treatment effect", "Standard error", "Treatment effect", "Standard error"),
                    booktabs = TRUE,
                    align = c("lcccc"),
                    linesep = '',
                    label = "bigpic-models",
                    digits = 3) %>% 
  kable_styling(font_size = 10, latex_options = "scale_down") %>%
  add_header_above(header = c(" " = 1, "MLR" = 2, "BART" = 2), bold = TRUE) %>% 
  row_spec(c(0), bold = TRUE) %>% 
  column_spec(1, bold = TRUE) %>% 
  #row_spec(c(1:3, 7:9, 13:15), background = "lightgray") %>% 
  footnote(general = c("BART = Bayesian Additive Regression Trees;",  "MLR = Multiple Linear Regression;", "UC = Usual Care; GMV = Group medical visits;", "MF = Microfinance; GMVMF = Group medical visit plus microfinance.")) 
bpic_table


bpic = bpic.df %>% mutate_if(is.character, as.factor)
bpic = bpic[c(1:4, 8:11), ]

bpic$Estimator <- factor(bpic$Estimator, labels = c("BART", "LR"))
bpic_figure = ggplot(data = bpic, aes(x = Treatment, y = Estimate, color = Estimator,
                                      ymin = Estimate - Std.Error, 
                                      ymax = Estimate + Std.Error), group = Estimator)+
  theme_bw()+
  geom_pointrange(size = 0.6) +
  theme(panel.spacing = unit(2, "lines"),
        text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(face = "bold"))+
  facet_wrap(~Estimator, scales = "free_x") + scale_color_npg(name = "Estimator", label = c("BART", "LR"))
bpic_figure


## BIGPIC when using observed modified analysis and bootstrap for computing estimated effects
##--------------------------------
bt <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/bart_mlr_boot.csv")

bigpic <- kable(bt,
                format = "latex",
                caption = "Estimated average change in systolic blood pressure by treatment arm and associated standard errors computed over 1000 bootstrap samples using BART and MLR.",
                digits = 2,
                booktabs = TRUE,
                align = c("lcccccc"),
                linesep = '',
                escape = FALSE,
                label = "bart_mlr2", 
                col.names = c("Treatment", "Estimate (SE)", "Lower", "Upper", "Estimate (SE)", "Lower", "Upper" )) %>% 
  add_header_above(header = c(" " = 2, "$95\\\\%$ Confidence Interval" = 2, " " = 1, "$95\\\\%$ Credible Interval" = 2), bold = TRUE, escape = FALSE) %>%
  add_header_above(header = c(" " = 1, "LR" = 3, "BART" = 3), bold = TRUE) %>%
  kableExtra::group_rows(group_label = "Unadjusted effect", start_row = 1, end_row = 7, bold = TRUE) %>% 
  kableExtra::group_rows(group_label = "Adjusted effect", start_row = 8, end_row = 14, bold = TRUE, hline_before = TRUE, hline_after = TRUE) %>% 
  kable_styling(latex_options = c("scale_down", "hold_position"), font_size = 9) %>% 
  row_spec(c(0), bold = TRUE) %>% 
  column_spec(1, bold = TRUE) %>% 
  row_spec(c(5:7, 12:14), background = "lightgray") %>% 
  footnote(general = c("Highlighted rows denote the estimated average reduction in systolic blood pressure for each arm relative to UC", "LR = Linear Regression;", "BART = Bayesian Additive Regression Trees;", "UC = Usual Care;", "MF = Microfinance;", "GMV = Group Medical Visit;", "GMVMF = Group Medical Visit plus MF."), general_title = "Notes: ", escape = FALSE)
bigpic



################################################################################
#########################APPENDICES
################################################################################


##------------------------------
## MISSING AT RANDOM SETTING ONE
##------------------------------

## Precision of the estimated treatment effects
##----------------------------------------------
sim_s1_prec <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/s1_trt_prec.csv", header = FALSE)[ -1, ]
rownames(sim_s1_prec) <- NULL
names(sim_s1_prec) <- NULL
sim_s1_prec <- kable(sim_s1_prec,
                format = "latex",
                caption = "Treatment effect estimates and their associated standard errors averaged over 1000 simulated data sets under MAR.",
                booktabs = TRUE,
                align = c("lllccccccccccccc"),
                linesep = '',
                escape = FALSE,
                label = "sim_one_prec") %>% 
  kable_styling(latex_options = c("scale_down", "hold_position"), font_size = 9) %>% 
  add_header_above(header = c(" " = 3, "Correct" = 2, "MLR" = 2, "IMLR" = 2, "BART" = 2, "SL" = 2, "XGBoost" = 2, "RF" = 2), line_sep = 0, bold = TRUE) %>% 
  row_spec(c(0, 1), bold = TRUE, hline_after = TRUE) %>% 
  column_spec(1, bold = TRUE) %>% 
  row_spec(c(5, 6, 10, 11, 15, 16), background = "lightgray") %>% 
  column_spec(c(1, 3:17), width = "1.2cm") %>% 
  column_spec(2, width = "1.5cm") %>% 
  footnote(general = c("DGP: Data generating process; n: actual analyzed sample;", "Full: adjustment model corresponds to a complete specification of the observed covariates $X$; Miss.: adjustment model omits $X_1$ and is thus incompletely specified;", "MLR:Multiple Linear Regression; IMLR: Interacted Multiple Linear Regression;", "BART: Bayesian Additive Regression Trees; SL: SuperLearner;", "XGBoost: eXtreme Gradient Boosted Regression Trees; RF:Random Forest."), general_title = "Notes: ", escape = FALSE) #%>% 
  #landscape()
sim_s1_prec

## Bias of the estimated treatment effects
##----------------------------------------------
sim_s1_bias <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/s1_trt_bias.csv", header = FALSE)[ -1, ]
rownames(sim_s1_bias) <- NULL
names(sim_s1_bias) <- NULL
sim_s1_bias <- kable(sim_s1_bias,
                     format = "latex",
                     caption = "Bias in the estimated treatment effects averaged over 1000 simulated data sets under MAR.",
                     booktabs = TRUE,
                     align = c("lllccccccccccccc"),
                     linesep = '',
                     escape = FALSE,
                     label = "sim_one_bias") %>% 
  kable_styling(font_size = 9, latex_options = c("hold_position", "scale_down"))%>% 
  add_header_above(header = c(" " = 3, "Correct" = 2, "MLR" = 2, "IMLR" = 2, "BART" = 2, "SL" = 2, "XGBoost" = 2, "RF" = 2), line_sep = 0, bold = TRUE) %>% 
  row_spec(c(0, 1), bold = TRUE, hline_after = TRUE) %>% 
  column_spec(1, bold = TRUE) %>% 
  row_spec(c(5, 6, 10, 11, 15, 16), background = "lightgray") %>% 
  column_spec(c(1, 3:17), width = "1cm") %>% 
  column_spec(2, width = "1.5cm") %>% 
  footnote(general = c("DGP: Data generating process; n: actual analyzed sample;", "Full: adjustment model corresponds to a complete specification of the observed covariates $X$; Miss.: adjustment model omits $X_1$ and is thus incompletely specified;", "MLR:Multiple Linear Regression; IMLR: Interacted Multiple Linear Regression;", "BART: Bayesian Additive Regression Trees; SL: SuperLearner;", "XGBoost: eXtreme Gradient Boosted Regression Trees; RF:Random Forest."), general_title = "Notes: ", escape = FALSE) #%>% 
  #landscape()
sim_s1_bias


##------------------------------
## MISSING AT RANDOM SETTING TWO
##------------------------------

## Precision of the estimated treatment effects
##---------------------------------------------
sim_s2_prec <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/s2_trt_prec.csv", header = FALSE)[ -1, ]
rownames(sim_s2_prec) <- NULL
names(sim_s2_prec) <- NULL
sim_s2_prec <- kable(sim_s2_prec,
                     format = "latex",
                     caption = "Bias in the testimated reatment effects averaged over 1000 simulated data sets when the missing outcome data mechanism is a function of $A$ and $Z$.",
                     booktabs = TRUE,
                     align = c("lllccccccccccccc"),
                     linesep = '',
                     escape = FALSE,
                     label = "sim_two_prec") %>% 
  kable_styling(latex_options = c("scale_down", "hold_position"), font_size = 9) %>% 
  add_header_above(header = c(" " = 3, "Correct" = 2, "MLR" = 2, "IMLR" = 2, "BART" = 2, "SL" = 2, "XGBoost" = 2, "RF" = 2), line_sep = 0, bold = TRUE) %>% 
  row_spec(c(0, 1), bold = TRUE, hline_after = TRUE) %>% 
  column_spec(1, bold = TRUE) %>% 
  row_spec(c(5, 6, 10, 11, 15, 16), background = "lightgray") %>% 
  column_spec(c(1, 3:17), width = "1cm") %>% 
  column_spec(2, width = "1.5cm") %>% 
  footnote(general = c("DGP: Data generating process; n: actual analyzed sample;", "Full: adjustment model corresponds to a complete specification of the observed covariates $X$; Miss.: adjustment model omits $X_1$ and is thus incompletely specified;", "MLR:Multiple Linear Regression; IMLR: Interacted Multiple Linear Regression;", "BART: Bayesian Additive Regression Trees; SL: SuperLearner;", "XGBoost: eXtreme Gradient Boosted Regression Trees; RF:Random Forest."), general_title = "Notes: ", escape = FALSE) #%>% 
  #landscape()
sim_s2_prec



## Bias of the estimated treatment effects
##-----------------------------------------
sim_s2_bias <- read.csv("/Users/aokutse/Library/CloudStorage/OneDrive-BrownUniversity/ThesisPaper/paper/data/s2_trt_bias.csv", header = FALSE)[ -1, ]
rownames(sim_s2_bias) <- NULL
names(sim_s2_bias) <- NULL
sim_s2_bias <- kable(sim_s2_bias,
                     format = "latex",
                     caption = "Bias in the estimated treatment effects averaged over 1000 simulated data sets when the missing outcome data mechanism is a function of $A$ and $Z$.",
                     booktabs = TRUE,
                     align = c("lllccccccccccccc"),
                     linesep = '',
                     escape = FALSE,
                     label = "sim_two_bias") %>% 
  kable_styling(latex_options = c("scale_down"), font_size = 9) %>% 
  add_header_above(header = c(" " = 3, "Correct" = 2, "MLR" = 2, "IMLR" = 2, "BART" = 2, "SL" = 2, "XGBoost" = 2, "RF" = 2), line_sep = 0, bold = TRUE) %>% 
  row_spec(c(0, 1), bold = TRUE, hline_after = TRUE) %>% 
  column_spec(1, bold = TRUE) %>% 
  row_spec(c(5, 6, 10, 11, 15, 16), background = "lightgray") %>% 
  column_spec(c(1, 3:17), width = "1cm") %>% 
  column_spec(2, width = "1.5cm") %>% 
  footnote(general = c("DGP: Data generating process; n: actual analyzed sample;", "Full: adjustment model corresponds to a complete specification of the observed covariates $X$; Miss.: adjustment model omits $X_1$ and is thus incompletely specified;", "MLR:Multiple Linear Regression; IMLR: Interacted Multiple Linear Regression;", "BART: Bayesian Additive Regression Trees; SL: SuperLearner;", "XGBoost: eXtreme Gradient Boosted Regression Trees; RF:Random Forest."), general_title = "Notes: ", escape = FALSE) #%>% 
  #landscape()
sim_s2_bias

