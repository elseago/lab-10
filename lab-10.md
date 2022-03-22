Lab 10 - Grading the professor, Pt. 2
================
Elayna Seago
3/22/22

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
library(MASS)
```

### Exercise 1

score = 3.88 + (x\*.066) r-squared-.035 adjusted-r-squared-.032

``` r
m_bty <- lm(score ~ bty_avg, data=evals)  

print(m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Coefficients:
    ## (Intercept)      bty_avg  
    ##     3.88034      0.06664

``` r
summary(m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

### Exercise 2

#score = 3.74 + (bty score*.074) + (gender*.17239) R squared = .059
Adjusted R squared = .055

``` r
m_bty_gen <- lm(score ~ bty_avg + gender, data=evals)

print(m_bty_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender, data = evals)
    ## 
    ## Coefficients:
    ## (Intercept)      bty_avg   gendermale  
    ##     3.74734      0.07416      0.17239

``` r
summary(m_bty_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

#Exercise 3

#score = 3.74 + (bty score*.074) + (gender*.17239)

the intercept is a professors expected score if they have a beauty score
of zero and are female.

slope 1 (.074) is the amount that each professors score is expected to
increase if they have an increase in beauty score of 1.

slope 2 (.172) is the amount that male professors score is higher on
average (for male professor the multiplier is 1, for females it is 0).

#Exercise 4

5.5% variance explained

# Exercise 5

#male score = 3.74 + (.074*x) + (.17239*1)

# Exercise 6

males

# Exercise 7

I don’t think I can tell the difference with the information I have.

# Exercise 8

bty r-squared-.035 adjusted-r-squared-.032

bty_gen R squared = .059 Adjusted R squared = .055

Adding gender leads to more variance explained in both R squared and
adjusted r squared. it adds about 2% explanatory power.

# Exercise 9

bty=.066 bty_gen=.074 yes, it has changed it.

# Exercise 10

``` r
m_bty_rank <- lm(score ~ bty_avg + rank, data=evals)

print(m_bty_rank)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + rank, data = evals)
    ## 
    ## Coefficients:
    ##      (Intercept)           bty_avg  ranktenure track       ranktenured  
    ##          3.98155           0.06783          -0.16070          -0.12623

``` r
summary(m_bty_rank)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + rank, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

teaching track: score= 4.28 + (0) + (0)

tenure track: score = 4.28 + (1\*-.1297) + (0)

tenured: score = 4.28 + (0) + (1\*-.1452)

the intercept = 4.28, this is a professors score at level 0 0 (aka
teaching track) slope 1 = -.1297, this is how much a professors score
decreases at level 1 0 (in the contrast matrix, this is a tenure track
professor) slope 2 = -.1452, this is how much a professors score
decreases at level 0 1 (tenured)

# Exercise 11

I think cls did eval is least helpful because the size of the class
varies so the number of people who did it isn’t really informative.

# Exercise 12

this is somewhat correct, not much explanatory power.

``` r
m_cls_did_eval <- lm(score ~ cls_did_eval, data=evals)

print(m_cls_did_eval)
```

    ## 
    ## Call:
    ## lm(formula = score ~ cls_did_eval, data = evals)
    ## 
    ## Coefficients:
    ##  (Intercept)  cls_did_eval  
    ##    4.1469347     0.0007589

``` r
summary(m_cls_did_eval)
```

    ## 
    ## Call:
    ## lm(formula = score ~ cls_did_eval, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8545 -0.3595  0.1303  0.4269  0.8485 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.1469347  0.0325682 127.331   <2e-16 ***
    ## cls_did_eval 0.0007589  0.0005616   1.351    0.177    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5434 on 461 degrees of freedom
    ## Multiple R-squared:  0.003946,   Adjusted R-squared:  0.001786 
    ## F-statistic: 1.827 on 1 and 461 DF,  p-value: 0.1772

# exercise 13

I would not include cls_did_eval because it does not add any additional
information beyond what we get from # of students and percent who filled
out the survey.

# exercise 14

``` r
m_full_model <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data=evals)

print(m_full_model)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Coefficients:
    ##           (Intercept)       ranktenure track            ranktenured  
    ##             3.5305036             -0.1070121             -0.0450371  
    ## ethnicitynot minority             gendermale    languagenon-english  
    ##             0.1869649              0.1786166             -0.1268254  
    ##                   age          cls_perc_eval           cls_students  
    ##            -0.0066498              0.0056996              0.0004455  
    ##        cls_levelupper        cls_profssingle  cls_creditsone credit  
    ##             0.0187105             -0.0085751              0.5087427  
    ##               bty_avg  
    ##             0.0612651

``` r
summary(m_full_model)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84482 -0.31367  0.08559  0.35732  1.10105 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.5305036  0.2408200  14.660  < 2e-16 ***
    ## ranktenure track      -0.1070121  0.0820250  -1.305 0.192687    
    ## ranktenured           -0.0450371  0.0652185  -0.691 0.490199    
    ## ethnicitynot minority  0.1869649  0.0775329   2.411 0.016290 *  
    ## gendermale             0.1786166  0.0515346   3.466 0.000579 ***
    ## languagenon-english   -0.1268254  0.1080358  -1.174 0.241048    
    ## age                   -0.0066498  0.0030830  -2.157 0.031542 *  
    ## cls_perc_eval          0.0056996  0.0015514   3.674 0.000268 ***
    ## cls_students           0.0004455  0.0003585   1.243 0.214596    
    ## cls_levelupper         0.0187105  0.0555833   0.337 0.736560    
    ## cls_profssingle       -0.0085751  0.0513527  -0.167 0.867458    
    ## cls_creditsone credit  0.5087427  0.1170130   4.348  1.7e-05 ***
    ## bty_avg                0.0612651  0.0166755   3.674 0.000268 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.504 on 450 degrees of freedom
    ## Multiple R-squared:  0.1635, Adjusted R-squared:  0.1412 
    ## F-statistic: 7.331 on 12 and 450 DF,  p-value: 2.406e-12

# Exercise 15

``` r
m_backward_model<-stepAIC(m_full_model , direction = "backward" , trace = FALSE)


summary(m_backward_model)
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_credits + bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9067 -0.3103  0.0849  0.3712  1.0611 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.446967   0.203191  16.964  < 2e-16 ***
    ## ethnicitynot minority  0.204710   0.074710   2.740 0.006384 ** 
    ## gendermale             0.184780   0.049889   3.704 0.000238 ***
    ## languagenon-english   -0.161463   0.103213  -1.564 0.118427    
    ## age                   -0.005008   0.002606  -1.922 0.055289 .  
    ## cls_perc_eval          0.005094   0.001438   3.543 0.000436 ***
    ## cls_creditsone credit  0.515065   0.104860   4.912 1.26e-06 ***
    ## bty_avg                0.064996   0.016327   3.981 7.99e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.503 on 455 degrees of freedom
    ## Multiple R-squared:  0.1576, Adjusted R-squared:  0.1446 
    ## F-statistic: 12.16 on 7 and 455 DF,  p-value: 2.879e-14

#score = 3.45 + (.204*x1) + (.184*x2) + (-.161*x3) + (-.005*x4) +
(.005*x5) + (.515*x6) + (.064\*x7)

# exercise 16
