# Frequentist Equivalence Testing {#Frequentist-Equivalence}

In the lectures, we mentioned equivalence testing is another option for statistically supporting there is no meaningful effect present. No technique can support the null hypothesis is precisely 0, so equivalence testing flips the standard hypothesis testing logic around. 

Instead of testing against a single point-null hypothesis of 0, you can apply two one-sided tests to boundaries representing your smallest effect size of interest. You apply a one-sided test to a lower bound and see whether your effect is significantly higher. You apply a one-sided test to an upper bound and see whether your effect is significantly lower. If both tests are significant, you can conclude your effect is statistically equivalent, at least within your chosen boundaries. 

You can apply this technique to different focal tests such as t-tests, correlations, and meta-analyses. The package we will use is called <code class='package'>TOSTER</code>. Make sure you install it before running any of the following code, and load the necessary packages for this appendix. 


```r
# Applying equivalence testing in R 
library(TOSTER)
# Wrangling and plotting functions
library(tidyverse)
```

To follow along to this appendix and try the code yourself, please download the data files we will be using in [this zip file](data/equivalence_testing.zip).

## t-tests

For a t-test applied to two groups, we will reanalyse data from @bastian_pain_2014. This study wanted to investigate whether experiencing pain together can increase levels of bonding between participants. The study was trying to explain how people often say friendships are strengthened by adversity. 

Participants were randomly allocated into two conditions: pain or control. Participants in the pain group experienced mild pain through a cold pressor task (leaving your hand in ice cold water) and a wall squat (sitting against a wall). The control group completed a different task that did not involve pain. The participants then completed a scale to measure how bonded they felt to other participants in the group. Higher values on this scale mean greater bonding. 

The independent variable is called <code><span class='st'>"CONDITION"</span></code>. The control group has the value 0 and the pain group has the value 1. They wanted to find out whether participants in the pain group would have higher levels of bonding with their fellow participants than participants in the control group. After a little processing, the dependent variable is called <code><span class='st'>"mean_bonding"</span></code> for the mean of 7 items related to bonding. 


```r
Bastian_data <- read_csv("data/Bastian.csv")

# Relabel condition to be more intuitive which group is which 
Bastian_data$CONDITION <- factor(Bastian_data$CONDITION, 
                                   levels = c(0, 1), 
                                   labels = c("Control", "Pain"))

# We also need to get our DV from the mean of 7 items
Bastian_data <- Bastian_data %>% 
  pivot_longer(names_to = "item", # var for item names
               values_to = "score", # var for item scores
               cols = group101:group107) %>% # Range of columns for group bonding items
  group_by(across(.cols = c(-item, -score))) %>% # Group by everything but ignore item and score
  summarise(mean_bonding = mean(score)) %>% # Summarise by creating a subscale name and specify sum or mean
  ungroup() # Always ungroup
```

For t-tests, the package has shifted to a single function that can be applied in different ways: 

- For a one-sample t-test, you must enter the mean, SD, and sample size for the sample, alongside the value of mu to test against. 

- For an independent samples t-test, you must enter the mean, SD, and sample size for each group. 

- For a paired samples t-test, you must enter the mean and SD for each condition, the sample size, and the correlation between conditions. 

Bastian et al. requires the independent samples t-test inputs. The first step will be calculating the summary statistics for our two groups and saving them to clearly label and enter into the TOSTER function later. You could enter the values directly into the function, but I like to calculate them first and save them as objects to avoid copy-paste errors. 


```r
# Group by condition to get one row for control and pain values
Brandt_summary <- Bastian_data %>% 
  group_by(CONDITION) %>% 
  summarise(mean = mean(mean_bonding),
            sd = sd(mean_bonding),
            n = n()) %>% 
  as.data.frame() # Doesn't work well with tibbles

# Save values for Control
Control_mean <- Brandt_summary[1, 2]
Control_sd <- Brandt_summary[1, 3]
Control_n <- Brandt_summary[1, 4]

# Save values for Pain
Pain_mean <- Brandt_summary[2, 2]
Pain_sd <- Brandt_summary[2, 3]
Pain_n <- Brandt_summary[2, 4]
```

Now we have our values, it is time to think about what values we will use for the boundaries. From the lecture, there are different strategies (this is a non-exhaustive list) to choosing and justifying your smallest effect size of interest boundaries: 

1. Your understanding of the applications / mechanisms (e.g., a clinically meaningful decrease in pain). 

2. Smallest effects from previous research (e.g., lower bound of individual study effect sizes or lower bound of a meta-analysis).

3. Small telescopes (the effect size the original study had 33% power to detect).

For Bastian et al. (2014), they measured bonding on a 5-point Likert scale, so we might consider anything less than a one-point difference as too small to be practically meaningful for demonstration purposes. We can now enter these values into the TOSTER function. 


```r
# Save as object and present with brackets around 
(Brandt_TOST <- tsum_TOST(m1 = Control_mean, # Group 1: Control
                         sd1 = Control_sd,
                         n1 = Control_n,
                         m2 = Pain_mean, # Group 2: Pain
                         sd2 = Pain_sd,
                         n2 = Pain_n,
                         hypothesis = "EQU", # Equivalence test rather than minimal effects test
                         low_eqbound = -1, # Our lower and upper equivalence bounds
                         high_eqbound = 1,
                         eqbound_type = "raw", # We can specify raw or standardised values
                         alpha = 0.05)) # We could change alpha from the default
```

```
## 
## Welch Modified Two-Sample t-Test
## Hypothesis Tested: Equivalence
## Equivalence Bounds (raw):-1.000 & 1.000
## Alpha Level:0.05
## The equivalence test was non-significant, t(51.7) = 1.484, p = 7.2e-02
## The null hypothesis test was significant, t(51.7) = -2.022, p = 4.84e-02
## NHST: reject null significance hypothesis that the effect is equal to zero 
## TOST: don't reject null equivalence hypothesis
## 
## TOST Results 
##                    t        SE       df      p.value
## t-test     -2.021774 0.2852542 51.69508 4.839002e-02
## TOST Lower  1.483871 0.2852542 51.69508 7.195754e-02
## TOST Upper -5.527420 0.2852542 51.69508 5.390001e-07
## 
## Effect Sizes 
##                 estimate        SE  lower.ci    upper.ci conf.level
## Raw           -0.5767196 0.2852542 -1.054483 -0.09895642        0.9
## Hedges' g(av) -0.5422288 0.2830491 -1.024025 -0.09475207        0.9
## 
## Note: SMD confidence intervals are an approximation. See vignette("SMD_calcs").
```

We have quite a lot of output to break down and there are essentially three parts. 

The first provides an overview of the two one-sided test procedure you applied and what you can conclude from it. After confirming your inputs, you have one t-test for the equivalence test and one traditional t-test. You only get one for the equivalence test as both must be significant to conclude the effect was statistically equivalent, so you typically report the largest *p*-value as if that is significant, the other test will be lower. You then get two one sentence statements on what you can conclude from the tests. For this example, the traditional t-test was significant to suggest we can reject the point-null hypothesis, but the equivalence test was not statistically equivalent for our choice of boundaries. 

The second provides the full output for all three t-tests: the traditional t-test and the two one-sided tests. This is more helpful for reporting as you can produce the full t-test statistics, for example the one-sided test with the largest p-value would be: *t* (51.70) = 1.48, *p* = .07. The degrees of freedom is not an integer as it produces the Welch version of the t-test ([Delacre et al., 2017](http://www.rips-irsp.com/articles/10.5334/irsp.82/)). If you really want the student t-test, you can use the argument `var.equal = TRUE`.  

Finally, the third provides estimates and 90% confidence intervals for the effect size in raw and standardised units. This is the estimate of your effect size and what you can see in the plots below. Remember, you get a 90% confidence interval as the specified alpha is .05 but you are performing two one-sided tests. So, the overall confidence level is .90. 

For the t-test TOSTER function, we do not get an accompanying plot by default. However, since we saved the model object, we can call a plot to visualise the equivalence testing procedure. 


```r
plot(Brandt_TOST)
```

<img src="appendix-b-equivalence-testing_files/figure-html/Brandt TOST plot-1.png" width="100%" style="display: block; margin: auto;" />

This plots both the raw and standardised effects from the final part of the output above. The vertical dashed lines represent your user-defined bounds for the smallest effect size of interest. The circle and thick black line at the bottom of each panel represent the estimate for the effect size and its 90% confidence interval. This is what you are using for the equivalence testing procedure. If the confidence interval is entirely within your bounds, the test is statistically equivalent. However, in this scenario, the confidence interval crosses the lower bound, visually reinforcing how the largest one-sided test above was not statistically significant. 

As additional information, note there are different confidence levels plotted above and colour coded. The confidence interval bar reaches the end of the green region for 90%. You also have two further levels of 95% and 99.9% confidence intervals. 

## Correlation

## Meta-analysis

## Further reading

In this appendix, we mainly focused on the practical part of applying equivalence testing in R. Possibly the hardest part of this technique is choosing and justifying your equivalence bounds. Therefore, there are some recommendations for further reading to explore more of the topic. 

- [Lakens (2023)](https://lakens.github.io/statistical_inferences/equivalencetest.html) has an online open access textbook on improving your statistical inferences. Chapter 9 focuses on equivalence testing and explains different methods for setting your smallest effect size of interest. 

- [Lakens (2017)](https://journals.sagepub.com/doi/10.1177/1948550617697177) and [Lakens et al. (2018)](https://journals.sagepub.com/doi/10.1177/2515245918770963) have two tutorial articles where they walk through applying and reporting equivalence testing. 

- [Bartlett et al. (2022)](https://journal.trialanderror.org/pub/attentionalsmoker/release/2) provides a published example of equivalence testing if you want to see how it can look and be reported in a results section. 
