Assignment 3 - Part 1 - Assessing voice in schizophrenia
--------------------------------------------------------

Individuals with schizophrenia (SCZ) tend to present voice atypicalities. Their tone is described as "inappropriate" voice, sometimes monotone, sometimes croaky. This is important for two reasons. First, voice could constitute a direct window into cognitive, emotional and social components of the disorder, thus providing a cheap and relatively non-invasive way to support the diagnostic and assessment process (via automated analyses). Second, voice atypicalities play an important role in the social impairment experienced by individuals with SCZ, and are thought to generate negative social judgments (of unengaged, slow, unpleasant interlocutors), which can cascade in more negative and less frequent social interactions.

Several studies show *significant* differences in acoustic features by diagnosis (see meta-analysis in the readings), but we want more. We want to know whether we can diagnose a participant only from knowing the features of their voice.

The corpus you are asked to analyse is a relatively large set of voice recordings from people with schizophrenia (just after first diagnosis) and matched controls (on gender, age, education). Each participant watched several videos of triangles moving across the screen and had to describe them (so you have several recordings per person). We have already extracted the pitch once every 10 milliseconds as well as several duration related features (e.g. number of pauses, etc).

N.B. For the fun of it, I threw in data from 3 different languages: 1) Danish (study 1-4); 2) Mandarin Chinese (Study 5-6); 3) Japanese (study 7). Feel free to only use the Danish data, if you think that Mandarin and Japanese add too much complexity to your analysis.

In this assignment (A3), you will have to discuss a few important questions (given the data you have). More details below.

*Part 1 - Can we find a difference in acoustic features in schizophrenia?* 1) Describe your sample number of studies, number of participants, age, gender, clinical and cognitive features of the two groups. Furthemore, critically assess whether the groups (schizophrenia and controls) are balanced. N.B. you need to take studies into account.

1.  Describe the acoustic profile of a schizophrenic voice: which features are different? E.g. People with schizophrenia tend to have high-pitched voice, and present bigger swings in their prosody than controls. N.B. look also at effect sizes. How do these findings relate to the meta-analytic findings?

2.  Discuss the analysis necessary to replicate the meta-analytic findings Look at the results reported in the paper (see meta-analysis in the readings) and see whether they are similar to those you get. 3.1) Check whether significance and direction of the effects are similar 3.2) Standardize your outcome, run the model and check whether the beta's is roughly matched (matched with hedge's g) which fixed and random effects should be included, given your dataset? E.g. what about language and study, age and gender? Discuss also how studies and languages should play a role in your analyses. E.g. should you analyze each study individually? Or each language individually? Or all together? Each of these choices makes some assumptions about how similar you expect the studies/languages to be. *Note* that there is no formal definition of replication (in statistical terms).

Your report should look like a methods paragraph followed by a result paragraph in a typical article (think the Communication and Cognition paper)

*Part 2 - Can we diagnose schizophrenia from voice only?* 1) Discuss whether you should you run the analysis on all studies and both languages at the same time You might want to support your results either by your own findings or by that of others 2) Choose your best acoustic feature from part 1. How well can you diagnose schizophrenia just using it? 3) Identify the best combination of acoustic features to diagnose schizophrenia using logistic regression. 4) Discuss the "classification" process: which methods are you using? Which confounds should you be aware of? What are the strength and limitation of the analysis?

Bonus question: Logistic regression is only one of many classification algorithms. Try using others and compare performance. Some examples: Discriminant Function, Random Forest, Support Vector Machine, Penalized regression, etc. The packages caret and glmnet provide them. Tidymodels is a set of tidyverse style packages, which take some time to learn, but provides a great workflow for machine learning.

Learning objectives
-------------------

-   Critically design, fit and report multilevel regression models in complex settings
-   Critically appraise issues of replication

Overview of part 1
------------------

In the course of this part 1 of Assignment 3 you have to: - combine the different information from multiple files into one meaningful dataset you can use for your analysis. This involves: extracting descriptors of acoustic features from each pitch file (e.g. mean/median, standard deviation / interquartile range), and combine them with duration and demographic/clinical files - describe and discuss your sample - analyze the meaningful dataset to assess whether there are indeed differences in the schizophrenic voice and compare that to the meta-analysis

There are three pieces of data:

1- Demographic data (<https://www.dropbox.com/s/6eyukt0r5du0xif/DemoData.txt?dl=0>). It contains

-   Study: a study identifier (the recordings were collected during 6 different studies with 6 different clinical practitioners in 2 different languages)
-   Language: Danish, Chinese and Japanese
-   Participant: a subject ID
-   Diagnosis: whether the participant has schizophrenia or is a control
-   Gender
-   Education
-   Age
-   SANS: total score of negative symptoms (including lack of motivation, affect, etc). Ref: Andreasen, N. C. (1989). The Scale for the Assessment of Negative Symptoms (SANS): conceptual and theoretical foundations. The British Journal of Psychiatry, 155(S7), 49-52.
-   SAPS: total score of positive symptoms (including psychoses, such as delusions and hallucinations): <http://www.bli.uzh.ch/BLI/PDF/saps.pdf>
-   VerbalIQ: <https://en.wikipedia.org/wiki/Wechsler_Adult_Intelligence_Scale>
-   NonVerbalIQ: <https://en.wikipedia.org/wiki/Wechsler_Adult_Intelligence_Scale>
-   TotalIQ: <https://en.wikipedia.org/wiki/Wechsler_Adult_Intelligence_Scale>

1.  Articulation.txt (<https://www.dropbox.com/s/v86s6270w39g0rd/Articulation.txt?dl=0>). It contains, per each file, measures of duration:

-   soundname: the name of the recording file
-   nsyll: number of syllables automatically inferred from the audio
-   npause: number of pauses automatically inferred from the audio (absence of human voice longer than 200 milliseconds)
-   dur (s): duration of the full recording
-   phonationtime (s): duration of the recording where speech is present
-   speechrate (nsyll/dur): average number of syllables per second
-   articulation rate (nsyll / phonationtime): average number of syllables per spoken second
-   ASD (speakingtime/nsyll): average syllable duration

1.  One file per recording with the fundamental frequency of speech extracted every 10 milliseconds (excluding pauses): <https://www.dropbox.com/sh/b9oc743auphzxbg/AAChUsvFc6dIQSlM9eQTL53Aa?dl=0>

-   time: the time at which fundamental frequency was sampled
-   f0: a measure of fundamental frequency, in Herz

NB. the filenames indicate: - Study: the study, 1-6 (1-4 in Danish, 5-6 in Mandarin Chinese) - D: the diagnosis, 0 is control, 1 is schizophrenia - S: the subject ID (NB. some controls and schizophrenia are matched, so there is a 101 schizophrenic and a 101 control). Also note that study 5-6 have weird numbers and no matched participants, so feel free to add e.g. 1000 to the participant ID in those studies. - T: the trial, that is, the recording ID for that participant, 1-10 (note that study 5-6 have more)

### Getting to the pitch data

You have oh so many pitch files. What you want is a neater dataset, with one row per recording, including a bunch of meaningful descriptors of pitch. For instance, we should include "standard" descriptors: mean, standard deviation, range. Additionally, we should also include less standard, but more robust ones: e.g. median, iqr, mean absoluted deviation, coefficient of variation. The latter ones are more robust to outliers and non-normal distributions.

Tip: Load one file (as a sample) and: - write code to extract the descriptors - write code to extract the relevant information from the file names (Participant, Diagnosis, Trial, Study) Only then (when everything works) turn the code into a function and use map\_df() to apply it to all the files. See placeholder code here for help.

``` r
#load packages
library(tidyverse, lmer4)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   0.8.3     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
pacman::p_load(lmerTest, simr, DescTools, goeveg, sjstats, lmer4, effsize)
```

    ## Installing package into '/Users/nicoledwenger/Library/R/3.6/library'
    ## (as 'lib' is unspecified)

    ## Warning: package 'lmer4' is not available (for R version 3.6.1)

    ## Warning: 'BiocManager' not available.  Could not check Bioconductor.
    ## 
    ## Please use `install.packages('BiocManager')` and then retry.

    ## Warning in p_install(package, character.only = TRUE, ...):

    ## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
    ## logical.return = TRUE, : there is no package called 'lmer4'

    ## Warning in pacman::p_load(lmerTest, simr, DescTools, goeveg, sjstats, lmer4, : Failed to install/load:
    ## lmer4

``` r
#create functions to read files and summarise
read_pitch <- function(filename) {
    # load data
    file <- read.delim(filename)
    # parse filename to extract study, diagnosis, subject and trial
    string <- str_split(filename, "\\D+", simplify = T)
    study <- str_extract(string[1,2], "\\d+")
    diagnosis <- str_extract(string[1,3], "\\d+")
    subject <- str_extract(string[1,4], "\\d+")
    trial <- str_extract(string[1,5], "\\d+")
    # extract pitch descriptors (mean, sd, iqr, etc)
    mean <- mean(file$f0)
    median <- median(file$f0)
    sd <- sd(file$f0)
    iqr <- IQR(file$f0)
    meanAD <- MeanAD(file$f0) #average absolute difference from the mean
    coef_var <- cv(file$f0) #relation of standard deviation and mean
    # combine all this data in one dataset
    data <- data.frame("Participant" = subject, "Study" = study, "Diagnosis" = diagnosis, "Trial" = trial, "mean" = mean, "median" = median, "sd" =sd, "iqr" = iqr, "meanAD" = meanAD, "coef_var" = coef_var)
    return(data)
    }

#test if function is working on one file
test_data = read_pitch("/Users/nicoledwenger/Documents/University/3SEMESTER/Experimental Methods 3/GitHub/Class5/data/Pitch/Study1D0S126T6_f0.txt")

#use function on all files
#pitch_data = list.files(path = "/Users/nicoledwenger/Documents/University/3SEMESTER/Experimental Methods 3/GitHub/Class5/", pattern = ".txt") %>%
## NB replace with your path to the files %>% 
  #purrr::map_df(read_pitch)

#save as csv file
#write_csv(pitch_data, "dataframeall")
```

### Now you need to merge demographic/clinical, duration and pitch data

``` r
#DEMOGRAPHIC/CLINICAL DATA
#load demographic data 
demo <- read.csv("/Users/nicoledwenger/Documents/University/3SEMESTER/Experimental Methods 3/GitHub/Class5/data/DemographicData.csv", sep = ";")
# change diagnosis to 0 and 1 in demographic data to merge
demo$Diagnosis <- ifelse(grepl("Control", demo$Diagnosis), "0", "1")

#DURATION DATA
#load duration data 
dur <- read.delim("/Users/nicoledwenger/Documents/University/3SEMESTER/Experimental Methods 3/GitHub/Class5/data/Articulation.txt", sep = ",")
#seperate the column into multiple ones (similar to parse) to merge 
dur <- separate(data = dur, col=soundname, into = c("Remove", "Study", "Diagnosis", "Participant", "Trial"), sep = "\\D+")
```

    ## Warning: Expected 5 pieces. Additional pieces discarded in 6 rows [742,
    ## 745, 3997, 3999, 4001, 4005].

``` r
#remove empty column from seperate
dur$Remove <- NULL 
#remove the 0 before the participant number
dur$Participant <- as.integer(dur$Participant) 

#PITCH DATA
pitch <- read.csv("dataframeall")

#MERGE 
data1 <- merge(pitch, dur, allow.new.levels = TRUE)
data <- merge(data1, demo, allow.new.levels = TRUE)

#SAVE
write_csv(data, "dataSchizophreniaVoice")
```

Now we need to describe our sample
----------------------------------

First look at the missing data: we should exclude all recordings for which we do not have complete data. Then count the participants and recordinsgs by diagnosis, report their gender, age and symptom severity (SANS, SAPS and Social) Finally, do the same by diagnosis and study, to assess systematic differences in studies. I like to use group\_by() %&gt;% summarize() for quick summaries

``` r
#remove rows with NA's in columns 
data <- data[complete.cases(data[1:4,8]),]

#create unique IDs and pairIDs
data <- data %>% mutate(uPairID = paste(Participant, Study, sep = "_"), 
                       uPairID = as.numeric(as.factor(uPairID)), 
                       uID = paste(Participant, Study, Diagnosis, sep = "_"), 
                       uID = as.numeric(as.factor(uID)))

danish <- subset(data, Study == "1" | Study == "2" | Study == "3" | Study == "4")
danish <- subset(data, Study < 5)

#number of participants
danish$uID <- as.factor(danish$uID)
nlevels(danish$uID)
```

    ## [1] 221

``` r
#number of recordings by diagnosis
danish %>% group_by(Diagnosis) %>% 
  summarise(recordings = n())
```

    ## # A tibble: 2 x 2
    ##   Diagnosis recordings
    ##       <int>      <int>
    ## 1         0        993
    ## 2         1        903

``` r
#993 control, 903 diagnosis

#to only get one row by participant
onerow <- danish[!duplicated(danish$uID),]

#descriptive stats by diagnosis 
d1 <- onerow %>% group_by(Diagnosis) %>% 
                    summarize(
                      TotalNumber = n(), 
                      Girls = sum(Gender == "F"), 
                      Boys = sum(Gender == "M"), 
                      Age_Mean = mean(Age, na.rm = TRUE),
                      Age_SD = sd(Age, na.rm = TRUE),
                      Education_Mean = mean(Education),
                      Education_SD = sd(Education),
                      SANS_Mean = mean(SANS, na.rm=TRUE), 
                      SANS_SD = sd(SANS, na.rm = TRUE),
                      SAPS_Mean = mean(SAPS, na.rm=TRUE), 
                      verbalIQ_Mean = mean(VerbalIQ, na.rm=TRUE),
                      verbalIQ_SD = sd(VerbalIQ, na.rm = TRUE),
                      nonverbalIQ_Mean = mean(NonVerbalIQ, na.rm=TRUE), 
                      nonverbalIQ_SD = sd(NonVerbalIQ, na.rm = TRUE),
                      IQ_Mean = mean(TotalIQ, na.rm=TRUE),
                      IQ_SD = sd(TotalIQ, na.rm = TRUE))

d1 <- round(d1, digits = 2)
d1
```

    ## # A tibble: 2 x 17
    ##   Diagnosis TotalNumber Girls  Boys Age_Mean Age_SD Education_Mean
    ##       <dbl>       <dbl> <dbl> <dbl>    <dbl>  <dbl>          <dbl>
    ## 1         0         116    50    66     26.7   9.19           14.9
    ## 2         1         105    45    60     26.7   9.02           12.9
    ## # … with 10 more variables: Education_SD <dbl>, SANS_Mean <dbl>,
    ## #   SANS_SD <dbl>, SAPS_Mean <dbl>, verbalIQ_Mean <dbl>,
    ## #   verbalIQ_SD <dbl>, nonverbalIQ_Mean <dbl>, nonverbalIQ_SD <dbl>,
    ## #   IQ_Mean <dbl>, IQ_SD <dbl>

``` r
write.csv(d1, "d1.csv")

#descriptive stats by diagnosis and study 
d2 <- onerow %>% group_by(Diagnosis, Study) %>% 
                    summarize(
                      TotalNumber = n(), 
                      Girls = sum(Gender == "F"), 
                      Boys = sum(Gender == "M"), 
                      Age_Mean = mean(Age, na.rm = TRUE),
                      Age_SD = sd(Age, na.rm = TRUE),
                      Education_Mean = mean(Education),
                      Education_SD = sd(Education),
                      SANS_Mean = mean(SANS, na.rm=TRUE), 
                      SANS_SD = sd(SANS, na.rm = TRUE),
                      SAPS_Mean = mean(SAPS, na.rm=TRUE), 
                      verbalIQ_Mean = mean(VerbalIQ, na.rm=TRUE),
                      verbalIQ_SD = sd(VerbalIQ, na.rm = TRUE),
                      nonverbalIQ_Mean = mean(NonVerbalIQ, na.rm=TRUE), 
                      nonverbalIQ_SD = sd(NonVerbalIQ, na.rm = TRUE),
                      IQ_Mean = mean(TotalIQ, na.rm=TRUE),
                      IQ_SD = sd(TotalIQ, na.rm = TRUE))

d2 <- round(d2, digits = 2)
d2
```

    ## # A tibble: 8 x 18
    ## # Groups:   Diagnosis [2]
    ##   Diagnosis Study TotalNumber Girls  Boys Age_Mean Age_SD Education_Mean
    ##       <dbl> <dbl>       <dbl> <dbl> <dbl>    <dbl>  <dbl>          <dbl>
    ## 1         0     1          36    17    19     22.7   3.19           13.4
    ## 2         0     2          23     7    16     23.6   3.61           15.2
    ## 3         0     3          28    13    15     37.5  13.1            15.9
    ## 4         0     4          29    13    16     24.4   4.58           15.8
    ## 5         1     1          34    16    18     22.8   3.13           12.1
    ## 6         1     2          23     6    17     23.4   3.94           12.0
    ## 7         1     3          19    11     8     40.8  12.4            12.8
    ## 8         1     4          29    12    17     24.8   3.66           14.7
    ## # … with 10 more variables: Education_SD <dbl>, SANS_Mean <dbl>,
    ## #   SANS_SD <dbl>, SAPS_Mean <dbl>, verbalIQ_Mean <dbl>,
    ## #   verbalIQ_SD <dbl>, nonverbalIQ_Mean <dbl>, nonverbalIQ_SD <dbl>,
    ## #   IQ_Mean <dbl>, IQ_SD <dbl>

``` r
write.csv(d2, "d2.csv")
```

Now we can analyze the data
---------------------------

If you were to examine the meta analysis you would find that the differences (measured as Hedges' g, very close to Cohen's d, that is, in standard deviations) to be the following - pitch variability (lower, Hedges' g: -0.55, 95% CIs: -1.06, 0.09) - proportion of spoken time (lower, Hedges' g: -1.26, 95% CIs: -2.26, 0.25) - speech rate (slower, Hedges' g: -0.75, 95% CIs: -1.51, 0.04) - pause duration (longer, Hedges' g: 1.89, 95% CIs: 0.72, 3.21). (Duration - Spoken Duration) / PauseN

We need therefore to set up 4 models to see how well our results compare to the meta-analytic findings (Feel free of course to test more features) Describe the acoustic profile of a schizophrenic voice *Note* in this section you need to describe the acoustic profile of a schizophrenic voice and compare it with the meta-analytic findings (see 2 and 3 in overview of part 1).

N.B. the meta-analytic findings are on scaled measures. If you want to compare your results with them, you need to scale your measures as well: subtract the mean, and divide by the standard deviation. N.N.B. We want to think carefully about fixed and random effects in our model. In particular: how should study be included? Does it make sense to have all studies put together? Does it make sense to analyze both languages together? Relatedly: does it make sense to scale all data from all studies together? N.N.N.B. If you want to estimate the studies separately, you can try this syntax: Feature ~ 0 + Study + Study:Diagnosis + \[your randomEffects\]. Now you'll have an intercept per each study (the estimates for the controls) and an effect of diagnosis per each study

-   Bonus points: cross-validate the models and report the betas and standard errors from all rounds to get an idea of how robust the estimates are.

``` r
#adding collumn for proportion of spoken time 
danish$proportionSpoken <- (danish$phonationtime..s./danish$dur..s.)
#adding collumn for duration of pauses
danish$pauseDuration <- ((danish$dur..s.- danish$phonationtime..s.)/danish$npause)
danish$pauseDuration[danish$npause == 0] = 0
danish$pauseDuration <- as.numeric(danish$pauseDuration)
#renaming the speechrate collumn 
names(danish)[names(danish) == "speechrate..nsyll.dur."] <- "speechrate"

write.csv(danish, "danishdata.csv")

#scaling
set.seed(1)
danish$scale_iqr <- as.numeric(scale(danish$iqr))
danish$scale_propSpoken <- as.numeric(scale(danish$proportionSpoken))
danish$scale_pausdur <- as.numeric(scale(danish$pauseDuration))
danish$scale_speechrate <- as.numeric(scale(danish$speechrate))

#changing to factors
danish$Diagnosis <- as.factor(danish$Diagnosis)
danish$Study <- as.factor(danish$Study)
danish$uPairID <- as.factor(danish$uPairID)

lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8))
```

    ## $optimizer
    ## [1] "nloptwrap"
    ## 
    ## $restart_edge
    ## [1] TRUE
    ## 
    ## $boundary.tol
    ## [1] 1e-05
    ## 
    ## $calc.derivs
    ## [1] TRUE
    ## 
    ## $use.last.params
    ## [1] FALSE
    ## 
    ## $checkControl
    ## $checkControl$check.nobs.vs.rankZ
    ## [1] "ignore"
    ## 
    ## $checkControl$check.nobs.vs.nlev
    ## [1] "stop"
    ## 
    ## $checkControl$check.nlev.gtreq.5
    ## [1] "ignore"
    ## 
    ## $checkControl$check.nlev.gtr.1
    ## [1] "stop"
    ## 
    ## $checkControl$check.nobs.vs.nRE
    ## [1] "stop"
    ## 
    ## $checkControl$check.rankX
    ## [1] "message+drop.cols"
    ## 
    ## $checkControl$check.scaleX
    ## [1] "warning"
    ## 
    ## $checkControl$check.formula.LHS
    ## [1] "stop"
    ## 
    ## 
    ## $checkConv
    ## $checkConv$check.conv.grad
    ## $checkConv$check.conv.grad$action
    ## [1] "warning"
    ## 
    ## $checkConv$check.conv.grad$tol
    ## [1] 0.002
    ## 
    ## $checkConv$check.conv.grad$relTol
    ## NULL
    ## 
    ## 
    ## $checkConv$check.conv.singular
    ## $checkConv$check.conv.singular$action
    ## [1] "message"
    ## 
    ## $checkConv$check.conv.singular$tol
    ## [1] 1e-04
    ## 
    ## 
    ## $checkConv$check.conv.hess
    ## $checkConv$check.conv.hess$action
    ## [1] "warning"
    ## 
    ## $checkConv$check.conv.hess$tol
    ## [1] 1e-06
    ## 
    ## 
    ## 
    ## $optCtrl
    ## $optCtrl$xtol_abs
    ## [1] 1e-08
    ## 
    ## $optCtrl$ftol_abs
    ## [1] 1e-08
    ## 
    ## 
    ## attr(,"class")
    ## [1] "lmerControl" "merControl"

``` r
#CREATE MODELS 

#PITCH VARIABITLITY 
#random effect for matched
m1 <- lmer(scale_iqr ~ 1 + Diagnosis + (1|uID), data = danish, REML = F)
summary(m1)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: scale_iqr ~ 1 + Diagnosis + (1 | uID)
    ##    Data: danish
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4572.0   4594.2  -2282.0   4564.0     1892 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0739 -0.1844 -0.0751  0.0202 13.0747 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  uID      (Intercept) 0.5114   0.7151  
    ##  Residual             0.4987   0.7062  
    ## Number of obs: 1896, groups:  uID, 221
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error        df t value Pr(>|t|)   
    ## (Intercept)   0.13600    0.07014 219.01222   1.939  0.05380 . 
    ## Diagnosis1   -0.26631    0.10175 218.92009  -2.617  0.00948 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## Diagnosis1 -0.689

``` r
#random effect for single id = is better
m2 <- lmer(scale_iqr ~ 1 + Diagnosis + (1+Diagnosis|uPairID), data = danish, REML = F)
summary(m2)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: scale_iqr ~ 1 + Diagnosis + (1 + Diagnosis | uPairID)
    ##    Data: danish
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4437.0   4470.3  -2212.5   4425.0     1890 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.5564 -0.2575 -0.1108  0.0149 14.3060 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  uPairID  (Intercept) 0.9574   0.9785        
    ##           Diagnosis1  1.0517   1.0255   -0.99
    ##  Residual             0.4985   0.7060        
    ## Number of obs: 1896, groups:  uPairID, 122
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error        df t value Pr(>|t|)   
    ## (Intercept)   0.13879    0.09359 115.65658   1.483  0.14081   
    ## Diagnosis1   -0.26907    0.10056 117.97447  -2.676  0.00852 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## Diagnosis1 -0.962

``` r
anova(m1,m2)
```

    ## Data: danish
    ## Models:
    ## m1: scale_iqr ~ 1 + Diagnosis + (1 | uID)
    ## m2: scale_iqr ~ 1 + Diagnosis + (1 + Diagnosis | uPairID)
    ##    Df  AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
    ## m1  4 4572 4594.2 -2282.0     4564                             
    ## m2  6 4437 4470.3 -2212.5     4425 139.02      2  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#by study, matched ID random effect won't converge
m3 <- lmer(scale_iqr ~ 0 + Study + Study:Diagnosis + (1|uID), data = danish, REML = F)
summ3 <- summary(m3)
coef3 <- round(as.data.frame(summ3$coefficients), digits = 2)
write.csv(coef3, "coef3.csv")
m2.1 <- lmer(scale_iqr ~ 1 + Diagnosis + Age + (1|uID), data = danish, REML = F)

#PROPORTION OF SPOKEN TIME
m4 <- lmer(scale_propSpoken ~ 1 + Diagnosis + (1+Diagnosis|uPairID), data = danish, REML = F)
#by study
m5 <- lmer(scale_propSpoken ~ 0 + Study + Study:Diagnosis + (1+Diagnosis|uPairID), data = danish, REML = F)
summ5 <- summary(m5)
summ5
```

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: 
    ## scale_propSpoken ~ 0 + Study + Study:Diagnosis + (1 + Diagnosis |  
    ##     uPairID)
    ##    Data: danish
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4410.0   4476.6  -2193.0   4386.0     1884 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0390 -0.5566  0.0251  0.5579  4.1992 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  uPairID  (Intercept) 0.3876   0.6226        
    ##           Diagnosis1  0.8694   0.9324   -0.60
    ##  Residual             0.4546   0.6742        
    ## Number of obs: 1896, groups:  uPairID, 122
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error        df t value Pr(>|t|)    
    ## Study1             -0.06374    0.11009 112.73396  -0.579    0.564    
    ## Study2             -0.22856    0.13899 117.33496  -1.645    0.103    
    ## Study3              0.58419    0.12588 116.90668   4.641 9.13e-06 ***
    ## Study4              0.05370    0.12374 117.22797   0.434    0.665    
    ## Study1:Diagnosis1   0.10476    0.16665 100.12162   0.629    0.531    
    ## Study2:Diagnosis1  -0.30180    0.20757 111.11120  -1.454    0.149    
    ## Study3:Diagnosis1  -0.34605    0.21368 123.60359  -1.619    0.108    
    ## Study4:Diagnosis1  -0.20777    0.18465 112.29150  -1.125    0.263    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             Study1 Study2 Study3 Study4 St1:D1 St2:D1 St3:D1
    ## Study2       0.000                                          
    ## Study3       0.000  0.000                                   
    ## Study4       0.000  0.000  0.000                            
    ## Stdy1:Dgns1 -0.604  0.000  0.000  0.000                     
    ## Stdy2:Dgns1  0.000 -0.618  0.000  0.000  0.000              
    ## Stdy3:Dgns1  0.000  0.000 -0.542  0.000  0.000  0.000       
    ## Stdy4:Dgns1  0.000  0.000  0.000 -0.619  0.000  0.000  0.000

``` r
coef5 <- round(as.data.frame(summ5$coefficients), digits = 2)
write.csv(coef5, "coef5.csv")

#SPEECH RATE
m6 <- lmer(scale_speechrate ~ 1 + Diagnosis + (1+Diagnosis|uPairID), data = danish, REML = F)
summary(m6)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: scale_speechrate ~ 1 + Diagnosis + (1 + Diagnosis | uPairID)
    ##    Data: danish
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4614.4   4647.7  -2301.2   4602.4     1890 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5038 -0.5846 -0.0106  0.5579  4.2481 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  uPairID  (Intercept) 0.4142   0.6436        
    ##           Diagnosis1  0.7249   0.8514   -0.57
    ##  Residual             0.5176   0.7194        
    ## Number of obs: 1896, groups:  uPairID, 122
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)   0.15082    0.06398 116.23990   2.358  0.02007 *  
    ## Diagnosis1   -0.30801    0.08883 114.82719  -3.468  0.00074 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## Diagnosis1 -0.574

``` r
#by study
m7 <- lmer(scale_speechrate ~ 0 + Study + Study:Diagnosis + (1+Diagnosis|uPairID), data = danish, REML = F)
summary(m7)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: 
    ## scale_speechrate ~ 0 + Study + Study:Diagnosis + (1 + Diagnosis |  
    ##     uPairID)
    ##    Data: danish
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4599.0   4665.6  -2287.5   4575.0     1884 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5217 -0.5849 -0.0046  0.5555  4.2397 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  uPairID  (Intercept) 0.3386   0.5819        
    ##           Diagnosis1  0.6831   0.8265   -0.58
    ##  Residual             0.5176   0.7195        
    ## Number of obs: 1896, groups:  uPairID, 122
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error        df t value Pr(>|t|)    
    ## Study1             -0.04848    0.10459 111.71251  -0.464   0.6439    
    ## Study2             -0.13835    0.13233 117.76215  -1.045   0.2979    
    ## Study3              0.58248    0.11986 117.13107   4.860 3.68e-06 ***
    ## Study4              0.22028    0.11780 117.64975   1.870   0.0640 .  
    ## Study1:Diagnosis1  -0.03542    0.15103 103.14991  -0.235   0.8151    
    ## Study2:Diagnosis1  -0.41808    0.18924 116.17161  -2.209   0.0291 *  
    ## Study3:Diagnosis1  -0.45595    0.19523 127.40899  -2.335   0.0211 *  
    ## Study4:Diagnosis1  -0.36000    0.16832 117.02885  -2.139   0.0345 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             Study1 Study2 Study3 Study4 St1:D1 St2:D1 St3:D1
    ## Study2       0.000                                          
    ## Study3       0.000  0.000                                   
    ## Study4       0.000  0.000  0.000                            
    ## Stdy1:Dgns1 -0.588  0.000  0.000  0.000                     
    ## Stdy2:Dgns1  0.000 -0.605  0.000  0.000  0.000              
    ## Stdy3:Dgns1  0.000  0.000 -0.528  0.000  0.000  0.000       
    ## Stdy4:Dgns1  0.000  0.000  0.000 -0.607  0.000  0.000  0.000

``` r
summ7 <- summary(m7)
coef7 <- round(as.data.frame(summ7$coefficients), digits = 2)
write.csv(coef7, "coef7.csv")

#PAUSE DURATION
m8 <- lmer(scale_pausdur ~ 1 + Diagnosis + (1+Diagnosis|uPairID), data = danish, REML = F)
summary(m8)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: scale_pausdur ~ 1 + Diagnosis + (1 + Diagnosis | uPairID)
    ##    Data: danish
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   5188.9   5222.2  -2588.5   5176.9     1890 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6491 -0.3620 -0.1231  0.2238 11.7870 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr
    ##  uPairID  (Intercept) 0.04865  0.2206       
    ##           Diagnosis1  0.26932  0.5190   0.05
    ##  Residual             0.80792  0.8988       
    ## Number of obs: 1896, groups:  uPairID, 122
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error        df t value Pr(>|t|)  
    ## (Intercept)  -0.07128    0.03513 120.47704  -2.029   0.0447 *
    ## Diagnosis1    0.14551    0.06607 106.78178   2.202   0.0298 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## Diagnosis1 -0.340

``` r
#by study
m9 <- lmer(scale_pausdur ~ 0 + Study + Study:Diagnosis + (1|uID), data = danish, REML = F)
summary(m9)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: scale_pausdur ~ 0 + Study + Study:Diagnosis + (1 | uID)
    ##    Data: danish
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   5225.5   5281.0  -2602.8   5205.5     1886 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3181 -0.3558 -0.1205  0.2094 12.1193 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  uID      (Intercept) 0.1729   0.4158  
    ##  Residual             0.8078   0.8988  
    ## Number of obs: 1896, groups:  uID, 221
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error        df t value Pr(>|t|)  
    ## Study1             -0.01147    0.08467 207.32980  -0.135   0.8924  
    ## Study2             -0.01308    0.10913 235.94792  -0.120   0.9047  
    ## Study3             -0.25455    0.09866 233.30706  -2.580   0.0105 *
    ## Study4             -0.03607    0.09711 235.23029  -0.371   0.7107  
    ## Study1:Diagnosis1   0.03461    0.12115 205.93554   0.286   0.7754  
    ## Study2:Diagnosis1   0.26697    0.15477 238.45618   1.725   0.0858 .
    ## Study3:Diagnosis1   0.19909    0.15581 235.94833   1.278   0.2026  
    ## Study4:Diagnosis1   0.14286    0.13709 233.49583   1.042   0.2985  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             Study1 Study2 Study3 Study4 St1:D1 St2:D1 St3:D1
    ## Study2       0.000                                          
    ## Study3       0.000  0.000                                   
    ## Study4       0.000  0.000  0.000                            
    ## Stdy1:Dgns1 -0.699  0.000  0.000  0.000                     
    ## Stdy2:Dgns1  0.000 -0.705  0.000  0.000  0.000              
    ## Stdy3:Dgns1  0.000  0.000 -0.633  0.000  0.000  0.000       
    ## Stdy4:Dgns1  0.000  0.000  0.000 -0.708  0.000  0.000  0.000

``` r
summ9 <- summary(m9)
coef9 <- round(as.data.frame(summ9$coefficients), digits = 2)
write.csv(coef9, "coef9.csv")
```

N.B. Remember to save the acoustic features of voice in a separate file, so to be able to load them next time
-------------------------------------------------------------------------------------------------------------

Reminder of the report to write
-------------------------------

Part 1 - Can we find a difference in acoustic features in schizophrenia?

1.  Describe your sample number of studies, number of participants, age, gender, clinical and cognitive features of the two groups. Furthemore, critically assess whether the groups (schizophrenia and controls) are balanced. N.B. you need to take studies into account.

2.  Describe the acoustic profile of a schizophrenic voice: which features are different? E.g. People with schizophrenia tend to have high-pitched voice, and present bigger swings in their prosody than controls. N.B. look also at effect sizes. How do these findings relate to the meta-analytic findings?

3.  Discuss the analysis necessary to replicate the meta-analytic findings Look at the results reported in the paper (see meta-analysis in the readings) and see whether they are similar to those you get. 3.1) Check whether significance and direction of the effects are similar 3.2) Standardize your outcome, run the model and check whether the beta's is roughly matched (matched with hedge's g) which fixed and random effects should be included, given your dataset? E.g. what about language and study, age and gender? Discuss also how studies and languages should play a role in your analyses. E.g. should you analyze each study individually? Or each language individually? Or all together? Each of these choices makes some assumptions about how similar you expect the studies/languages to be.

-   Your report should look like a methods paragraph followed by a result paragraph in a typical article (think the Communication and Cognition paper)
