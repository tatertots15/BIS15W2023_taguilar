---
title: "BIS 15L Midterm 2"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---



## Instructions
Answer the following questions and complete the exercises in RMarkdown. Please embed all of your code and push your final work to your repository. Your code should be organized, clean, and run free from errors. Remember, you must remove the `#` for any included code chunks to run. Be sure to add your name to the author header above.  

After the first 50 minutes, please upload your code (5 points). During the second 50 minutes, you may get help from each other- but no copy/paste. Upload the last version at the end of this time, but be sure to indicate it as final. If you finish early, you are free to leave.

Make sure to use the formatting conventions of RMarkdown to make your report neat and clean! Use the tidyverse and pipes unless otherwise indicated. To receive full credit, all plots must have clearly labeled axes, a title, and consistent aesthetics. This exam is worth a total of 35 points. 

Please load the following libraries.

```r
library("tidyverse")
library("janitor")
library("naniar")
```


```r
options(scipen=999)
```

## Data
These data are from a study on surgical residents. The study was originally published by Sessier et al. “Operation Timing and 30-Day Mortality After Elective General Surgery”. Anesth Analg 2011; 113: 1423-8. The data were cleaned for instructional use by Amy S. Nowacki, “Surgery Timing Dataset”, TSHS Resources Portal (2016). Available at https://www.causeweb.org/tshs/surgery-timing/.

Descriptions of the variables and the study are included as pdf's in the data folder.  

Please run the following chunk to import the data.

```r
surgery <- read_csv("data/surgery.csv")
```


```r
p<-surgery
```

1. (2 points) Use the summary function(s) of your choice to explore the data and get an idea of its structure. Please also check for NA's.

```r
glimpse(surgery)
```

```
## Rows: 32,001
## Columns: 25
## $ ahrq_ccs            <chr> "<Other>", "<Other>", "<Other>", "<Other>", "<Othe…
## $ age                 <dbl> 67.8, 39.5, 56.5, 71.0, 56.3, 57.7, 56.6, 64.2, 66…
## $ gender              <chr> "M", "F", "F", "M", "M", "F", "M", "F", "M", "F", …
## $ race                <chr> "Caucasian", "Caucasian", "Caucasian", "Caucasian"…
## $ asa_status          <chr> "I-II", "I-II", "I-II", "III", "I-II", "I-II", "IV…
## $ bmi                 <dbl> 28.04, 37.85, 19.56, 32.22, 24.32, 40.30, 64.57, 4…
## $ baseline_cancer     <chr> "No", "No", "No", "No", "Yes", "No", "No", "No", "…
## $ baseline_cvd        <chr> "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "Ye…
## $ baseline_dementia   <chr> "No", "No", "No", "No", "No", "No", "No", "No", "N…
## $ baseline_diabetes   <chr> "No", "No", "No", "No", "No", "No", "Yes", "No", "…
## $ baseline_digestive  <chr> "Yes", "No", "No", "No", "No", "No", "No", "No", "…
## $ baseline_osteoart   <chr> "No", "No", "No", "No", "No", "No", "No", "No", "N…
## $ baseline_psych      <chr> "No", "No", "No", "No", "No", "Yes", "No", "No", "…
## $ baseline_pulmonary  <chr> "No", "No", "No", "No", "No", "No", "No", "No", "N…
## $ baseline_charlson   <dbl> 0, 0, 0, 0, 0, 0, 2, 0, 1, 2, 0, 1, 0, 0, 0, 0, 0,…
## $ mortality_rsi       <dbl> -0.63, -0.63, -0.49, -1.38, 0.00, -0.77, -0.36, -0…
## $ complication_rsi    <dbl> -0.26, -0.26, 0.00, -1.15, 0.00, -0.84, -1.34, 0.0…
## $ ccsmort30rate       <dbl> 0.0042508, 0.0042508, 0.0042508, 0.0042508, 0.0042…
## $ ccscomplicationrate <dbl> 0.07226355, 0.07226355, 0.07226355, 0.07226355, 0.…
## $ hour                <dbl> 9.03, 18.48, 7.88, 8.80, 12.20, 7.67, 9.53, 7.52, …
## $ dow                 <chr> "Mon", "Wed", "Fri", "Wed", "Thu", "Thu", "Tue", "…
## $ month               <chr> "Nov", "Sep", "Aug", "Jun", "Aug", "Dec", "Apr", "…
## $ moonphase           <chr> "Full Moon", "New Moon", "Full Moon", "Last Quarte…
## $ mort30              <chr> "No", "No", "No", "No", "No", "No", "No", "No", "N…
## $ complication        <chr> "No", "No", "No", "No", "No", "No", "No", "Yes", "…
```


```r
anyNA(surgery)
```

```
## [1] TRUE
```

2. (3 points) Let's explore the participants in the study. Show a count of participants by race AND make a plot that visually represents your output.

```r
names(surgery)
```

```
##  [1] "ahrq_ccs"            "age"                 "gender"             
##  [4] "race"                "asa_status"          "bmi"                
##  [7] "baseline_cancer"     "baseline_cvd"        "baseline_dementia"  
## [10] "baseline_diabetes"   "baseline_digestive"  "baseline_osteoart"  
## [13] "baseline_psych"      "baseline_pulmonary"  "baseline_charlson"  
## [16] "mortality_rsi"       "complication_rsi"    "ccsmort30rate"      
## [19] "ccscomplicationrate" "hour"                "dow"                
## [22] "month"               "moonphase"           "mort30"             
## [25] "complication"
```

```r
surgery %>% 
  tabyl(race)
```

```
##              race     n    percent valid_percent
##  African American  3790 0.11843380    0.12023730
##         Caucasian 26488 0.82772413    0.84032867
##             Other  1243 0.03884254    0.03943403
##              <NA>   480 0.01499953            NA
```


```r
surgery %>% 
  group_by(race) %>% 
  ggplot(aes(x=race)) +
  geom_bar() +
  labs(title = "Race of Patients",
       x= "Race",
       y="Number of Individuals")
```

![](midterm_2_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

3. (2 points) What is the mean age of participants by gender? (hint: please provide a number for each) Since only three participants do not have gender indicated, remove these participants from the data.


```r
surgery %>% 
  filter(gender!="NA") %>% 
  group_by(gender) %>% 
  summarise(mean_age=mean(age, na.rm = T))
```

```
## # A tibble: 2 × 2
##   gender mean_age
##   <chr>     <dbl>
## 1 F          56.7
## 2 M          58.8
```

4. (3 points) Make a plot that shows the range of age associated with gender.

```r
surgery %>% 
  filter(gender!="NA") %>% 
  ggplot(aes(x=gender, y=age)) +
  geom_boxplot() +
  coord_flip() +
  labs(title="Comparison of Age and Gender",
       x="Gender",
       y="Age")
```

```
## Warning: Removed 2 rows containing non-finite values (`stat_boxplot()`).
```

![](midterm_2_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

5. (2 points) How healthy are the participants? The variable `asa_status` is an evaluation of patient physical status prior to surgery. Lower numbers indicate fewer comorbidities (presence of two or more diseases or medical conditions in a patient). Make a plot that compares the number of `asa_status` I-II, III, and IV-V.

```r
surgery %>% 
  group_by(asa_status) %>% 
  ggplot(aes(x=asa_status)) +
  geom_bar() +
  labs(title = "ASA Status of Patients",
       x= "ASA Status",
       y="Number of Individuals")
```

![](midterm_2_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

6. (3 points) Create a plot that displays the distribution of body mass index for each `asa_status` as a probability distribution- not a histogram. (hint: use faceting!)

```r
surgery %>% 
  ggplot(aes(x=bmi))+
  geom_density(fill="deepskyblue4", alpha  =0.4, color = "black")+
  facet_grid(asa_status~., scales = "free_y") +
  labs(title ="Distribution of BMI for each ASA Status",
       x="BMI",
       y="Density")
```

```
## Warning: Removed 3290 rows containing non-finite values (`stat_density()`).
```

![](midterm_2_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

The variable `ccsmort30rate` is a measure of the overall 30-day mortality rate associated with each type of operation. The variable `ccscomplicationrate` is a measure of the 30-day in-hospital complication rate. The variable `ahrq_ccs` lists each type of operation.  

7. (4 points) What are the 5 procedures associated with highest risk of 30-day mortality AND how do they compare with the 5 procedures with highest risk of complication? (hint: no need for a plot here)

```r
highest_risk_mortality <- surgery%>% 
  group_by(ahrq_ccs) %>% 
  summarise(mortality_risk=mean(ccsmort30rate)) %>% 
  arrange(desc(mortality_risk)) %>% 
  slice_head(n=5)
highest_risk_mortality
```

```
## # A tibble: 5 × 2
##   ahrq_ccs                                             mortality_risk
##   <chr>                                                         <dbl>
## 1 Colorectal resection                                        0.0167 
## 2 Small bowel resection                                       0.0129 
## 3 Gastrectomy; partial and total                              0.0127 
## 4 Endoscopy and endoscopic biopsy of the urinary tract        0.00811
## 5 Spinal fusion                                               0.00742
```

```r
highest_risk_complication <- surgery%>% 
  group_by(ahrq_ccs) %>% 
  summarise(complication_risk=mean(ccscomplicationrate)) %>% 
  arrange(desc(complication_risk)) %>% 
  slice_head(n=5)
highest_risk_complication
```

```
## # A tibble: 5 × 2
##   ahrq_ccs                         complication_risk
##   <chr>                                        <dbl>
## 1 Small bowel resection                        0.466
## 2 Colorectal resection                         0.312
## 3 Nephrectomy; partial or complete             0.197
## 4 Gastrectomy; partial and total               0.190
## 5 Spinal fusion                                0.183
```

8. (3 points) Make a plot that compares the `ccsmort30rate` for all listed `ahrq_ccs` procedures.

```r
surgery%>% 
  group_by(ahrq_ccs) %>% 
  ggplot(aes(x=ccsmort30rate, y=ahrq_ccs)) +
  geom_boxplot() +
  labs(title="Mortality Rates by Procedure",
       x="Mortality Rate",
       y="Procedure")
```

![](midterm_2_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

9. (4 points) When is the best month to have surgery? Make a chart that shows the 30-day mortality and complications for the patients by month. `mort30` is the variable that shows whether or not a patient survived 30 days post-operation.

```r
#surgery %>% group_by(month) %>% summarise(mean_ccsmort30rate=mean(ccsmort30rate),mean_ccscomplicationrate=mean(ccscomplicationrate))
```

10. (4 points) Make a plot that visualizes the chart from question #9. Make sure that the months are on the x-axis. Do a search online and figure out how to order the months Jan-Dec.

```r
surgery %>% 
  mutate(Month = factor(month, levels = month.abb)) %>% 
  arrange(Month)
```

```
## # A tibble: 32,001 × 26
##    ahrq_ccs   age gender race      asa_s…¹   bmi basel…² basel…³ basel…⁴ basel…⁵
##    <chr>    <dbl> <chr>  <chr>     <chr>   <dbl> <chr>   <chr>   <chr>   <chr>  
##  1 <Other>   49.4 M      Caucasian III      41.3 No      Yes     No      Yes    
##  2 <Other>   70.5 F      Caucasian III      50.5 No      Yes     No      No     
##  3 <Other>   74.8 M      Caucasian IV-VI    35.4 Yes     Yes     No      No     
##  4 <Other>   63   F      Caucasian III      22.5 Yes     No      No      No     
##  5 <Other>   57.1 M      Caucasian III      26.1 No      Yes     No      No     
##  6 <Other>   48.7 M      Caucasian IV-VI    29.4 Yes     No      No      No     
##  7 <Other>   61.9 M      <NA>      IV-VI    NA   Yes     No      No      No     
##  8 <Other>   77.8 F      Caucasian III      NA   Yes     Yes     No      No     
##  9 <Other>   55.6 M      <NA>      III      21.3 No      No      No      No     
## 10 <Other>   57   M      <NA>      III      NA   Yes     Yes     No      No     
## # … with 31,991 more rows, 16 more variables: baseline_digestive <chr>,
## #   baseline_osteoart <chr>, baseline_psych <chr>, baseline_pulmonary <chr>,
## #   baseline_charlson <dbl>, mortality_rsi <dbl>, complication_rsi <dbl>,
## #   ccsmort30rate <dbl>, ccscomplicationrate <dbl>, hour <dbl>, dow <chr>,
## #   month <chr>, moonphase <chr>, mort30 <chr>, complication <chr>,
## #   Month <fct>, and abbreviated variable names ¹​asa_status, ²​baseline_cancer,
## #   ³​baseline_cvd, ⁴​baseline_dementia, ⁵​baseline_diabetes
```

```r
#surgery %>% mutate(mort30day =
```

Please provide the names of the students you have worked with with during the exam:

Please be 100% sure your exam is saved, knitted, and pushed to your github repository. No need to submit a link on canvas, we will find your exam in your repository.
