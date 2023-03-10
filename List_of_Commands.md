---
title: "List_of_Commands"
author: "Laurine Cabiling"
date: "2023-01-23"
output: 
  html_document: 
    keep_md: yes
---



#Intro
This is a list of commands from lecture, excluding commands we may have used independtly from homework assignments. Not all the codes are runable because there is no connected files.

# Lab 1 commands

```r
#get working directory
#getwd()
#set working directory 
#setwd()
```


```r
#making a vector or string of values into an object 
# c means concatenate
x <- c(3,43,7,8,534)
```


```r
#mean of object
mean(x)
```

```
## [1] 119
```


```r
#median of object
median(x)
```

```
## [1] 8
```

## "#" = titles 
## = reduced font size, the more # = reduced size
smallest font size 
_italics_
**bold**
line break = two places 

## Links 
[website name]
(website link)
(mailto: email address)


```r
#input code chunk with mac is ...
#`option+command+i`
```


```r
#install.packages("name of package")
#library("name of package") to download data every session
```


```r
#ggplot(data, subset(x = factor(cyl))) + geom_bar()
#to create a plot graph?
```

#Lab 2 commands


```r
#assigning values to an object 
x <- 34
s <- 44
```


```r
#printing object 
x
```

```
## [1] 34
```


```r
#suming objects 
distance <- sum(x,s)
```

## Types of Data

```r
#Five frequently used classes of data: 
  # 1. numeric
  # 2. integer - adding an L automatically denotes an integer
  # 3. character
  # 4. logical
  # 5. complex
my_numeric <- 43
my_integer <- 2L 
my_character <- "universe"
my_logical <- FALSE
my_complex <- 2+4i
```


```r
#find class of function
class(my_numeric)
```

```
## [1] "numeric"
```


```r
# is my numeric an integer? 
is.integer(my_numeric)
```

```
## [1] FALSE
```


```r
# create a new object specified as a different class
my_integer <- as.integer(my_numeric)
```


```r
# is there NA in my data - says which data is T/F
is.na(x)
```

```
## [1] FALSE
```


```r
# are there any NAs in my data - T/F
anyNA(x)
```

```
## [1] FALSE
```


```r
#to remove any NAs when calculating
# na.rm = T
mean(x, na.rm = T)
```

```
## [1] 34
```

data structures include...
  vectors
  lists
  matrices
  data frames
  factors
  

```r
#numeric vector
my_vector <- c(20,34,23)
```


```r
#character vector
days_of_the_week <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
```


```r
# generate a sequence of numbers
my_vector_sequence <- c(1:100)
```


```r
# use [] to pull out elements in a vector
days_of_the_week[3]
```

```
## [1] "tuesday"
```


```r
# pull out data greater than ...
my_vector_sequence > 80
```

```
##   [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [37] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [49] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [61] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [73] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
##  [85]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
##  [97]  TRUE  TRUE  TRUE  TRUE
```

```r
# pull out data greater than or equal to ...
my_vector_sequence >= 49
```

```
##   [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [37] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [49]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
##  [61]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
##  [73]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
##  [85]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
##  [97]  TRUE  TRUE  TRUE  TRUE
```

```r
# pull out data less than ...
my_vector_sequence < 30
```

```
##   [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
##  [13]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
##  [25]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [37] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [49] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [61] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [73] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [85] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [97] FALSE FALSE FALSE FALSE
```

```r
# pull out data less than or equal to...
my_vector_sequence <= 43
```

```
##   [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
##  [13]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
##  [25]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
##  [37]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
##  [49] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [61] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [73] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [85] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [97] FALSE FALSE FALSE FALSE
```

```r
# pull out data equal to ...
my_vector_sequence == 39
```

```
##   [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [37] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [49] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [61] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [73] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [85] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [97] FALSE FALSE FALSE FALSE
```

```r
# Use [] to get the values, not the logical evaluation
my_vector_sequence[my_vector_sequence <= 20]
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
```

## How to do Data Matrices 


```r
Philosophers_Stone <- c(317.5, 657.1)
Chamber_of_Secrets <- c(261.9, 616.9)
Prisoner_of_Azkaban <- c(249.5, 547.1)
Goblet_of_Fire <- c(290.0, 606.8)
Order_of_the_Phoenix <- c(292.0, 647.8)
Half_Blood_Prince <- c(301.9, 632.4)
Deathly_Hallows_1 <- c(295.9, 664.3)
Deathly_Hallows_2 <- c(381.0, 960.5)
```


```r
#combine all objects into one
box_office <- c(Philosophers_Stone, Chamber_of_Secrets, Prisoner_of_Azkaban, Goblet_of_Fire, Order_of_the_Phoenix, Half_Blood_Prince, Deathly_Hallows_1, Deathly_Hallows_2)
```


```r
#create the matrix, and organize using `nrow` and `byrow`
harry_potter_matrix <- matrix(box_office, nrow = 8, byrow = T)
```


```r
#name the vectors for the rows and columns 
region <- c("US", "non-US")
titles <- c("Philosophers_Stone", "Chamber_of_Secrets", "Prisoner_of_Azkaban", "Goblet_of_Fire", "Order_of_the_Phoenix", "Half_Blood_Prince", "Deathly_Hallows_1", "Deathly_Hallows_2")
```


```r
#name the columns
colnames(harry_potter_matrix) <- region
#name the rows
rownames(harry_potter_matrix) <- titles
```


```r
# print the matrix
harry_potter_matrix
```

```
##                         US non-US
## Philosophers_Stone   317.5  657.1
## Chamber_of_Secrets   261.9  616.9
## Prisoner_of_Azkaban  249.5  547.1
## Goblet_of_Fire       290.0  606.8
## Order_of_the_Phoenix 292.0  647.8
## Half_Blood_Prince    301.9  632.4
## Deathly_Hallows_1    295.9  664.3
## Deathly_Hallows_2    381.0  960.5
```


```r
#sum the rows 
global <- rowSums(harry_potter_matrix)
```


```r
#add new column to show calculation
all_harry_potter_matrix <- cbind(harry_potter_matrix, global)
```


```r
#sum the columns
final_earnings <- colSums(all_harry_potter_matrix)
```


```r
# pull out data, the first value is the column, then the row
harry_potter_matrix[2,1]
```

```
## [1] 261.9
```

```r
# pull out data, `:` selects specified elements in a column
harry_potter_matrix[1:4]
```

```
## [1] 317.5 261.9 249.5 290.0
```


```r
#calculate an entire row or column
non_us_earnings <- all_harry_potter_matrix[ ,2]
mean(non_us_earnings)
```

```
## [1] 666.6125
```

# Lab 3 commands 
Building data frames


```r
Sex <- c("Male", "Female", "Male")
Length <- c(3.2, 3.7, 3.4)
Weight <- c(2.9, 4.0, 3.1)
```


```r
#get the data frame
hbirds <- data.frame(Sex, Length, Weight)
```


```r
# column names
names(hbirds)
```

```
## [1] "Sex"    "Length" "Weight"
```


```r
# dimensions of data frame
dim(hbirds)
```

```
## [1] 3 3
```


```r
# structure of data frame
str(hbirds)
```

```
## 'data.frame':	3 obs. of  3 variables:
##  $ Sex   : chr  "Male" "Female" "Male"
##  $ Length: num  3.2 3.7 3.4
##  $ Weight: num  2.9 4 3.1
```


```r
# switching to lowercase
hbirds <- data.frame(sex = Sex, length_in = Length, weight_oz = Weight)
```


```r
#Select values in an entire column using `$` sign. 
w <- hbirds$weight_oz
```


```r
#add new data using `rbind()`, bind new vector to data frame row-wise
new_bird <- c("Female", 3.6, 3.9)
hbirds <- rbind(hbirds, new_bird)
```


```r
# adding columns
hbirds$neighborhood <- c("Lakewood", "Brentwood", "Lakewood", "Scenic Heights")
```


```r
#Saving our dataframe as a csv
write.csv(hbirds, "hbirds_data.csv", row.names = FALSE)
```


```r
#Opening a .csv file & give it a name
hbirds <- readr::read_csv("hbirds_data.csv")
```

```
## Rows: 4 Columns: 4
## ?????? Column specification ????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
## Delimiter: ","
## chr (2): sex, neighborhood
## dbl (2): length_in, weight_oz
## 
## ??? Use `spec()` to retrieve the full column specification for this data.
## ??? Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


```r
#Looking at the data frame in different ways

#structure
#str()

#a type of summary
#glimpse()

#a type of summary
#summary()

#number of rows or observations
#nrow()

#number of columns or variables 
#ncol()

#dimensions
#dim()

#names of column
#name()

#prints the first n rows of the data frame
#head(data, n = )

#prints the last n rows of the data frame
#tail(data, n = )

#table - useful when limited number of categorical variables
#table(data$variable)

#See the data frame in the separate tab
#View()
```


```r
#Looking at a class within the data frame
class(hbirds$neighborhood)
```

```
## [1] "character"
```

```r
#looking at the level within the data frame
levels(hbirds$neightborhood)
```

```
## Warning: Unknown or uninitialised column: `neightborhood`.
```

```
## NULL
```

```r
#changing the class of data
hbirds$neighborhood <- as.factor(hbirds$neighborhood)
```


```r
#subsets are to pull out observations that meet a critera in a variable
# little_fish <- subset(fish, length <= 110)
```

##Lab 4 commands


```r
#select is to filter but with columns instead of rows
#select(fish, "lakeid", "scalelength")
```


```r
#to add range of columns
#select(fish, fish_id:length)
```


```r
# use - operator to select everything except these variables
#select(fish, -fish_id, -annumber, -length)
```


```r
#select columns that contain a character string
#select(fish, contains("length"))

#select columns that start with this name
#select(fish, starts_with("radii"))

#select columns that end with a charater string
#select(fish, ends_with())

#select columns that match a regular expression
#select(fish, matches())

#select column names that are from a group of names
#select(fish, one_of())

#a column contains a letter (in this case "a") followed by a subsequent string (in this case "er").  
#select(fish,matches("a.+er"))

#select column based on class of data
#select_if(fish, is.numeric)

#select all columns that are NOT a class of data, add `~`
#select_if(fish,~is.numeric(.))
```


```r
#renaming columns
#mammals_new <- rename(mammals, genus = "Genus",wean_mass = "wean mass", max_life = "max. life", litter_size = "litter size", litters_per_year = "litters/year")

#to lower case all of the columns
#mammals <- select_all(mammals, tolower)

#to upper case all of the columns
#mammals <- select_all(mammals, toupper)

#to remove the blank spaces
#select_all(mammals, ~str_replace(., " ","_"))
#select_all(mammals, ~str_replace(., "[ ._/]", "" ))
```


```r
# filter extracts data that meets specific criteria within a variable
#filter(fish, lakeid == "AL")

# filter allows to use the expected operators to filter through data ;
  # >, >=, <, <=, != (not equal), and == (equal)

# filter multiple values within same variable
#filter(fish, length %in% c(167,175))

#between
#filter(fish, between(scalelength, 2.5, 3))

#extract observations "near" a certain value, and specify a tolerance
#filter(fish, near(radii_length_mm, 2, tol = 0.2))

#filter to extract data based on multiple conditions
#filter(fish, lakeid == "AL", length == 350)

# `|` operator means or 
#filter(fish, lakeid == "AL" | lenght > 350)

#will return rows where both conditions are met
#filter(condition1, condition2)

# will return all rows where condition one is true but condition 2 is not.  
#filter(condition1, !condition2)

# will return rows where condition 1 and/or condition 2 is met.  
#filter(condition1 | condition2)
  
# will return all rows where only one of the conditions is met, and not when both conditions are met. 
#filter(xor(condition1, condition2))
```


##Lab 5

How to change classes efficiently when wanting to change all of one class into another class 

```r
#mammals %>% mutate_if(is.character, as.factor)
```

In order to start combining `select()`, `filter()`, and other functions efficiently, we need to learn pipes. Pipes feed the output from one function into the input of another function. This helps us keep our code sequential and clean.  
### shift+Cmd+M to get pipe --- can combine column and row 

```r
#fish_subset <- fish %>% 
  #filter(lakeid == "AL" | lakeid == "AR") %>% 
  #filter(radii_length_mm >= 2, radii_length_mm <= 4)
```

The `arrange()` command is a bit like a sort command in excel. Note that the default is ascending order.  
To sort in decreasing order, wrap the variable name in `desc()`.

```r
#fish %>% 
  #select(lakeid, scalelength) %>% 
  #arrange(scalelength)
```

## `mutate()`  
Mutate allows us to create a new column from existing columns in a data frame. We are doing a small introduction here and will add some additional functions later. Let's convert the length variable from cm to millimeters and create a new variable called length_mm.  

### can do calculations and add it as a new column 

```r
#fish %>% 
 # mutate(length_mm = length*10) %>% 
 # select(fish_id, length, length_mm)
```

## `mutate_all()`
This last function is super helpful when cleaning data. With "wild" data, there are often mixed entries (upper and lowercase), blank spaces, odd characters, etc. These all need to be dealt with before analysis. 

```r
#mammals %>%
 # mutate_all(tolower)

#mammals %>% 
  #mutate(across(c("order", "family"), tolower))
```

## `if_else()`
We will briefly introduce `if_else()` here because it allows us to use `mutate()` but not have the entire column affected in the same way. In a sense, this can function like find and replace in a spreadsheet program. With `ifelse()`, you first specify a logical statement, afterwards what needs to happen if the statement returns `TRUE`, and lastly what needs to happen if it's  `FALSE`.  

mutating or changing all of the -999.00 into NA

```r
#mammals %>% 
  #select(genus, species, newborn) %>%
  #mutate(newborn_new = ifelse(newborn == -999.00, NA, newborn))%>% 
  #arrange(newborn)
```

Check out the way I am loading these data. If I know there are NAs, I can take care of them at the beginning. But, we should do this very cautiously. At times it is better to keep the original columns and data intact.  

```r
#superhero_info <- readr::read_csv("data/heroes_information.csv", na = c("", "-99", "-"))
#superhero_powers <- readr::read_csv("data/super_hero_powers.csv", na = c("", "-99", "-"))
```

The `clean_names` function takes care of everything in one line! Now that's a superpower!

```r
#superhero_powers <- janitor::clean_names(superhero_powers)

#superhero_info <-
 #janitor::clean_names(superhero_info)
```

## `tabyl`
The `janitor` package has many awesome functions that we will explore. Here is its version of `table` which not only produces counts but also percentages. Very handy! Let's use it to explore the proportion of good guys and bad guys in the `superhero_info` data.  


```r
#tabyl(superhero_info, alignment)
```


```r
#superhero_powers %>% 
 # filter(hero_names == "Loki") %>% 
 # select_if(all_vars(.=="TRUE"))
```

## dplyr Practice
Let's do a bit more practice to make sure that we understand `select()`, `filter()`, and `mutate()`. Start by building a new data frame `msleep24` from the `msleep` data that: contains the `name` and `vore` variables along with a new column called `sleep_total_24` which is the amount of time a species sleeps expressed as a proportion of a 24-hour day. Remove any rows with NA's and restrict the `sleep_total_24` values to less than 0.3. Arrange the output in descending order.  

```r
#msleep24 <- msleep %>% 
 # mutate(sleep_total_24 = sleep_total/24) %>% 
 # select(name, vore, sleep_total_24) %>% 
 # filter(!is.na(vore)) %>%  #removing NAs from a variable 
 # filter(sleep_total_24 <= 0.3) %>% 
 # arrange(desc(sleep_total_24))
```

Histogram

```r
#hist()
```

## `group_by()`
The `summarize()` function is most useful when used in conjunction with `group_by()`. Although producing a summary of body weight for all of the mammals in the data set is helpful, what if we were interested in body weight by feeding ecology?

```r
#msleep %>%
#  group_by(vore) %>% #we are grouping by feeding ecology, a categorical variable
#  summarize(min_bodywt = min(bodywt),
#            max_bodywt = max(bodywt),
#            mean_bodywt = mean(bodywt),
#            total=n())
```

## Counts
Although these summary functions are super helpful, oftentimes we are mostly interested in counts. The [janitor package](https://garthtarr.github.io/meatR/janitor.html) does a lot with counts, but there are also functions that are part of dplyr that are useful.  

`count()` is an easy way of determining how many observations you have within a column. It acts like a combination of `group_by()` and `n()`.

```r
#penguins %>% 
#  count(island, sort = T) #sort=T sorts the column in descending order
```

You can also use `count()` across multiple variables.

```r
#penguins %>% 
#  count(island, species, sort = T) # sort=T will arrange in descending order
```

You can also use `count()` to find NAs in data 

```r
#penguins %>% 
#  count(sex, island)
```

## `across()`
What about using `filter()` and `select()` across multiple variables? There is a function in dplyr called `across()` which is designed to work across multiple variables.

By using `across()` we can reduce the clutter and make things cleaner. 

```r
#penguins %>%
#  summarize(across(c(species, island, sex), n_distinct))
```

This is very helpful for continuous variables.

```r
#penguins %>%
#  summarize(across(contains("mm"), mean, na.rm=T))
```

`group_by` also works.

```r
#penguins %>%
#  group_by(sex) %>% 
#  summarize(across(contains("mm"), mean, na.rm=T))
```

Here we summarize across all variables.

```r
#penguins %>%
#  summarise_all(mean, na.rm=T)
```

the !c(...) says across all variables except ...

```r
#penguins %>%
#  summarise(across(!c(species, island, sex, year), 
#                   mean, na.rm=T))
```

All variables that include "bill"...all of the other dplyr operators also work.

```r
#penguins %>%
#  summarise(across(starts_with("bill"), mean, na.rm=T))
```
