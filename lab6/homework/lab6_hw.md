---
title: "Lab 6 Homework"
author: "Joel Ledford"
date: "2023-02-02"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---



## Instructions
Answer the following questions and complete the exercises in RMarkdown. Please embed all of your code and push your final work to your repository. Your final lab report should be organized, clean, and run free from errors. Remember, you must remove the `#` for the included code chunks to run. Be sure to add your name to the author header above.  

Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  

## Load the libraries

```r
library(tidyverse)
library(janitor)
library(skimr)
```

For this assignment we are going to work with a large data set from the [United Nations Food and Agriculture Organization](http://www.fao.org/about/en/) on world fisheries. These data are pretty wild, so we need to do some cleaning. First, load the data.  

Load the data `FAO_1950to2012_111914.csv` as a new object titled `fisheries`.

```r
fisheries <- readr::read_csv(file = "data/FAO_1950to2012_111914.csv")
```

```
## Rows: 17692 Columns: 71
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (69): Country, Common name, ISSCAAP taxonomic group, ASFIS species#, ASF...
## dbl  (2): ISSCAAP group#, FAO major fishing area
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

1. Do an exploratory analysis of the data (your choice). What are the names of the variables, what are the dimensions, are there any NA's, what are the classes of the variables?  

```r
names(fisheries)
```

```
##  [1] "Country"                 "Common name"            
##  [3] "ISSCAAP group#"          "ISSCAAP taxonomic group"
##  [5] "ASFIS species#"          "ASFIS species name"     
##  [7] "FAO major fishing area"  "Measure"                
##  [9] "1950"                    "1951"                   
## [11] "1952"                    "1953"                   
## [13] "1954"                    "1955"                   
## [15] "1956"                    "1957"                   
## [17] "1958"                    "1959"                   
## [19] "1960"                    "1961"                   
## [21] "1962"                    "1963"                   
## [23] "1964"                    "1965"                   
## [25] "1966"                    "1967"                   
## [27] "1968"                    "1969"                   
## [29] "1970"                    "1971"                   
## [31] "1972"                    "1973"                   
## [33] "1974"                    "1975"                   
## [35] "1976"                    "1977"                   
## [37] "1978"                    "1979"                   
## [39] "1980"                    "1981"                   
## [41] "1982"                    "1983"                   
## [43] "1984"                    "1985"                   
## [45] "1986"                    "1987"                   
## [47] "1988"                    "1989"                   
## [49] "1990"                    "1991"                   
## [51] "1992"                    "1993"                   
## [53] "1994"                    "1995"                   
## [55] "1996"                    "1997"                   
## [57] "1998"                    "1999"                   
## [59] "2000"                    "2001"                   
## [61] "2002"                    "2003"                   
## [63] "2004"                    "2005"                   
## [65] "2006"                    "2007"                   
## [67] "2008"                    "2009"                   
## [69] "2010"                    "2011"                   
## [71] "2012"
```

```r
glimpse(fisheries)
```

```
## Rows: 17,692
## Columns: 71
## $ Country                   <chr> "Albania", "Albania", "Albania", "Albania", …
## $ `Common name`             <chr> "Angelsharks, sand devils nei", "Atlantic bo…
## $ `ISSCAAP group#`          <dbl> 38, 36, 37, 45, 32, 37, 33, 45, 38, 57, 33, …
## $ `ISSCAAP taxonomic group` <chr> "Sharks, rays, chimaeras", "Tunas, bonitos, …
## $ `ASFIS species#`          <chr> "10903XXXXX", "1750100101", "17710001XX", "2…
## $ `ASFIS species name`      <chr> "Squatinidae", "Sarda sarda", "Sphyraena spp…
## $ `FAO major fishing area`  <dbl> 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, …
## $ Measure                   <chr> "Quantity (tonnes)", "Quantity (tonnes)", "Q…
## $ `1950`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1951`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1952`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1953`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1954`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1955`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1956`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1957`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1958`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1959`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1960`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1961`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1962`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1963`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1964`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1965`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1966`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1967`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1968`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1969`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1970`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1971`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1972`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1973`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1974`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1975`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1976`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1977`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1978`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1979`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1980`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1981`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1982`                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ `1983`                    <chr> NA, NA, NA, NA, NA, NA, "559", NA, NA, NA, N…
## $ `1984`                    <chr> NA, NA, NA, NA, NA, NA, "392", NA, NA, NA, N…
## $ `1985`                    <chr> NA, NA, NA, NA, NA, NA, "406", NA, NA, NA, N…
## $ `1986`                    <chr> NA, NA, NA, NA, NA, NA, "499", NA, NA, NA, N…
## $ `1987`                    <chr> NA, NA, NA, NA, NA, NA, "564", NA, NA, NA, N…
## $ `1988`                    <chr> NA, NA, NA, NA, NA, NA, "724", NA, NA, NA, N…
## $ `1989`                    <chr> NA, NA, NA, NA, NA, NA, "583", NA, NA, NA, N…
## $ `1990`                    <chr> NA, NA, NA, NA, NA, NA, "754", NA, NA, NA, N…
## $ `1991`                    <chr> NA, NA, NA, NA, NA, NA, "283", NA, NA, NA, N…
## $ `1992`                    <chr> NA, NA, NA, NA, NA, NA, "196", NA, NA, NA, N…
## $ `1993`                    <chr> NA, NA, NA, NA, NA, NA, "150 F", NA, NA, NA,…
## $ `1994`                    <chr> NA, NA, NA, NA, NA, NA, "100 F", NA, NA, NA,…
## $ `1995`                    <chr> "0 0", "1", NA, "0 0", "0 0", NA, "52", "30"…
## $ `1996`                    <chr> "53", "2", NA, "3", "2", NA, "104", "8", NA,…
## $ `1997`                    <chr> "20", "0 0", NA, "0 0", "0 0", NA, "65", "4"…
## $ `1998`                    <chr> "31", "12", NA, NA, NA, NA, "220", "18", NA,…
## $ `1999`                    <chr> "30", "30", NA, NA, NA, NA, "220", "18", NA,…
## $ `2000`                    <chr> "30", "25", "2", NA, NA, NA, "220", "20", NA…
## $ `2001`                    <chr> "16", "30", NA, NA, NA, NA, "120", "23", NA,…
## $ `2002`                    <chr> "79", "24", NA, "34", "6", NA, "150", "84", …
## $ `2003`                    <chr> "1", "4", NA, "22", NA, NA, "84", "178", NA,…
## $ `2004`                    <chr> "4", "2", "2", "15", "1", "2", "76", "285", …
## $ `2005`                    <chr> "68", "23", "4", "12", "5", "6", "68", "150"…
## $ `2006`                    <chr> "55", "30", "7", "18", "8", "9", "86", "102"…
## $ `2007`                    <chr> "12", "19", NA, NA, NA, NA, "132", "18", NA,…
## $ `2008`                    <chr> "23", "27", NA, NA, NA, NA, "132", "23", NA,…
## $ `2009`                    <chr> "14", "21", NA, NA, NA, NA, "154", "20", NA,…
## $ `2010`                    <chr> "78", "23", "7", NA, NA, NA, "80", "228", NA…
## $ `2011`                    <chr> "12", "12", NA, NA, NA, NA, "88", "9", NA, "…
## $ `2012`                    <chr> "5", "5", NA, NA, NA, NA, "129", "290", NA, …
```

```r
anyNA(fisheries)
```

```
## [1] TRUE
```

2. Use `janitor` to rename the columns and make them easier to use. As part of this cleaning step, change `country`, `isscaap_group_number`, `asfis_species_number`, and `fao_major_fishing_area` to data class factor. 

```r
fisheries<-janitor::clean_names(fisheries)
names(fisheries)
```

```
##  [1] "country"                 "common_name"            
##  [3] "isscaap_group_number"    "isscaap_taxonomic_group"
##  [5] "asfis_species_number"    "asfis_species_name"     
##  [7] "fao_major_fishing_area"  "measure"                
##  [9] "x1950"                   "x1951"                  
## [11] "x1952"                   "x1953"                  
## [13] "x1954"                   "x1955"                  
## [15] "x1956"                   "x1957"                  
## [17] "x1958"                   "x1959"                  
## [19] "x1960"                   "x1961"                  
## [21] "x1962"                   "x1963"                  
## [23] "x1964"                   "x1965"                  
## [25] "x1966"                   "x1967"                  
## [27] "x1968"                   "x1969"                  
## [29] "x1970"                   "x1971"                  
## [31] "x1972"                   "x1973"                  
## [33] "x1974"                   "x1975"                  
## [35] "x1976"                   "x1977"                  
## [37] "x1978"                   "x1979"                  
## [39] "x1980"                   "x1981"                  
## [41] "x1982"                   "x1983"                  
## [43] "x1984"                   "x1985"                  
## [45] "x1986"                   "x1987"                  
## [47] "x1988"                   "x1989"                  
## [49] "x1990"                   "x1991"                  
## [51] "x1992"                   "x1993"                  
## [53] "x1994"                   "x1995"                  
## [55] "x1996"                   "x1997"                  
## [57] "x1998"                   "x1999"                  
## [59] "x2000"                   "x2001"                  
## [61] "x2002"                   "x2003"                  
## [63] "x2004"                   "x2005"                  
## [65] "x2006"                   "x2007"                  
## [67] "x2008"                   "x2009"                  
## [69] "x2010"                   "x2011"                  
## [71] "x2012"
```


```r
fisheries <- 
  fisheries %>% mutate(across(c(country, isscaap_group_number, asfis_species_number, fao_major_fishing_area), as_factor))
```

We need to deal with the years because they are being treated as characters and start with an X. We also have the problem that the column names that are years actually represent data. We haven't discussed tidy data yet, so here is some help. You should run this ugly chunk to transform the data for the rest of the homework. It will only work if you have used janitor to rename the variables in question 2!

```r
fisheries_tidy <- fisheries %>% 
  pivot_longer(-c(country,common_name,isscaap_group_number,isscaap_taxonomic_group,asfis_species_number,asfis_species_name,fao_major_fishing_area,measure),
               names_to = "year",
               values_to = "catch",
               values_drop_na = TRUE) %>% 
  mutate(year= as.numeric(str_replace(year, 'x', ''))) %>% 
  mutate(catch= str_replace(catch, c(' F'), replacement = '')) %>% 
  mutate(catch= str_replace(catch, c('...'), replacement = '')) %>% 
  mutate(catch= str_replace(catch, c('-'), replacement = '')) %>% 
  mutate(catch= str_replace(catch, c('0 0'), replacement = ''))

fisheries_tidy$catch <- as.numeric(fisheries_tidy$catch)
```

3. How many countries are represented in the data? Provide a count and list their names.

```r
fisheries_tidy %>% 
tabyl(country)
```

```
##                    country     n      percent
##                    Albania   934 2.478959e-03
##                    Algeria  1561 4.143100e-03
##             American Samoa   556 1.475697e-03
##                     Angola  2119 5.624106e-03
##                   Anguilla   129 3.423830e-04
##        Antigua and Barbuda   356 9.448710e-04
##                  Argentina  3403 9.032011e-03
##                      Aruba   172 4.565107e-04
##                  Australia  8183 2.171876e-02
##                    Bahamas   423 1.122698e-03
##                    Bahrain  1169 3.102680e-03
##                 Bangladesh   169 4.485483e-04
##                   Barbados   795 2.110035e-03
##                    Belgium  2530 6.714954e-03
##                     Belize  1075 2.853192e-03
##                      Benin  1419 3.766213e-03
##                    Bermuda   846 2.245396e-03
##   Bonaire/S.Eustatius/Saba     4 1.061653e-05
##     Bosnia and Herzegovina    21 5.573677e-05
##                     Brazil  4784 1.269737e-02
##   British Indian Ocean Ter    97 2.574508e-04
##     British Virgin Islands   332 8.811719e-04
##          Brunei Darussalam   186 4.936686e-04
##                   Bulgaria  1596 4.235995e-03
##                 Cabo Verde   462 1.226209e-03
##                   Cambodia   238 6.316834e-04
##                   Cameroon  1340 3.556537e-03
##                     Canada  5099 1.353342e-02
##             Cayman Islands    84 2.229471e-04
##            Channel Islands  1313 3.484875e-03
##                      Chile  3878 1.029272e-02
##                      China  2801 7.434224e-03
##       China, Hong Kong SAR  1782 4.729663e-03
##           China, Macao SAR   206 5.467512e-04
##                   Colombia  2710 7.192698e-03
##                    Comoros   965 2.561237e-03
##    Congo, Dem. Rep. of the   484 1.284600e-03
##         Congo, Republic of  1527 4.052860e-03
##               Cook Islands   810 2.149847e-03
##                 Costa Rica  1171 3.107989e-03
##                    Croatia   947 2.513463e-03
##                       Cuba  2981 7.911968e-03
##                 Cura\xe7ao    18 4.777438e-05
##                     Cyprus  1703 4.519987e-03
##           C\xf4te d'Ivoire  1859 4.934032e-03
##                    Denmark  3741 9.929108e-03
##                   Djibouti   352 9.342545e-04
##                   Dominica   213 5.653301e-04
##         Dominican Republic  1958 5.196791e-03
##                    Ecuador  1595 4.233341e-03
##                      Egypt  2467 6.547744e-03
##                El Salvador   620 1.645562e-03
##          Equatorial Guinea   551 1.462427e-03
##                    Eritrea   653 1.733148e-03
##                    Estonia  1088 2.887696e-03
##                   Ethiopia   129 3.423830e-04
##     Falkland Is.(Malvinas)   502 1.332374e-03
##              Faroe Islands  2283 6.059384e-03
##          Fiji, Republic of  1798 4.772129e-03
##                    Finland   706 1.873817e-03
##                     France 10639 2.823731e-02
##              French Guiana   231 6.131045e-04
##           French Polynesia   672 1.783577e-03
##       French Southern Terr   139 3.689244e-04
##                      Gabon  1089 2.890350e-03
##                     Gambia  1214 3.222116e-03
##                    Georgia   428 1.135969e-03
##                    Germany  4813 1.277434e-02
##                      Ghana  2462 6.534473e-03
##                  Gibraltar    61 1.619021e-04
##                     Greece  4091 1.085805e-02
##                  Greenland  1311 3.479567e-03
##                    Grenada  1635 4.339506e-03
##                 Guadeloupe   372 9.873371e-04
##                       Guam   520 1.380149e-03
##                  Guatemala   622 1.650870e-03
##                     Guinea   697 1.849930e-03
##               GuineaBissau   634 1.682720e-03
##                     Guyana   251 6.661872e-04
##                      Haiti   204 5.414429e-04
##                   Honduras   842 2.234779e-03
##                    Iceland  2346 6.226594e-03
##                      India  5588 1.483129e-02
##                  Indonesia  9274 2.461442e-02
##     Iran (Islamic Rep. of)  1210 3.211500e-03
##                       Iraq   150 3.981198e-04
##                    Ireland  3235 8.586117e-03
##                Isle of Man   952 2.526734e-03
##                     Israel  1359 3.606966e-03
##                      Italy  4567 1.212142e-02
##                    Jamaica   149 3.954657e-04
##                      Japan 15429 4.095060e-02
##                     Jordan   226 5.998339e-04
##                      Kenya   958 2.542659e-03
##                   Kiribati   875 2.322366e-03
##   Korea, Dem. People's Rep   210 5.573677e-04
##         Korea, Republic of 10824 2.872833e-02
##                     Kuwait   805 2.136576e-03
##                     Latvia  1101 2.922199e-03
##                    Lebanon   614 1.629637e-03
##                    Liberia  1514 4.018356e-03
##                      Libya   578 1.534088e-03
##                  Lithuania  1274 3.381364e-03
##                 Madagascar  1008 2.675365e-03
##                   Malaysia  6963 1.848072e-02
##                   Maldives   487 1.292562e-03
##                      Malta  2156 5.722309e-03
##           Marshall Islands   292 7.750066e-04
##                 Martinique   672 1.783577e-03
##                 Mauritania  1501 3.983852e-03
##                  Mauritius   991 2.630245e-03
##                    Mayotte   194 5.149016e-04
##                     Mexico  6202 1.646093e-02
##  Micronesia, Fed.States of   413 1.096157e-03
##                     Monaco    43 1.141277e-04
##                 Montenegro   168 4.458942e-04
##                 Montserrat    63 1.672103e-04
##                    Morocco  4758 1.262836e-02
##                 Mozambique   434 1.151893e-03
##                    Myanmar   117 3.105335e-04
##                    Namibia   905 2.401990e-03
##                      Nauru   150 3.981198e-04
##                Netherlands  2944 7.813765e-03
##       Netherlands Antilles   338 8.970966e-04
##              New Caledonia   789 2.094110e-03
##                New Zealand  4594 1.219308e-02
##                  Nicaragua   904 2.399335e-03
##                    Nigeria  1479 3.925461e-03
##                       Niue   145 3.848492e-04
##             Norfolk Island    41 1.088194e-04
##       Northern Mariana Is.   488 1.295216e-03
##                     Norway  3747 9.945033e-03
##                       Oman  1086 2.882387e-03
##                  Other nei  1556 4.129830e-03
##                   Pakistan  2166 5.748850e-03
##                      Palau   636 1.688028e-03
##    Palestine, Occupied Tr.   429 1.138623e-03
##                     Panama  1773 4.705776e-03
##           Papua New Guinea   686 1.820735e-03
##                       Peru  2767 7.343983e-03
##                Philippines  4548 1.207099e-02
##           Pitcairn Islands    63 1.672103e-04
##                     Poland  2553 6.775999e-03
##                   Portugal 11570 3.070831e-02
##                Puerto Rico   918 2.436493e-03
##                      Qatar   941 2.497538e-03
##                    Romania  1738 4.612882e-03
##         Russian Federation  4736 1.256997e-02
##                 R\xe9union   736 1.953441e-03
##        Saint Barth\xe9lemy     6 1.592479e-05
##               Saint Helena   609 1.616366e-03
##      Saint Kitts and Nevis   397 1.053690e-03
##                Saint Lucia   558 1.481006e-03
##   Saint Vincent/Grenadines   715 1.897704e-03
##                SaintMartin     6 1.592479e-05
##                      Samoa   386 1.024495e-03
##      Sao Tome and Principe  1035 2.747027e-03
##               Saudi Arabia  2200 5.839091e-03
##                    Senegal  2988 7.930547e-03
##      Serbia and Montenegro   516 1.369532e-03
##                 Seychelles  1142 3.031019e-03
##               Sierra Leone  1526 4.050206e-03
##                  Singapore  1937 5.141054e-03
##               Sint Maarten     4 1.061653e-05
##                   Slovenia   644 1.709261e-03
##            Solomon Islands   505 1.340337e-03
##                    Somalia   141 3.742326e-04
##               South Africa  3881 1.030069e-02
##                      Spain 17482 4.639954e-02
##                  Sri Lanka  1351 3.585732e-03
##    St. Pierre and Miquelon  1038 2.754989e-03
##                      Sudan     3 7.962396e-06
##             Sudan (former)    90 2.388719e-04
##                   Suriname   234 6.210669e-04
##     Svalbard and Jan Mayen    41 1.088194e-04
##                     Sweden  3115 8.267621e-03
##       Syrian Arab Republic   793 2.104727e-03
##   Taiwan Province of China  9927 2.634757e-02
##   Tanzania, United Rep. of  1277 3.389327e-03
##                   Thailand  4843 1.285396e-02
##                 TimorLeste    98 2.601049e-04
##                       Togo  1723 4.573070e-03
##                    Tokelau   102 2.707215e-04
##                      Tonga   403 1.069615e-03
##        Trinidad and Tobago   923 2.449764e-03
##                    Tunisia  3019 8.012825e-03
##                     Turkey  3326 8.827643e-03
##       Turks and Caicos Is.   193 5.122475e-04
##                     Tuvalu   162 4.299694e-04
##          US Virgin Islands   348 9.236380e-04
##                    Ukraine  1823 4.838483e-03
##         Un. Sov. Soc. Rep.  7084 1.880187e-02
##       United Arab Emirates  1801 4.780092e-03
##             United Kingdom  6577 1.745623e-02
##   United States of America 18080 4.798671e-02
##                    Uruguay  2134 5.663918e-03
##                    Vanuatu   789 2.094110e-03
##    Venezuela, Boliv Rep of  3409 9.047936e-03
##                   Viet Nam   405 1.074923e-03
##      Wallis and Futuna Is.   128 3.397289e-04
##             Western Sahara     0 0.000000e+00
##                      Yemen  1278 3.391981e-03
##             Yugoslavia SFR  1383 3.670665e-03
##                   Zanzibar   247 6.555706e-04
```

4. Refocus the data only to include country, isscaap_taxonomic_group, asfis_species_name, asfis_species_number, year, catch.

```r
selected_fisheries<-fisheries_tidy %>%
  select(country, isscaap_taxonomic_group, asfis_species_name, asfis_species_number, year, catch)
selected_fisheries
```

```
## # A tibble: 376,771 × 6
##    country isscaap_taxonomic_group asfis_species_name asfis_specie…¹  year catch
##    <fct>   <chr>                   <chr>              <fct>          <dbl> <dbl>
##  1 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX      1995    NA
##  2 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX      1996    53
##  3 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX      1997    20
##  4 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX      1998    31
##  5 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX      1999    30
##  6 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX      2000    30
##  7 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX      2001    16
##  8 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX      2002    79
##  9 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX      2003     1
## 10 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX      2004     4
## # … with 376,761 more rows, and abbreviated variable name ¹​asfis_species_number
```

5. Based on the asfis_species_number, how many distinct fish species were caught as part of these data?

```r
selected_fisheries %>% 
  summarise(n_asfis_species_number=n_distinct(asfis_species_number))
```

```
## # A tibble: 1 × 1
##   n_asfis_species_number
##                    <int>
## 1                   1551
```

6. Which country had the largest overall catch in the year 2000?

```r
fisheries_tidy %>% 
  filter(year==2000) %>% 
  group_by(country) %>% 
  summarise(catch_total=sum(catch, na.rm=T)) %>%
  arrange(desc(catch_total))
```

```
## # A tibble: 193 × 2
##    country                  catch_total
##    <fct>                          <dbl>
##  1 China                          25899
##  2 Russian Federation             12181
##  3 United States of America       11762
##  4 Japan                           8510
##  5 Indonesia                       8341
##  6 Peru                            7443
##  7 Chile                           6906
##  8 India                           6351
##  9 Thailand                        6243
## 10 Korea, Republic of              6124
## # … with 183 more rows
```

7. Which country caught the most sardines (_Sardina pilchardus_) between the years 1990-2000?

```r
fisheries_tidy %>%
  group_by(country) %>% 
  filter(asfis_species_name=="Sardina pilchardus") %>%
  filter(between(year, 1990, 2000)) %>%
  summarise(sardine_catch=sum(catch, na.rm=T)) %>%
  arrange(desc(sardine_catch))
```

```
## # A tibble: 37 × 2
##    country               sardine_catch
##    <fct>                         <dbl>
##  1 Morocco                        7470
##  2 Spain                          3507
##  3 Russian Federation             1639
##  4 Ukraine                        1030
##  5 France                          966
##  6 Portugal                        818
##  7 Greece                          528
##  8 Italy                           507
##  9 Serbia and Montenegro           478
## 10 Denmark                         477
## # … with 27 more rows
```

8. Which five countries caught the most cephalopods between 2008-2012?

```r
fisheries_tidy %>% 
  group_by(country) %>% 
  filter(asfis_species_name=="Cephalopoda") %>% 
  filter(between(year, 2008, 2012)) %>%
  summarise(cephalopods_catch=sum(catch, na.rm=T)) %>%
  arrange(desc(cephalopods_catch)) %>% 
  head(n=5)
```

```
## # A tibble: 5 × 2
##   country cephalopods_catch
##   <fct>               <dbl>
## 1 India                 570
## 2 China                 257
## 3 Spain                 198
## 4 Algeria               162
## 5 France                101
```

9. Which species had the highest catch total between 2008-2012? (hint: Osteichthyes is not a species)

```r
fisheries_tidy %>% 
  group_by(asfis_species_name) %>% 
  filter(between(year, 2008, 2012)) %>% 
  summarise(species_catch=sum(catch, na.rm=T)) %>% 
  arrange(desc(species_catch))
```

```
## # A tibble: 1,472 × 2
##    asfis_species_name    species_catch
##    <chr>                         <dbl>
##  1 Osteichthyes                 107808
##  2 Theragra chalcogramma         41075
##  3 Engraulis ringens             35523
##  4 Katsuwonus pelamis            32153
##  5 Trichiurus lepturus           30400
##  6 Clupea harengus               28527
##  7 Thunnus albacares             20119
##  8 Scomber japonicus             14723
##  9 Gadus morhua                  13253
## 10 Thunnus alalunga              12019
## # … with 1,462 more rows
```

10. Use the data to do at least one analysis of your choice.

What year did the most fish get caught?

```r
fisheries_tidy %>% 
  group_by(year) %>% 
  summarise(year_catch=sum(catch, na.rm=T)) %>% 
  arrange(desc(year_catch))
```

```
## # A tibble: 63 × 2
##     year year_catch
##    <dbl>      <dbl>
##  1  2007     268130
##  2  2006     257669
##  3  2001     257320
##  4  2002     255807
##  5  2012     255406
##  6  2008     255013
##  7  2004     254115
##  8  2005     251877
##  9  2009     251181
## 10  2010     244839
## # … with 53 more rows
```

## Push your final code to GitHub!
Please be sure that you check the `keep md` file in the knit preferences.   
