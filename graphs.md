DM\_HT\_COVID19
================
Oscar J. Ponce & Francisco Barrera
5/2/2020

## Data cleaning & Databases information

Data was cleaned via RStudio. All databases are found in the file named
“Databases” located in the github projected called “DM\_HT\_COVID19”.

A total of four databases were used:

1.  **general\_info**: contains ID and real author names with RoB
    results.
2.  **proportions**: contains data needed to perform meta-analysis of
    proportions.
3.  **effectsize**: contains data neded to calculate meta-analysis of
    relative risks.
4.  **unimulti**: contains reported univaraite or multivariate analysis
    of our exposure and outcomes of interest.

## Proportions - summary of forest plots

\#\#Primer forestplot individual

![](graphs_files/figure-gfm/forestplot1%20-%20prop%20-%20DM%20in%20overall-1.png)<!-- -->

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](graphs_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
