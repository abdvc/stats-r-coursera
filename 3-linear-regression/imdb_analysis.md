Modeling and prediction for movies
================

## Setup

### Load packages

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.6.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.6.3

``` r
library(statsr)
```

    ## Warning: package 'statsr' was built under R version 3.6.3

``` r
library(corrplot)
```

    ## Warning: package 'corrplot' was built under R version 3.6.3

### Load data

Make sure your data and R Markdown files are in the same directory. When
loaded your data file will be called `movies`. Delete this note when
before you submit your work.

``` r
load("movies.Rdata")
```

-----

## Part 1: Data

The data set consists of 651 movies produced and released before 2016.
The source of this data comes from Rotten Tomatoes and IMDb (Internet
Movie Database). According to the information given, the data set is
randomly sampled, meaning the sample can be generalized to the
population. However, there is no mention of random assignment, so
causation cannot be established as a part of this study.

-----

## Part 2: Research question

For this project, I will determine which attributes, from the dataset
provided, make a movie popular.

Additionally, through exploratory data analysis, I will determine if
there is a possible relationship between the movie’s popularity, and
whether it won an Oscar award or not.

-----

## Part 3: Exploratory data analysis

First, I shall look into the relationship between a movie’s popularity
and if the movie had won an oscar. To determine what makes a movie
popular, I have created a variable *score* which is the average of the
rating on IMDB and the audience score on Rotten Tomatoes. I have not
considered critic score as I am considering only audience ratings as a
measure of popularity.

### Creating the score variable

Lets take a look at the `audience_score` and `imdb_rating`:

``` r
movies %>% select(imdb_rating, audience_score)
```

    ## # A tibble: 651 x 2
    ##    imdb_rating audience_score
    ##          <dbl>          <dbl>
    ##  1         5.5             73
    ##  2         7.3             81
    ##  3         7.6             91
    ##  4         7.2             76
    ##  5         5.1             27
    ##  6         7.8             86
    ##  7         7.2             76
    ##  8         5.5             47
    ##  9         7.5             89
    ## 10         6.6             66
    ## # ... with 641 more rows

``` r
summary(movies$audience_score)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   11.00   46.00   65.00   62.36   80.00   97.00

``` r
summary(movies$imdb_rating)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.900   5.900   6.600   6.493   7.300   9.000

For these two variables to be combined they need to be on the same
scale. This can be achieved by multiplying the values in the
`imdb_rating` column by 10.

I will create a variable `score` which will be the average of the
`imdb_rating` and `audience_score`.

``` r
movie_score = ((movies$imdb_rating*10)+movies$audience_score)/2
summary(movie_score)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   17.50   52.50   65.50   63.65   76.00   93.50

### Plot of Oscar nominees and Oscar winners

Taking a subset from the dataset, I will show the distribution of movies
that have won an Oscar.

``` r
movies_1 <- movies %>% mutate(score = movie_score)
oscar_score <- movies_1 %>% select(audience_score, imdb_rating, best_pic_nom, best_pic_win, score)

oscar_score
```

    ## # A tibble: 651 x 5
    ##    audience_score imdb_rating best_pic_nom best_pic_win score
    ##             <dbl>       <dbl> <fct>        <fct>        <dbl>
    ##  1             73         5.5 no           no            64  
    ##  2             81         7.3 no           no            77  
    ##  3             91         7.6 no           no            83.5
    ##  4             76         7.2 no           no            74  
    ##  5             27         5.1 no           no            39  
    ##  6             86         7.8 no           no            82  
    ##  7             76         7.2 no           no            74  
    ##  8             47         5.5 no           no            51  
    ##  9             89         7.5 no           no            82  
    ## 10             66         6.6 no           no            66  
    ## # ... with 641 more rows

``` r
oscar_score %>% 
  group_by(best_pic_nom, best_pic_win) %>% 
  summarise(count = n())
```

    ## # A tibble: 4 x 3
    ## # Groups:   best_pic_nom [2]
    ##   best_pic_nom best_pic_win count
    ##   <fct>        <fct>        <int>
    ## 1 no           no             628
    ## 2 no           yes              1
    ## 3 yes          no              16
    ## 4 yes          yes              6

There seems to be a data issue as one of the records say that it was not
nominated for an Oscar but it won the award. This is most likely an
issue in the dataset.

``` r
movies_1 %>% 
  filter(best_pic_nom == 'no', best_pic_win == 'yes') %>% 
  select(title)
```

    ## # A tibble: 1 x 1
    ##   title          
    ##   <chr>          
    ## 1 The Hurt Locker

According to IMDB, The Hurt Locker has won an Oscar for best picture, so
I can rectify this issue in the data.

``` r
movies_1 <- movies_1 %>% 
  mutate(best_pic_nom = replace(best_pic_nom, best_pic_nom=="no" & best_pic_win=="yes","yes"))
```

Now taking a subset of the data again, we can see that the data
inaccuracy has been handled.

``` r
oscar_score <- movies_1 %>% 
  select(audience_score, imdb_rating, best_pic_nom, best_pic_win) %>% 
  mutate(score = movie_score)

oscar_score %>% 
  group_by(best_pic_nom, best_pic_win) %>% 
  summarise(count = n())
```

    ## # A tibble: 3 x 3
    ## # Groups:   best_pic_nom [2]
    ##   best_pic_nom best_pic_win count
    ##   <fct>        <fct>        <int>
    ## 1 no           no             628
    ## 2 yes          no              16
    ## 3 yes          yes              7

Now onto visualizating the Oscar winners and their scores.

``` r
ggplot(oscar_score, aes(x = best_pic_win, y = score)) + 
  geom_point(aes(color=best_pic_nom)) + theme_minimal()
```

![](imdb_analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

We can see that movies that were given a high score won the Oscar award.
To see if these two groups are significantly different, we can visualize
with a boxplot.

``` r
ggplot(oscar_score, aes(x = best_pic_win, y = score)) + 
  geom_boxplot() + theme_minimal()
```

![](imdb_analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Since the two boxes are not intersecting with eachother, we can say that
the two groups **are** different from eachother.

### Cleaning data

Lets check which columns in the dataset have missing values.

``` r
colnames(movies_1)[colSums(is.na(movies_1)) > 0]
```

    ##  [1] "runtime"       "studio"        "dvd_rel_year"  "dvd_rel_month"
    ##  [5] "dvd_rel_day"   "director"      "actor1"        "actor2"       
    ##  [9] "actor3"        "actor4"        "actor5"

The columns `studio`, `director`, and actor columns from 1 to 5 do not
seem to be relevant to our task so I will not be considering them in the
data cleaning process.

``` r
summary(movies_1[c('runtime','dvd_rel_year','dvd_rel_month','dvd_rel_day')])
```

    ##     runtime       dvd_rel_year  dvd_rel_month     dvd_rel_day   
    ##  Min.   : 39.0   Min.   :1991   Min.   : 1.000   Min.   : 1.00  
    ##  1st Qu.: 92.0   1st Qu.:2001   1st Qu.: 3.000   1st Qu.: 7.00  
    ##  Median :103.0   Median :2004   Median : 6.000   Median :15.00  
    ##  Mean   :105.8   Mean   :2004   Mean   : 6.333   Mean   :15.01  
    ##  3rd Qu.:115.8   3rd Qu.:2008   3rd Qu.: 9.000   3rd Qu.:23.00  
    ##  Max.   :267.0   Max.   :2015   Max.   :12.000   Max.   :31.00  
    ##  NA's   :1       NA's   :8      NA's   :8        NA's   :8

Lets take a look at these NA rows in more detail.

``` r
movies_1 %>% 
  filter(is.na(dvd_rel_year) | is.na(dvd_rel_month) | is.na(dvd_rel_day) | is.na(runtime)) %>% 
  select(title, runtime, dvd_rel_year, dvd_rel_month, dvd_rel_day)
```

    ## # A tibble: 9 x 5
    ##   title                           runtime dvd_rel_year dvd_rel_month dvd_rel_day
    ##   <chr>                             <dbl>        <dbl>         <dbl>       <dbl>
    ## 1 Charlie: The Life and Art of C~     132           NA            NA          NA
    ## 2 Streets of Gold                      95           NA            NA          NA
    ## 3 The Squeeze                         101           NA            NA          NA
    ## 4 The End of America                   NA         2009             1          20
    ## 5 Electric Dreams                      95           NA            NA          NA
    ## 6 Porky's Revenge                      92           NA            NA          NA
    ## 7 Teen Wolf Too                        95           NA            NA          NA
    ## 8 The Last Remake of Beau Geste        85           NA            NA          NA
    ## 9 Let It Be                            81           NA            NA          NA

Initially, I had planned to impute these missing values based on
information from IMDB. However, the movies with NA values in columns
related to the DVD release date were released before DVD was introduced.

``` r
movies_1 %>% 
  filter(is.na(dvd_rel_year)) %>% 
  select(title, thtr_rel_year, thtr_rel_month, thtr_rel_day)
```

    ## # A tibble: 8 x 4
    ##   title                                thtr_rel_year thtr_rel_month thtr_rel_day
    ##   <chr>                                        <dbl>          <dbl>        <dbl>
    ## 1 Charlie: The Life and Art of Charle~          2004              2           13
    ## 2 Streets of Gold                               1986             11           14
    ## 3 The Squeeze                                   1987              7           10
    ## 4 Electric Dreams                               1984              7           20
    ## 5 Porky's Revenge                               1985              3           22
    ## 6 Teen Wolf Too                                 1987             11           20
    ## 7 The Last Remake of Beau Geste                 1977              7           15
    ## 8 Let It Be                                     1970              5           20

**Charlie: The Life and Art of Charles Chaplin** is the only row where
the DVD release date is available. So I have decided to impute values
for this movie, and **The End of America**. The rest of the rows will be
dropped.

``` r
movies_2 <- movies_1 %>% 
  mutate(runtime = replace(runtime, title=="The End of America",74),
      dvd_rel_year = replace(dvd_rel_year, title == "Charlie: The Life and Art of Charles Chaplin", 2003),
      dvd_rel_month = replace(dvd_rel_month, title == "Charlie: The Life and Art of Charles Chaplin", 11),
      dvd_rel_day = replace(dvd_rel_day, title == "Charlie: The Life and Art of Charles Chaplin", 3))

movies_2 %>% 
  filter(title == "The End of America" | title == "Charlie: The Life and Art of Charles Chaplin") %>% 
  select(title, runtime, dvd_rel_year, dvd_rel_month, dvd_rel_day)
```

    ## # A tibble: 2 x 5
    ##   title                           runtime dvd_rel_year dvd_rel_month dvd_rel_day
    ##   <chr>                             <dbl>        <dbl>         <dbl>       <dbl>
    ## 1 Charlie: The Life and Art of C~     132         2003            11           3
    ## 2 The End of America                   74         2009             1          20

Now I shall drop the remaining rows with NA.

``` r
movies_3 <- movies_2 %>% filter(!is.na(dvd_rel_year), !is.na(dvd_rel_month), !is.na(dvd_rel_day))

head(movies_3)
```

    ## # A tibble: 6 x 33
    ##   title title_type genre runtime mpaa_rating studio thtr_rel_year thtr_rel_month
    ##   <chr> <fct>      <fct>   <dbl> <fct>       <fct>          <dbl>          <dbl>
    ## 1 Fill~ Feature F~ Drama      80 R           Indom~          2013              4
    ## 2 The ~ Feature F~ Drama     101 PG-13       Warne~          2001              3
    ## 3 Wait~ Feature F~ Come~      84 R           Sony ~          1996              8
    ## 4 The ~ Feature F~ Drama     139 PG          Colum~          1993             10
    ## 5 Male~ Feature F~ Horr~      90 R           Ancho~          2004              9
    ## 6 Old ~ Documenta~ Docu~      78 Unrated     Shcal~          2009              1
    ## # ... with 25 more variables: thtr_rel_day <dbl>, dvd_rel_year <dbl>,
    ## #   dvd_rel_month <dbl>, dvd_rel_day <dbl>, imdb_rating <dbl>,
    ## #   imdb_num_votes <int>, critics_rating <fct>, critics_score <dbl>,
    ## #   audience_rating <fct>, audience_score <dbl>, best_pic_nom <fct>,
    ## #   best_pic_win <fct>, best_actor_win <fct>, best_actress_win <fct>,
    ## #   best_dir_win <fct>, top200_box <fct>, director <chr>, actor1 <chr>,
    ## #   actor2 <chr>, actor3 <chr>, actor4 <chr>, actor5 <chr>, imdb_url <chr>,
    ## #   rt_url <chr>, score <dbl>

### Scatter plots

``` r
ggplot(movies_3, aes(x = runtime, y = score)) + 
  geom_point() + stat_smooth(method=lm) + theme_minimal()
```

    ## `geom_smooth()` using formula 'y ~ x'

![](imdb_analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Looking at the plot above, there seems to be a very weak relationship
between `runtime` and `score`.

``` r
ggplot(movies_3, aes(x = critics_score, y = score)) + 
  geom_point() + stat_smooth(method=lm) + theme_minimal()
```

    ## `geom_smooth()` using formula 'y ~ x'

![](imdb_analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

For the plot of `critics_score` and `score`, there seems to be a
moderately strong relationship between the two variables.

``` r
ggplot(movies_3, aes(x = imdb_num_votes, y = score)) + 
  geom_point() + stat_smooth(method=lm) + theme_minimal()
```

    ## `geom_smooth()` using formula 'y ~ x'

![](imdb_analysis_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

There seems to be an extreme right skewness in the imdb\_num\_votes
variable. A weak linear relationship may exist between the two
variables.

### Correlation Matrix

For the correlation matrix, I shall extract the numerical columns from
the dataset. Note that I am leaving out `imdb_rating` and
`audience_score` as these both were used to make the `score` variable.
It is expected that the `score` variable would high very high
correlation with the two other variables, and this might be undesirable
for our model.

``` r
nums <- unlist(lapply(movies_3, is.numeric))
numerical_cols <- movies_3[,nums]
numerical_cols <- select(numerical_cols, -c(8,11))
```

Now plotting the correlation matrix:

``` r
cor_mat <- cor(numerical_cols)

corrplot(cor_mat, type="upper", method="number")
```

![](imdb_analysis_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

There only seems to be a strong correlation with `critics_score`. The
two other variables we plotted above, `imdb_num_votes` and `runtime`
have a very weak positive correlation with score. Additionally, there
seems to be a moderately strong positive relatonship between
`thtr_rel_year` and `dvd_rel_year`. Even though this relationship is not
strong, it is in our best interest to remove this variable to prevent
any collinearity in the model.

-----

## Part 4: Modeling

For modeling, I will be considering the following variables:

`title_type`, `genre`, `runtime`, `mpaa_rating`, `thtr_rel_year`,
`thtr_rel_month`, `thtr_rel_day`, `dvd_rel_month`, `dvd_rel_day`,
`imdb_num_votes`, `critics_rating`, `critics_score`, `audience_rating`,
`best_pic_nom`, `best_pic_win`, and `top200_box`.

In addition to variables related to ‘score’ being removed, variables
such as the name of the movie, directors, or actors have been removed as
well since they don’t seem to be meaningful in our model. URLs to the
movie page has been removed as well.

Forward selection will be implemented in this model to make sure our
model has only the relevant variables, making it parsimonious. We will
refit the model, removing each variable until we get the highest R
squared value. For convenience of the readers, the working has been
omitted from this markdown file.

### Model Selection

``` r
model_1 <- lm(score ~ audience_rating, data=movies_3)

summary(model_1)$adj.r.squared
```

    ## [1] 0.6910981

``` r
model_2 <- lm(score ~ audience_rating + critics_score, data=movies_3)

summary(model_2)$adj.r.squared
```

    ## [1] 0.7932606

``` r
model_3 <- lm(score ~ audience_rating + critics_score + imdb_num_votes, data=movies_3)

summary(model_3)$adj.r.squared
```

    ## [1] 0.803056

``` r
model_4 <- lm(score ~ audience_rating + critics_score + imdb_num_votes + genre, data=movies_3)

summary(model_4)$adj.r.squared
```

    ## [1] 0.8107389

``` r
model_5 <- lm(score ~ audience_rating + critics_score + imdb_num_votes + genre + critics_rating, data=movies_3)

summary(model_5)$adj.r.squared
```

    ## [1] 0.8144446

``` r
model_6 <- lm(score ~ audience_rating + critics_score + imdb_num_votes + genre + critics_rating + thtr_rel_year, data=movies_3)

summary(model_6)$adj.r.squared
```

    ## [1] 0.8149227

``` r
model_7 <- lm(score ~ audience_rating + critics_score + imdb_num_votes + genre + critics_rating + thtr_rel_year + dvd_rel_month, data=movies_3)

summary(model_7)$adj.r.squared
```

    ## [1] 0.8152376

``` r
model_8 <- lm(score ~ audience_rating + critics_score + imdb_num_votes + genre + critics_rating + thtr_rel_year + dvd_rel_month + best_pic_nom, data=movies_3)

summary(model_8)$adj.r.squared
```

    ## [1] 0.8154762

``` r
model_9 <- lm(score ~ audience_rating + critics_score + imdb_num_votes + genre + critics_rating + thtr_rel_year + dvd_rel_month + best_pic_nom + title_type, data=movies_3)

summary(model_9)$adj.r.squared
```

    ## [1] 0.8157536

``` r
model_10 <- lm(score ~ audience_rating + critics_score + imdb_num_votes + genre + critics_rating + thtr_rel_year + dvd_rel_month + best_pic_nom + title_type + top200_box, data=movies_3)

summary(model_10)$adj.r.squared
```

    ## [1] 0.8159692

``` r
model_11 <- lm(score ~ audience_rating + critics_score + imdb_num_votes + genre + critics_rating + thtr_rel_year + dvd_rel_month + best_pic_nom + title_type + top200_box + runtime, data=movies_3)

summary(model_11)$adj.r.squared
```

    ## [1] 0.8161424

``` r
model_12 <- lm(score ~ audience_rating + critics_score + imdb_num_votes + genre + critics_rating + thtr_rel_year + dvd_rel_month + best_pic_nom + title_type + top200_box + runtime + best_pic_win, data=movies_3)

summary(model_12)$adj.r.squared
```

    ## [1] 0.8163103

This is the final model as addition of any more variables will not
increase the adjust R squared value

``` r
final_model <- model_12

summary(final_model)
```

    ## 
    ## Call:
    ## lm(formula = score ~ audience_rating + critics_score + imdb_num_votes + 
    ##     genre + critics_rating + thtr_rel_year + dvd_rel_month + 
    ##     best_pic_nom + title_type + top200_box + runtime + best_pic_win, 
    ##     data = movies_3)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -24.1665  -4.1272   0.3586   4.4620  16.1881 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     1.099e+02  5.302e+01   2.073 0.038570 *  
    ## audience_ratingUpright          1.691e+01  6.657e-01  25.395  < 2e-16 ***
    ## critics_score                   2.367e-01  2.081e-02  11.378  < 2e-16 ***
    ## imdb_num_votes                  1.914e-05  2.964e-06   6.458 2.14e-10 ***
    ## genreAnimation                 -1.160e+00  2.320e+00  -0.500 0.617151    
    ## genreArt House & International  3.064e+00  1.993e+00   1.537 0.124702    
    ## genreComedy                    -4.380e-01  1.077e+00  -0.407 0.684306    
    ## genreDocumentary                6.811e+00  2.544e+00   2.677 0.007615 ** 
    ## genreDrama                      9.719e-01  9.404e-01   1.034 0.301760    
    ## genreHorror                    -1.114e+00  1.584e+00  -0.703 0.482391    
    ## genreMusical & Performing Arts  5.342e+00  2.208e+00   2.419 0.015855 *  
    ## genreMystery & Suspense         5.051e-01  1.186e+00   0.426 0.670399    
    ## genreOther                     -7.170e-01  1.840e+00  -0.390 0.696842    
    ## genreScience Fiction & Fantasy -1.142e+00  2.422e+00  -0.472 0.637435    
    ## critics_ratingFresh             1.268e+00  8.261e-01   1.535 0.125399    
    ## critics_ratingRotten            4.456e+00  1.296e+00   3.439 0.000623 ***
    ## thtr_rel_year                  -3.895e-02  2.617e-02  -1.488 0.137187    
    ## dvd_rel_month                   1.114e-01  7.587e-02   1.469 0.142465    
    ## best_pic_nomyes                 2.667e+00  1.711e+00   1.559 0.119597    
    ## title_typeFeature Film          8.528e-01  2.393e+00   0.356 0.721673    
    ## title_typeTV Movie             -3.848e+00  3.762e+00  -1.023 0.306772    
    ## top200_boxyes                  -2.445e+00  1.787e+00  -1.368 0.171775    
    ## runtime                         1.937e-02  1.510e-02   1.283 0.200052    
    ## best_pic_winyes                -3.739e+00  2.986e+00  -1.252 0.211040    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.412 on 620 degrees of freedom
    ## Multiple R-squared:  0.8229, Adjusted R-squared:  0.8163 
    ## F-statistic: 125.2 on 23 and 620 DF,  p-value: < 2.2e-16

### Model Diagnostics

#### Linear Relationship between Explanatory and Response variable

``` r
plot(final_model$residuals ~ movies_3$critics_score) +abline(h = 0)
```

![](imdb_analysis_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

    ## integer(0)

``` r
plot(final_model$residuals ~ movies_3$imdb_num_votes) +abline(h = 0)
```

![](imdb_analysis_files/figure-gfm/unnamed-chunk-35-2.png)<!-- -->

    ## integer(0)

``` r
plot(final_model$residuals ~ movies_3$thtr_rel_year) +abline(h = 0)
```

![](imdb_analysis_files/figure-gfm/unnamed-chunk-35-3.png)<!-- -->

    ## integer(0)

``` r
plot(final_model$residuals ~ movies_3$dvd_rel_month) +abline(h = 0)
```

![](imdb_analysis_files/figure-gfm/unnamed-chunk-35-4.png)<!-- -->

    ## integer(0)

``` r
plot(final_model$residuals ~ movies_3$runtime) +abline(h = 0)
```

![](imdb_analysis_files/figure-gfm/unnamed-chunk-35-5.png)<!-- -->

    ## integer(0)

There only seems to be an issue in the `imdb_num_votes` plot. All other
plots seem to meet the condition.

``` r
hist(final_model$residuals, prob=TRUE)
lines(density(final_model$residuals), col="blue", lwd = 2)
```

![](imdb_analysis_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
qqnorm(final_model$residuals)
qqline(final_model$residuals)
```

![](imdb_analysis_files/figure-gfm/unnamed-chunk-36-2.png)<!-- -->

Majority of the points are on the line except for points at the tails,
this indicates some skewness. Additionally, the histogram appears to be
nearly normal.

``` r
plot(final_model$residuals ~ final_model$fitted) + abline(h=0, col="blue")
```

![](imdb_analysis_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

    ## integer(0)

``` r
plot(abs(final_model$residuals) ~ final_model$fitted) + abline(h=0, col="blue")
```

![](imdb_analysis_files/figure-gfm/unnamed-chunk-37-2.png)<!-- -->

    ## integer(0)

Plot of fitted values shows the residuals are equally variable.

#### Coefficients Interpretation

Some of the coefficients interpretations are given below.

`audience_ratingUpright`: All else held constant, the model predicts
that movies rated ‘Upright’ will have a score 16.91 higher on average
than movies rated ‘Spilled’.

`critics_score`: All else held constant, for every unit increase in
`critics_score`, the model predicts a 0.237 decrease in score on
average.

`genreAnimation`: All else held constant, the model predicts that movies
of genre ‘Animation’ will have a score 1.17 lower on average than movies
of genre ‘Action & Animation’.

`critics_ratingFresh`: All else held constant, the model predicts that
movies given a critics rating of ‘Fresh’ will have a score 1.268 higher
on average than movies given a critics rating of ‘Certified Fresh’.

`title-typeTV Movie`: All else held constant, the model predicts that
movies of type ‘TV Movie’ will have a score 3.85 lower on average than
movies of type ‘Documentary’.

-----

## Part 5: Prediction

I will be predicting the hit film “Captain America: Civil War” with this
model. References: [IMDB](https://www.imdb.com/title/tt3498820/) [Rotten
Tomatoes](https://www.rottentomatoes.com/m/captain_america_civil_war)

I have calculated the score below, using the data from both pages.

``` r
cap_score = ((7.8*10)+89)/2
cap_score
```

    ## [1] 83.5

Creating test data for the model.

``` r
audience_rating <- "Upright"
critics_score <- 91
imdb_num_votes <- 639952
genre <- "Action & Adventure"
critics_rating <- "Certified Fresh"
thtr_rel_year <- 2016
dvd_rel_month <- 9
best_pic_nom <- "no"
title_type <- "Feature Film"
top200_box <- "yes"
runtime <- 147
best_pic_win <- "no"

test <- data.frame(audience_rating, critics_score, imdb_num_votes, genre,
                   critics_rating, thtr_rel_year, dvd_rel_month, best_pic_nom,
                   title_type, top200_box, runtime, best_pic_win)
```

``` r
predict(final_model, test, interval = "prediction", level = 0.95)
```

    ##        fit      lwr      upr
    ## 1 84.35501 71.11237 97.59764

We are 95% confident that, all else held equal, the predicted score for
“Captain America: Civil War” will be between 71.097 and 97.583 on
average.

-----

## Part 6: Conclusion

I have created a model using forward selection and adjusted R Squared as
our selection criteria. Although we managed to get an accurate
prediction, the margin of error is large (97.58 - 71.1 = 26.48). This
could be due to the way the `score` variable was created. Additionally,
it would be best to refit the model without any time related variables.
Additionally, we may get valuable insights on the data if our dataset
had a distinction for actors who won an Oscar prior to being cast in the
movie, and actors who won an Oscar due to their role in the movie.
