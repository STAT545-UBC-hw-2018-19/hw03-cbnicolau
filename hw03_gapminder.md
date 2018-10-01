hw003
================
Coni
September 29, 2018

Homework 03: Using dplyr/ggplot2 to manipulate and explore data
===============================================================

Load required libraries

``` r
suppressPackageStartupMessages(library(dplyr)) # to supress messages after library calls
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gapminder))
```

    ## Warning: package 'gapminder' was built under R version 3.4.4

``` r
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 3.4.4

Tasks (pick 3) - for each make a table and a figure, also make observations on what the figures show and the process

Max and Min GDP per capita for all continents
=============================================

The simplest way of finding the min or max is by grouping by continent and then summarizing. Disadvantage: it doesn't keep the name of the country or the year (which are kind of important)

``` r
min_gdp <- gapminder %>%
  group_by(continent) %>%
  summarize(minGDP = min(gdpPercap) ) %>% 
  arrange(continent) 

knitr::kable(min_gdp)
```

| continent |      minGDP|
|:----------|-----------:|
| Africa    |    241.1659|
| Americas  |   1201.6372|
| Asia      |    331.0000|
| Europe    |    973.5332|
| Oceania   |  10039.5956|

The same can be done for the max

``` r
max_gdp <- gapminder %>%
  group_by(continent) %>%
  summarize(maxGDP = max(gdpPercap) ) %>% 
  arrange(continent) 

knitr::kable(max_gdp)
```

| continent |     maxGDP|
|:----------|----------:|
| Africa    |   21951.21|
| Americas  |   42951.65|
| Asia      |  113523.13|
| Europe    |   49357.19|
| Oceania   |   34435.37|

And we can put both in the same table

``` r
min_max <- gapminder%>%
  group_by(continent) %>%
  summarize(minGDP = min(gdpPercap), maxGDP = max(gdpPercap))

knitr::kable(min_max)
```

| continent |      minGDP|     maxGDP|
|:----------|-----------:|----------:|
| Africa    |    241.1659|   21951.21|
| Americas  |   1201.6372|   42951.65|
| Asia      |    331.0000|  113523.13|
| Europe    |    973.5332|   49357.19|
| Oceania   |  10039.5956|   34435.37|

We can visualize this with a plot of maxGDP vs minGDP

``` r
min_max %>% ggplot(aes(x=minGDP, y=maxGDP)) +
  geom_point(aes(color = continent))
```

![](hw03_gapminder_files/figure-markdown_github/min_and_max_gdp%20plot-1.png)

An alternative, more elaborated way of doing it (repurposing code found [here](http://stat545.com/block010_dplyr-end-single-table.html#use-arrange-to-row-order-data-in-a-principled-way)) is the following. One advantage of this way is that we get to know the country and year associated to the lowest and highest GDP per continent.

``` r
my_df <- gapminder %>%
  select(continent, country, gdpPercap, year) %>% #select relevant variables
  group_by(continent) %>%
  filter(min_rank(desc(gdpPercap)) < 2 | min_rank(gdpPercap) < 2) %>% 
  # rank by gdp in desc order and ascending order and then apply filter to get only these numbers
  arrange(continent) 

knitr::kable(my_df)
```

| continent | country                |    gdpPercap|  year|
|:----------|:-----------------------|------------:|-----:|
| Africa    | Congo, Dem. Rep.       |     241.1659|  2002|
| Africa    | Libya                  |   21951.2118|  1977|
| Americas  | Haiti                  |    1201.6372|  2007|
| Americas  | United States          |   42951.6531|  2007|
| Asia      | Kuwait                 |  113523.1329|  1957|
| Asia      | Myanmar                |     331.0000|  1952|
| Europe    | Bosnia and Herzegovina |     973.5332|  1952|
| Europe    | Norway                 |   49357.1902|  2007|
| Oceania   | Australia              |   10039.5956|  1952|
| Oceania   | Australia              |   34435.3674|  2007|

spread of GDP per capita within continents
------------------------------------------

Let's have a look at the spread in gdp per continent with a boxplot and jitter plot first

``` r
ggplot(gapminder, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 0.2) +
  scale_y_log10()
```

![](hw03_gapminder_files/figure-markdown_github/gdpPercap%20per%20continent-1.png)

We see that Asia is the continent with a higher spread (more unequal between countries, and Oceania seems to be the one with the lower spread (less unequeal)

There are a number of different measures of the spread: 1. Standard deviation - `sd()`: square of the variance

1.  Interquartile range - `IWR()`: is the difference of its upper and lower quartiles. It is a measure of how far apart the middle portion of data spreads in value (more info [here](http://www.r-tutor.com/elementary-statistics/numerical-measures/interquartile-range))

We can use `summarise()`, `sd()` and `IQR()` to get these values:

``` r
gapminder %>%
  group_by(continent) %>%
  summarise(sd = sd(gdpPercap), IQR = IQR(gdpPercap)) %>%
  kable()
```

| continent |         sd|        IQR|
|:----------|----------:|----------:|
| Africa    |   2827.930|   1616.170|
| Americas  |   6396.764|   4402.431|
| Asia      |  14045.373|   7492.262|
| Europe    |   9355.213|  13248.301|
| Oceania   |   6358.983|   8072.258|

This values agree with the observation from the graph

We can visualize how the standard deviation has changed for each continent in time with the following plot:

``` r
gapminder %>%
  group_by(continent) %>%
  mutate(sd = sd(gdpPercap), IQR = IQR(gdpPercap)) %>%
  ggplot(aes(year, sd)) + geom_point(aes(color= continent))
```

![](hw03_gapminder_files/figure-markdown_github/gdpPercap%20sd%20trends%20per%20continent-1.png)

Trimmed mean of life expentacy for different years
==================================================

Acording to [wikipedia](https://en.wikipedia.org/wiki/Truncated_mean) "a truncated mean or trimmed mean is a statistical measure of central tendency, much like the mean and median. It involves the calculation of the mean after discarding given parts of a probability distribution or sample at the high and low end, and typically discarding an equal amount of both".

-   Compute a trimmed mean of life expectancy for different years.

``` r
gapminder %>% 
  group_by(continent, year) %>%
  filter(year == 1952 | year == 2007) %>% #filter by year
  summarize(trim_mean = mean(lifeExp, trim = 0.1)) %>% #compute the mean trimming the top and bottom 10%
  kable()
```

| continent |  year|  trim\_mean|
|:----------|-----:|-----------:|
| Africa    |  1952|    38.93298|
| Africa    |  2007|    54.07752|
| Americas  |  1952|    53.18157|
| Americas  |  2007|    74.01419|
| Asia      |  1952|    45.98107|
| Asia      |  2007|    71.31067|
| Europe    |  1952|    65.09708|
| Europe    |  2007|    77.82504|
| Oceania   |  1952|    69.25500|
| Oceania   |  2007|    80.71950|

This table allows to compare the trimmed mean life expectancy between continents and between the first and last year of measurement in the database.

Now we can make a plot to visualize the evolution of the trimmed mean (10%) of life expectancy per continent. In this case I added a third variable to see how life exp and population correlate

``` r
gapminder %>% 
  group_by(continent, year) %>%
  mutate(trim_mean_lifeExp = mean(lifeExp, trim = 0.1), mean_pop = mean(pop)) %>% 
  ggplot(aes(x = year, y = trim_mean_lifeExp)) +
  geom_point(aes(color = continent, size = mean_pop))
```

![](hw03_gapminder_files/figure-markdown_github/trim_mean_lifeExp%20trends%20per%20continent-1.png)

-   How is life expectancy changing over time on different continents?
