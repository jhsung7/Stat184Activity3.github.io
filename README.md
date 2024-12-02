# STAT184-HW
---
title: "Activity10"
subtitle: "Stat 184"
format: html
editor: visual
author: Junghyeon Sung
date: today
self-contained: true
---

```{r}
## dependencies
library(ggplot2)
```

# Q1

```{r}
#| echo: false
myFunction <- function(x){
  y <- (cos(4*x) + tan(0.5*x))^4
  return(y)
}

ggplot() +
  stat_function(
    fun = myFunction,
    xlim = c(0, 1),
    n = 1000
  ) +
  theme_bw() +
  labs(
    x = "x",
    y = "h(x)"
  )
```

# Q2

```{r}
#| echo: true
ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  labs(
    title = "Diamond Price by Carat and Clarity",
    x = "Carat",
    y = "Price (USD)"
  ) + theme_minimal()
```

# Q3 [^1]

[^1]: Source: Rick Chavelas via Kaggle at https://www.kaggle.com/datasets/richave/tortilla-prices-in-mexico

```{r}
tortilla <- read.csv("~/RStudio/Stat184/tortilla_data.csv")
```

### (a) For the year 2007, find the average price of tortillas across all of these 6 states together.

```{r}
data_2007 <- subset(tortilla, year == 2007)
avg2007 <- mean(data_2007$price, na.rm = TRUE)
avg2007
```

### (b) For the year 2024, find the average price of tortillas across all of these 6 states together.

```{r}
data_2024 <- subset(tortilla, year == 2024)
avg2024 <- mean(data_2024$price, na.rm = TRUE)
avg2024
```

### (c) Find the percent change in the average price of tortillas across all six states together from the year 2007 to 2024.

![Percent Change](percentChange.webp){width="80%"}

```{r}
percent_change <- ((avg2024-avg2007) / avg2007) * 100
percent_change
```

### (d) Make a plot that shows the average price of tortillas (across all six states together) each year from the years 2007 to 2024. Add an appropriate title for this figure using Quarto.

```{r}
ggplot(tortilla, aes(x = year, y = price)) +
  stat_summary(fun = mean, geom = "line", color = "blue") +
  stat_summary(fun = mean, geom = "point", color = "red") +
  labs(
    title = "Average Price of Tortillas Across All Six States (2007-2024)",
    x = "Year",
    y = "Average Price ($)"
  ) +
  theme_minimal()

```

### (e) For the year 2016, provide summary statistics for each of the six states' price of tortillas. Make a table of the average price of tortillas in each state in 2016. Add a title for this table using Quarto. Also make side-by-side boxplots for each state in 2016. Add an appropriate title and caption for this figure using Quarto.

```{r}
#| label: summary-table-2016
#| tbl-title: "Summary Statistics for Tortilla Prices by State in 2016"

data_2016 <- subset(tortilla, year == 2016)
summary_by_state <- tapply(data_2016$price, data_2016$state, function(x) {
  c(
    Min = min(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
})

# Convert the list into a data frame
summary_table <- do.call(rbind, summary_by_state)
summary_table <- as.data.frame(summary_table)
summary_table$State <- rownames(summary_table)
rownames(summary_table) <- NULL

# Display the table
knitr::kable(summary_table, caption = "Summary Statistics for Tortilla Prices by State in 2016")

```
```{r}
#| label: Avg-2016
#| tbl-title: "Average Price of Tortillas in Each State (2016)"

avg2016 <- aggregate(price ~ state, data = data_2016, mean, na.rm = TRUE)
names(avg2016) <- c("State", "Average_Price")
knitr::kable(avg2016, caption = "Average Price of Tortillas in Each State (2016)")
```

```{r}
#| label: fig-boxplots-2016
#| fig-title: "Boxplots of Tortilla Prices by State in 2016"
#| fig-cap: "This figure shows the price distribution of tortillas for each state in 2016."

ggplot(data_2016, aes(x = state, y = price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Boxplots of Tortilla Prices by State in 2016",
    x = "State",
    y = "Price ($)"
  ) +
  theme_minimal()

```

### (f) For the state of Nuevo León, make an overlapping plot of the price of tortillas each year from 2007 to 2024 based on store type (i.e., small businesses vs. large corporations). Comment on your findings. Add an appropriate title and caption for this figure using Quarto.

```{r}
#| label: fig-prices-nuevoleon
#| fig-title: "Tortilla Prices in Nuevo Leon (2007-2024) by Store Type"
#| fig-cap: "This figure shows the yearly price trends for tortillas in Nuevo Leon based on store type (small businesses vs. large corporations)."
#| warning: false

NL_data <- subset(tortilla, state == "NuevoLeon" & year >= 2007 & year <= 2024)

ggplot(NL_data, aes(x = year, y = price, color = storeType, group = storeType)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Tortilla Prices in Nuevo Leon (2007-2024) by Store Type",
    x = "Year",
    y = "Price ($)",
    color = "Store Type"
  ) +
  theme_minimal()
```

-   Large corporations tend to have more stable pricing over time compared to small businesses.
-   Small businesses show higher price variability, particularly between 2020 and 2024.

### (g) Include a picture of a tortilla. Add a caption in which you cite the source from which the picture originated.

```{r}
#| label: fig-tortilla-image
#| fig-title: "Tortillaa"
```

![Tortillas(By Times Food, https://recipes.timesofindia.com/recipes/homemade-tortillas/rs53784736.cms)](tortillas.png){width="400" height="400"}

### (h) Somewhere in your work, include a footnote citing the source of this data, with a link to the Kaggle site from which this data set was downloaded (as provided below in a similar footnote).


