---
title: "Semester Project"
author: "Moses Ibe"
date: "December 13, 2024"
format:
  html:
    code-fold: true
    code-line-numbers: true 
    df-print: paged
execute:
  warning: false
  message: false
  echo: false
  keep-md: true
---


::: {.cell}

:::



----

# Background

The BYU Idaho Cellular Department posses a large dataset of all orders for phone purchases placed over the past 7years, comprising over 24,000 observations. Each order includes details such as the Total Purchase Amount, Order Category, and type of device, amongst others.
Understanding whether the mean purchase amount of orders placed for an iphone is significantly different from that of an android.

Our main question then is:

Is there a significant difference between the means of prices for iphone and android devices purchased at the BYUI Cellular Department? 



::: {.cell}

```{.r .cell-code}
#| label: load-packages
#| echo: false

#load your libraries in this chunk
library(tidyverse)
library(knitr)
library(ggplot2)
library(pander)
library(ggrepel)
library(janitor)
library(lubridate)
library(riem)
library(mosaic)
library(DT)
library(readr)
library(car)
library(dtplyr)
library(dplyr)
library(ResourceSelection)
```
:::



----

## The Data

The data set contains the following key columns among others:

Type: This could be one of 3 possible categories that an order was placed under.

Phone: This column tells us whether a device is an IPhone or an Android.

Order Date: The date the order was placed.

Total: Total price amount for a particular order. Multiple devices can be purchased through one order so this does not necessarily mean the price of each device.


Below is a sample of the Data:



::: {.cell}

```{.r .cell-code}
orders = read_csv("C:\\Users\\mcurn\\Documents\\DS350_FA24_Ibe_Mose\\orders.csv")
#orders
#glimpse(orders)

kable(head(orders, caption = "Orders Table"))
```

::: {.cell-output-display}


| Order.Number|Order.Date      |  Total|Status  |Contact.Name      |Contact.Phone  |Contact.Email          |Ship.To.Kiosk |Shipping.Name     |Address.Line.1       |Address.Line.2 |City        |State |Zip        |Account.Holder |  I.Number|Email               |Type    |Provider |Canceled.Date |Order.Phone.Number | Temp.Phone.Number| Zip.Code| Activation.Number|Phone                                     |Sku      |Plan     |Price   |Fee   |Shipping |Insurance |Total.1 |Actual.Tax.Amount |Order.Number.1 |Tracking.Number |Device.Brand.and.Model |MEID.DEC.. |Requires.Sim.Card |Carrier.Name |Name.on.Bill |Account.. |Pin |Billing.Name |Billing.Line.1 |Billing.Line.2 |Billing.City |Billing.State |Billing.Zip |
|------------:|:---------------|------:|:-------|:-----------------|:--------------|:----------------------|:-------------|:-----------------|:--------------------|:--------------|:-----------|:-----|:----------|:--------------|---------:|:-------------------|:-------|:--------|:-------------|:------------------|-----------------:|--------:|-----------------:|:-----------------------------------------|:--------|:--------|:-------|:-----|:--------|:---------|:-------|:-----------------|:--------------|:---------------|:----------------------|:----------|:-----------------|:------------|:------------|:---------|:---|:------------|:--------------|:--------------|:------------|:-------------|:-----------|
|        22489|6/24/2024 18:59 |  79.99|New     |Michael Groesbeck |(208) 419-2173 |groesbeckm@byui.edu    |FALSE         |Michael Groesbeck |1276 N 4000 W        |NA             |REXBURG     |ID    |83440-3103 |Mike Groesbeck | 276110627|GROESBECKM@byui.edu |Upgrade |T-Mobile |NA            |2082061910         |                NA|       NA|                NA|Apple iPhone 13 Midnight 128 GB           |T-Mobile |Two Year |$79.99  |$0.00 |$0.00    |NA        |$79.99  |$0.00             |NA             |NA              |NA                     |NA         |NA                |NA           |NA           |NA        |NA  |NA           |NA             |NA             |NA           |NA            |NA          |
|        22488|6/24/2024 15:19 | 279.99|New     |Matt Dredge       |(208) 380-2698 |dredgeb@byui.edu       |TRUE          |NA                |NA                   |NA             |NA          |NA    |NA         |B Matt Dredge  | 561409642|dredgeb@byui.edu    |Upgrade |T-Mobile |NA            |2084032449         |                NA|       NA|                NA|Apple iPhone 14 Black 256 GB              |T        |Two Year |$279.99 |$0.00 |$0.00    |NA        |$279.99 |$0.00             |NA             |NA              |NA                     |NA         |NA                |NA           |NA           |NA        |NA  |NA           |NA             |NA             |NA           |NA            |NA          |
|        22487|6/24/2024 15:13 |  79.99|New     |krista hasler     |(208) 488-5577 |kristahasler@gmail.com |FALSE         |Krista Hasler     |7323 W RING PERCH DR |NA             |BOISE       |ID    |83709-5695 |Charles West   | 500251907|westc@byui.edu      |Upgrade |T-Mobile |NA            |2085731493         |                NA|       NA|                NA|Apple iPhone 13 Midnight 128 GB           |T-Mobile |Two Year |$79.99  |$0.00 |$0.00    |NA        |$79.99  |$0.00             |NA             |NA              |NA                     |NA         |NA                |NA           |NA           |NA        |NA  |NA           |NA             |NA             |NA           |NA            |NA          |
|        22486|6/24/2024 15:06 | 449.99|New     |Brian Kinghorn    |(208) 201-5748 |kinghornbr@byui.edu    |FALSE         |Brian Kinghorn    |54 E PETERSON LN     |NA             |SUGAR CITY  |ID    |83448-5045 |Brian Kinghorn | 208938152|KINGHORNBR@byui.edu |Upgrade |T-Mobile |NA            |2082015748         |                NA|       NA|                NA|Apple iPhone 15 Pro  Blue Titanium 128 GB |T        |Two Year |$449.99 |$0.00 |$0.00    |NA        |$449.99 |$0.00             |NA             |NA              |NA                     |NA         |NA                |NA           |NA           |NA        |NA  |NA           |NA             |NA             |NA           |NA            |NA          |
|        22485|6/24/2024 15:01 |   0.00|Pending |Brian Kinghorn    |(208) 201-5748 |kinghornbr@byui.edu    |FALSE         |NA                |NA                   |NA             |NA          |NA    |NA         |Brian Kinghorn | 208938152|KINGHORNBR@byui.edu |Byod    |T-Mobile |NA            |208201XXXX         |                NA|       NA|                NA|NA                                        |NA       |NA       |$0.00   |$0.00 |$0.00    |NA        |$0.00   |$0.00             |C0021048       |NA              |Iphone 13              |3.52E+14   |FALSE             |NA           |NA           |NA        |NA  |NA           |NA             |NA             |NA           |NA            |NA          |
|        22484|6/24/2024 14:54 |  49.99|New     |Dillon Smith      |(303) 591-1926 |dillfry1@gmail.com     |FALSE         |Dillon Smith      |2717 CLARENCE PL     |NA             |IDAHO FALLS |ID    |83402-4977 |Deborah Reed   | 982422712|ree21013@byui.edu   |Upgrade |T-Mobile |NA            |3035911926         |                NA|       NA|                NA|Google Pixel 8  Black 128 GB              |T        |Two Year |$49.99  |$0.00 |$0.00    |NA        |$49.99  |$0.00             |NA             |NA              |NA                     |NA         |NA                |NA           |NA           |NA        |NA  |NA           |NA             |NA             |NA           |NA            |NA          |


:::
:::



----

## Analysis

The first thing I did to answer this questions was to wrangle the data. I grouped all apple and android devices into two groups called IPhones and Androids. I then went ahead to find the average purchase price for each device group by year and the overall average subsequently.
Below is a graph that visualizes this:



::: {.cell}

```{.r .cell-code}
orders1 = orders %>% 
  mutate(phone_group = case_when(
    grepl("^Apple", Phone) ~ "iPhone",
    TRUE ~ "Android"
  )) %>% 
    mutate(year = year(mdy_hm(`Order.Date`))) %>% 
  mutate(year = as.numeric(year)) %>% 
  select(year, phone_group, Total)
#kable(head(orders1))

meanorders = orders1 %>% 
  group_by(phone_group) %>% 
  summarise(mean = round(mean(Total), ))
#meanorders

orders2 = orders1 %>% 
  group_by(year, phone_group) %>% 
  summarise(meanh = round(mean(Total)), )
#kable(head(orders2))
```
:::

::: {.cell}

```{.r .cell-code}
#Plot the time Series graph

ggplot(orders2, aes(x = year, y = meanh, color = phone_group, group = phone_group)) +
  geom_line(size = 0.8, alpha = 1) + 
  geom_text(aes(label = paste("$", meanh),  vjust = -1)) +
  geom_hline(yintercept = 274, linetype = "dashed", color = "black", size = 1) +
  geom_hline(yintercept = 52, linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = 2021, y = 280, label = "Average Price = $275", 
           color = "black", fontface = "plain", size = 4, hjust = 0) +
  annotate("text", x = 2020, y = 48, label = "Average Price = $53", 
           color = "black", fontface = "plain", size = 4, hjust = 0) +
  
  scale_y_continuous(limits = c(1, NA)) +
  scale_x_continuous(breaks = 2018:2024, limits = c(2018, 2024)) +
  
  labs(title = "Mobile device prices",
       subtitle = "The average prices for both android and iphones purchased at the BYUI university store since 2018.",
       caption = "Data source: BYUI Univeristy Store Cellular.", 
       x = NULL,
       y = "Average Price (dollars)") +
  
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 24),
    plot.subtitle = element_text(size = 16),
    legend.title = element_blank(),  # Remove legend title
    legend.position = "none") +
  theme(axis.line.x.bottom = element_line(color = 'black')) +
  scale_color_manual(values = c("iPhone" = "firebrick", 
                                "Android" = "blue4"))
```

::: {.cell-output-display}
![](SemesterProject_files/figure-html/unnamed-chunk-5-1.png){width=1152}
:::
:::


We can clearly see from the visual above that the overall average purchase amount for IPhone purchases are over 5 times that of Android purchases. The year 2019 where these prices are closest, we still see IPhone prices at $216 and Android"s at $77. This is not the case with year 2022 where the average for IPhones at $380 are almost 10 times that of androids at $35. 

From a visual point of view, I think it is safe to infer that there is indeed a significant difference between the average purchase amount for IPhone and Androids, however, I intend to take this analysis further in order to support these observations.


The next step taken to support our findings; to help answer our question is conducting an independent sample T-test.

An independent samples t test is used when a value is hypothesized for the difference between two (possibly) different population means.

The test is only appropriate when both of the following are satisfied:

a) Both samples are representative of the population. (Simple random samples are the best way to do this.)

b) The sampling distribution of the difference of the sample means can be assumed to be normal. (This is a safe assumption when the sample size of each group is 30 or greater or when the population data from each group can be assumed to be normal with a Q-Q Plot.)

Now the data set we have here is very large - 24,523 to be exact, therefore the central limit theorem is applicable and justifies that the data is normal. 


----

## Hypothesis & Test

The null and alternative hypothesis of the independent sample t.test was set up as follows:

$$
  H_0: \mu_\text{iphone Devices}(\mu_\text{1}) - \mu_\text{Android Devices}(\mu_\text{2}) = 0
$$

$$ 
  H_a: \mu_\text{iphone Devices}(\mu_\text{1}) - \mu_\text{Android Devices}(\mu_\text{2}) \neq 0
$$

I used a confidence interval of 0.95 which means the value of significance was 0.05.
A p-value greater than 0.05 means there is statistically no difference between the means of the two groups but a p-value less than 0.05 means there is a significant statistical difference between the means of both groups.

Below is the results of the independent sample test:


::: {.cell}

```{.r .cell-code}
t.test(Total ~ phone_group, data = orders1, mu = 0, alternative = "two.sided", conf.level = 0.95) %>% 
  pander()
```

::: {.cell-output-display}

-----------------------------------------------------------
 Test statistic    df     P value   Alternative hypothesis 
---------------- ------- --------- ------------------------
     -67.51       19898   0 * * *         two.sided        
-----------------------------------------------------------

Table: Welch Two Sample t-test: `Total` by `phone_group` (continued below)

 
----------------------------------------------
 mean in group Android   mean in group iPhone 
----------------------- ----------------------
         53.39                  274.6         
----------------------------------------------


:::
:::


From the results above we can see that the p value is much smaller than 0.05 signifying that there is a significant difference between the two means.

Now considering that a lot of the devices purchased were free... I am going to perform the same test on a filtered version of the data. This filtered dataset has over 10,000 observations and includes every phone purchase that were over $10 to see if we get a similar test result. Below are the results:



::: {.cell}

```{.r .cell-code}
orders11 = orders %>% 
  filter(Total > 10) %>% 
  mutate(phone_group = case_when(
    grepl("^Apple", Phone) ~ "iPhone",
    TRUE ~ "Android"
  )) %>% 
    mutate(year = year(mdy_hm(`Order.Date`))) %>% 
  mutate(year = as.numeric(year)) %>% 
  select(year, phone_group, Total)
#kable(head(orders11))

meanorders2 = orders11 %>% 
  group_by(phone_group) %>% 
  summarise(mean = round(mean(Total), ))
#meanorders2

meanorders3 = orders11 %>% 
  group_by(year, phone_group) %>% 
  summarise(mean = round(mean(Total), ))
#meanorders3

t.test(Total ~ phone_group, data = orders11, mu = 0, alternative = "two.sided", conf.level = 0.95) %>% 
  pander()
```

::: {.cell-output-display}

-------------------------------------------------------------------
 Test statistic    df        P value        Alternative hypothesis 
---------------- ------ ------------------ ------------------------
     -27.64       6038   1.757e-158 * * *         two.sided        
-------------------------------------------------------------------

Table: Welch Two Sample t-test: `Total` by `phone_group` (continued below)

 
----------------------------------------------
 mean in group Android   mean in group iPhone 
----------------------- ----------------------
         203.6                  369.2         
----------------------------------------------


:::
:::


The result gotten after the filter is still significant considering the very small p-value.

I created a boxplot to visualize the distribution of the filtered dataset and further broke them down into facets to show the same distribution over the years 2018 - 2024.



::: {.cell}

```{.r .cell-code}
#Plot the graph

ggplot(orders11, aes(y = Total, x = phone_group, color = phone_group)) +
  geom_boxplot() +
  geom_text(data = meanorders2, aes(x = phone_group, y = mean, label = paste("$", mean)), fill = 'black', size = 5) +
  scale_y_continuous(limits = c(1, NA)) +
  labs(title = "Mobile device purchases",
       subtitle = "The distribution of purchases for both android and iphone devices at the BYUI university store since 2018.",
       caption = "Data source: BYUI Univeristy Store Cellular.", 
       x = NULL,
       y = "Purchase Amount per order (dollars)") +
  theme_minimal(base_size = 12) +  # Match font size
  theme(
    plot.title = element_text(face = "bold", size = 24),
    plot.subtitle = element_text(size = 16),
    legend.title = element_blank(),  # Remove legend title
    legend.position = "none",
    panel.spacing = unit(1, "lines"), # Increase spacing between panels
    panel.background = element_rect(color = "black", fill = NA),
    strip.background = element_rect(color = "black", fill = "lightgray"),
    panel.grid.major.x = element_blank(),  # Remove minor grid lines
    panel.grid.minor = element_blank()) + 
  scale_color_manual(values = c("iPhone" = "firebrick", 
                                "Android" = "blue4"))
```

::: {.cell-output-display}
![](SemesterProject_files/figure-html/unnamed-chunk-8-1.png){width=1152}
:::
:::

::: {.cell}

```{.r .cell-code}
#Plot the graph

ggplot(orders11, aes(y = Total, x = phone_group, color = phone_group)) +
  geom_boxplot() +
  geom_text(data = meanorders3, aes(x = phone_group, y = mean, label = paste("$", mean)), fill = 'black', size = 3.5) +
  scale_y_continuous(limits = c(1, NA)) +
  labs(title = "Mobile device purchases",
       subtitle = "The distribution of purchases for both android and iphone devices at the BYUI university store since 2018.",
       caption = "Data source: BYUI Univeristy Store Cellular.", 
       x = NULL,
       y = "Total Purchase Amount") +
  facet_wrap("year", nrow = 2)  +
  theme_minimal(base_size = 12) +  # Match font size
  theme(
    plot.title = element_text(face = "bold", size = 24),
    plot.subtitle = element_text(size = 16),
    legend.title = element_blank(),  # Remove legend title
    legend.position = "none",
    panel.spacing = unit(1, "lines"), # Increase spacing between panels
    panel.background = element_rect(color = "black", fill = NA),
    strip.background = element_rect(color = "black", fill = "lightgray"),
    panel.grid.major.x = element_blank(),  # Remove minor grid lines
    panel.grid.minor = element_blank()) + 
  scale_color_manual(values = c("iPhone" = "firebrick", 
                                "Android" = "blue4"))
```

::: {.cell-output-display}
![](SemesterProject_files/figure-html/unnamed-chunk-9-1.png){width=1152}
:::
:::


Once again the results are pretty consistent with IPhone taking the lead across all years except for 2020 where the average price for android purchases were slightly higher than that of IPhones. The difference in means was closer this time, however the results are the same. There is indeed a significant difference in mean prices between IPhone purchases and android purchases at the cellular department, with Iphone purchases being higher than android's.


----

## Conclusion

The analysis conducted on the BYU Idaho Cellular Department's orders data aimed to determine whether there is a significant difference between the the means of prices for IPhone and android devices purchased at the BYUI Cellular Department. The data consisted of over 24,000 observations from the past 7 years. We plotted a line chart to visualize the average trend of these prices over the years and discovered the following:

1) The overall average purchase amount for IPhone purchases are over 5 times that of android purchases. 
2) The year 2019 have the closest difference in prices prices are closest while year 2022 have the furthest difference where the average for iphones are about 10 times that of androids.

The data was further filtered to exclude all purchases that were priced $10 and below and the results stayed fairly consistent with our initial observation. An independent T-test was then performed to possibly support our findings.

The test results:
$$
\text{A test statistic or t value = -27.64, p value = 1.757e-158 * * *}, \mu_\text{iphone} = 369.2, \mu_\text{Android} =  203.6
$$
indicate a highly significant difference in the means of prices between iphone and android phone purchases.
The extremely small p-value (1.757e-158) confirms that this difference is not due to random chance.

Finally, I used box plots to visually support the statistical test, showing a lower mean purchase price for Android device group and a higher mean purchase price with slightly greater variability for IPhone device group.

----


