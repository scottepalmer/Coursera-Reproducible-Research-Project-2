# Severe Weather Events' Impact on Public Health and the Economy
Scott Palmer 
May 23, 2017


---

### Synopsis

This report is based on data from the U.S. National Oceanic and Atmospheric Administration's(NOAA) database. 
This database tracks characteristics of major storms and weather events in the United States including when 
and where they occur, as well as estimates of any fatalities, injuries, and property damage. The focus in the 
report will be on the damages caused by weather events in terms of both population health and economic impact.
More precisely the report will address the following questions:

* 1.Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful 
with respect to population health?

* 2.Across the United States, which types of events have the greatest economic consequences?


---

### Data Processing

First of all, we load the required packages, then we do the data processing. We have to:

* Download,
* Unzip,
* Read the data 


Note: the following step can take quite long to be completed, please be patient.


```r
suppressPackageStartupMessages(library(dplyr))   
library(ggplot2)


if ("storm_data.bz2" %in% dir() == FALSE) {
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",destfile = 
                      "storm_data.bz2")
}
storm_data <- read.csv("storm_data.bz2")
```


#### Relevant Variables

Given the scope of our project, we will only require the following variables to determine which weather event has greatest impact to public health and the economy. Thus, we create a subset of the original data set named _Sub.data_


```r
Sub.data  <- storm_data  %>%  select(EVTYPE, FATALITIES, INJURIES, contains("DMG"))
names(Sub.data)
```

```
## [1] "EVTYPE"     "FATALITIES" "INJURIES"   "PROPDMG"    "PROPDMGEXP"
## [6] "CROPDMG"    "CROPDMGEXP"
```


So, the required variables are:

* **EVTYPE**, which describes the type of event. 
* **FATALITIES** and **INJURIES**, to determine health impact
* **PROPDMG**, **PROPDMGEXP**, **CROPDMG**, **CROPDMGEXP**   to determine economic impact

---


### Results

>Q1. Across the United States, which types of events (as indicated in the variable) are most harmful with respect to population health?**



```r
Sub.data  %>%  group_by(EVTYPE) %>%
                 summarize(Fatalities = sum(FATALITIES), Injuries = sum(INJURIES)) %>%
                 top_n(10, wt = Injuries)  %>% 
                 arrange(desc(Injuries)) %>% 
                   ggplot(aes(x=reorder(EVTYPE, Injuries),y=Injuries)) +
                     geom_bar(stat="identity",fill= "darkorange") +
                     ggtitle("Most harmful weather\n events by Injuries") + 
                     labs(x="Event", y=expression("INJURIES")) +
                     coord_flip()
```

![](Assignment.TWO_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
Sub.data  %>%  group_by(EVTYPE) %>%
                   summarize(Fatalities = sum(FATALITIES), Injuries = sum(INJURIES)) %>%
                   top_n(10, wt = Fatalities)  %>% 
                   arrange(desc(Fatalities)) %>% 
                     ggplot(aes(x=reorder(EVTYPE, Fatalities), y=Fatalities)) +  
                       geom_bar(stat="identity",fill= "red") +
                       ggtitle("Most harmful weather\n  events by Fatalities") + 
                       labs(x="Event", y=expression("FATALITIES")) +
                       coord_flip()
```

![](Assignment.TWO_files/figure-html/unnamed-chunk-3-2.png)<!-- -->


---



>Q2. Across the United States, which types of events have the greatest economic consequences?**

As we can see in the following tables, the factor variables PROPDMGEXP and CROPDMGEXP have levels expressed in a very confusing fashion:


```r
Sub.data  %>% select(PROPDMGEXP)   %>% table()
```

```
## .
##             -      ?      +      0      1      2      3      4      5 
## 465934      1      8      5    216     25     13      4      4     28 
##      6      7      8      B      h      H      K      m      M 
##      4      5      1     40      1      6 424665      7  11330
```

```r
Sub.data  %>% select(CROPDMGEXP)   %>% table()
```

```
## .
##             ?      0      2      B      k      K      m      M 
## 618413      7     19      1      9     21 281832      1   1994
```

We need to convert those levels to numbers/multipliers in order to have damage figures expressed in a consistent unit and make our analysis possible.



```r
#transform PROPDMGEXP
Sub.data.Mult  <- mutate(Sub.data, PropMult = 
                           ifelse(PROPDMGEXP %in% c(0:8), 10,
                           ifelse(PROPDMGEXP == "h" | PROPDMGEXP == "H", 100,
                           ifelse(PROPDMGEXP == "k" | PROPDMGEXP == "K", 1000,
                           ifelse(PROPDMGEXP == "m" | PROPDMGEXP == "M", 1000000,
                           ifelse(PROPDMGEXP == "b" | PROPDMGEXP == "B", 1000000000, 0 ))))))

#transform CROPDMGEXP
Sub.data.Mult <- mutate(Sub.data.Mult, CropMult = 
                          ifelse(CROPDMGEXP %in% c(0:8), 10,
                          ifelse(CROPDMGEXP == "h" | CROPDMGEXP == "H", 100,
                          ifelse(CROPDMGEXP == "k" | CROPDMGEXP == "K", 1000,
                          ifelse(CROPDMGEXP == "m" | CROPDMGEXP == "M", 1000000,
                          ifelse(CROPDMGEXP == "b" | CROPDMGEXP == "B", 1000000000, 0 ))))))



Sub.data.Mult %>%
             mutate(TotPropDmg = PROPDMG * PropMult, TotCropDmg = CROPDMG * CropMult)  %>%  
             mutate(TotDmg = TotPropDmg + TotCropDmg)  %>%
             group_by(EVTYPE) %>%
             summarise(TotDamages = sum(TotDmg/10^6))  %>%
             top_n(10) %>%
             arrange(desc(TotDamages))  %>%
               ggplot(aes(x=reorder(EVTYPE, -TotDamages),y=TotDamages)) +
                     geom_bar(stat="identity",fill= "darkgreen") +
                     ggtitle("Most damaging weather events") + 
                     labs(x="Event", y=expression("Total Damages (Milions USD)")) +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```
## Selecting by TotDamages
```

![](Assignment.TWO_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### Conclusion
  
  Floods, Hurricanes and Tornadoes cause the most overall economic damages. Drought causes the most damages to crop. Tornadoes have the highest impact in terms of population health
