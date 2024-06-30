---
title: "Crime Analysis in Baghdad"
author: "Dr. Jeffrey Strickland"
date: "8/28/2018"
output:
  word_document: default
  html_document:
    df_print: paged
always_allow_html: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE, fig.width=6, fig.height=4, dpi=330, error=FALSE))
```

# Using R for Crime Analysis

"Sherlock Holmes was a man, however, who, when he had an unsolved problem upon his mind, would go for days, and even for a week, without rest, turning it over, rearranging his facts, looking at it from every point of view until he had either fathomed it or convinced himself that his data were insufficient."

-Dr. John Watson, The Man with the Twisted Lip

## Introduction

Sherlock's data may have been insufficient, but at present we cannot make the same complaint. If we have any burden whatsoever, then it is possessing too much data. In this chapter, will have two objectives: (1) visualizing spatial and temporal trends of terrorist improvised explosive devices (IEDs) activity based on data, (2) analyzing factors that may affect what may appear to be random behavior, and (3) turning the "randomness" into predictable patterns, based upon the data. Taken together, we are really using R programming to cluster and describe data that the military combatant commands and police departments have already collected and turned over to the skillful hands of the analyst.

## Install R libraries

```{r}
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(DT)) install.packages("DT")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(leaflet)) install.packages("leaflet")
if(!require(data.table)) install.packages("data.table")
if(!require(ggmap)) install.packages("ggmap")
if(!require(scales)) install.packages("scales")
sessionInfo()
```

## the Crime Data

The crime data was simulated using distributions derived fro an analysis of major crimes in Atlanta, Georgia USA, San Fancisco, California USA, and Boston, Massatuchetts USA. For instance Larceny/theft was approximately uniformly distributed Uniform~[120000, 160000] with mean 140000 and standard deviation of approximately 11547. 

The distributions were then applied to Baghdad India using the inverse transform method. To simulate larceny/theft in Baghdad, we used the reverse inverse method, which for the Uniform distribution is $$X = a + (b - a)U,$$ where U is the Uniform[0,1] random number. In Excel, this is the `RAND()` function, so the inverse transform would be $$X = a + (b - a) *RAND().$$ The crimes are then geographically distributed in zones in a similar fashion. 

The data was simulated due to the constraint that crime data was only available at the state-level in India. The data include the following factors:

- `IncidntNum`	(N)	Incident number
- `Category`	(C)	Crime category, i.e., larceny/theft
- `Descript` 	(C)
- `DayOfWeek` 	(C)
- `Date` 	(D)	Date: DD/MM/YYYY
- `Time` 	(T)	Time: 24-hour system
- `PdDistrict`	(C)	Police district where incident occured
- `Resolution`	(C)	Resolution of the crime
- `X` 	(N)	Longitude
- `Y` 	(N)	Latitude
- `Location` 	(C) Lat/long
- `PdId` 	(N)	Police Department ID

(`N` = Numeric, `T` = Time, `D` = Date, `C` = Class)

## Read the data
First, we load the data using `readr` and `read_csv()`.


path <- "https://github.com/stricje1/R_Handbook_Project/blob/main/Baghdad_crimes.zip"

```{r echo = TRUE}
library(readr)
path <- "c:\\Users\\jeff\\Documents\\Data\\baghdad_ieds.csv"
df <- read_csv(path)
```

## Display Data

Now, we display the data using `DT` and `data.table()`. (NOTE: The after the publication of my book Predictive Crime Analysis using R, ISBN 978-0-359-43159-5, the function datatable() has been replaced by data.table().) The table below shows a partial view of the Baghdad crime data

```{r echo = TRUE}
library(DT)
df_sub <- df[1:100,]  # display the first 100 rows
df_sub$Time <- as.character(df_sub$Time) 
#html viewable only
datatable(df_sub, options = list(pageLength = 5,scrollX='400px'))
```

## Preprocess the Data

The All-Caps text is difficult to read. Let's force the text in the appropriate columns into proper case.


```{r echo = TRUE}
proper_case <- function(x) {
  return (gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2" , x, perl=TRUE))
}

library(dplyr)
df <- df %>% mutate(Category = proper_case(Category),
                    Descript = proper_case(Descript),
                    PdDistrict = proper_case(PdDistrict),
                    Resolution = proper_case(Resolution),
                    Time = as.character(Time))
df_sub <- df[1:100,]  # display the first 100 rows
#datatable(df_sub, options = list(pageLength = 5,scrollX='400px'))
df %>% head(10)
```

## Explore the Data

### Crime across space

In this section, we use the `leaflet` function. It creates a Leaflet map widget using `htmlwidgets.` The widget can be rendered on HTML pages generated from R Markdown. In addition to matrices and data frames, `leaflet` supports spatial objects from the `sp` package and spatial data frames from the sf package. We create a Leaflet map with these basic steps: First, create a map widget by calling `leaflet()`. Next, we add layers (i.e., features) to the map by using layer functions (e.g. `addTiles`, `addMarkers`, `addPolygons`) to modify the map widget. Then you keep adding layers or stop when satisfied with the result. We will add a tile layer from a known map provider, using the leaflet function `addProviderTiles`. A list of providers can be found at http://leaflet-extras.github.io/leaflet-providers/preview/. We will also add graphics elements and layers to the map widget with `addCondtroll` (`addTiles`). We use markers to call out points on the map. Marker locations are expressed in latitude/longitude coordinates, and can either appear as icons or as circles. When there are many markers on a map as in our case with crimes, we can cluster them together. 
<span style="color: blue;"> `library(leaflet)` </span>

The following code defines the popups used on the Leaflet map we will create. The popups will appear on the interactive Leaflet map. When you click on a popup, it will contain the information defined below:

```{r echo = TRUE}
data <- df[1:10000,] # display the first 10,000 rows
data$popup <- paste("<b>Incident #: </b>", data$IncidntNum, 
                    "<br>", "<b>Category: </b>", data$Category,
                    "<br>", "<b>Description: </b>", data$Descript,
                    "<br>", "<b>Day of week: </b>", data$DayOfWeek,
                    "<br>", "<b>Date: </b>", data$Date,
                    "<br>", "<b>Time: </b>", data$Time,
                    "<br>", "<b>PD district: </b>", data$PdDistrict,
                    "<br>", "<b>Resolution: </b>", data$Resolution,
                    "<br>", "<b>Address: </b>", data$Address,
                    "<br>", "<b>Longitude: </b>", data$X,
                    "<br>", "<b>Latitude: </b>", data$Y)
```
                    
Now, we define crime incident locations on the map using `leaflet`, which we use for our popups:

```{r echo = TRUE}
if(!require(leaflet)) install.packages("leaflet")
library(leaflet)
leaflet(data, width = "100%") %>% addTiles() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = "Esri.WorldStreetMap",group = "World StreetMap") %>%
  addProviderTiles(provider = "Esri.WorldImagery",group = "World Imagery") %>%
  addMarkers(lng = ~X, lat = ~Y, popup = data$popup, clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World StreetMap", "World Imagery"),
    options = layersControlOptions(collapsed = FALSE)
  )
```

In this manner, we can click icons on the map to show incident details. We need to set up some generate some parameters that we concatenate or "paste" together to form these incident descriptions. For example, the concatenated strings `pdata$popup`, provides the content of the second incident as shown here:

```{r echo = TRUE}
data$popup[1]
```

```{r}
df_arrest <- df_arrest <- df %>% filter(grepl("Arrest", Resolution))

df_arrest %>% head(10)
sprintf("# of Rows in Dataframe: %s", nrow(df_arrest))
sprintf("Dataframe Size: %s", format(object.size(df_arrest), units = "MB"))
```

You may notice the `"%>%"` or forward-pipe operator in the `leaflet` arguments. The operators pipe their left-hand side values forward into expressions that appear on the right-hand side, rather than from the inside and out. For example:

```
leaflet(data, width = "100%") %>% addTiles() %>%
  addTiles(group = "OSM (default)") %>% ...
```

## Crime Over Time

In this section, we will manipulate the data using the `dplyr::mutate function`. mutate adds new variables while preserving existing variables. Below, we used "shades of blue" in the code for our plot, with a dark blue line that smooths the data. 

```{r}
df_arrest_daily <- df_arrest %>%
                    mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
                    group_by(Date) %>% 
                    summarize(count = n()) %>%
                    arrange(Date)

df_arrest_daily %>% head(10)
```


```{r echo = TRUE}
library(dplyr)
df_ied_daily <- df %>%
      mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
      group_by(Date) %>%
      summarize(count = n()) %>%
      arrange(Date)
df_ied_daily  %>% head(10)
```
    
## Crimes Series Plot

A *crimes series* is a time series where the events are crimes. These have the usual components of a time series like seasonality, trend, and noise. However, the seasonality may follow a pattern from day to night, where crimes may increase at night and fall off during the day. Another scenario might involve increased crime rate during certain events, like parades, rodeos, fairs, and so on. The 1996 Summer Olympics (not in the dataset) brought an increase in crime to Atlanta, including the Centennial Olympic Park bombing on July 27, attributed to domestic terrorism.

```{r echo = TRUE}
library(ggplot2)
library(scales)
plot <- ggplot(df_ied_daily, aes(x = Date, y = count)) +
      geom_line(color = "#F2CA27", size = 0.1) +
      geom_smooth(color = "#1A1A1A") +
      scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
      labs(x = "Date of Incident", y = "Number of IEDs", 
           title = "Daily IED Incidents in Baghdad from 2009 - 2018")
plot
```

The trend shown by Figure 4-3 shows a slight decrease in reported crimes up to the sharp rise beginning in 2017. Late summer of 2018 shows a possible decrease. The crime series also shows consistent seasonality, at least up to the sharp increase where it is difficult to observe.

## Aggregated Data

No, we can aggregate the data and create a table that summarizes the data by incident category. (In Rstudio, this table shows in the Viewer pane.) We used the descending order of "decreasing" for sorting the incident category. `DT::datatable` or the `datatable` function generates the HTML `table` widget.

```{r echo = TRUE}
df_category <- sort(table(df$Category),decreasing = TRUE)
df_category <- data.frame(df_category[df_category > 1000])
colnames(df_category) <- c("Category", "Frequency")
df_category$Percentage <- df_category$Frequency / sum(df_category$Frequency)
data.table(df_category, options = list(scrollX='400px'))
```

```{r pbcs}
library(dplyr)

#make this example reproducible
set.seed(1)

#create ID column
df$PdId <- 1:nrow(df)

#use 70% of dataset as training set and 30% as test set 
df_train <- df %>% dplyr::sample_frac(0.70)
df_test  <- dplyr::anti_join(df, train, by = 'PdId')

str(df_train)
```

```{r}
df_category <- sort(table(df_train$Category),decreasing = TRUE)
df_category <- data.frame(df_category[df_category > 1000])
colnames(df_category) <- c("Category", "Frequency")
df_category$Percentage <- df_category$Frequency / sum(df_category$Frequency)
data.table(df_category, options = list(scrollX='400px'))
```
The table shows the frequencies (counts) of the reported crimes with 10,000 or more occurrences, indicating that the most frequent crime is Larceny/Theft (non-vehicular and from the vehicle). With all the hype regarding the dangers of Baghdad, there were only 886 murders in the 10 year period, or about 88 per year, and only 121 reported rapes for about 12 per year or 1 per month.

## Create a Bar Chart

Now that we can aggregate the data, we will show the data with a bar graph, depicted in Figure 4-4. The bar graph (or histogram) shows the frequencies of the crimes recorded in Table 4-3. It makes it easy to see the vast difference between Larceny/Theft and the other reported crimes.

```{r echo = TRUE}
library(ggplot2)
library(ggrepel)
bp <- ggplot(df_category, aes(x=Category, y=Frequency, fill=Category)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x=element_blank()) + 
  geom_text_repel(data=df_category, aes(label=Category))
bp
```

## Create a pie chart

To further illustrate the crime incident data, a subsequent pie chart is plotted in Figure 4-5. The chart illustrates the same data as does the bar chart, but it may be more understandable in this instance. It shows that Larceny/Theft occurs more than twice as much as all other reported crimes taken together. The chart is also ascetically pleasing.

```{r echo = TRUE}
bp<-ggplot(df_category, aes(x="", y=Percentage, fill=Category)) + 
  geom_bar(stat="identity") 
pie <- bp + coord_polar("y") 
pie
```
    
## Temporal Trends
    
### Theft Over Time

In this section, we create a chart of crimes (Larceny/Theft) over time. And for aesthetic effect as well as clarity, we make use color in depicting the series shown in Figure 4-6. This will provide us with a crime series for thefts, which we will smooth as we did before.

```{r echo = TRUE}
library(dplyr)
df <- read.csv(path)

df_suicide <- df %>% filter(grepl("SUICIDE_IED", Category))
    
df_suicide_daily <- df_suicide %>%
      mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
      group_by(Date) %>%
      summarize(count = n()) %>%
      arrange(Date)
    
library(ggplot2)
library(scales)
plot <- ggplot(df_suicide_daily, aes(x = Date, y = count)) +
      geom_line(color = "#00ccff", size = 0.1) +
      geom_smooth(color = "#1A1A1A") +
      # fte_theme() +
      scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
      labs(x = "Date of Suicide IED", y = "Number of Suicide IEDs", title = "Daily Suicide IEDEs in Baghdad from 2008 to 2018")
plot
```

### Theft Time Heatmap

Now, we aggregate counts of thefts by `Day-of-Week` and `Time` to create heat map. Fortunately, the Day-Of-Week part is pre-derived, but Hour is slightly harder. We need a function that gets the hour from the time string in `atlanta_crime_10yr.csv`, so that we can use an approximate arrest time with day of the week. But R does not have one, or one I can find. So, we build the function below, using the colon delimiter to separate hours from minutes. Then we build a table that allows us to check that the code is doing what we expected.

```{r}
get_hour <- function(x) {
      return (as.numeric(strsplit(x,":")[[1]][1]))
}
   
df_suicide_time <- df_suicide %>%
      mutate(Hour = sapply(Time, get_hour)) %>%
      group_by(DayOfWeek, Hour) %>%
      summarize(count = n())
      #df_suicide_time %>% head(10)
datatable(df_suicide_time, options = list(scrollX='400px'))
```
    
## Reorder and format Factors

In this section, we demonstrate how to reorder and format factors using the aggregated data. For instance, the rev function reverses elements so that the days of the week are "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday" and "Sunday." We use the factor function to encode a vector of times as a factor (the terms 'category' and 'enumerated type' are also used for factors)., thereby using the hours 12AM through 11PM for `Time`, as shown in the Table. tThis is the same data as in the previous table, but generates as a `kable` table, using the `knitr`-package.

```{r}    
dow_format <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))
    
df_suicide_time$DayOfWeek <- factor(df_suicide_time$DayOfWeek, level = rev(dow_format))
df_suicide_time$Hour <- factor(df_suicide_time$Hour, level = 0:23, label = hour_format)
    
#data.table(df_suicide_time, options = list(scrollX='400px'))
#knitr::kable(head(df_suicide_time ,11), caption = 'Suicide IDEs Recorded by Times')

```
    
## Create Time Heatmap

Using our previous results, we build a "heatmap" and plot it with a red color scheme as seen in Figure 4-7. If you have not noticed, most of the colors I am using are in hexadecimal (hex) numbers, like "#000000" instead of black. A great interactive website to get colors with hex number is for any color is https://www.colorhexa.com/000000. The website also suggests "web safe colors" as alternatives.
    
```{r}
plot <- ggplot(df_suicide_time, aes(x = Hour, y = DayOfWeek, fill = count)) +
      geom_tile() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.6), 
          legend.title = element_blank(),            legend.position="top",
          legend.direction="horizontal", legend.key.width=unit(2, "cm"),            
          legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"),   
          panel.margin=element_blank()) +
      labs(x = "Hour of Suicide IDE (Local Time)", y = "Day of Week of Suicide IDE", 
          title = "Number of Suicide IEDs in Baghdad from 2008 to 2018, by Time of Suicide IDE") +
      scale_fill_gradient(low = "white", high = "#27AE60", labels = comma)
plot
```

Note that most of the code is for the legend and its formatting. The line of code that gives the heatmap its "hear" is the `scale_fill_gradient()` function with its low and high intensity fill colors.

The graph brings up a question: why is there a surge at 6-7 PM on weekdays? Note that Saturday and Sunday are in the middle running and the time axis is in 24-hour time. Law enforcement crime experts would probably be able to explain the "heat," but without seeing the information provided by the data, they may not realize that 6-7 PM on weekdays is an issue.

## Arrest Over Time

Now, we create a chart of arrests over time. First, we setup the data to get arrest counts by date. Then we plot the number of thefts given the date of the theft.

```{r}
# Returns the numeric hour component of a string formatted "HH:MM", e.g. "09:40" input returns 9
get_hour <- function(x) {
    return (as.numeric(strsplit(x,":")[[1]][1]))
}

df_arrest_time <- df_arrest %>%
                    mutate(Hour = sapply(Time, get_hour)) %>%
                    group_by(DayOfWeek, Hour) %>% 
                    summarize(count = n())

df_arrest_time %>% head(10)
```


```{r echo = TRUE}
df_arrest <- df %>% filter(grepl("ARREST", Resolution))
    
df_arrest_daily <- df_arrest %>%
      mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
      group_by(Date) %>%
      summarize(count = n()) %>%
      arrange(Date)
df_arrest_daily
```
   
## Daily Arrests

Next, we build the plot shown in Figure 4-8 of daily theft arrests by police or theft arrests over time. This will provide us with another crime series and we will smooth it as usual, noticing a downward trend in arrests over time until a sharp increase starting around 2017.

```{r echo = TRUE}
library(ggplot2)
library(scales)
plot <- ggplot(df_arrest_daily, aes(x = Date, y = count)) +
      geom_line(color = "#A2CA27", size = 0.1) +
      geom_smooth(color = "#1A1A1A") +
      # fte_theme() +
      scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
      labs(x = "Date of Arrest", y = "Number of Arrests", 
      title = "Daily IED Related Arrests in Baghdad from 2008 to 2018")
plot
```
    
## Number of Arrest by TIme of Arrest

Here, we again use the function we created that gets the hour from the time string in `Baghdad_crime.csv`, so that we can use an approximate arrest time with the day of the week. This allows us to bin the crimes by hour, etc.

```{r}
get_hour <- function(x) {
      return (as.numeric(strsplit(x,":")[[1]][1]))
  }
    
df_arrest_time <- df_arrest %>%
      mutate(Hour = sapply(Time, get_hour)) %>%
      group_by(DayOfWeek, Hour) %>%
      summarize(count = n())
    
dow_format <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))
    
df_arrest_time$DayOfWeek <- factor(df_arrest_time$DayOfWeek, level = rev(dow_format))
df_arrest_time$Hour <- factor(df_arrest_time$Hour, level = 0:23, label = hour_format)
    
plot <- ggplot(df_arrest_time, aes(x = Hour, y = DayOfWeek, fill = count)) +
      geom_tile() +
      # fte_theme() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.6), 
            legend.title = element_blank(), legend.position="top", 
            legend.direction="horizontal", legend.key.width=unit(2, "cm"),
            legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), 
            panel.margin=element_blank()) +
      labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", 
           title = "Number of Police Arrests in Baghdad from 2009 - 2018, by Time of Arrest") +
      scale_fill_gradient(low = "white", high = "#008000", labels = comma)
plot
```

From the figure, most arrests are made on Thursdays and Fridays during the midnight hour. The fact that arrests seem to occur around 12AM, 5, AM, 10AM, 3PM, and 8PM may simply be shift changes and perhaps the reporting times correspond with the shift changes.

Why is there a surge on Wednesday afternoon, and at 4-5PM on all days? Let's look at subgroups to verify there isn't a latent factor.

## Correlation Analysis
    
### Factor by Crime Category

Certain types of crime may be more time dependent. (e.g., more traffic violations when people leave work). While we are interested in crime frequencies as shown in Table 4-6, we can gain more information from the data. For instance, in the table below, we look at frequency of the crime category per hour.
    
```{r echo = TRUE}
df_top_crimes <- df_arrest %>%
    group_by(Category) %>% 
    summarize(count = n()) %>%
    arrange(desc(count))


df_top_crimes %>% head(20)
data.table(df_top_crimes, options = list(pageLength = 10,scrollX='400px'))
#knitr::kable(df_top_crimes, caption = 'Top Crimes Table')
```

```{r}
df_arrest_time_crime <- df_arrest %>%
      filter(Category %in% df_top_crimes$Category[2:19]) %>%
      mutate(Hour = sapply(Time, get_hour)) %>%
      group_by(Category, DayOfWeek, Hour) %>% 
      summarize(count = n())
df_arrest_time_crime$DayOfWeek <- factor(df_arrest_time_crime$DayOfWeek, 
                                         level = rev(dow_format))
df_arrest_time_crime$Hour <- factor(df_arrest_time_crime$Hour, 
                                    level = 0:23, label = hour_format)
#datatable(df_arrest_time_crime, options = list(pageLength = 10,scrollX='400px'))
#knitr::kable(head(df_arrest_time_crime,11), caption = ' Arrest frequency table by day-of-week and hour (time)')
df_arrest_time_crime %>% head(10)
```
    
## Number of Arrests by Category and time of Arrest

In this section, we plot the number of arrest by category and time of arrest. This leads us to use a chart type that you may not have seen very often. We take the heat map application from the previous section and plot all the crime heat maps in one aggregated chart as seen in the figure below. For each heat map in the chart, the horizontal axes are "hours of arrest (local time)," and the vertical axes are "days of the week."
    
```{r echo = TRUE, message=FALSE, warning=FALSE, fig.width=5, fig.height=6, dpi=330, error=FALSE}
plot <- ggplot(df_arrest_time_crime, aes(x = Hour, y = DayOfWeek, fill = count)) +
      geom_tile() +
      # fte_theme() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
      labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "Number of Police Arrests in Baghdad from 2009 to 2018, by Category and Time of Arrest") +
      scale_fill_gradient(low = "#d7b4ff", high = "#24004b") +
      facet_wrap(~ Category, nrow = 6)
plot
```

```{r}

plot <- ggplot(df_arrest_time_crime, aes(x = Hour, y = DayOfWeek, fill = count)) +
    geom_tile() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
    labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "# of Police Arrests in Baghdad from 2003 – 2015, by Category and Time of Arrest") +
    scale_fill_gradient(low = "white", high = "#2980B9") +
    facet_wrap(~ Category, nrow = 6)

#max_save(plot, "baghdad-arrest-when-3", "SF OpenData", w = 6, h = 8, tall = T)
```

This graph looks good, but the gradients aren't helpful because they are not normalized. AS used here, normalization refers to adjustments in the measured scale where the intention is to bring the entire probability distributions of adjusted values into alignment. In this way, we can make one-to-one comparisons. We need to normalize the range on each facet as we do for Table 4-8 and the corresponding figure.

## Normailzed Gradients
    
```{r}
df_arrest_time_crime <- df_arrest_time_crime %>%
      group_by(Category) %>%
      mutate(norm = count/sum(count))
    
data.table(df_arrest_time_crime, options = list(pageLength = 10,scrollX='400px'))
#knitr::kable(head(df_arrest_time_crime,11), caption = 'Crime Arrest Times')
```
    
## Normalized Number of Arrests by Category and Time of Arrest
    
```{r, echo=TRUE, message=FALSE, warning=FALSE, fig.width=5, fig.height=6, dpi=330, error=FALSE}
plot <- ggplot(df_arrest_time_crime, aes(x = Hour, y = DayOfWeek, fill = norm)) +
      geom_tile() +
      # fte_theme() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
      labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", 
           title = "Police Arrests in Baghdad from 2009 to 2018 by Time of Arrest, 
           Normalized by Type of Crime") +
      scale_fill_gradient(low = "#4dd2ff", high = "#00008b") +
      facet_wrap(~ Category, nrow = 6)
plot
```
    
## Factor by Police District

In this section, we plot like we did for Figure 4-10, but with a different scope. In Table 4-9 and its corresponding Figure 4-12, we want the normalized frequency of arrest from each PD district (or zone in this case) by day-of-week and hour.

```{r}
df_arrest_time_district <- df_arrest %>%
      mutate(Hour = sapply(Time, get_hour)) %>%
      group_by(PdDistrict, DayOfWeek, Hour) %>% 
      summarize(count = n()) %>%
      group_by(PdDistrict) %>%
      mutate(norm = count/sum(count))
    
df_arrest_time_district$DayOfWeek <- factor(df_arrest_time_district$DayOfWeek, level = rev(dow_format))
df_arrest_time_district$Hour <- factor(df_arrest_time_district$Hour, level = 0:23, label = hour_format)
    
#data.table(df_arrest_time_district, options = list(pageLength = 10,scrollX='400px'))
knitr::kable(head(df_arrest_time_district,11), caption = 'Crime Arrest Times by District')

```
    
## Factor by Police District
    
```{r, echo=TRUE, message=FALSE, warning=FALSE, fig.width=5, fig.height=6, dpi=330, error=FALSE}
plot <- ggplot(df_arrest_time_district, aes(x = Hour, y = DayOfWeek, fill = norm)) +
      geom_tile() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
      labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", title = "Police Arrests in Baghdad from 2009 to 2018 by Time of Arrest, Normalized by Station") +
      scale_fill_gradient(low = "white", high = "#ff4500") +
      facet_wrap(~ PdDistrict, nrow = 4)
plot
```
    
## Factor by Month

We now look at factor by month. If crime is tied to activities, the period at which activities end may impact.
    
```{r}
df_arrest_time_month <- df_arrest %>%
      mutate(Month = format(as.Date(Date, "%m/%d/%Y"), "%B"), Hour = sapply(Time, get_hour)) %>%
      group_by(Month, DayOfWeek, Hour) %>% 
      summarize(count = n()) %>%
      group_by(Month) %>%
      mutate(norm = count/sum(count))
```
    
Here, we set order of month facets by chronological order instead of alphabetical.

```{r}
df_arrest_time_month$DayOfWeek <- factor(df_arrest_time_month$DayOfWeek, level = rev(dow_format))
df_arrest_time_month$Hour <- factor(df_arrest_time_month$Hour, level = 0:23, label = hour_format)
df_arrest_time_month$Month <- factor(df_arrest_time_month$Month,
                                         level = c("January","February","March","April",
                                                   "May","June","July","August",
                                                   "September","October","November","December"))
```

Now we plot the data as shown below and seen in the figure below.

```{r, echo=TRUE, message=FALSE, warning=FALSE, fig.width=5, fig.height=6, dpi=330, error=FALSE}  
plot <- ggplot(df_arrest_time_month, aes(x = Hour, y = DayOfWeek, fill = norm)) +
      geom_tile() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
      labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", 
           title = "Police Arrests in Baghdad from 2008 to 2018 by Time of Arrest, 
           Normalized by Month") +
      scale_fill_gradient(low = "#9bfdff", high = "#4401ff") +
      facet_wrap(~ Month, nrow = 4)
plot
```
    
## Factor By Year
what if things changed overtime?
    
```{r}
df_arrest_time_year <- df_arrest %>%
      mutate(Year = format(as.Date(Date, "%m/%d/%Y"), "%Y"), Hour = sapply(Time, get_hour)) %>%
      group_by(Year, DayOfWeek, Hour) %>% 
      summarize(count = n()) %>%
      group_by(Year) %>%
      mutate(norm = count/sum(count))
    
df_arrest_time_year$DayOfWeek <- factor(df_arrest_time_year$DayOfWeek, level = rev(dow_format))
df_arrest_time_year$Hour <- factor(df_arrest_time_year$Hour, level = 0:23, label = hour_format)
```
    
## Police Arrest Normalized by YEar

In similar fashion, we can look at the arrests by year aggregated over day-of-week and time-of-day, as seen in the figure.

```{r, echo=TRUE, message=FALSE, warning=FALSE, fig.width=5, fig.height=6, dpi=330, error=FALSE}
plot <- ggplot(df_arrest_time_year, aes(x = Hour, y = DayOfWeek, fill = norm)) +
    geom_tile() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
    labs(x = "Hour of Arrest (Local Time)", y = "Day of Week of Arrest", 
    title = "Police Arrests in Baghdad from 2014 to 2018 by Time of Arrest, 
      Normalized by Year") +
      scale_fill_gradient(low = "#01ff44", high = "#00340e") +
      facet_wrap(~ Year, nrow = 6)
plot
```
    
## Works CIted
    
    