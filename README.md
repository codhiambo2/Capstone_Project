# Capstone_Project
Flexidashboard Capstone Project 
---
title: "Captone Project_HIV/TB Coinfection in Kenya in 2020"
author: "Collins Odhiambo"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    social: menu
    source_code: embed
---

```{r setup, include=FALSE}
library(ggplot2)
library(plotly)
library(plyr)
library(flexdashboard)
library(maptools)
library(raster)
library(dplyr)
library(rgdal)
library(ggmap)
library(scales)
library(readr)
library(tidyverse)
library(hrbrthemes)
# HIV data
HIV_Data= read_csv("C:/Users/Dr.Collins/Desktop/PostDoc Materials_JHU/4th Term/Advanced Data Science II/Capsone Project/HIV_Data.csv")
#HIV-TB Data
HIV_Data$Period=as.Date(HIV_Data$Period)
AnnimationHIV= read_csv("C:/Users/Dr.Collins/Desktop/PostDoc Materials_JHU/4th Term/Advanced Data Science II/Capsone Project/AnnimationHIV.csv")
set.seed(955)
dat <- data.frame(cond = rep(c("A", "B"), each=10),
                  xvar = 1:20 + rnorm(20,sd=3),
                  yvar = 1:20 + rnorm(20,sd=3))
```

Summaries
=======================================================================

Row
-----------------------------------------------------------------------

### Scatter Chart by counties/sub-regions in Kenya

```{r}
p= ggplot(HIV_Data, aes(x=ART_Enrollment, fill=County)) +
  geom_histogram()
ggplotly(p)
```


### Trend Line for ART Current

```{r}
p1= ggplot(HIV_Data, aes(Period, `On ART_Total`))
p1= p1 + geom_point() + stat_smooth()

fig1= ggplotly(p1)

fig1
```

Row
-----------------------------------------------------------------------

### Loess Smoothed Fit for regional ART Current spread

```{r}
p3 = ggplot(AnnimationHIV, aes(x=Current_onART, y=Regions)) +
            geom_point(shape=1) +    # Use hollow circles
            geom_smooth()            # Add a loess smoothed fit curve with confidence region
ggplotly(p3)
```

### Trend Line

```{r}
#Trend lines 

p4=ggplot(data = AnnimationHIV, aes (x = Period, y=Current_onART,group=Regions, color = Regions)) +
    geom_smooth(method = "lm") +
    geom_point(alpha=0) + 
    theme(legend.position = "none") +
    labs(y="Current ART in Months", x = "Active on ART") 
ggplotly(p4)
```

Data Visualization
=======================================================================

Row
-----------------------------------------------------------------------

### Histogram for Viral Load Suppression

```{r}
p5= ggplot(AnnimationHIV, aes(x=VL_Suppression, fill=Regions)) +
  geom_histogram()
ggplotly(p5)
```


### Smooth Linear Regression TB/HIV Coinfection

```{r}
p6= ggplot(AnnimationHIV, aes(x=Period, y=TB_onHAART)) +
            geom_point(shape=1) +    # Use hollow circles
            geom_smooth(method=lm)   # Add linear regression line
ggplotly(p6)
```

Row
-----------------------------------------------------------------------

### Smooth Linear Regression TB/HIV Coinfection with Loess Smoothed Fit

```{r}
p7= ggplot(AnnimationHIV, aes(x=Period, y=ART_Enrollment)) +
            geom_point(shape=1) +    # Use hollow circles
            geom_smooth()            # Add a loess smoothed fit curve with confidence region
ggplotly(p7)
```

### Constraining Slope with stat_smooth

```{r}
AnnimationHIV$Malnutrition= as.factor(AnnimationHIV$Malnutrition)

fm= lm(TB_onHAART ~ ART_Enrollment + Malnutrition, data = AnnimationHIV)

p8= ggplot(data = cbind(AnnimationHIV, pred = predict(fm)), aes(x = ART_Enrollment, y = TB_onHAART, color = Malnutrition))
p8 = p8 + geom_point() + geom_line(aes(y = pred))
ggplotly(p8)
```

Density plots and Spatial Maps
=======================================================================

Row
-----------------------------------------------------------------------

### stat_density Example

```{r}
dataFrameHIV = data.frame(AnnimationHIV$TB_onHAART,
           AnnimationHIV$ART_Enrollment,
           AnnimationHIV$VL_Suppression)

dataFrameHIV = stack(dataFrameHIV)

p9= ggplot(dataFrameHIV, aes(x = values)) +
            stat_density(aes(group = ind, color = ind),position="identity",geom="line")
ggplotly(p9)
```

### Spatial Spread in Kenyan counties

```{r}
Kenya=getData("GADM", country="KE", level=0)
Kenya1=getData("GADM", country="KE", level=1)
Kenya1_UTM=spTransform(Kenya1, CRS("+init=EPSG:32737"))  
NAME_1<-Kenya1_UTM@data$NAME_1
OnART_Total=c(4579,10539,25828,33593,3675,9872,1065,114725,1337,15412,44778,15053,41883,26438,11384,33941,112086,22607,10198,9408,1667,28133,22131,688,1213,20530,75528,47040,16286,165897,42273,10929,9920,15680,9294,18374,1661,94308,5145,996,7139,16083,9397,30349,16709,273,3110)
count_df<-data.frame(NAME_1, OnART_Total)
Kenya1_UTM@data$id <- rownames(Kenya1_UTM@data)
Kenya1_UTM@data <- join(Kenya1_UTM@data, count_df, by="NAME_1")
Kenya1_df= fortify(Kenya1_UTM)
Kenya1_df= join(Kenya1_df,Kenya1_UTM@data, by="id")

theme_opts=list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_blank(),
                       plot.background = element_blank(),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_blank()))
m=ggplot() + 
  geom_polygon(data = Kenya1_df, aes(x = long, y = lat, group = group, fill =
                                       OnART_Total), color = "black", size = 0.25) +
  theme(aspect.ratio=1)+
  scale_fill_distiller(name="Current on ART", palette = "YlGn", direction = -1, breaks = pretty_breaks(n = 5))+
  labs(title="Patients Currently on ART")+
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
plot_m=ggplotly(m)
plot_m
```

Row
-----------------------------------------------------------------------

### Density and facet_wrap Predictions Together

```{r}
fit1=lm(AnnimationHIV$TB_onHAART~AnnimationHIV$ART_Enrollment)
PredictedValue=4.87+0.099689*AnnimationHIV$ART_Enrollment
dd=data.frame(AnnimationHIV$ART_Enrollment,PredictedValue,AnnimationHIV$Malnutrition)
colnames(dd) <- c("x_value", "Predicted_value",  "Malnutrition")
grid= with(dd, seq(min(Predicted_value), max(Predicted_value), length = 100))
normaldens= ddply(dd, "Malnutrition", function(df) {
  data.frame(
    Predicted_value= grid,
    density = dnorm(grid, mean(df$Predicted_value), sd(df$Predicted_value))
  )
})

p10 = ggplot(dd, aes(Predicted_value))  +
            geom_density() +
            geom_line(aes(y = density), data = normaldens, colour = "red") +
            facet_wrap(~ Malnutrition)
ggplotly(p10)
```

### Density and Scatterplot Overlay Using geom_density

```{r}
df2= data.frame(TX_Current =AnnimationHIV$Current_onART,
                 TB_HIV_Coinfection = AnnimationHIV$TB_onHAART)

p11= ggplot(df2, aes(TX_Current, TB_HIV_Coinfection)) + 
     geom_point(alpha = 0.5) + 
     geom_density_2d() + 
     theme(panel.background = element_rect(fill = '#ffffff'))

ggplotly(p11)
```

Inference and Predictions
=======================================================================

Row
-----------------------------------------------------------------------

### stat_density Example

```{r}
dfGamma = data.frame(Started_ART = AnnimationHIV$ART_Enrollment,
           TB_ART = AnnimationHIV$TB_onHAART)

dfGamma = stack(dfGamma)

p12= ggplot(dfGamma, aes(x = values)) +
            stat_density(aes(group = ind, color = ind),position="identity",geom="line")
ggplotly(p12)
```

### Inference and Predictions

```{r}
fig = AnnimationHIV %>%
  plotly::plot_ly(
    x = ~TB_onHAART, 
    y = ~ART_Enrollment, 
    size = ~Current_onART, 
   #color = ~continent, 
    frame = ~Period, 
    text = ~County, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )
fig <- fig %>% layout(
  xaxis = list(
    type = "log"
  )
)

fig
```

Row
-----------------------------------------------------------------------

### Map on Viral Load Suppression

```{r}
Kenya=getData("GADM", country="KE", level=0)
Kenya1=getData("GADM", country="KE", level=1)
Kenya1_UTM=spTransform(Kenya1, CRS("+init=EPSG:32737"))  
NAME_1<-Kenya1_UTM@data$NAME_1
VL_Suppression=c(54,90,90,84,84,86,79,93,9,85,78,63,62,75,91,83,90,82,59,58,83,88,83,0,0,68,93,63,88,70,91,80,81,83,73,91,69,91,53,0,65,88,75,45,88,100,79)
count_df<-data.frame(NAME_1, VL_Suppression)
Kenya1_UTM@data$id <- rownames(Kenya1_UTM@data)
Kenya1_UTM@data <- join(Kenya1_UTM@data, count_df, by="NAME_1")
Kenya1_df= fortify(Kenya1_UTM)
Kenya1_df= join(Kenya1_df,Kenya1_UTM@data, by="id")

theme_opts=list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_blank(),
                       plot.background = element_blank(),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_blank()))
m=ggplot() + 
  geom_polygon(data = Kenya1_df, aes(x = long, y = lat, group = group, fill =
                                       VL_Suppression), color = "black", size = 0.25) +
  theme(aspect.ratio=1)+
  scale_fill_distiller(name="Viral Load Suppression", palette = "Reds", direction = -1, breaks = pretty_breaks(n = 5))+
  labs(title="Patients Virally Suppressed")+
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
plot_VL=ggplotly(m)
plot_VL
```

### Density and Scatterplot Overlay Using geom_density

```{r}
df= data.frame(TB_HIV_Coinfection = AnnimationHIV$TB_onHAART,
                 VL_Suppression=AnnimationHIV$VL_Suppression)

p13= ggplot(df, aes(TB_HIV_Coinfection, VL_Suppression)) + 
     geom_point(alpha = 0.5) + 
     geom_density_2d() + 
     theme(panel.background = element_rect(fill = '#ffffff'))

ggplotly(p13)
```

About the Dashboard
=======================================================================
**The Dashboard**
This dashboard is produced as part of the post-doctoral learning on Advanced Data Science

**Data**
The HIV data used in this Dashboard has been extracted from Kenya District Health Information System 2 and is publicly available on https://hiskenya.org/dhis-web-commons/security/login.action.
The dataset is in aggregate form and do not have patient identification.

The data was extracted with the following indicators; Period (Jan-2020 to Dec-2020), County	Number of Clinical visits, Number Completed IPT in 12months, ART_Enrollment, Number Malnourished, Net Cohort On ART in 12 months, Number actively	On ART, Presumed TB, Number screened for TB, Number Start on ART, TB Cases Tested for HIV, TB cases on HAART	and viral load suppression

**Packages**
library(ggplot2)
library(plotly)
library(plyr)
library(flexdashboard)
library(maptools)
library(raster)
library(dplyr)
library(rgdal)
library(ggmap)
library(scales)
library(readr)
library(tidyverse)
library(hrbrthemes)

**Linear Prediction Result**

Call:
lm(formula = TB_onHAART ~ART_Enrollment)

Residuals:
     Min       1Q   Median       3Q      Max 
     
-135.971   -5.862   -2.227    5.114   87.565 

Coefficients:
                             Estimate Std. Error t value Pr(>|t|)  
                             
(Intercept)                  4.867440   0.722345   6.738 3.97e-11 ***

AnnimationHIV$ART_Enrollment 0.099689   0.001832  54.416  < 2e-16 ***

Deployment code - https://github.com/RamiKrispin/flexdashboard_example

Flexdashboard - https://rmarkdown.rstudio.com/flexdashboard/

**Questions and Feedback**

For any question or feedback you can reach out to me: codhiam2@jhmi.edu
