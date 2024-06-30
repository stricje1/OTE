---
title: "PECVD"
author: "Jeffrey Strickland"
date: "2024-05-14"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, dpi = 330)
```

## Antireﬂection Coating (ARC)

Nyberg (1999) has shown that silicon nitride (SiNx) grown by Plasma Enhanced Chemical Vapor Deposition (PECVD) is a promising candidate for an antireﬂection coating (ARC) on military grade crystalline silicon solar cells. Silicon nitride was grown on polished (100)-oriented 4A silicon wafers using a parallel plate Plasma Technology PECVD reactor. The diameter of the electrodes of the PECVD is 24 cm and the diameter of the shower head (through which the gases enter) is 2cm. The RF frequency was 13.56 MHz. The thickness of the silicon nitride was one-quarter of the wavelength of light in the nitride, the wavelength being 640 nm. This wavelength is expected to be close to optimal for silicon solar cell purposes. The process gases were ammonia and a mixture of 3% silane in argon. The experiments were carried out according to a $2^5$ factorial design. The results are shown in the table below.

### Factors and Responses

The dataset pecvd.csv is comprised of the folloing variables and reposnses. we will nee to convert the variables to factors for our experimental design.

$A = SiH_4$ to $NH_3$ Flow Rate Ratio	

$B =$ Total Gas Flow Rate (sccm)	

$C =$ Pressure (mtorr)	

$D =$ Temperature (C○)	

$E =$ Power (W)	

$y_1 =$ Refracttion Index	

$y_2 =$ Growth Rate ("nm" /"min" )


### Data Prep

#### Load required packages

```{r, results='hide'}
library(daewr)
library(gmodels)
library(car)
library(lsmeans)
library(FrF2)
library(BsMD)
library(AlgDesign)
library(ggplot2)
library(agricolae)
library(DoE.base)
```

#### Load the Data
Next, we load the data using the `read.csv` function and inspect its structure.

```{r pressure, echo=FALSE}
pecvd <- read.csv("C:\\Users\\jeff\\Documents\\Data\\pecvd.csv")
structure(pecvd)
```  

#### **Problem 1:** Using the data in pecvd.csv, we fit the factorial model to the response $y_1$ including all interactions up to the 5-way.

#### **Solution 1A:** We start by loading the necessary R packages: `daewr`, 

##### Create Factors and Sepsonse Dataframes

Now, we extract the variables from the dataframe and using the `as.factor` function, we convert each into factor (A...B). If we inspect the `pecvd_fac` dataframe, there appears to be no change. We use the `str` function to inspect the structure of `pecvd_fac`.

```{r}
pecvd_fac <- pecvd[2:6]
pecvd_fac$A <- as.factor(pecvd_fac$A)
pecvd_fac$B <- as.factor(pecvd_fac$B)
pecvd_fac$C <- as.factor(pecvd_fac$C)
pecvd_fac$D <- as.factor(pecvd_fac$D)
pecvd_fac$E <- as.factor(pecvd_fac$E)

str(pecvd_fac)
```

#### **Solution 1B:** 
Once we are satisfied that we have the right factors structure, we add the response that we are interested in, $y_1$, to the dataframe, for in a new dataframe with five factors and a response.

```{r}
pecvd_df <- data.frame(cbind(pecvd_fac, pecvd$y1))
head(pecvd_df)
```

#### **Solution 1C:** 

##### Fit a linear model

 Finaly, we fit a linear model including all interactions up to the 5-way.

```{r}
fit <- lm( pecvd.y1 ~ .^5, data=pecvd_df)
summary(fit)
```
##### Fit an ANOVA Model
**Solution 1D:** 

Now we'll it an ANOVA model.

```{r}
aov.fit <- aov(pecvd.y1 ~ ., data = pecvd_df)
summary(aov.fit)
```
```{r}
TukeyHSD(aov.fit, c("A","B"))
```

##### Generate Cell Means Table
**Solution 1E:** 

 Now we'll generate model cell means.

```{r}
model.tables( aov.fit, type = "means" )$tables
```

#### Problem 2.	Make a normal plot of the eﬀects or regression coeﬃcients to determine which main eﬀects and interactions are signiﬁcant.

```{r}
library(auditor)
model_glm <- glm(pecvd.y1 ~ ., family = gaussian(), data = pecvd_df)
lm_audit <- audit(model_glm, data = pecvd_df, y = pecvd_df$pecvd.y1)
hn_lm <- model_halfnormal(lm_audit)
plot_halfnormal(hn_lm)
plot(hn_lm)
```

#### **Solution 2A:** 

##### Daniel Plots


```{r}
model_glm <- glm(pecvd.y1 ~ ., family = gaussian(), data = pecvd_df)
par(mfrow=c(1,3),oma=c(0,0,1,0),pty="s")
DanielPlot(model_glm, half = TRUE, main = "Half-Normal Plot")
DanielPlot(model_glm, main = "Normal Plot of Effects")
DanielPlot(model_glm,
        faclab = list(idx = c(12,4,13), lab = c("A","B","C","D","E")),
        main = "Active Contrasts")
```

#### **Solution 2B:** 

##### Interaction Plot Matrix

```{r}
library(FrF2)
tiff("C:\\Users\\jeff\\Documents\\Fig2.29.tiff", width = 6, height = 5, units = 'in', res = 360)
IAPlot(fit, sel=c(1,2,3,4), abbrev=7, lwd=2)
dev.off()
```

#### **Solution 2C:**

##### Sample of Interaction Plots

```{r}
with(pecvd_df, (interaction.plot(A,C, pecvd.y1, type = "b",
                            pch = c(0,2,6,15), leg.bty = "o", main = "Interaction for SiH4 to NH3 Flow Rate Ratio by Pressure", 
                            xlab = "Pressure", ylab = "average Refract. Index", lwd = 2, lty = 1:4, col=c("dodgerblue","magenta"))))

with(pecvd_df, (interaction.plot(A,E, pecvd.y1, type = "b",
                            pch = c(0,2,6,15), leg.bty = "o", main = "Interaction for SiH4 to NH3 Flow Rate Ratio by Temperature", 
                            xlab = "Temperature", ylab = "average Refract. Index", lwd = 2, lty = 1:4, col=c("dodgerblue","orange2"))))
```


#### Problem 3.	Drop insigniﬁcant terms from the model and make residual plots to check the assumptions of the model ﬁt.

#### **Solution 3A:** 

```{r}
model_glm2 <- glm(pecvd.y1 ~ ., family = gaussian(), data = pecvd_df)
model_glm3 <- glm(pecvd.y1 ~ .^2, family = gaussian(), data = pecvd_df)
model_aov2 <- aov(pecvd.y1 ~ .,  data = pecvd_df)
model_aov3 <- aov(pecvd.y1 ~ .^2,  data = pecvd_df)
print(summary(model_glm2))
print(summary(model_glm3))
print(summary(model_aov2))
print(summary(model_aov3))
```

#### **Solution 1B:** 

```{r}
model_aov4 <- aov(pecvd.y1 ~ A+B+C+D+A:B+A:C+A:D,  data = pecvd_df)
print(summary(model_aov4))
```

#### **Solution 1C:** 
```{r}
aov_audit <- audit(model_aov4, data = pecvd_df, y = pecvd_df$pecvd.y1)
score_mse(aov_audit)
```

##### Residual Plots

```{r, fig.width=6, fig.height=7, dpi = 330}
tiff("C:\\Users\\jeff\\Documents\\Fig3.32.tiff", width = 4.5, height = 5, units = 'in', res = 360)
residualPlots(model_glm4)
dev.off()
```

##### Additional Output Analysis using `auditor`

##### Contrasts using Tukey

```{r}
TukeyHSD(model_aov4, c("A","B","C","D"))
```

##### Autocorrelation Plot using Autitor

```{r}
mr_lm <- model_residual(lm_audit)
plot_autocorrelation(mr_lm)
plot(mr_lm, type = "autocorrelation")
plot_autocorrelation(mr_lm, smooth = TRUE)
```

##### Daniel Plots

```{r}
par(mfrow=c(1,3),oma=c(0,0,1,0),pty="s")
DanielPlot(model_glm, half = TRUE, main = "Half-Normal Plot")
DanielPlot(model_glm, main = "Normal Plot of Effects")
DanielPlot(model_glm,
        faclab = list(idx = c(12,4,13), lab = c("A","B","C","D","E")),
        main = "Active Contrasts")
```
##### ANOVA Model

Now we'll it an ANOVA model.

```{r}
aov.fit <- aov(pecvd.y1 ~ ., data = pecvd_df)
summary(aov.fit)
```
```{r}
TukeyHSD(aov.fit, c("A","B"))
```

##### More Outcomes

 Now we'll genrate model caell means.

```{r}
model.tables( aov.fit, type = "means" )$tables
```



