---
title: "Aerogel DOE"
author: "Jeffrey Strickland"
date: "2024-05-03"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, dpi = 300)
```

#Applied Example: Crosslinking Amine-Modified Silica Aerogels with Epoxies

## Introduction
Aerogels comprise a special class of low-density open-cell solid foams (typically, with porosity over 90%) which exhibit many unique properties such as exceptionally lightweight, high surface area, low thermal conductivity, extremely low dielectric constant, low sound wave transmission, high optical transparency in a wide range of wavelengths close to that of glass, and a very low refractive index (Fricke, 1988) (Emmerling, et al., 1995) (Ma, Roberts, Prévost, Jullien, & Scherer, 2000) (Woignier & Phalippou, 1987). These properties result from the microstructure of aerogels, which consists of a three-dimensional amorphous solid skeleton network with interconnected nanometer-sized pores in between. Silica aerogels are used for thermal and electrical insulation, especially in space applications, oxygen and humidity sensors, aerosol particle collectors, space mirror protectors, catalyst supports, battery electrodes, etc. (Woignier & Phalippou, Mechanical strength of silica aerogels, 1988).
NASA Glenn Research Center and the Ohio Aerospace Institute have recently demonstrated that templated polymerization of di-, tri-, and tetra-isocyanates on the surface of the nanoparticle building blocks of silica aerogels increases resulting conformal coatings increase the density of the native aerogels by a factor of 2-3 but the strength of the resulting materials may increase by more than two orders of magnitude. 
The mesoporous surfaces of tetramethoxysilane (TMOS)-derived silica aerogels have been modified with amines by co-polymerization of TMOS with aminopropyltriethoxysilane (APTES). The amine sites have become anchors for crosslinking the nanoparticles of the skeletal backbone of the aerogel by attachment of di-, tri, and tetra-functional epoxies. Processing variables such as amount of APTES used to make the gels, the epoxy type and concentration used for crosslinking, as well as the crosslinking temperature and time were varied according to a multivariable DOE model. It was found that while elastic modulus follows a similar trend with density, maximum strength is attained neither at the maximum density nor at the highest concentration of -NH2 groups, suggesting surface saturation effects. Aerogels crosslinked with the tri-functional epoxide always show improved strength compared with aerogels crosslinked with the other two epoxides under identical conditions. Solid 13C NMR studies show residual unreacted epoxides, which condense with one another by heating crosslinked aerogels at 150 °C.

The effect of polymer accumulation on the particles has been quantified as a function of the processing parameters, a, e, c, t and θ as described above using a statistical experimental design approach, by following aerogel properties such as physical dimensions, density, surface area, porosity, strength, and flexibility. It was deemed reasonable to assume that linear and nonlinear effects of any variable on any physical property could be captured adequately by a full quadratic model of the form:

$physical property=A+Ba+Ce+Dc+Et+Fθ+Ga^2+Hc^2+ It^2+Jθ^2+Kae+Lac +Mat+Nat+Oec+Peθ+Qet+Rcθ+Set+Ttθ$	Eq. 6.1

where $A$ through $T$ are coefficients that would be derived empirically from experimental data. The model contains terms for first order effects of all five variables and second order terms for $a$, $c$, $t$ and $$θ$$, as well as all possible two-way interaction terms. (Owing to the discrete nature of variable epoxy type, $e$, (epoxy type) there is no physical meaning to a second order term, $e^2$.) To evaluate first and second order terms for $a$, $c$, $t$ and $θ$, a minimum of two levels of each variable must be considered. The three levels of variable $a$ (APTES Percent) were 0%, 25% and 50% v/v APTES in TMOS+APTES. The three levels of variable $c$ (Expoxy Percent) were 15%, 45% and 75% v/v epoxy in THF+epoxy. Variable Time was evaluated as *Low* ($\le 50 hr$) and *High* otherwise. The t or Temp variable is assigned two levels, *Low* @ $\le 72$ and otherwise *High* The discrete variable epoxy type, e, was also considered at three levels corresponding to the di-, tri- and tetra-functional epoxies discussed.

In Chapter 5, we consider a full-factorial design to evaluate this model (the original approach), which would contain at least 243 experiments ($3^5$ experiments representing three levels each of five variables), not counting repeats. However, for now we want to evaluate a $2^k$ model witn $k=2$ using the set of experimental runs is computer-generated from the 243 candidate experiments. In total, there are only 24 usable samples.Thus, at $2^2=4$ experiments are required, we can repeat experients. To develop the $2^2$ model, we must have at least two factors with two levels each (i.e., Time and Temp). These were prepared according to Scheme 1 in random order, and were analyzed for their physical dimensions, density, surface area, porosity, strength, and flexibility. Table 1 summarizes the design runs and the experimental results.

The density of the samples was determined from their physical dimensions and their weight. Surface area and pore diameters were determined by nitrogen adsorption porosimetry. Mechanical strength data (i.e., stress at break point and elastic modulus) were obtained by a three-point bend test method.

```{r packages}
library(AlgDesign)
library(daewr)
library(DoE.base)
library(FrF2)
library(leaps)
library(lme4)
```


## Loading the Aerogel data

The Aerogel data is recorded by Meador, et al. in the paper "Crosslinking Amine-Modified Silica Aerogels with Epoxies: Mechanically Strong Lightweight Porous Materials" and I have extracted the table to generate a .CSV of the data.

```{r data_upload, echo=FALSE}
aerogel <- read.csv("C:\\Users\\jeff\\Documents\\Data\\aerogel.csv")
head(aerogel)
```

The data elements include:
*   Run Number
*   Density in grams/centimeters ($g/cm^3$)
*   Epoxy Type: Three different epoxy crosslinkers were used
*   Time in hours ($h$): duration of chemical treatment
*   Temperature in degrees Celsius ($^o C$)
*   Epoxy percent used in treatment
*   Surface area in centimeter-squared per gram ($cm^2$)
*   Average pore diameter in angstroms ($Å$)
*   Load force in kilograms ($km$): the force exerted on a surface or body
*   Max Stress at rupture in newtons per meters squared
*   Modulus of rupture in mega-pascals ($MPa$): maximum bending stress that can be applied
*   Weight loss percent during treatment

For our purposes we will use a subset of these factors each having three levels

Factor    Attribute	Levels
--------------------------
APTES_percent   1 = 0%
  2 = 25%
  3 = 50%
--------------------------  
Epoxy_percent   1 = 15%
  2 = 45%
  3 = 75%
--------------------------  
Epoxy_type    1 = di-epoxy
  2 = tri-epoxy
  3 = tetra-epoxy
--------------------------  
Time    1 = 16 hours
  2 = 44 hours
  3 = 72 hours
--------------------------  		
Temp    1 = 50 ($^o C$)
  2 = 72.5 ($^o C$) 
  3 = 90 ($^o C$)
--------------------------  

# Defining a Dataframe

Here we demonstrate creating a dta from from the imported data and also rename variables as needed. We use the cbind fuction that binds column for data toegther in a new data structure with a different name to preserve the original data file and structure. We create the dataframe in three steps:

Step 1: 
Bind selected columns of aerogel and form the dataframe.

Step 2: 
Forming the dataframe as we did in Step 1 renames the variables as X1,..., XN, for N variables. We could leave them as is or change them to something more intuitive, like they were in the CSV file.

Step 3:
Delete variable X1 through X4

```{r}
aero <- data.frame(cbind(aerogel$Density,aerogel$Epoxy_type,aerogel$Time,aerogel$Temp))
head(aero)
aero$Density = aero$X1
aero$Epoxy_Type = aero$X2
aero$Time = aero$X3
aero$Temp = aero$X4
aero_df = subset(aero, select = -c(X1,X2,X3,X4) )
head(aero_df)
```

# Develop an Experimental Design

 For this experiment, we want to study the density of an aerogel depending upon the expoxy type, time (duration) of treatment, and temperature during treatment, each having three levels.
 
```{r, echo=FALSE}
des1 <- gen.factorial( levels = 3, nVar = 3, varNames = c("Epoxy_type","Time", "Temp"))
des1 
```

### Define Factors

```{r}
aero_df$Epoxy_Type = factor(aero_df$Epoxy_Type)
aero_df$Time = factor(aero_df$Time)
aero_df$Temp = factor(aero_df$Temp)
head(aero_df)
colormap(aero_df,mod=1)
```


```{r}
model1 = lm(Density~., data = aero_df)
summary(model1)
```


```{r}
model2 = lm(Density~.^2, data = aero_df)
summary(model2)
```

```{r}
anova(model1,model2)
```


```{r}
mod1<- aov(  Density ~ Epoxy_Type + Time + Temp,  data = aero_df)
summary(mod1)
```

```{r}
aero.aov = aero_df
aero.aov$Epoxy_Type = factor(aero.aov$Epoxy_Type)
aero.aov$Time = factor(aero.aov$Time)
aero.aov$Temp = factor(aero.aov$Temp)
```



```{r}
aov1.out = aov(Density~., data=aero.aov)
summary(aov1.out)
```


```{r}
aov2.out = aov(Density~.^2, data=aero.aov)
summary(aov2.out)
```


```{r}
model.tables(aov2.out,type="means", se=T)
```

```{r}
plot(aov2.out,1)
```


```{r}
plot(aov2.out,2)
```

## All Linear Models (Regression)

```{r}
all_dat <- data.frame(aerogel)
all_df <- subset(all_dat, select = -c(Run))
head(all_df)
```

### Density Model

```{r}
density <- subset(all_df, select = -c(Max_Stress,Surface_Area,Load_Force,Average_Pore_Diam,Modulus))
mod1 = lm(Density ~ .^2, data = density)
summary(mod1)
```

### Surface Area Model

```{r}
surface <- subset(all_df, select = -c(Density,Load_Force,Max_Stress,Average_Pore_Diam,Modulus))
mod2 = lm(Surface_Area ~ .^2, data = surface)
summary(mod2)
```

### Pore Diameter Model

```{r}
pore <- subset(all_df, select = -c(Density,Surface_Area,Load_Force,Max_Stress,Modulus))
mod3 = lm(Average_Pore_Diam ~ .^2, data = pore)
summary(mod3)
```

### Max Stress Model

```{r}
stress <- subset(all_df, select = -c(Modulus,Surface_Area,Load_Force,Average_Pore_Diam,Density))
mod4 = lm(log(Max_Stress)  ~ .^2, data = stress)
summary(mod4)
```

### Modulus Model

```{r}
modulus <- subset(all_df, select = -c(Max_Stress,Surface_Area,Load_Force,Average_Pore_Diam,Density))
mod5 = lm(log(Modulus) ~ .^2, data = modulus)
summary(mod5)
```

# Model Comparison

Response    DOE Multiple R-squared
----------------------------------
Density     ~.^2  0.9943
Suface      ~.^2  0.9926
Pores     ~.^2    0.9424
Modulus   ~.^2    0.9897
Stress    ~.^2    0.9405


## ANOVA Models

### Density ANOVA Model
```{r}
density.aov <- aov(Density ~ .^2, data = density)
summary(density.aov)
```
### Surface Area ANOVA Model

```{r}
surface.aov <- aov(Surface_Area ~ .^2, data = surface)
summary(surface.aov)
```

### Average Pore Diameter ANOVA Model

```{r}
pore.aov <- aov(Average_Pore_Diam ~ .^2, data = pore)
summary(pore.aov)
``` 

### Max Stress ANOVA Model

```{r}
stress.aov <- aov(Max_Stress ~ .^2, data = stress)
summary(stress.aov)
``` 

### Modulus ANOVA Model

```{r}
modulus.aov <- aov(log(Modulus) ~ .^2, data = modulus)
summary(modulus.aov)
``` 

## doer R ShinyApp

This is the R code used to generate these results in doer.
Copy this into an R script and rerun to reproduce these results.

### Load skpr Package

```{r}
library(skpr)
```

### Optional: rand seed settings
Consider setting a seed to make this script fully reproducible.
Go to Advanced->Set Random Number Generator Seed, click 
the checkbox, and set Random Seed to any whole number.



### Generating candidate set:

```{r}
candidateset = expand.grid(X1 = c("0%","25%","50%"), 
                           X2 = c("15%","45%","75%"), 
                           X3 = c("di","tri","tetra"), 
                           X4 = c("16h","44h","72h"), 
                           X5 = c("50C","72.5C","90C"))
```

### Generating design:


```{r}
design = gen_design(candidateset = candidateset, 
                    model = ~X1 + X2 + X3 + X4 + X5 , 
                    trials = 28)
```

### Evaluating (Monte Carlo) Design:

```{r}
eval_design_mc(design = design, 
               model = ~X1 + X2 + X3 + X4 + X5 , 
               alpha = 0.05, 
               effectsize = 2, 
               detailedoutput = TRUE)
```

### Analysis

How to analyze this experiment when the data have been collected:
1. (to run, remove one # from this section) 
2. First, assign the results to a column in the data frame. Each 
3. entry in the vector corresponds to the result from that run in the design. 

### Construct the design matrix by adding the dependent variable or response = Density

```{r}
design$Y = density$Density
```

### Now analyze the generalized linear model with glm:

### glm with main effects only

```{r}
design.lm1 <- lm(formula = Y ~ . , data = design, 
   contrasts = list(X1 = contr.sum, X2 = contr.sum, X3 = contr.sum, X4 = contr.sum, X5 = contr.sum))
summary(design.lm1)
```


### glm with main effects and 2nd orer effects

```{r}
design.lm2 <- lm(formula = Y ~ .^2 , data = design, 
    contrasts = list(X1 = contr.sum, X2 = contr.sum, X3 = contr.sum, X4 = contr.sum, X5 = contr.sum))
summary(design.lm2)
```

Is the model a perfect fit or is it overfit?

### ANOVA Model with Main Effect only

```{r}
design.aov1 <- aov(Y~., data=design)
summary(design.aov1)
```

### ANOVA Model with main effects and 2nd orer effects

```{r}
design.aov2 <- aov(Y~.^2, data=design)
summary(design.aov2)
```

