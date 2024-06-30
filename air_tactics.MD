---
title: "Force-Level Encounter Assessment"
author: "Jeffrey Strickland"
date: "2024-05-13"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, dpi = 300)
```

## Force-Level Encounter Assessment

Frequently, military testers encounter the problem of engaging in simulated combat operations against an "aggressor" adversary to determine methods of employing some new system or capability—tactics development. In the Air Force, force sizes range from one versus one to 50–75 aircraft encounters ("many vs. many") in the periodic Red Flag exercises outside Las Vegas, Nevada. Valiant Shield, a June 2006 exercise, involved 22,000 personnel, 280 aircraft, and more than 30 ships (including three aircraft carriers and their strike groups) in the Paciﬁc Ocean and sur-rounding lands.

Such large-scale force encounters offer appropriate scale to realistically exercise military systems against an unpredictable thinking adversary. In this sense, exercises are the best simulation of combat short of war. On the other hand, large-scale encounters are unwieldy, noisy, and offer fewer battles as experimental units than smaller force exercises. Experimental controls may restrict tactical free-play, thus hindering ﬁghting force training. Nevertheless, force exercises are an important opportunity to test our military systems and tactics in an environment far too expensive for any single military test activity to afford on its own. This case illustrates effective experimentation in the midst of large force exercises. This case was adapted from McAllister’s dissertation research (2003) concerning tactical employment of ﬁghters. Air Force doctrine calls for rapidly establish-ing air supremacy—the unrestricted use of air and space—while denying it to the adversary. For the case study, eight friendly (traditionally "Blue") ﬁghters with modern sensors, weapons, and communications contest the airspace with eight adversary ("Red") ﬁghters. Engagements of this size are typical of air combat exercises such as Red Flag.

Figure 1 illustrates some possible input and output conditions for the engagement. The Appendix contains more complete lists. "SA" refers to the gold standard of air combat: situational awareness—accurately knowing where friends and enemies are. Lack of (or loss of) SA is frequently a terminal condition in air combat.

The output measures count the losses on both sides and the exchange ratio. Combat exchange ratios have a long history and useful interpretations but are uninformative if the losses are zero on either side. McAllister (2003) considered three adjustments to the exchange ratios to deal with these problems.

On the input side, some discussion is in order. Rules of engagement (ROE) specify the conditions under which a ﬁghter is authorized to engage and destroy another aircraft. Rules of engagement may range from loose—allowing the destruction of any aircraft not positively identiﬁed to be friendly (a relatively quick process)—to tight ROE calling for clos-ing the target for positive visual identiﬁcation. Looser ROE allow sensors and missiles to be employed at maximum range (usually to Blue’s advantage), whereas tighter ROE delay missile ﬁrings considerably. Radar jammers are employed to mask own-side aircraft from the enemy. This condition counts the number of dedicated stand-off jamming aircraft available to the Red forces. Blue supporting assets refers to the number of airborne early warn-ing, command and control, and intelligence aircraft available to the Blue side. Finally, the Red and Blue tactics options are inserted in the experiment in an attempt to answer whether one Blue tactic is universally superior to the other and whether Red’s choice of tactics should inﬂuence Blue’s tactical choices. As an illustration of such tactics, consider Figure 6 and the two notional tactics developed for the Blue forces.

A prime tenant of modern air warfare is to avoid closing (merging) with the adversary and engaging in what is popularly known as a dogﬁght. Such turn-ing engagements nullify superior U.S. weapons and sensors, putting even relatively unsophisticated opponents in a position from which they may be able to destroy Blue aircraft. With the Lead-Trail tactic, one pair of ﬁghters is always positioned to engage the adversary while the other turns away to maintain stand-off distance from the adversary. With the Line-Abreast tactic, all four shooters are available for the initial salvo, maximizing the number of ﬁrst-shot missiles in the air. The drawback to line abreast is that all four ﬁghters turn away simultaneously, increasing the risk of a dogﬁght when Blue ﬁghters turn back into the engagement.

### Choice of Experimental Designs and Data Generation

As originally stated, the objective is to determine whether any tactical choices are superior for the Blue forces across an array of typical combat encounters. In line with G. E. P. Box’s (1999) advice on sequential experimentation referenced earlier, the experiment begins with a fractional factorial screening design1 with ﬁve factors, each at two levels (originally it was $(2^3)(3^2)$: a fraction requiring 16 trials and yielding excellent information on the ﬁve main effects and 10 two-factor interactions.

The design table and constructive response data are provided in Tables 1 and 2. The ROE values represent the number of seconds typically required for a positive identiﬁcation under the two rule sets; both Red and Blue supporting aircraft are represented by numeric counts, and the Red=Blue tactics choices are designated by the closest approach of the two adversary forces, with "0" representing a possible merge and resulting dogﬁght between Red and Blue ﬁghters.

The simulated data shown in Table 2 were generated by an Excel Monte Carlo simulation created some years ago. The simulation has been used to produce sample data for classroom instruction, tactics development planning discussions, and a variety of technical papers (McAllister 2003 is an example). The Excel simulation emulates up to four missile exchanges between Red and Blue forces. It ends when the simulated missiles are exhausted or one force loses 50% of their aircraft.

### Full Factorial Design

If we were to analyze all five factors under homogeneous conditions, we would have to provide a full factorial design. In this case, it would be a $(2^3)(3^2)$ ot 72 runs. Yet, this same design can be easily converted into a 2k by converting the 3 level factors into two 2-level factors. For our experiment, the Red Jammers variable with three counts was divided into low count level (count = 0 or 1) and a high count level (count = 2)
and Blue Support AC variable, with three counts, was divided into low count level (count = 2 or 4) and a high level (count = 8).

Table 1. Design factors and levels

Factor	|Name			|Units		|Type		|Design Value
*	A	|ROE_t_ID		|seconds	|Numeric	|10,60
*	B	|Red_Jammers	|count		|Numeric	|0,2
*	C	|Blue_spt_AC	|count		|Numeric	|2,8
*	D	|Red_Tactic		|nm			|Numeric	|0,5 
*	E	|Blue_Tactic	|nm			|Numeric	|0,5          

### Load FrF2 package & Kill Ratio Data

```{r}
library(FrF2)
k_ratio <- read.csv("C:\\Users\\jeff\\Documents\\Data\\kill_ratio.csv")
```

### Load the Yield Factors

```{r}
yield_factors <- read.csv("C:\\Users\\jeff\\Documents\\Data\\kill_ratio.csv")
colnames(yield_factors) <- c("Factor","A","B", "C","D","E","Y")
yield_factors
```

### Fractional Factorial Design 1

By using the R package 'FrF2' we can determine the fractional factorial design. The highest resolution for this experimental design is Resolution III, with 5 factors and 16 runs.

```{r}
runs <- 2^(6-2)
nam2 <- c("A","B","C","D","E")
frac_design <- FrF2(runs, factor.names = nam2 , default.levels = c("0","1"))
summary(frac_design)
```

Next, we place the data into a dataframe using the R data frame function.

```{r}
frac1 <-data.frame(frac_design)
```

As observed, the aliasing structure of the fractional factorial design can be obtained and the generators of this design, A=A, B=B, C=C, D=D, and E=E. In this particular experimental design, the generator of the fractional factorial design is E=ABCD, that is, the main effect corresponding to the factor Blue_Tactic, This specific structure also suggests that there are no aliasing among main affects and second order affects.

```{r}
aliasprint(frac_design)
```

### Fractional Factorial Design 2

By using the aliased interactions and multiplying by their main effects, the generator can also be determined as I=ABD=ACE=BCE. By adding this generator row to the matrix, it can be corroborated that they all equal the generator column:

```{r}
frac2_design <- FrF2(runs, factor.names = c("A","B","C","D","E"), default.levels = c("-1","1"))
frac2 <-data.frame(frac2_design)
frac2["I"] = 1
```

### Randomization,Blocking and Replication

It is important to note the principles that guide experimental design: randomization, blocking and replication. In this experiment, the runs shown above must be completely randomized. That means, they should be randomly selected, assigned and executed. Replication and blocking increases the precision of the results. Yet, in this experimental study we will not be using replication as the purpose is to have reduced number of experimental runs. If we decided to increase the number of runs for some reason, we might be better off generating a higher resolution design, such as a 2^(6−2).

### Adding Response Variable to the Design

The next step in the design is to gather the data. As stated previously this has to be randomly selected and the experimental runs must be done in a random order

As a first step, given that in this case we have a database from where to obtain the data, the data is allocated as samples that match the different factor levels:

For our fractional factorial design the order of experimental runs was developed as follows:

### Randomization for First Design

```{r}
rand_fd <- k_ratio[sample(nrow(frac1)),]
rand_fd
```
### Randomization for Second Design

```{r}
rand_fd2 <- k_ratio[sample(nrow(frac2)),]
rand_fd2
```

### Model Comparison

We compared models two ways; First we compared the main effects and main effects with second order effetcs, Second, we compared the models using the two diffeent designs (frac1 and frac2). After comparing the outcome, we remodeled using the main effects A, B, C, D, E nad the second order interactions A:C, A:E, B:D, and C:D. We then creetaed two models, a linear model and an ANOVA model, using the second design.

```{r}
mod3 <- lm(Kratio ~ A+B+C+D+E+A:C+A:E+B:D+C:D, data=rand_fd)
summary(mod3)
anova(mod3)
```

```{r}
aov3 <- aov(Kratio ~ A+B+C+D+E+A:C+A:E+B:D+C:D, data=rand_fd)
summary(aov3)
anova(aov3)
```

### Estimation

The model can be further tested and the main and interaction effects can be estimating using a linear model:

```{r}
fit1 <- lm(Kratio ~ C+D+E+A:E+B:D, data=rand_fd2)
summary(fit1)
```

### Final Modeling & Analysis

As observered from this first model results, the main effects concerning the ROE_t_ID and Red_Jammers were not statistically significant. On the other hand, Blue Tactics, Bule_Spt_AC, and Red_Tactic appears to be significant with the the kill ratio. Analysis of variance performed indicated significant differences among the Blue Support Aircraft, Red Tactics Choice, and ROE. 

With respect to interaction effects, there are only two interaction effects that seems to be statistically significant: (1) the is the effect of ROE and Blue Tactics Choice, and (2) Red Jammers and Red Tactics Choice..

If we reduce the model to the significant interactions Blue Tactics Choice, Blue Support Aircraft, and ROE the resulting model is as follows:

```{r}
fit2 <- lm(k_ratio$Kratio ~ (k_ratio$E + k_ratio$C:k_ratio$E+ k_ratio$C:k_ratio$E:k_ratio$A))
summary(fit2)
```

```{r}
fit3 <- aov(k_ratio$Kratio ~ (k_ratio$E + k_ratio$C:k_ratio$E+ k_ratio$C:k_ratio$E:k_ratio$A))
summary(fit3)
anova(fit3)
```

This second model seems to be more accurate although it can be even reduced some more as only two interaction effects appear to be significant


### Calculate Main Effects

The numerical results of these main effects are provided in this report for ROE, Red Jammers, Blue Supporting Aircraft, Red Tactics Choice, and Blue Tactics Choice. Main effects calculated were as follows:

```{r}
me_roe <- mean(subset(k_ratio$Kratio, k_ratio$A == "10")) - mean(subset(k_ratio$Kratio, k_ratio$A == "60"))

me_red_jam <- mean(subset(k_ratio$Kratio, k_ratio$B == "0")) - mean(subset(k_ratio$Kratio, k_ratio$B == "2"))

me_blue_spt <- mean(subset(k_ratio$Kratio, k_ratio$C == "2")) - mean(subset(k_ratio$Kratio, k_ratio$C == "8"))

me_red_tac <- mean(subset(k_ratio$Kratio, k_ratio$D == "0")) - mean(subset(k_ratio$Kratio, k_ratio$D == "5"))

me_blue_tac <- mean(subset(k_ratio$Kratio, k_ratio$E == "0")) - mean(subset(k_ratio$Kratio, k_ratio$E == "5"))
```

### Print Main Effects

```{r,	echo=FALSE}
print(paste("ROE_t_ID    =", me_roe))
print(paste("Red_Jammers =", me_red_jam))
print(paste("Blue_spt_AC =", me_blue_spt))
print(paste("Red_Tactic  =", me_red_tac))
print(paste("Blue_Tactic =", me_blue_tac))
```

### Interaction Plots

```{r, echo=FALSE}
#ROE & Red Jammers
interaction.plot(response=k_ratio$Kratio, k_ratio$A, k_ratio$B,  type = "b", pch=c(18,20), leg.bty="o", lwd = 2, col=c("purple", "orange"), main = "ROE & Blue Spt Aircraft")

#ROE & Blue Spt Aircraft
interaction.plot(response=k_ratio$Kratio, k_ratio$A, k_ratio$C,  type = "b", pch=c(18,20), leg.bty="o", lwd = 2, col=c("purple", "green2"), main = "ROE & Red Jammers")

#ROE & Red Tactics
interaction.plot(response=k_ratio$Kratio, k_ratio$A, k_ratio$D,  type = "b", pch=c(18,20), leg.bty="o", lwd = 2, col=c("purple", "red"), main = "ROE & Red Tactics")

#ROE & Blue Tactics
interaction.plot(response=k_ratio$Kratio, k_ratio$A, k_ratio$E,  type = "b", pch=c(18,20), leg.bty="o", lwd = 2, col=c("purple", "blue"), main = "ROE & Blue Tactics")

#Red Jammers & Blue Spt Aircraft
interaction.plot(response=k_ratio$Kratio, k_ratio$B, k_ratio$C,  type = "b", pch=c(18,20), leg.bty="o", lwd = 2, col=c("green2", "orange"), main = "Red Jammers & Blue Tactics")

#Red Jammers & Red Tactics
interaction.plot(response=k_ratio$Kratio, k_ratio$B, k_ratio$D,  type = "b", pch=c(18,20), leg.bty="o", lwd = 2, col=c("green", "red"), main = "Red Jammers & Red Tactics")

#Red Jammers & Blue Tactics
interaction.plot(response=k_ratio$Kratio, k_ratio$B, k_ratio$E,  type = "b", pch=c(18,20), leg.bty="o", lwd = 2, col=c("green2", "blue"), main = "Red Jammers & Blue Tactics")

#Blue Spt Aircraft & Red Tactcs
interaction.plot(response=k_ratio$Kratio, k_ratio$C, k_ratio$D,  type = "b", pch=c(18,20), leg.bty="o", lwd = 2, col=c("orange", "red"), main = "Blue Spt Aircraft & Red Tactcs")

#Blue Spt Aircraft & Blue Tactics
interaction.plot(response=k_ratio$Kratio, k_ratio$C, k_ratio$E,  type = "b", pch=c(18,20), leg.bty="o", lwd = 2, col=c("orange", "blue"), main = "Blue Spt Aircraft & Blue Tactics")

#Blue Tactics & REd Tactics
interaction.plot(response=k_ratio$Kratio, k_ratio$D, k_ratio$E,  type = "b", pch=c(18,20), leg.bty="o", lwd = 2, col=c("red", "blue"), main = "Red Tactics & Blue Tactics")
```