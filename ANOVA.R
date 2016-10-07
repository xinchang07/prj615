###====================== ANOVA ====================###
library(car)
library(ggplot2)

may =c(2166, 1568, 2233, 1882, 2019)
sep =c(2279, 2075, 2131, 2009, 1793)
dec =c(2226, 2154, 2583, 2710, 2390)

calories = c(2166, 1568, 2233, 1882, 2019, 2279, 2075, 2131, 2009, 1793, 2226, 2154, 2583, 2710, 2390) 
month = c(rep("may",5), rep("sep",5), rep("dec",5)) 
calories_df = data.frame(calories,month)

ggplot(calories_df, aes(x = month, y = calories))+geom_boxplot(fill = "grey80", colour = "blue")+
  scale_x_discrete() + xlab("Time") + ylab("Caloric intake")

##====================== one-way ANOVA
## H0: all the populations are the same
## oneway.test()
oneway.test(calories ~ month, data =calories_df, var.equal = TRUE)

## oneway.test()
d =stack(list(may=may, sep=sep, dec=dec))
names(d)
oneway.test(values ~ ind, data =d, var.equal = TRUE)

#F=(sst/(k-1))/SSE/(n-k)
## aov()
#returns model object to lm - different - looking outputs
res =aov(values ~ ind, data =d) #no specification of equal variance
summary(res)

## Compare multiple differences
TukeyHSD(res,, conf.level = 0.95) #all pairwise comparision at one time by giving CIs
plot(TukeyHSD(res))

## lm()
res =lm(calories ~ month, data =calories_df)
summary(res) #F-statistics is for one-way ANOVA


##======================Contrasts  
framingham <- read.csv("C:/Users/yiton/Desktop/16 FALL/684/discussion/data/framingham.csv", stringsAsFactors=FALSE)

### stratify age
framingham$AGE_c=framingham$AGE
framingham$AGE_c[ which( framingham$AGE >= 35 & framingham$AGE < 45)] <- 1
framingham$AGE_c[ which( framingham$AGE >= 45 & framingham$AGE < 55)] <- 2
framingham$AGE_c[ which( framingham$AGE >= 55 & framingham$AGE < 65)] <- 3
framingham$AGE_c[ which( framingham$AGE >= 65 & framingham$AGE < 75)] <- 4
framingham$AGE_c = factor(framingham$AGE_c)
levels(framingham$AGE_c)

model <- aov(SYSBP ~ AGE_c, data = framingham) 
summary(model)

ggplot(framingham, aes(x=AGE_c, y=SYSBP)) + geom_boxplot(aes(group = cut_width(AGE_c, 1)))
plot(framingham$AGE_c, framingham$SYSBP) 

# tell R which groups to compare
contrasts(framingham$AGE_c) # default
summary.lm(aov(SYSBP ~ AGE_c, data = framingham)) #compare baseline level with other levels
#level one is base line, so it should be zero
c1 <- c(0,1,-1,0) #sum should be one #compare level 2 and level 3
c2 <- c(0,0,-1,1) #compare level 3 and 4
c3 <- c(0,1,0,-1) 
#c(1,1,-2,0) compare the average of level one and two with level three
# tell R the contrast matrix 
contrasts(framingham$AGE_c) <- cbind(c1,c2,c3)
summary.lm(aov(SYSBP ~ AGE_c, data = framingham)) 

# the AGE_cc1 is -6.333 , it means that the be2 level 2 and 3 is -6.33, and its significant
