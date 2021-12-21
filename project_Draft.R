library(ggplot2)
library(dplyr)
library(gridExtra)
library(corrplot)

diabetes <- read_excel("C:/Users/dashi/Desktop/Jaya_Rutgers/diabetes-data-for-project.xlsx")
dim(diabetes) #dimension of dataset 
head(diabetes, 10) #loaded the data into R and viewing into how it looks like.
# Dimension
dim(diabetes)

# Number of columns
ncol(diabetes)

# Number of rows
nrow(diabetes)

colnames(diabetes)  #name of columns

summary(diabetes)  #summary of dataset

#datacleaning -bp.2s and bp.2d columns as it has mostly missing /time.ppn

diabetes = diabetes[, -which(colnames(diabetes) %in% c( "bp.2s", "bp.2d", "time.ppn"))]

summary(diabetes)

colSums(is.na(diabetes)) -- No missing data

hist(diabetes$stab.glu, xlab = "Stabilized Glucose concentration in blood", main = "")
abline(v = summary(diabetes$stab.glu)[2:5], col = c("blue", "red", "black", "orange"), 
       lty = 2)

boxplot(diabetes$gender, xlab = "glycosylated hemoglobin", horizontal = T)
abline(v = summary(diabetes$glyhb)[2:5], col = c("blue", "red", "black", "orange"), 
       lty = 2)

mean(diabetes$chol)
median(diabetes$chol)
quantile(diabetes$chol)

plot(diabetes$age, diabetes$glyhb, pch = 20, xlab = "age", ylab = "Glycosylated hemoglobin")
## compute the Pearson correlaton between cholestrol and glyhb
cor(diabetes$chol, diabetes$glyhb, method = "pearson")
cor(diabetes$chol, diabetes$glyhb, method = "spearman")

## compute the Pearson correlaton between age and glyhb
cor(diabetes$age, diabetes$glyhb, method = "pearson")
cor(diabetes$age, diabetes$glyhb, method = "spearman")

## compute the point biserial correlaton between gender and glyhb.We will recode gender as it is categorical now 
diabetes$gender.Dummy<-ifelse(diabetes$gender=="male",1,0)
colnames(diabetes)
summary(diabetes)


cor.test(diabetes$gender.Dummy, diabetes$glyhb)


diabetes1 = diabetes[, c(8, 6, 11, 9, 10, 14, 15, 2,7, 5, 1, 3, 4, 12, 13)] #reordering the colunns

cor.mat = cor(diabetes1[, 4:ncol(diabetes1)], method = "pearson") 
round(cor.mat, 2)  # all paiwise correlations

par(mar = c(3, 3, 0.5, 0.5), mgp = c(1.5, 0.5, 0), las = 2)
pairs(diabetes1[, 4:ncol(diabetes1)], pch = 20, cex = 0.5, col = "grey")

with(diabetes1, table(glyhb, gender))
with(diabetes1, table(frame, gender))

with(diabetes1, table(glyhb, gender))
ggplot(diabetes1, aes(x = glyhb, fill = gender)) + geom_bar()

library(ggplot2)
with(diabetes1, table(glyhb, gender))
ggplot(diabetes1, aes(x = glyhb, fill = gender)) + geom_bar()

ggplot(diabetes1, aes(x = glyhbl)) + geom_histogram(binwidth = 2) + facet_grid(gender ~
                                                                                .)
ggplot(diabetes1, aes(x = glyhb, gender))


diabetes1$gender.Dummy<-ifelse(diabetes1$gender=="Male",1,0)

##The code below estimates a logistic regression model using the glm (generalized linear model) function. First, we convert rank to a factor to indicate that gender.dummy should be treated as a categorical variable.
diabetes$gender.Dummy <- factor(diabetes$gender.Dummy)

mylogit1 = lm(glyhb~age, data=diabetes)
summary(mylogit1)

mylogit2 = lm(glyhb~gender.Dummy, data=diabetes)
summary(mylogit2)

mylogit3 = lm(glyhb~chol, data=diabetes)
summary(mylogit3)

mylogit4 = lm(glyhb~chol+age, data=diabetes)
summary(mylogit4)

mylogit5 = lm(glyhb~chol+gender.Dummy, data=diabetes)
summary(mylogit5)

mylogit6 = lm(glyhb~age+gender.Dummy, data=diabetes)
summary(mylogit6)

mylogit7 = lm(glyhb~age+gender.Dummy+chol, data=diabetes)
summary(mylogit7)
multiple.regression <- lm(glyhb ~ age + gender.Dummy)
View(multiple.regression)
summary(multiple.regression)
