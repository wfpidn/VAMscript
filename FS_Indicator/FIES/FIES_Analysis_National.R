# R Script for the Analysis of Food Insecurity Experience Scale (FIES)

# Set Working Directory
setwd("/Users/kautsartandipanga/Desktop/FIES")

# Call the required library

library(RM.weights)
library(foreign)
library(writexl)
library(expss)

# Convert data from SPSS to R format
myfies17 <- as.data.frame(read.spss("FIES17RT.sav", use.value.labels = FALSE))
# Recode code 5 in R1501 - R15008 to code 0
myfies17[,5:12][myfies17[,5:12]==5] <- 0
# Recode code 8 in R1501 - R1508 to "missing value"
myfies17[,5:12][myfies17[,5:12]==8] <- NA
# Recode code 9 in R1501 - R1508 to "missing value"
myfies17[,5:12][myfies17[,5:12]==9] <- NA
# Select data containing no missing value
myfies17 <- myfies17[complete.cases(myfies17[,5:12]),]

# National Level 2017

## Select Data
data = subset(myfies17, select = c(5:12))
## Rename the variables R1501 - R1508
colnames(data) = c("WORRIED", "HEALTHY", "FEWFOOD", "SKIPPED", "ATELESS",
                 "RUNOUT", "HUNGRY", "WHLDAY")
                 
## Select and activate the weight
wt <- myfies17$FWT
load("FIES_glob_st.RData")

## Running the Rasch Model and extract the output to file Indonesia.csv
rasch = RM.w(as.data.frame(data),wt/sum(wt)*length(wt), country = "Indonesia", write.file = T, max.it = 1000)
result = rasch

# Display the output of severity and infit value
cbind("sev" = result$b, "infit" = result$infit)
# Display the output of reliability
result$reliab.fl
# Display the residual plot
screeplot(prcomp(result$mat.res), type = "lines")

# Call FIES Global Standard
load("FIES_glob_st.RData")
tolerance = 0.35
# Adjust the infit value with the Global Standard data
adj_b = (result$b - mean(result$b))/sd(result$b)*sd(fies.global.st)+
  mean(fies.global.st)

# Show the list of variables
(common = which(abs(fies.global.st - adj_b) < tolerance))

adj_fies = (fies.global.st - mean(fies.global.st[common]))/
  sd(fies.global.st[common])*sd(result$b[common]) + 
  mean(result$b[common])
mean(adj_fies[common]) - mean(result$b[common])
sd(adj_fies[common])-sd(result$b[common])

plot(result$b, adj_fies, 
     xlim = c(2.1*min(adj_fies),1.1*max(adj_fies)), 
     ylim = c(2.1*min(adj_fies),1.1*max(adj_fies)))
points(result$b[-common],adj_fies[-common],col = "red")
text(result$b, adj_fies, names(fies.global.st),cex = 0.5, pos = 3)
abline(0,1)
title(main = tolerance, sub = result$country)

abline(v=adj_fies[c(2,8)], lty = 2)
abline(h=adj_fies[c(2,8)], lty = 2)

P_mod = 1 - pnorm(adj_fies[5], mean = result$a, sd = result$se.a)
P_mod[1] = 0
P_sev = 1 - pnorm(adj_fies[8], mean= result$a, sd = result$se.a)

#round(cbind("RS"  = 0:8, " Severity" =  result$a, 
#            "S.E." = result$se.a, P_mod, P_sev, "WN"= result$wt.rs),2)

(Prev_mod = t(P_mod)%*%result$wt.rs / sum(result$wt.rs))
(Prev_sev = t(P_sev)%*%result$wt.rs / sum(result$wt.rs))
FIES_Nat = Prev_mod*100
FIES_Nat
