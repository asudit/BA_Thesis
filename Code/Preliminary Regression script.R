setwd("~/Users/Adam/Research/BA_Thesis/Data")
library("openxlsx")
library(foreign, gplots)
#Panel <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/preliminary_merge.xlsx", 1)
Panel <- read.xlsx("/Users/Adam/Research/BA_Thesis/Data/regression_var.xlsx", 1)
library(plm)

panel_elements <- Panel[c('Year', 'ID.code')]
dup <- data.frame(duplicated(panel_elements))

new_panel = data.frame(Panel, dup)
Panel <- new_panel[new_panel$duplicated.panel_elements. == F,]
#Panel <- Panel[, c(4:10)]
#Panel[, 4] <- sapply(Panel[,4], as.factor)
#need it to be numeric
#Panel <- sapply(Panel, as.numeric)

#banks_sus <- Panel[, 201]
#banks_total <-Panel[, 202]
#banks_sus_ratio <- banks_sus/banks_total * 100
#Panel$bank_sus_norm <- as.numeric(as.character(Panel$bank_sus_norm))
var_interest <- Panel$bank_sus_norm
var_interest <- as.numeric(var_interest)

val_added  <- Panel$Total.value.of.products
ID <- Panel$ID.code
Year <- Panel$Year
county <- Panel$County
subsidiary <- Panel$Branch.or.subsidiary.of.other.firm
#labor <- Panel$Man.hours..total
labor <- Panel$Wage.earners.by.months..total
#fixed_capital <- Panel$Total.HP
#variable_capital <- Panel$Cost.of.all.materials.and.raw.stock.actually.used
capital <- Panel$Total.cost.of.materials..fuel..and.electric.cost.sum.of.f001..f002..f003.
#Nanda gets the credit
Post <- Panel$Post_1929


#issue -- the merge with FDIC did not work as well as hoped. lots of blanks
#fixed <- plm(val_added ~ var_interest + labor + fixed_capital + variable_capital, data=Panel, index=c('ID.code', 'Year'), model="within")
fixed <- plm(val_added ~  Post*var_interest  +  labor + capital, data=Panel, index=c('ID.code', 'Year'), model="within")
#fixed <- plm(val_added ~  Post*var_interest, data=Panel, index=c('ID.code', 'Year'), model="within")
summary(fixed)
random <- plm(val_added ~ banks_sus_ratio + labor + capital, data=Panel, index=c('ID.code', 'Year'), model="random")

#test autocorrelation in variables -- from http://stats.stackexchange.com/questions/14914/how-to-test-the-autocorrelation-of-the-residuals
#mod = lm(val_added ~ fixed_capital)
#res = mod$res 
#n = length(res) 
#mod2 = lm(res[-n] ~ res[-1]) 
#summary(mod2)
# autocorrelation in residuals definitely seems to be happening....

#test serial correlation
pbgtest(fixed) #oh crap yea there's serial correlation

#cluster standard errors on county level--taken from https://diffuseprior.wordpress.com/2012/06/15/standard-robust-and-clustered-standard-errors-computed-in-r/

#get X matrix/predictors
X <- model.matrix(fixed)
# number of obs
n <- dim(X)[1]
# n of predictors
k <- dim(X)[2]
# calculate stan errs as in the above
# sq root of diag elements in vcov
se <- sqrt(diag(solve(crossprod(X)) * as.numeric(crossprod(resid(fixed))/(n-k))))

# cluster name
cluster <- "County"
clus <- cbind(X,Panel[,cluster],resid(fixed))
colnames(clus)[(dim(clus)[2]-1):dim(clus)[2]] <- c(cluster,"resid")
# number of clusters
m <- dim(table(clus[,cluster]))
dfc <- (m/(m-1))*((n-1)/(n-k))
# uj matrix
uclust <- matrix(NA, nrow = m, ncol = k) #took out the NA, left it blank
gs <- names(table(Panel[,cluster]))
#set i =1 ; convert county to factor then run it
for(i in 1:m){
  uclust[i,] <- t(matrix(clus[clus[,cluster]==gs[i],k+2])) %*% clus[clus[,cluster]==gs[i],1:k] 
}
se <- sqrt(diag(solve(crossprod(X)) %*% (t(uclust) %*% uclust) %*% solve(crossprod(X)))*dfc








