##############################################

#Regression with MICE
library(mice)
library(dplyr)
##Data.frame with missing data named as [df_nomi]

##explore data for # of complete cases 
#% incomplete cases should be the # of multiple imputations 
table(complete.cases(df_nomi))
perc_complete.cases <- round(table(complete.cases(df_nomi))/nrow(df_nomi), 2)*100 
perc_complete.cases

count_missing <- function(x){sum(is.na(x))}
count_vars <- apply(df_nomi, 2, count_missing)
count_vars #count of missing for each variable

perc_missing <- function(x){sum(is.na(x))/length(x)*100}
perc_vars_missing <- apply(df_nomi, 2, perc_missing)
mean(perc_var_missing) #mean %missing across all variables 
perc_var_missing #%missing for each variable 

#implement multiple imputation with mice package 
#citation: van Buuren S, Groothuis-Oudshoorn K (2011). “mice: Multivariate Imputation by Chained Equations in R.” Journal of Statistical Software, 45(3), 1-67. doi:10.18637/jss.v045.i03. 


#initial set up 
initial_imp <- mice(df_nomi,maxit=0) 
pred1 <- initial_imp$predictorMatrix 
pred1[, c("[insert variables not to be included in imputation]")] <- 0  #e.g. participant IDs

#run mice model
#m reflect % of incomplete cases from above
imp_model <-mice(df_nomi, m=25,  maxit = 10,  pred = pred1,  seed = 1234) 
imp_model$meth #check that numeric = pmm, ordinal categorical = polyreg 

#assess imputations 
densityplot(imp_model)
plot(imp_model) 


#pooling descriptive statistics for [var] to compare with df_nomi descriptives 
# mids converted to long format data frame with only imputed datasets
imp_model_long <- complete(imp_model,"long",include = F) 

#mean, sd, mean variation 
mn_var1 <- tapply(imp_model_long$var1, imp_model_long$.imp, mean) 
mean(mn_var1)
sd_var1 <- tapply(imp_model_long$var1, imp_model_long$.imp, sd) 
mean(sd_var1)
vari_mn_var1 <- tapply(imp_model_long$var1, imp_model_long$.imp, var) / nrow(imp_model$data)
mean(vari_mn_var1)

# also checking variance between  [var] in each imputed dataset 
var(mn_var1)

#descriptive freq and percentages for categorical variables
#counts 
count_var1 <- with(imp_model_long, by(imp_model_long, .imp, function(x) 
  c(table(x$ cat_var1== 1))))#change according to category in question
round(Reduce("+", count_var1)/length(cat_var1),2)

#%percentages
perc_var1 <- with(imp_model_long, by(imp_model_long, .imp, function(x) 
  c(prop.table(table(x$cat_var1 == 1)))))
round(Reduce("+", perc_var1)/length(cat_var1),2)


##regression analysis 
#include potential interaction terms
m <- with(imp_model, lm( response ~  predictor + covariates))

summary(pool(m), conf.int = TRUE)
pool.r.squared(m)


###############################################