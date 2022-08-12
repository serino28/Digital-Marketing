install.packages("CLVTools")
install.packages("devtools")


library("CLVTools")

### CREATE DATAFRAME FOR CLTV ANALYSIS ###

df_CLTV <- df_RFM_final

### Initialize the CLV-Object ###

clv.apparel <- clvdata(df_CLTV,  
                       date.format="ymd", 
                       time.unit = "week",
                       name.id = "ID_CLI",
                       name.date = "TIC_DATE",
                       name.price = "REVENUE")
clv.apparel
summary(clv.apparel)

### Estimate Model Parameters ###

est.pnbd <- pnbd(clv.data = clv.apparel)
est.pnbd


est.pnbd <- pnbd(clv.data = clv.apparel, 
                 start.params.model = c(r=1, alpha = 2, s = 1, beta = 2), 
                 optimx.args = list(control=list(trace=5),
                                    method="Nelder-Mead" 
                 ))

#Full detailed summary of the parameter estimates
summary(est.pnbd)

#Extract the coefficients only
coef(est.pnbd)

#Extract the confidence intervals
confint(est.pnbd)


# LogLikelihood at maximum
logLik(est.pnbd)

# Variance-Covariance Matrix at maximum
vcov(est.pnbd)



est.ggomnbd <- ggomnbd(clv.data = clv.apparel, 
                       start.params.model = c(r=0.7, alpha=5, b=0.005,  s=0.02, beta=0.001), 
                       optimx.args = list(control=list(trace=5),
                                          method="Nelder-Mead"))



### Predict Customer Behavior ###


results <- predict(est.pnbd, prediction.end = 12)
print(results)



### Plotting ###

plot(clv.apparel)
plot(clv.apparel, which="interpurchasetime")
plot(est.pnbd)

results <- results %>%
  mutate(Id = as.numeric(Id))

results_final <-results[,c(1,8,9)]


summary(results_final)



