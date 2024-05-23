library(nlme)
library(car)
data <- read.csv("/Users/nzeyimanajanvier/Desktop/ITS/nile.csv",
                 header = T)
View(data)

# FIRST VISUALIZATION 
plot(data$time,data$flow,
     main="Nile annual flow",
     ylab="Nile Flow",
     ylim=c(0,4500),
     xlab="Year",
     type="l",
     col="red",
     xaxt="n")
# ADD X-AXIS YEAR LABELS
axis(1, at=1:60, labels=data$year )

# ADD IN THE POINTS FOR THE FIGURE
points(data$time,data$flow,
       col="red",
       pch=20)

# LABEL THE WEATHER CHANGE
abline(v=27.5, lty=2)

# CREATE A MODEL OLS
model_ols <- lm(flow ~ time + level + trend, data=data)

# SEE SUMMARY OF THE MODEL
summary(model_ols)

# GET CONFIDENCE INTERVAL FOR THE COEFFICIENTS
confint(model_ols)

# CHECK FOR AUTOCORRELATION
# RUN THE DURBIN WATSON TEST
dwt(model_ols,max.lag=12,alternative="two.sided")

# PLOT RESIDUALS FROM OLS MODEL
plot(data$time, residuals(model_ols),
     type="o",
     pch=16,
     xlab="Time",
     ylab="OLS Residuals")

# SET PLOTTING TO TWO TWO RECORDS PER PAGE
par(mfrow=c(2,1))

# PRODUCE PLOTS
acf(residuals(model_ols))
acf(residuals(model_ols, type="partial"))

# FIT A FINAL MODEL
model_p10 <- gls(flow ~ time + level + trend,, 
                 data=data,
                 correlation = corARMA(p=10, form=~time),
                 method="ML") 
summary(model_p10)

# DIAGNOSTIC CHECKS
model_p11 <- update(model_p10, correlation=corARMA(p=11,form=~time))
anova(model_p10, model_p11)

par(mfrow=c(1,1))
# PLOT THE RESULTS
plot(data$time,data$flow,
     ylab="Water Flow",
     ylim=c(0,4500),
     xlab="Year",
     pch=20,
     col="pink",
     xaxt="n")

# ADD X-AXIS YEAR LABELS
axis(1, at=1:60, labels=data$year )

# LABEL THE WEATHER CHANGE
abline(v=27.5, lty=2)

# PLOT THE FIRST LINE SEGMENT
lines(data$time[1:27], fitted(model_p10)[1:27], col="red", lwd=2)

# PLOT THE SECOND LINE SEGMENT
lines(data$time[28:60], fitted(model_p10)[28:60], col="red", lwd=2)

# PlOT THE COUNTERFACTUAL LINE SEGMENT
segments(1,
         model_p10$coef[1]+model_p10$coef[2],60,
         model_p10$coef[1]+model_p10$coef[2]*60,
         lty=2,
         lwd=2,
         col="red")

# PREDICTED VALUE AT 25YEARS AFTER THE WEATHER CHANGE
pred <- fitted(model_p10)[52]

# ESTIMATED COUNTERFACTUAL
cfac <- model_p10$coef[1]+model_p10$coef[2]*52

# ABSOLUTE CHANGE AT 25 YEARS
pred - cfac

# RELATIVE CHANGE AT 25 YEARS
(pred - cfac)/cfac
