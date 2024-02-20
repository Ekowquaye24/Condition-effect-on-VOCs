# install.packages("heavy")
# install.packages("readxl")
# install.packages("car")
# install.packages("robustbase")

setwd("/media/wsadmin/My Passport/Kiana/2022.07.29/")

set.seed(1)

## Settings
do.preprocess.flag       = TRUE
do.3d.plots.flag         = TRUE
do.diagnostic.plots.flag = TRUE
do.prediction.plots.flag = TRUE

sheet.name = "SOPThaw" # "SOPUrine" or "SOPThaw" or "SOPFast"

## Tuning

sample.exclusion.list = c("SOP2_25_0_Blank TDT")
# VOC.short.list = c("129086-73-3", "101100-38-3", "1000272-50-0", "1000215-25-2", "020548-62-3",
#                    "003557-49-1", "003555-47-3", "001120-21-4",  "001020-31-1",  "000995-83-5",
#                    "000822-67-3", "000472-41-3", "000292-64-8",  "000124-19-6",  "000050-28-2")
# 
# VOC.short.list = c("001120-21-4")

VOC.short.list = c() # go with the full list

## Preprocess the data or load preprocessed data directly

if (do.preprocess.flag) {
  # load up
  df = readxl::read_excel("KLH ALL SOP 072322.xlsx", sheet = sheet.name)
  df = as.data.frame(df)
  
  # drop unnecessary variables and rename
  df = df[, c("CAS Number", "Sample Name", "Area Ratio", "Sample Temperature", "Sample Duration")]
  colnames(df) = c("CAS", "sample", "area", "temperature", "duration")
  
  # clean up the variables
  df[, "temperature"] = factor(df[, "temperature"], 
                               levels = c("80", "20", "4"), 
                               labels = c("-80",  "-20",  "4"))
  df[, "temperature"] = as.numeric(as.character(df[, "temperature"]))
  
  #I = which(is.na(df[, "amount"]))
  #df[I, "amount"] = 190
  #rm("I")
  
  df[, "duration"] = factor(df[, "duration"], 
                            levels = c(1, 2, 3),
                            labels = c(1, 2, 3))
  df[, "duration"] = as.numeric(as.character(df[, "duration"]))
  
  # Exclude certain samples
  I = rep(FALSE, nrow(df))
  for (i in 1:nrow(df))
  for (token in sample.exclusion.list) {
    I[i] = I[i] || length(grep(token, df$sample[i]) > 0)
  }
  df = df[!I, ]
  df$sample = NULL
  
  # Only include VOCs from the short list
  if (length(VOC.short.list) > 0) {
    I = rep(FALSE, nrow(df))
    for (i in 1:nrow(df))
    for (token in VOC.short.list) {
        I[i] = I[i] || length(grep(token, df$CAS[i]) > 0)
    }
    df = df[I, ]
    df$CAS = NULL
  }
  
  write.csv(df, file = paste("data-", sheet.name, ".csv", sep = ""), row.names = FALSE)
} else {
  df = read.csv(paste("data-", sheet.name, ".csv", sep = ""))
}

## Transform variables (define new features based on original attributes)

raw.df = df
area.min = 1E-3

#shift.constant = NA

def.features <- function(df, include.response = TRUE) {
  if (include.response) {
    y = df$area
    y[which(y < 0)] = area.min
    y = log10(y)
    
    # assign("shift.constant", min(y), envir=.GlobalEnv)
    # y = y - min(y) + area.min
    # y = log10(y)
  }
  
  df = data.frame(x1 = df$temperature,
                  x2 = df$duration)
  
  if (include.response) {
    df = cbind(y, df)
    colnames(df)[1] = "y"
  }
  
  return(df)
}

df = def.features(raw.df)

## Fit a linear regression model y vs. 3-rd degree polynomials in x1, x2, x3

model = lm(y ~ polym(x1, x2, degree = 2, raw = TRUE), data = df) # center and scale all predictors
print(summary(model)) # display summary
eps = model$residuals

## 3D plots - check output pdf files

if (do.3d.plots.flag) {
  library("plot3D")
  
    I = sample(1:nrow(df), 1000L)
    
    grDevices::pdf(file = paste("3d.plot.persp.pdf", sep = ""))
    
    x1 = 0.5*(min(df[, "x1"]) + max(df[, "x1"])) + c(-0.6, 0.6)*(max(df[, "x1"]) - min(df[, "x1"]))
    x2 = 0.5*(min(df[, "x2"]) + max(df[, "x2"])) - c(-0.6, 0.6)*(max(df[, "x2"]) - min(df[, "x2"]))
    
    x1.grid = seq(x1[1], x1[2], length.out = 20L)
    x2.grid = seq(x2[1], x2[2], length.out = 20L)
    
    grid = expand.grid(x1 = x1.grid, x2 = x2.grid)
    
    resp = matrix(predict(model, newdata = grid), nrow = 20L, ncol = 20L)
    
    res = plot3D::scatter3D(x = df[I, "x1"], y = df[I, "x2"], z = df[I, "y"],
                            surf = list(x = x1.grid, y = x2.grid, z = resp, facets = NA),
                            xlab = "x1 (temperature)", ylab = "x2 (duration)", zlab = "y (log10(area))",
                            main = paste("Scatterplot and response surface"), color = rgb(0, 0, 1, 0.1))
    grDevices::dev.off()
    
    grDevices::pdf(file = paste("3d.plot.isometric.pdf", sep = ""))
    res = scatterplot3d::scatterplot3d(x = df[I, "x1"], y = df[I, "x2"], z = df[I, "y"],
                                       xlab = "x1 (temperature)", ylab = "x2 (duration)", zlab = "y (log10(area))",
                                       main = paste("Scatterplot and response surface"), color = rgb(0, 0, 1, 0.1))
    f <- function(x1, x2) {predict(model, newdata = data.frame(x1 = x1, x2 = x2))}
    res$contour3d(f, x.count = 20, y.count = 20)
    grDevices::dev.off()
}

## Post hoc diagnostic plots

if (do.diagnostic.plots.flag) {
  # Check for nonlinear trends in the residual - no strong trends seem to be present (ok!)
  grDevices::pdf(file = "residual.trend.plot.pdf")
  plot(model, which = 1) 
  grDevices::dev.off()
  
  # Check for heteroskedasticity - no strong heteroskedasticity seems to be present (ok!)
  grDevices::pdf(file = "heteroskedasticity.plot.pdf")
  plot(model, which = 3)
  grDevices::dev.off()
  
  # Check for Gaussianity of the residuals:
  # The residuals seem fairly Gaussian, definitely not heavy-tailed.
  # Due to the large sample size, the Central Limit Theorem (CLT) remains valid.
  # Thus, the quality of estimation is not compromised.
  # For prediction pursposes, we still go for a non-parameteric approach and do not rely on non-Gaussianity.
  
  grDevices::pdf(file = "residual.density.plot.pdf")
  plot(density(sample(model$residuals, 5000L)), main = "Residuals")
  legend("topleft", legend = c("Empirical density", "Non-robust normal fit", "Robust normal fit"), 
         lty = c(1, 1, 1), col = c("black", "red", "blue"))
  
  xbar = mean(model$residuals)
  s = sd(model$residuals)
  x = seq(-5*s, 5*s, length.out = 500L)
  lines(x, dnorm(x, mean = xbar, sd = s), col = "red")
  
  mcd = robustbase::covMcd(model$residuals)
  xbar = mcd$center
  s = as.numeric(sqrt(mcd$cov)*0.88)
  x = seq(-5*s, 5*s, length.out = 500L)
  lines(x, dnorm(x, mean = xbar, sd = s), col = "blue")
  grDevices::dev.off()
}

## Deploy the model for prediction

conf.level = 0.9 # confidence/prediction level

phi <- function(y) 10^y

# Reference conditions ("golden standard")
raw.ref.df = data.frame(temperature =  25, duration = 1)
# Alternative conditions
raw.new.df = data.frame(temperature = -80, duration = 1)

ref.df = def.features(raw.ref.df, include.response = FALSE)
new.df = def.features(raw.new.df, include.response = FALSE)

pred.ref = predict(model, newdata = ref.df, interval = "confidence", level = conf.level)
pred.new = predict(model, newdata = new.df, interval = "confidence", level = conf.level)

pred.ref.area = phi(eps + pred.ref[1])
pred.new.area = phi(eps + pred.new[1])

# Text summary

cat("\n")
cat("Reference conditions: ", "temperature = ", raw.ref.df$temperature, ", ",
                              "duration = ", raw.ref.df$duration, "\n", sep = "")
cat(conf.level*100.0, "%-confidence interval for the mean area: ",
    "(", phi(pred.ref[2]), ", ", phi(pred.ref[3]), ")\n", sep = "")
cat(conf.level*100.0, "%-prediction interval for the area: ",
    "(", quantile(pred.ref.area, 0.5*conf.level), ", ", quantile(pred.ref.area, 1 - 0.5*conf.level), ")\n", sep = "")

cat("\n")

cat("Alternative conditions: ", "temperature = ", raw.new.df$temperature, ", ",
    "duration = ", raw.new.df$duration, "\n", sep = "")
cat(conf.level*100.0, "%-confidence interval for the mean area: ",
    "(", phi(pred.new[2]), ", ", phi(pred.new[3]), ")\n", sep = "")
cat(conf.level*100.0, "%-prediction interval for the area: ",
    "(", quantile(pred.new.area, 0.5*conf.level), ", ", quantile(pred.new.area, 1 - 0.5*conf.level), ")\n", sep = "")

cat("\n")
cat("Difference between conditions (reference minus akternative): \n")
norm.eps = summary(model)$sigma*rnorm(length(eps))/sqrt(length(eps))
diff.mean.area = phi(pred.ref[1] + norm.eps) - phi(pred.new[1] + norm.eps)
cat(conf.level*100.0, "%-confidence interval for the difference in mean areas: ",
    "(", quantile(diff.mean.area, 0.5*conf.level), ", ", 
    quantile(diff.mean.area, 1 - 0.5*conf.level), ")\n", sep = "")
cat(conf.level*100.0, "%-prediction interval for the difference in areas: ",
    "(", quantile(pred.ref.area - pred.new.area, 0.5*conf.level), ", ", 
    quantile(pred.ref.area - pred.new.area, 1 - 0.5*conf.level), ")\n", sep = "")

cat("\n")
cat("Comparison of quantiles: \n\n")

alphas = seq(from = 0.05, to = 0.95, by = 0.05)

for (alpha in alphas) {
  q = quantile(pred.ref.area, alpha)
  alpha.new = sum(pred.new.area <= q)/length(pred.new.area)
  cat("Quantile = ", q, ", % of areas below (reference vs. alternative) = ", alpha, 
      " vs. ", alpha.new, ", ",
      "ratio = ",  alpha/alpha.new, "\n", sep = "")
}

# Do plotting

if (do.prediction.plots.flag) {
  grDevices::pdf(file = "prediction.plot.pdf")
  
  par(mfrow = c(2, 1))
  
  # difference in CDFs
  plot(ecdf(pred.ref.area), col = "blue",
       xlab = "area", ylab = "CDF", main = "CDF comparison")
  lines(ecdf(pred.new.area), col = "red")
  
  legend("bottomright", legend = c("reference conditions", "alternative conditions"),
         lty = c(1, 1), col = c("blue", "red"))
  
  # difference in PDFs
  
  x.range = c(min(quantile(pred.ref.area, 0.01), quantile(pred.new.area, 0.01)),
              max(quantile(pred.ref.area, 0.99), quantile(pred.new.area, 0.99)))
  
  pdf.ref = density(pred.ref.area, from = x.range[1], to = x.range[2], n = 5000L, adjust = 10.0)
  pdf.new = density(pred.new.area, from = x.range[1], to = x.range[2], n = 5000L, adjust = 10.0)
  
  y.range = c(0, max(pdf.ref$y, pdf.new$y))
  
  plot(pdf.ref,  xlim = x.range, ylim = y.range, col = "blue",
       xlab = "area", ylab = "PDF",
       main = "PDF comparison")
  lines(pdf.new, xlim = x.range, ylim = y.range, col = "red")
  
  legend("topright", legend = c("reference conditions", "alternative conditions"),
         lty = c(1, 1), col = c("blue", "red"))
  
  grDevices::dev.off()
}