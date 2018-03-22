library(easypackages)
libraries('yuima', 'aTSA', 'openxlsx', 'ggplot2', 'ineq', 'tictoc', 'reshape2', 'plyr', 'fitdistrplus', 'actuar')
load("D:/R/Inequality leaks/ildata.RData")
set.seed(1234)

#functions
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#Income functional form
dx <- function(x, y, g) {y^exp(g*x)}
curve(dx(x, 2, 0.2), 0, 10)
abline(h = 0, col = 'grey'); abline(v = 0, col = 'grey')

#WIID inequality database
WIID <- read.xlsx("United Nations University's World Income Inequality Database/WIID3.4_19JAN2017New.xlsx")
WIID_10 <- WIID[WIID$Year == 2010,]
WIID_10$Incomegroup <- as.factor(WIID_10$Incomegroup)
WIID_10[WIID_10$Incomegroup %in% c("High income: non-OECD", "High income: OECD"), "income_cat"] <- "High Income"
WIID_10[WIID_10$Incomegroup %in% c("Lower middle income", "Upper middle income"), "income_cat"] <- "Middle Income"
WIID_10[WIID_10$Incomegroup %in% c("Low income"), "income_cat"] <- "Low Income"
WIID_10$income_cat <- as.factor(WIID_10$income_cat)

#Regions by gini
boxplot(Gini ~ income_cat, data = WIID_10, 
        xlab = "Income Categories", ylab = "Gini Coefficient", main = "Inequality within the Rich and Poor")
ggplot(WIID_10, aes(income_cat, Gini)) + geom_violin(trim = F)
by(WIID_10$Gini, WIID_10$income_cat, function(x) {mean(x, na.rm = T)})

#WB data
wb_data <- read.csv("World bank data/WBData.csv")
wb_data$Year <- as.Date(wb_data$Year, 'X%Y')
wb_data$Year <- format(wb_data$Year, '%Y')

#Foreign investment data
plot(1, 1, type = 'n', xlim = c(0, 14), ylim = c(-15, 5))
y <- seq(1980, 2015, 5)
for (i in 1:length(y)) {
points(log(wb_data[wb_data[,'Year'] == y[i], c('GDP.per.capita..constant.2010.US..', 'Foreign.direct.investment..net.outflows....of.GDP.')]),
     xlim = c(5, 15), ylim = c(-15, 5), pch = 19, col = i)
}

ggplot() + geom_point(data = wb_data[wb_data$Foreign.direct.investment..net.outflows....of.GDP. > 0,], aes(x = log(GDP.per.capita..constant.2010.US..), y = log(Foreign.direct.investment..net.outflows....of.GDP.), colour = Year), na.rm = T) + 
  coord_cartesian(xlim = c(1, 12)) + guides(colour=FALSE) + 
  labs(x = 'log GDP per capita', y = 'log Foreign Investment as a % of GDP', title = 'Increase in foreign investment 1960-2015') + 
  theme(plot.title = element_text(hjust = 0.5))


#WID inequality data
US <- read.csv('WID Data/US/US Inequality.csv', sep = ';', header = T, skip = 1); colnames(US) <- c('Percentile', 'Year', 'Net personal wealth', 'Market-value national wealth')
US_df_m <- melt(US, id.vars = c(1, 2))
Us_df <- dcast(US_df_m, Year ~ Percentile, value.var = 'value', fun.aggregate = sum, na.rm = T)
US_full <- read.csv('WID Data/WID_full_dataset/WID_fulldataset_US/Data/WID_US_InequalityData.csv', sep = ';')
US_macro <- read.csv('WID Data/WID_full_dataset/WID_fulldataset_US/Data/WID_US_MacroData.csv', sep = ';')
colnames_Us_full <- colnames(US_full)
w_sh <- US_full[8:nrow(US_full), c('..1','..2','Net.personal.wealth...equal.split.adults...Share...Adults...share.of.total..ratio.')]
colnames(w_sh) <- c('year','perc','wealth share')
w_avg <- US_full[8:nrow(US_full), c('..1','..2','Net.personal.wealth...equal.split.adults...Average...Adults...constant.2015.local.currency')]
colnames(w_avg) <- c('year','perc','avg wealth')
y_dis <- US_full[8:nrow(US_full), c('..1','..2','Post.tax.disposable.income...equal.split.adults...Average...Adults...constant.2015.local.currency')]
colnames(y_dis) <- c('year','perc','disposable income')


#SaezZucman2015FullData
r <- read.csv('SaezZucman2015FullData/r.csv', skip = 1)
colnames(r) <- c("year", "rAll" , "rBottom.90.", "rTop.10.", "rTop.1.", "rTop.0.5.", "rTop.0.1.", "rTop.0.01.") 

#MPC from Carroll, C., Slacalek, J., Tokuoka, K., & White, M. N. (2017)
US_df$mpc_all <- 0.44
US_df$mpc_top1 <- 0.12
US_df$mpc_top10 <- 0.12
US_df$mpc_top20 <- 0.13
US_df$mpc_top20_40 <- 0.27
US_df$mpc_top40_60 <- 0.41
US_df$mpc_top60_80 <- 0.52
US_df$mpc_bot20 <- 0.71

#Top inequality 
w_sh_m <- melt(w_sh, id.vars = c('year','perc'))
w_sh <- dcast(w_sh_m, year ~ perc)
matplot(w_sh$year, w_sh[,c('p99p100')], type = 'l')
w_avg_m <- melt(w_avg, id.vars = c('year','perc'))
w_avg <- dcast(w_avg_m, year ~ perc)
matplot(w_avg$year, w_avg[,c('p99p100')], type = 'l')
y_dis_m <- melt(y_dis, id.vars = c('year','perc'))
y_dis <- dcast(y_dis_m, year ~ perc)
matplot(y_dis$year, y_dis[,c('p99p100')], type = 'l')
View(y_dis[,c('year','p99p100')])

#CBO
g <- read.csv('US CBO/income growth.csv', skip = 3)


US_gdpperc_gr <- wb_data[wb_data$Country.Code == 'USA',c('Year','GDP.per.capita.growth..annual...')]
US_df <- merge(US_df, US_gdpperc_gr, by = 'year', all.x = T, all.y = T)
US_df <- merge(US_df, r, by = 'year', all.x = T, all.y = T)
US_df <- merge(US_df, g, by = 'year', all.x = T, all.y = T)
US_df$r_g_mpc <- US_df$rTop.1. - US_df$g - US_df$mpc_top1

ggplot(data = US_df, aes(x = year)) + geom_line(aes(y = top1, color = 'top1')) + 
  geom_line(aes(y = top10, color = 'top10')) + scale_colour_discrete(name = 'Series')

ggplot(data = US_df, aes(x = year)) + geom_line(aes(y = r_g))
matplot(US_df$year, US_df[,22], type = 'l')

#Top 1% wealth model
dr <- 'y(t) + mu(t)*w'
diff <- 'sigma*w'
jump <- list(intensity = "1", df = "dgamma(z, 1)")
y <- function(t) {approx(y_dis$p99p100[48:103], n = 56)[[2]][t]}
mu <- function(t) {approx(US_df$r_g_mpc[48:103], n = 56)[[2]][t]}
top1p <- setModel(drift = dr, diffusion = diff, solve.variable = 'w')
par <- as.list(c(sigma = 0.3))
grid <- setSampling(1, 56, 55, delta = 1)
out <- simulate(top1p, true.parameter = par, sampling = grid, xinit = 4298462.30231)
top1p_ts <- as.data.frame(out@data@original.data)
matplot(1960:2015, cbind(w_avg$p99p100[48:103], top1p_ts), type = 'l', ylab = 'Top 1% Wealth', xlab = 'Years')

simout <- replicate(100, simulate(top1p, true.parameter = par, sampling = grid, xinit = 4298462.30231)@data@original.data)
simout <- as.data.frame(simout)
simout$year <- 1960:2015
matplot(1960:2015, simout, type = 'l', ylab = 'Top 1% Wealth', xlab = 'Years')
simout_avg <- apply(simout, 1, mean, na.rm = T)

matplot(simout$year, cbind(w_avg$p99p100[48:103], simout_avg), type = 'l', 
        xlab = 'Year', ylab = 'Top 1% Wealth', lwd = 2, main = 'Wealth Inequality in the US')
simout_m <- melt(simout, id.vars = 'year')
summary <- summarySE(simout_m, 'value', 'year')
gg_df <- data.frame(year = simout$year, wtop1 = as.numeric(w_avg$p99p100[48:103]), wtop1_mod = summary$value, se = summary$se)
ggplot(data = gg_df, aes(x = year)) + 
  geom_line(aes(y = wtop1, color = 'Data'), size = 1) + 
  geom_line(aes(y = wtop1_mod, color = 'Model'), size = 1) + 
  geom_ribbon(aes(ymin = wtop1_mod - se, ymax = wtop1_mod + se), fill = 'light blue', alpha = 0.3) + 
  scale_colour_discrete(name = 'Top 1% Wealth') + 
  labs(x = 'Year', y = 'Wealth', title = 'Wealth Inequality') + 
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5))
  
#Stationary distribution 
plot.new()
text(0.5, 0.5, expression((1/sigma^2*w^2)*exp((-2*y/w + 2*mu*log(w))/sigma^2)))
f <- function(w, sigma, y, mu) {
  #(1/(sigma*w)^2)*exp((-2*y/w + 2*mu*log(w))/sigma^2)
  exp((-2*y)/(w*sigma^2))*(w^(2*mu/sigma^2 - 2))/sigma^2
  #(w^(2*mu/sigma^2 - 2))/sigma^2
}
w_s <- seq(1e6, 1e8, length.out = 1e3)
plot(w_s, f(w_s, 0.3, 811769.6, -0.04945753), type = 'l', xlab = 'Wealth', ylab = 'Density', lwd = 2,
     main = 'Stationary distribution top 1% wealth')
w_density <- f(w_s, 0.3, 7e5, -0.1)
id <- order(w_s)
AUC <- sum(diff(w_s[id])*rollmean(w_density[id],2))
plot(w_s, w_density/AUC, type = 'l', xlab = 'Wealth', ylab = 'Density', lwd = 2,
     main = 'Stationary distribution top 1% wealth')
memp  <-  function(x, order) mean(x^order)
w_fit <- fitdist(w_density, 'pareto', method = 'mme', memp = memp, order = c(1, 2), start=list(shape=5, scale=1))
w_fit <- fitdist(w_density, 'pareto', method = 'qme', probs=c(1/3, 2/3), start=list(shape=5, scale=1))
plot(w_fit)
summary(w_fit)

#Basic model income dynamics for single country with 100 households   
top1 <- function(x) {
  s <- sort(x, decreasing = T)
  n <- round(length(x) * 0.01)
  top <- 100*sum(s[1:n])/sum(s)
  return(top)}
y <- paste('y', 1:100, sep = '')
mu <- paste('mu', 1:100, sep = '')
sigma <- paste('sigma', 1:100, sep = '')
w <- paste('w', 1:100, sep = '')
xx <- function(t) {ret <- 0:100; return(ret[t])}
djump <- function(z, shp) {rep(0, 100)}
rjump <- function(z, shp) {rbind(replicate(100, rgamma(z, shape = shp, rate = 1)))}
jump <- list(intensity = "1", df = "djump(z, shp)")
jump_c <- matrix(0, 100, 100)
diag(jump_c) <- '1'
dr <- c(paste(y, ' + ', mu, '*', w, sep = ''))
diff <- matrix(0, 100, 100)
diag(diff) <- c(paste(sigma, '*', w, sep = ''))
IL <- setModel(drift = dr, diffusion = diff, solve.variable = w, 
               state.variable = w, jump.coeff = jump_c, 
               measure = jump, measure.type = 'CP')

par <- as.list(c(y = 100*rgamma(100, 2, 1), 
                 mu = rnorm(100, 0, 0.01), 
                 sigma = rep(0.1, 100),
                 shp = 2))
grid <- setSampling(0, 100, 100)
out <- simulate(IL, true.parameter = par, sampling = grid, xinit = runif(100, 0, 500))
w_ts <- as.data.frame(out@data@original.data)
plot.ts(w_ts, plot.type = "single", col = 1:100)
w_top1 <- apply(w_ts, 1, top1) 
w_gini <- apply(w_ts, 1, Gini)
plot(w_top1, type = 'l')
plot(w_gini, type = 'l')

#Multiple simulations for a two country model #dw = (y - mu*w)dt + sigma*wdZ + gDN
grid <- setSampling(0, 1000, 1000)
fi <- function(t, w_r, w_p, alpha) {
  ret <- 0
  if (w_r > 10000) {ret <- alpha*w_r}
  return(ret)
}
fi_ret <- function(t, w_r, w_p, beta, alpha) {
  if (wr_r > 10000) {ret <- beta*alpha*w_r}
  return(ret)
}
dr <- matrix(c("sy_r - mu_r*w_r", "sy_p - mu_p*w_p"), 2, 1)
diff <- matrix(c("sigma_r*w_r", "sigma_p*w_p"), 2, 1)
jump_c <- matrix(c("1", "0", "0", "1"), 2, 2)
djump <- function(z, shp_r, shp_p) {rep(0, 2)}
rjump <- function(z, shp_r, shp_p) {cbind(rgamma(z, shp_r, 1), rgamma(z, shp_p, 1))}
jump <- list(intensity = "10", df = "djump(z, shp_r, shp_p)")
IL <- setModel(drift = dr, diffusion = diff, solve.variable = c("w_r", "w_p"), 
               state.variable = c("w_r", "w_p"), jump.coeff = jump_c, measure = jump, 
               measure.type = "CP")
#for (i in 1:20) {curve(dgamma(x, i, 1), 0, 20, add = T)}
par <- list(mu_r = 0.1, mu_p = 0.2, sigma_r = 0.2, sigma_p = 0.1, sy_r = 100, sy_p = 10, 
            shp_r = 2, shp_p = 2)
out <- simulate(IL, true.parameter = par, sampling = grid, xinit = c(1, 1))
plot(out)

sim_n <- 100
out <- list()
for (i in 1:sim_n) {
  cat("iteration", i, "\n")
  par <- list(mu_r = 0.1, mu_p = 0.2, 
              sigma_r = 0.2, sigma_p = 0.1, 
              sy_r = 10*rgamma(1, 10, 0.2), sy_p = 10*rgamma(1, 1, 0.05), 
              shp_r = 2, shp_p = 2)
  out[[i]] <- simulate(IL, true.parameter = par, sampling = grid, 
                       xinit = c(1, 1))@data@original.data
}

rich <- list(); for (i in 1:sim_n) {rich[[i]] <- out[[i]][,1]}; 
poor <- list(); for (i in 1:sim_n) {poor[[i]] <- out[[i]][,2]}; 
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
ymax <- max(as.data.frame(poor), as.data.frame(rich))
plot.ts(as.data.frame(poor), plot.type = "single", ylab = "Income", xaxt='n',
        ylim = c(0, ymax), main = "Poor Country")
plot.ts(as.data.frame(rich), plot.type = "single", ylab = "Income", xlab = "Time",
        ylim = c(0, ymax), main = "Rich Country")

poormean <- colMeans(as.data.frame(poor))
richmean <- colMeans(as.data.frame(rich))
xmax <- max(poormean, richmean)
hist(poormean, probability = T, xlim = c(0, xmax), breaks = 20, main = "Poor Country"); lines(density(poormean))
hist(richmean, probability = T, xlim = c(0, xmax), breaks = 20, main = "Rich Country"); lines(density(richmean))

Gini(richmean)
Gini(poormean)

#Vectorize model for 200 individuals 
tic("model")
grid <- setSampling(0, 100, 100)
R <- paste("R", 1:100, sep = '')
I <- paste("I", 1:100, sep = '')
X <- paste("X", 1:100, sep = '')
D <- paste("D", 1:100, sep = '')
w_r <- paste("w_r", 1:100, sep = '')
w_p <- paste("w_p", 1:100, sep = '')
i <- paste("i", 1:100, sep = '')
ii <- paste("i", 1:100, sep = '', collapse = ",")
XX <- paste("X", 1:100, sep = '', collapse = ",")
sum_i <- paste('sum(', ii, ')', sep = '')
sum_X <- paste('sum(', XX, ')', sep = '')
sy_r <- paste("sy_r", 1:100, sep = '')
sy_p <- paste("sy_p", 1:100, sep = '')
mu_r <- paste("mu_r", 1:100, sep = '')
mu_p <- paste("mu_p", 1:100, sep = '')
s_var <- c(w_r, i, w_p)
eval(parse(text = paste(R,  " <- function(", i, ", beta, T2, n) 
                        {beta*", i, "*1/(1 + (T2/", i, ")^n)}", sep = '')))
eval(parse(text = paste(I,  " <- function(", w_p, ", gamma, ", ii, ", T3, n) 
                        {gamma*(1 - 1/(1 + (T3/", w_p, ")^n))*", sum_i,"}", sep = '')))
eval(parse(text = paste(X,  " <- function(", w_r, ", tau, T4, n) 
                        {tau*", w_r,"*1/(1 + (T4/", w_r, ")^n)}", sep = '')))
eval(parse(text = paste(D,  " <- function(", w_r, ", epsilon, ", XX, ", T5, n) 
                        {epsilon*(1 - 1/(1 + (T5/", w_r, ")^n))*", sum_X,"}", sep = '')))


dr <- c(paste(sy_r, " + ", mu_r, "*", w_r, " - alpha*", w_r,"*1/(1 + (T1/", w_r, ")^n) + ", i, 
              " + ", R, "(", i, ", beta, T2, n)", " - ", X, "(", w_r, ", tau, T4, n)", " + ", 
              D,  "(", w_r, ", epsilon, ", XX, ", T5, n)", sep = ''),
        paste("alpha*", w_r,"*1/(1 + (T1/", w_r, ")^n) - ", i, sep = ''),
        paste(sy_p, " + ", mu_p, "*", w_p, " + ", I, "(", w_p, ", gamma, ", ii, ", T3, n)", sep = ''))
diff <- matrix(0, 300, 300)
diag(diff) <- c(paste("sigma_r*", w_r, sep = ''), 
                rep("0", 100),
                paste("sigma_p*", w_p, sep = ''))
jump_c <- matrix(0, 300, 300)
diag(jump_c) <- c(rep("1", 100), rep("0", 100), rep("1", 100))
djump <- function(z, shp) {rep(0, 300)}
rjump <- function(z, shp) {rbind(replicate(300, rgamma(z, shp, 1)))}
jump <- list(intensity = "100", df = "djump(z, shp)")

IL <- setModel(drift = dr, diffusion = diff, state.variable = s_var, jump.coeff = jump_c, 
               measure = jump, measure.type = "CP", jump.variable = "z",
               time.variable = "t", solve.variable = s_var)
toc()

#Parameters distribution
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
d <- rgamma(10000, 2.4, 1)
hist(d, probability = T); lines(density(d)); Gini(d)
hist(rnorm(100, 0.01, 0.005), probability = T); lines(density(rnorm(100, 0.01, 0.005)))
hist(rnorm(100, 0, 0.01), probability = T); lines(density(rnorm(100, 0, 0.01)))
plot(dgamma(seq(0, 10, 0.1), 2, 1), type = 'l')

tic("simuation")
par <- as.list(c(sy_r = rgamma(100, 10, 0.2), sy_p = rgamma(100, 1, 0.05), 
            mu_r = rnorm(100, 0.01, 0.005), mu_p = rnorm(100, 0, 0.01), 
            sigma_r = 0.2, sigma_p = 0.1, shp = 2, T1 = 0, T2 = 0, T3 = 0,
            T4 = 1e5, T5 = 1e5, n = 20, alpha = 0, beta = 0, gamma = 0,
            epsilon = 0.01, tau = 0.01))

out <- simulate(IL, true.parameter = par, sampling = grid)
toc()

rich <- out@data@original.data[, 1:100]
poor <- out@data@original.data[, 201:300]
ymax <- max(c(rich, poor))
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2)) #c(bottom, left, top, right)
plot.ts(rich, plot.type = "single", col = 1:100, ylab = "Income", xlab='Time',
        ylim = c(0, ymax), main = "Rich Country")
plot.ts(poor, plot.type = "single", col = 1:100, ylab = "Income", xlab = "Time",
        ylim = c(0, ymax), main = "Poor Country")

richmean <- colMeans(rich)
poormean <- colMeans(poor)
xmax <- max(poormean, richmean)
hist(richmean, probability = T, xlim = c(0, xmax), breaks = 20, 
     main = "Rich Country (Mean Income)"); lines(density(richmean))
hist(poormean, probability = T, xlim = c(0, xmax), breaks = 20, 
     main = "Poor Country (Mean Income)"); lines(density(poormean))

Gini(richmean); Gini(poormean) 

#Side analysis
i <- function(w_r, thres, n, alpha, k) {alpha*(w_r/(1 + (thres/w_r)^n) - (w_r^2)/k)}
i <- function(i, w_r, thres, n, alpha) {alpha*w_r/(1 + (thres/w_r)^n) - i}
h <- function(w_p, thres, n) {-1/(1 + (thres/w_p)^n) + 1}
plot(i(1:150, 150, 80, 20, 0.1), type = 'l')
plot(h(1:100, 50, 20), type = 'l')
abline(v = 15, col = 'red'); abline(v = 100, col = 'red')
abline(v = 0, col = 'lightgrey')
abline(h = 0, col = 'lightgrey')
r_fi <- function(t, w_r) {
  t1 <- 0
  ret <- 0
  if (w_r == 8000) {t1 <- t}
  if (t >= t1 + 10) {ret <- 1}
  return(ret)
}

par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot(fi(1:20000, 8000, 20), type = 'l')
plot(r_fi(20:40, 10, 20 ), type = 'l')
abline(v = 8000, col = 'lightgrey')

x <- paste("x", 1:2, sep = "")
xx <- paste("x", 1:2, sep = '', collapse = ',')
s <- paste("mu*sum(", xx, ")", sep = '')
grid <- setSampling(0, 100, 100)
f_tau <- function(t, tau){t - tau}
model <- setModel(drift = 'mu*x', diffusion = '0', solve.variable = 'x')
out <- simulate(model, true.parameter = list(mu = 10), xinit = '1')
plot(out)

X1 <- function(w_r1, tau, T4, n) {tau*w_r1*1/(1 + (T4/w_r1)^n)}
X2 <- function(w_r2, tau, T4, n) {tau*w_r2*1/(1 + (T4/w_r2)^n)}

D <- function(w_r1, epsilon, T5, n, w_r2, tau, T4) 
{epsilon*(1 - 1/(1 + (T5/w_r1)^n))*sum(X1(w_r1, tau, T4, n),X2(w_r2, tau, T4, n))}


y <- function(t, c, g) {c*exp(g*t)}
plot(y(1:100, 10, -0.1), type = 'l')


