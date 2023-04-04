print('Good luck, Remember to check the parameters for each copy!')


# 🔎 < graph part & R basic code >:
# 🔺 Scatterplot(散点图)
library(ggplot2)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_smooth(method = "lm") + facet_wrap(~ Species)

# 🔺 Barplot(条形图)
library(ggplot2)
ggplot(iris) + geom_bar(aes(Species), width = 0.5) + coord_flip()

# 🔺 Histogram(直方图)
library(ggplot2)
ggplot(iris) + geom_histogram(aes(Sepal.Length))

# 🔺 Boxplot(箱线图)
River_A <- c(2, 1.95, 1.99, 2.08, 1.99, 1.95, 2.03, 2.09, 2.07, 2.01)
River_B <- c(1.85, 1.94, 1.87, 1.89, 1.91, 1.93, 2.01, 2, 1.96, 1.98)
boxplot(River_A, River_B, names = c('River A','River B'))

library(ggplot2)
ggplot(iris) + geom_boxplot(aes(Sepal.Length))

# 🔺 Pairplot(组合图)
# install.packages("GGally")
library(GGally)
ggpairs(iris, aes(colour=Species, alpha=0.5))
ggsave("df_plot.pdf")   # Save graphs

# 🔺 R basic operations:
# 随机取样
df <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                 y = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i' ,'j'))   
sample(df$y[df$x > 5], 2)
sample(df$y[df$x <= 5], 2)

# Two ways to remove NA & outlier from data set 🎁
x=c(1,2,NA,4)
mean(x, na.rm = TRUE)   # 在函数内
x <- x[!is.na(x)]   # 调整数据集(多维数据筛选"选择了未定义的列"需要在!is.na(***)后加',')
x <- x[x != boxplot(x, plot=FALSE)$out]   # 排除了数据集中的离群值

# correlation coefficient function:
cor_coe <- function(x, y){
  N <- length(x)
  co <- 1 / (N - 1) * sum((y - mean(y)) * (x - mean(x)))
  sx <- sd(x)
  sy <- sd(y)
  co / (sx * sy)
}


# 🔎 < Statistic part >:
# H0：通常是研究者想收集证据予以反对的一方，也称零假设（一般包括等于）
# H1：通常是研究者收集证据予以支持的一方，也称对立假设（一般包括不等于）
# Type 1 error: False positive (the hypothesis is right but we reject it) [alpha]
# Type 2 error: False negative (the hypothesis is wrong but we didn't reject it) [beta]
# Alpha(Significance level): The maximum allowable probability of making a type I error [0.10, 0.05, 0.01 ...]
# Beta: ...
# P-value(Probability value): if P-value <= alpha, reject H0; if P-value > alpha, not reject H0.
# Rejection region, critical region: the range of values for which H0 is not probable(不可能发生H0的区间)


# 🔺 Binomial distribution (e.g. Toss a coin for "n" times, "x" is the frequency of head)
x <- 2
n <- 2
p <- 0.5
dbinom(x, size = n, prob = p)  # Same as choose(n, x) * p^x * (1-p)^(n-x)


# 🔺 Z-test (已知总体均值以及总体方差)
x <- c(24, 36, 44, 35, 44, 34, 29, 40, 39, 43, 41, 32, 33, 29, 29, 43, 25, 39, 25, 42, 
       29, 22, 22, 25, 14, 15, 14, 29, 25, 27, 22, 24, 18, 17)
mu <- 29
sd_p <- 9
x_bar <- mean(x)
se <- sd_p / sqrt(length(x))
(z_score <- (x_bar - mu) / se)
(z_critical <- qnorm(1 - 0.05))
(p_value <- pnorm(z_score))


# 🔺 T-test (已知总体均值但未知总体方差)
mu <- 100
x <- c(91, 100, 70, 87, 104, 92, 104, 88, 72, 119)
t.test(x, mu = mu , alternative = "less", conf.level = 0.05)   # One-tailed

mu <- 0.02
x <- c(0.011, 0.021, 0.001, 0.007, 0.031, 0.023, 0.026, 0.019)
t.test(x, mu = 0.02, alternative = "two.sided", conf.level = 0.1)   # Two-tailed

x1 <- c(24.58, 22.09, 23.70, 18.89, 22.02, 28.71, 24.44, 20.91, 23.83, 20.83)
x2 <- c(21.61, 19.06, 20.72, 15.77, 19, 25.88, 21.48, 17.85, 20.86, 17.77)
t.test(x1, x2, var.equal = TRUE, alternative = "two.sided", mu = 0)   # Two samples

# 🔺 F-test (检验两组数值的差异的显著性, mu1 = mu2 = mu3 = mu4...)
x1 <- c(24.58, 22.09, 23.70, 18.89, 22.02, 28.71, 24.44, 20.91, 23.83, 20.83)
x2 <- c(21.61, 19.06, 20.72, 15.77, 19, 25.88, 21.48, 17.85, 20.86, 17.77)
var.test(x2, x1, ratio = 1, alternative = "two.sided", conf.level = 0.95)   # Two set of data "x1, x2" ANOVA calculation

dtf <- data.frame(before = c(198, 201, 210, 185, 204, 156, 167, 197, 220, 186),
                  one = c(194, 203, 200, 183, 200, 153, 166, 197, 215, 184),
                  two = c(191, 200, 192, 180, 195, 150, 167, 195, 209, 179),
                  three = c(188, 196, 188, 178, 191, 145, 166, 192, 205, 175))
rownames(dtf) <- LETTERS[1:10]
dtf2 <- stack(dtf)
names(dtf2) <- c("w", "level")
w_aov <- aov(w ~ level, data = dtf2)
summary(w_aov)
dtf2$subject <- rep(LETTERS[1:10], 4)
w_aov2 <- aov(w ~ level + Error(subject/level), data = dtf2)
summary(w_aov2)    # One-way ANOVA, 看Pr(>F)后的'*'符号判断significant effect

dtf <- data.frame(w = c(90,95,100,75,78,90,120,125,130,100,118,112,125,130,135,118,125,132),
                  diet = rep(c("Diet1", "Diet2", "Diet3"), each = 6),
                  gender = rep(c("Male", "Female"), each = 3))
aov_wg <- aov(w ~ diet * gender, data = dtf)
summary(aov_wg)    # Two-way ANOVA, 看Pr(>F)后的'*'符号判断significant effect


# 🔺 X^2-test(检验两个变量之间有没有关系, association between A and B)
dtf <- data.frame(colour = c("red", "white", "pink"),   # One-category X^2 test
                  observed = c(72, 63, 125))   # H0:The ratio of red to white to pink is 1:1:2
dtf$expected <- sum(dtf$observed) * c(1, 1, 2) / 4
df <- nrow(dtf) - 1
(chi_sq_score <- sum((dtf$observed - dtf$expected)^2 / dtf$expected))
(critical_point <- qchisq(0.05, df, lower.tail = FALSE))
(p_value <- pchisq(chi_sq_score, df, lower.tail = FALSE))

dtf <- data.frame(Endangered = c(162, 143, 38, 17, 45),   # Multiple-category X^2 test
                  Threatened = c( 18,  16, 19, 10, 32), 
                  row.names = c('Mammals', 'Birds', 'Reptiles', 'Amphibians', 'Fish'))
chisq.test(dtf)   # Yates’ correction


# 🔺 Regression & Correlation(回归&相关)[Week11]
# Hypothesis test
dtf0 <- read.csv("students_env221.csv")
dtf <- dtf0[dtf0$SHOE != boxplot(dtf0$SHOE, plot=FALSE)$out & !is.na(dtf0$SHOE), ]    # 🎁排除了NA以及离群值
env_lm <- lm(SHOE ~ HEIGHT, data = dtf)
env_lm$coefficients       # Intercept & Slope

# Hypothesis test about correlation (continue)
(r <- cor(dtf$HEIGHT, dtf$SHOE))    # correlation coefficient
r ^ 2       # coefficient of determination. When R^2 is closer to 1, it means that the related equation is more referential; The closer to 0, the less referential.
n <- nrow(dtf)
(t_score <- r * sqrt((n-2) / (1 - r ^ 2)))
(t_critical <- qt(0.975, df = n - 2))
(p_value <- pt(t_score, df = n - 2, lower.tail = FALSE) * 2)
summary(env_lm)    # Fast way, 其中R-squared就代表了Correlation coefficient

# Hypothesis test about slope (continue)
summary(env_lm)    # Fast way, 其中HEIGHT列既是slope，包含t_score & p_value
(t_critical <- qt(0.975, df = n - 2))

# Hypothesis test about intercept (continue)
summary(env_lm)     # Fast way, 其中(Intercept)列既是slope，包含t_score & p_value
(t_critical <- qt(0.025, df = n - 1))

# Visualization (another example)
X <- c(4.69, 4.43, 4.04, 4.06, 3.89, 3.93, 3.49, 3.4, 2.97, 2.47, 2.22, 2.13)
Y <- c(15.6, 15.4, 14.9, 14.5, 13.5, 13.4, 12.7, 12.3, 11.4, 10.5, 10.2, 10.0)
dtf <- data.frame(X, Y)
XY_lm <- lm(Y ~ X, data = dtf)
plot(x = dtf$X, y = dtf$Y, xlab = "X(unit)", ylab = "Y(unit)", las = 1, pch = 16)
abline(XY_lm, col = "blue")
mean_X <- mean(dtf$X, na.rm = TRUE)
mean_Y <- mean(dtf$Y, na.rm = TRUE)
points(x = mean_X, y = mean_Y, col = "red", cex = 3)
abline(h = mean_Y, v = mean_X, col = "red", lty = 2)



# 🧐 < Below is the final exam answer sheet >:

# F-test
# H0: var(Urban) == var(Rural)
# H1: var(Urban) != var(Rural)
dtf <- data.frame(Urban = c(7, 26, 91),
                  Rural = c(5, 12.2, 126),
                  row.names = c('sample size', 'sample mean (ppm)', 'sample variance (ppm²)'))
my_aov <- aov(Urban ~ Rural, data = dtf)
summary(my_aov)
(k <- ncol(dtf))
(n <- length(unlist(dtf)))
(dfW <- n - k)
(dfB <- k - 1)
(F_critical <- qf(0.95, df1 = dfB, df2 = dfW))
# H0: pollution(urban area) <= pollution(rural area)
# H1: pollution(urban area) > pollution(rural area)
dtf2 <- stack(dtf)
names(dtf2) <- c("a", "b")
w_aov <- aov(a ~ b, data = dtf2)
summary(w_aov)
(k <- ncol(dtf2))
(n <- length(unlist(dtf2)))
(dfW <- n - k)
(dfB <- k - 1)
(F_critical <- qf(0.95, df1 = dfB, df2 = dfW))

'--------------------------------------------------------------------------------------------------'
Location_A <- c(37, 40, 46, 41, 40, 38)
Location_B <- c(29, 33, 34, 31, 30, 33)
Location_C <- c(49, 47, 46, 48, 48, 47)
Location_D <- c(40, 38, 42, 39, 41, 41)
Location_E <- c(50, 46, 49, 48, 49, 46)
boxplot(Location_A, Location_B, Location_C, Location_D, Location_E, 
        names = c('Location A','Location B','Location C','Location D','Location E'))
# H0: all mean weight are same
# H1: at least one mean weight are different
dtf <- data.frame(Location_A = c(37, 40, 46, 41, 40, 38),
                  Location_B = c(29, 33, 34, 31, 30, 33),
                  Location_C = c(49, 47, 46, 48, 48, 47),
                  Location_D = c(40, 38, 42, 39, 41, 41),
                  Location_E = c(50, 46, 49, 48, 49, 46))
(k <- ncol(dtf))
(n <- length(unlist(dtf)))
(dfW <- n - k)
(dfB <- k - 1)
(dfT <- n-1)
(xbar <- mean(unlist(dtf)))
(xibar <- colMeans(dtf))
(SSW1 <- sum((dtf$Location_A - xibar[1]) ^ 2))
(SSW2 <- sum((dtf$Location_B - xibar[2]) ^ 2))
(SSW3 <- sum((dtf$Location_C - xibar[3]) ^ 2))
(SSW4 <- sum((dtf$Location_D - xibar[4]) ^ 2))
(SSW5 <- sum((dtf$Location_E - xibar[5]) ^ 2))
(SSW <- SSW1 + SSW2 + SSW3 + SSW4 + SSW5)
(SSB1 <- length(dtf$Location_A) * (xibar[1] - xbar) ^ 2)
(SSB2 <- length(dtf$Location_B) * (xibar[2] - xbar) ^ 2)
(SSB3 <- length(dtf$Location_C) * (xibar[3] - xbar) ^ 2)
(SSB4 <- length(dtf$Location_D) * (xibar[4] - xbar) ^ 2)
(SSB5 <- length(dtf$Location_E) * (xibar[5] - xbar) ^ 2)
(SSB <- SSB1 + SSB2 + SSB3 + SSB4 + SSB5)
(SST <- sum((unlist(dtf) - xbar) ^ 2))
SSW + SSB
SSB/SST     # Correlation ratio
(MSW <- SSW / dfW)
(MSB <- SSB / dfB)
(F_score <- MSB / MSW)
(F_critical <- qf(0.95, df1 = dfB, df2 = dfW))
pf(F_score, df1 = dfB, df2 = dfW, lower.tail = FALSE)

'--------------------------------------------------------------------------------------------------'
dtf <- data.frame(Solar_radiation <- c(224.4, 315.6, 495.6, 588.1, 416.9, 533.1, 541.9, 803.1, 878.1, 921.9, 830.6, 903.1),
                  Soil_temperature <- c(31.535, 31.663, 31.893, 32.278, 32.794, 33.235, 33.548, 33.914, 34.44, 35.102, 35.689, 36.146))
library(ggplot2)
ggplot(dtf, aes(x = Solar_radiation, y = Soil_temperature)) + 
  geom_point(aes(x = Solar_radiation, y = Soil_temperature)) + 
  geom_smooth(method = "lm") + labs(x="Solar radiation (W/m²)",y = "Soil temperature (°C)")
(r <- cor(dtf$Solar_radiation, dtf$Soil_temperature))
# H0: correlation coefficient == 0
# H1: correlation coefficient != 0
n <- nrow(dtf)
(t_score <- r * sqrt((n-2) / (1 - r ^ 2)))
(t_critical <- qt(0.975, df = n - 2))
(p_value <- pt(t_score, df = n - 2, lower.tail = FALSE) * 2)
my_lm <- lm(Solar_radiation ~ Soil_temperature, data = dtf)
my_lm$coefficients       # Intercept & Slope
# H0: slope of the regression line == 0
# H1: slope of the regression line != 0
summary(my_lm)    # Fast way, 其中HEIGHT列既是slope，包含t_score & p_value
(t_critical <- qt(0.975, df = n - 2))
# H0: intercept of the regression line == 0
# H1: intercept of the regression line != 0
summary(my_lm)     # Fast way, 其中(Intercept)列既是slope，包含t_score & p_value
(t_critical <- qt(0.025, df = n - 1))
r^2
# coefficient of determination = r^2 = 0.8097406 
# When R^2 is closer to 1, it means that the related equation is more referential; The closer to 0, the less referential. Here coefficient of determination is 0.8097406 means the soil temperature and the solar radiation have strong relationship.