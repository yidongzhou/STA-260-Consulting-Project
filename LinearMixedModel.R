setwd("/Users/easton/Google Drive/Courses/STA260/Consulting Project")
docsEEBO <- read.csv("docs.meta.csv")
docsEEBO$Date <- as.Date(docsEEBO$Date)
str(docsEEBO)
table(docsEEBO$Date)

df <- read.table('pmi/vocab_10.txt', header = TRUE)
term <- df$term
for(i in 1:9){
  filename <- paste0('pmi/vocab_0', i, '.txt')
  df <- read.table(filename, header = TRUE)
  term <- intersect(df$term, term)
}
for(i in 1:10){
  filename <- paste0('new/', i, '.txt')
  df <- read.table(filename, skip = 1)
  term <- intersect(df$V1, term)# 15909
}

N <- length(term)
emb <- lapply(1:10, function(i){
  filename <- paste0('new/', i, '.txt')
  df <- read.table(filename, skip = 1)
  df <- df[is.element(df$V1, term), ]
  df <- df[order(df$V1), ]
  as.matrix(df[, -1])
})
save(emb, file = 'emb.RData')
load('emb.RData')
delta <- rep(0, N*9)
for(i in 1:9){
  M <- t(emb[[i+1]])%*%emb[[i]]
  f <- svd(M)
  R <- f$u%*%t(f$v)
  Wi <- emb[[i]]%*%t(R)
  delta[((i-1)*N+1):(i*N)] <- 1-diag(Wi%*%t(emb[[i+1]]))/sqrt(rowSums(Wi^2)*rowSums(emb[[i+1]]^2))
}
delta <- log(delta)
delta <- (delta-mean(delta))/sd(delta)
freq <- sapply(1:9, function(i) {
  filename <- paste0('pmi/vocab_0', i, '.txt')
  df <- read.table(filename, header = TRUE)
  df1 <- df[is.element(df$term, term), ]
  df1 <- df1[order(df1$term), ]
  df1$term_count/sum(df$term_count)
})
poly <- sapply(1:9, function(i) {
  filename <- paste0('pmi/vocab_0', i, '.txt')
  df <- read.table(filename, header = TRUE)
  df <- df[is.element(df$term, term), ]
  df <- df[order(df$term), ]
  df$d
})
df <- data.frame(delta, 
                 t = factor(rep(1:9, each = N)), 
                 word = rep(1:N, 9), 
                 frequency = log(c(freq)),
                 polysemy = -log(-c(poly)))
save(df, file = 'df.RData')
library(lme4)
library(lmerTest)
fit <- lmer(delta ~ frequency + polysemy + t + (1|word) - 1, data = df)
summary(fit)
beta <- fixef(fit)
conf <- confint(fit)

library(ggplot2)
ggplot(data = df, aes(x = frequency, y = delta)) +
  geom_point(size = 0.3, alpha = 0.2) +
  stat_smooth(method = lm) +
  ylim(c(-4, 4)) +
  ylab('Rate of semantic change') +
  xlab('Log(frequency)')
  geom_abline(intercept = 0, slope = beta[1], color="green")
ggplot(data = df, aes(x = polysemy, y = delta)) +
  geom_point(size = 0.3, alpha = 0.2) +
  stat_smooth(method = lm) +
  ylim(c(-4, 4)) +
  ylab('Rate of semantic change') +
  xlab('Log(polysemy)')
  geom_abline(intercept = 0, slope = beta[2], color="blue")

qqnorm(unlist(ranef(fit)))
qqline(unlist(ranef(fit)))
qqnorm(residuals(fit))
qqline(residuals(fit))
