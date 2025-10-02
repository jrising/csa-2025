  df <- read.table("sstoi.indices.txt", header=T)
  df$time <- df$YR + (df$MON - .5) / 12
  df$simple <- df$NINO3.4 - mean(df$NINO3.4)
  df$status <- ifelse(df$simple > .5, 'El Nino',
               ifelse(df$simple < -.5, 'La Nina', 'Neutral'))

library(ggplot2)

ggplot(df, aes(time, simple, fill=status)) +
    geom_col() + xlab(NULL) + ylab('NINO 3.4 Index')

head(df)

df$Lsimple = c(NA, df$simple[1:(nrow(df)-1)])

tail(df)

summary(lm(simple ~ Lsimple, data=df))

mod = lm(simple ~ Lsimple, data=df)

df$yhat = predict(mod, df)

library(ggplot2)

ggplot(df, aes(time, simple, fill=status)) +
    geom_col() + geom_point(aes(y=yhat))

make.lags = function(df, var, count) {
    lagvar = paste0("L", var)
    df[, lagvar] = c(NA, df[1:(nrow(df)-1), var])
    if (count == 1)
        return(df)
    return(make.lags(df, lagvar, count-1))
}

df2 = make.lags(df, 'simple', 3)

head(df2)

mod = lm(simple ~ Lsimple + LLsimple + LLLsimple, data=df2)

summary(mod)

make.formula.helper = function(var, count) {
    if (count == 1)
        return(paste0("L", var))
    return(paste0("L", c(var, make.formula.helper(var, count-1))))
}
make.formula = function(var, count) {
    as.formula(paste(var, "~", paste(make.formula.helper(var, count), collapse=" + ")))
}

summary(lm(make.formula('simple', 3), make.lags(df, 'simple', 3)))

results = data.frame()
for (count in 1:20) {
    mod = lm(make.formula('simple', count), make.lags(df, 'simple', count))
    results = rbind(results, data.frame(count, mse=mean(resid(mod)^2)))
}

results

results$loo = NA
for (count in 1:20) {
    df2 = make.lags(df, 'simple', count)
    form = make.formula('simple', count)
    resids = c()
    for (ii in 1:nrow(df)) {
        mod = lm(form, df2[-ii,])
        residii = predict(mod, df2[ii,]) - df2$simple[ii]
        resids = c(resids, residii)
    }
    results$loo[count] = mean(resids^2, na.rm=T)
}

results

df$fold = rep(1:10, each=ceiling(nrow(df) / 10))[1:nrow(df)]

results$tenfold = NA
for (count in 1:20) {
    df2 = make.lags(df, 'simple', count)
    form = make.formula('simple', count)
    resids = c()
    for (fold in 1:10) {
        mod = lm(form, df2[df2$fold != fold,])
        residii = predict(mod, df2[df2$fold == fold,]) - df2$simple[df2$fold == fold]
        resids = c(resids, residii)
    }
    results$tenfold[count] = mean(resids^2, na.rm=T)
}

results

ggplot(results, aes(count)) +
geom_line(aes(y=mse, colour='MSE')) + 
geom_line(aes(y=loo, colour='LOO')) + 
geom_line(aes(y=tenfold, colour='10x'))

df2 = df[, c('time', 'simple', 'Lsimple')]
mod = lm(simple ~ Lsimple, data=df)
for (ii in 1:24) {
    lastrow = tail(df2, 1)
    simple = predict(mod, data.frame(Lsimple=lastrow$simple))
    df2 = rbind(df2, data.frame(time=lastrow$time + 1/12, simple, Lsimple=lastrow$simple))
}

ggplot(df, aes(time, simple)) +
    geom_col() + geom_point(data=df2, aes(y=simple))

df2 = make.lags(df, 'simple', 2)[, c('time', 'simple', 'Lsimple', 'LLsimple')]
mod = lm(simple ~ Lsimple + LLsimple, data=df2)
for (ii in 1:24) {
    lastrow = tail(df2, 1)
    nextrow = data.frame(time=lastrow$time + 1/12, Lsimple=lastrow$simple, LLsimple=lastrow$Lsimple)
    nextrow$simple = predict(mod, nextrow)
    df2 = rbind(df2, nextrow)
}

ggplot(df, aes(time, simple)) +
    geom_col() + geom_point(data=df2, aes(y=simple))

project = function(df2, count, mod, len) {
    for (ii in 1:len) {
        df2 = rbind(df2, tail(df2, 1))
        df2 = make.lags(df2, 'simple', count)
        df2$time[nrow(df2)] = lastrow$time[nrow(df2)] + 1/12
        df2$simple[nrow(df2)] = predict(mod, df2[nrow(df2),])
    }
    df2
}

results$tenfold2 = NA
for (count in 1:20) {
    df2 = make.lags(df, 'simple', count)
    form = make.formula('simple', count)
    resids = c()
    for (fold in 2:10) {
        mod = lm(form, df2[df2$fold != fold,])
        df3 = project(df2[df2$fold < fold,], count, mod, sum(df2$fold == fold))
        residii = df3$simple[df2$fold == fold] - df2$simple[df2$fold == fold]
        resids = c(resids, residii)
    }
    results$tenfold2[count] = mean(resids^2, na.rm=T)
}

results


