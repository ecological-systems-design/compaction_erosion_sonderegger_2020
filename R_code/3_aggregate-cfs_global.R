#install.packages('readxl')
#install.packages('spatstat')
library(readxl)
library(spatstat)

start.time = Sys.time()


# Set working directory
#setwd('/home/sothomas/R_code/soil')
setwd('C:/Users/Sonderegger/Documents/Sync/R_code/soil')

df = read_excel('./data/calculated/SI7_characterization-factors_COPY.xlsx', sheet='cfs', col_names=TRUE)
df


m = df[['comp_ts_m']]
s = df[['comp_ts_sd']]
w = df[['comp_ts_w']]

weighted.median(m, w, na.rm=T)
s = s[is.na(s) == F]
n = length(s)
sqrt(exp(( sum(log(s^2)^2) )^(1/n)))


m = df[['comp_ms_m']]
s = df[['comp_ms_sd']]
w = df[['comp_ms_w']]

weighted.median(m, w, na.rm=T)
s = s[is.na(s) == F]
n = length(s)
sqrt(exp(( sum(log(s^2)^2) )^(1/n)))


m = df[['comp_bs_m']]
s = df[['comp_bs_sd']]
w = df[['comp_ms_w']]

weighted.median(m, w, na.rm=T)
s = s[is.na(s) == F]
n = length(s)
sqrt(exp(( sum(log(s^2)^2) )^(1/n)))


m = df[['krls_m']]
s = df[['krls_sd']]
w = df[['krls_w']]

weighted.median(m, w, na.rm=T)
s = s[is.na(s) == F]
n = length(s)
sqrt(exp(( sum(log(s^2)^2) )^(1/n)))

