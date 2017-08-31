
library(dplyr)

dat <- read.csv("test_curve.csv") %>%
  mutate(RWCrev = 1 - RWC,
         Pinv = 1/psi_bars)


with(dat, plot(RWCrev, Pinv, ylim=c(0,0.5), xlim=c(0, 0.1)))

tlp <- 0.02
curve(0.25*exp(-100*(x - tlp)) + 0.15 * (1 - 8*x), add=T)


pv_fun <- function(x, y0, k, tlp, y1, slope)y0*exp(-k*(x-tlp)) + y1 * (1 - slope*x)

fit <- nls(Pinv ~ pv_fun(RWCrev, y0, k, tlp, y1, slope), 
           start=list(y0=0.25, y1=0.15, k=100, tlp=0.02, slope=8),
           data=dat)
