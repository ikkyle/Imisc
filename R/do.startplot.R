# args:
# x is the value thtat goes on the x axis (age_at_testing for example)
# y is your estimate
# ll and ul are your lower and upper limits
# eb.jitter is the amount of jitter to add to the error bars
# alength is the length of the head of the arrows
# additional arguments can be passed to do.startplot() like xlab, cex, etc.

# do.startplot will open a new graphic device and plot the first chunk of data (obtained by splitting your data on one or more factors)
# do.addplot will add additional layers onto the original plot with the supplied data and parameters

do.startplot <- function(x, y, ll, ul, col='black', lty=1, lwd=2, eb.jitter=0, alength=.08, bty='n',...){
  dots <- substitute(...)
  plot(x=x, y=y, bty=bty,
       col=col, type='l', lty=lty, lwd=lwd, ...)
  arrows(x0=x + eb.jitter, 
         y0=y,
         y1=c(ll, ul), 
         angle=90, length=alength, 
         col='black')
}

do.addplot <- function(x, y, ll, ul, col='black', lty=1, lwd=2, eb.jitter=0, alength=.08){
  lines(x=x, y=y, col=col, lty=lty, lwd=lwd)
  arrows(x0=x + eb.jitter, 
         y0=y,
         y1=c(ll, ul), 
         angle=90, length=alength, 
         col='black')
}
