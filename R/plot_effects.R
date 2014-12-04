plot_effects <- function (x, 
                           x.var, 
                           linetype = 'l',
                           hzline=FALSE,
                           z.var = which.min(levels), 
                           multiline = is.null(x$se), 
                           rug = TRUE, 
                           xlab, ylab, main = paste(effect, "effect plot"), 
                           colors = palette(), symbols = 1:length(colors), lines = 1:length(colors), 
                           cex = 1.5, lwd = 2, ylim, xlim = NULL, factor.names = TRUE, 
                           ci.style, band.transparency = 0.15, band.colors = colors, 
                           type = c("response", "link"), ticks = list(at = NULL, n = 5), 
                           alternating = TRUE, rotx = 0, roty = 0, grid = FALSE, layout, 
                           rescale.axis = TRUE, transform.x = NULL, ticks.x = NULL, 
                           key.args = NULL, row = 1, col = 1, nrow = 1, ncol = 1, more = FALSE, 
                           use.splines = TRUE, partial.residuals = c("adjusted", "raw"), 
                           show.fitted = FALSE, residuals.color = "blue", residuals.pch = 1, 
                           span = 2/3,...) 
{
  est <- x$fit
  .mod <- function(a, b) ifelse((d <- a%%b) == 0, b, d)
  .modc <- function(a) .mod(a, length(colors))
  .mods <- function(a) .mod(a, length(symbols))
  .modl <- function(a) .mod(a, length(lines))
  .modb <- function(a) .mod(a, length(band.colors))
  ci.style <- if (missing(ci.style)) 
    NULL
  else match.arg(ci.style, c("bars", "lines", "bands", "none"))
  type <- match.arg(type)
  partial.residuals <- match.arg(partial.residuals)
  levels <- sapply(x$variables, function(z) length(as.vector(z[["levels"]])))
  thresholds <- x$thresholds
  has.thresholds <- !is.null(thresholds)
  effect.llines <- llines
  if (missing(ylab)) {
    ylab <- if (has.thresholds) 
      paste(x$response, ": ", paste(x$y.levels, collapse = ", "), 
            sep = "")
    else x$response
  }
  if (has.thresholds) {
    threshold.labels <- abbreviate(x$y.levels, minlength = 1)
    threshold.labels <- paste(" ", paste(threshold.labels[-length(threshold.labels)], 
                                         threshold.labels[-1], sep = " - "), " ", sep = "")
  }
  trans.link <- x$transformation$link
  trans.inverse <- x$transformation$inverse
  if (!rescale.axis) {
    x$lower[!is.na(x$lower)] <- trans.inverse(x$lower[!is.na(x$lower)])
    x$upper[!is.na(x$upper)] <- trans.inverse(x$upper[!is.na(x$upper)])
    x$fit[!is.na(x$fit)] <- trans.inverse(x$fit)[!is.na(x$fit)]
    trans.link <- trans.inverse <- I
  }
  x.all <- x$x.all
  residuals <- if (partial.residuals == "adjusted") 
    x$partial.residuals.adjusted
  else x$partial.residuals.raw
  fitted <- x$fitted.rounded
  mod.matrix.all <- x$mod.matrix.all
  split <- c(col, row, ncol, nrow)
  ylab
  if (missing(x.var)) 
    x.var <- x$x.var
  if (!is.null(x.var) && is.numeric(x.var)) 
    x.var <- names(x.var)
  x.data <- x$data
  effect <- paste(sapply(x$variables, "[[", "name"), collapse = "*")
  vars <- x$variables
  x <- as.data.frame(x, transform = I)
  for (i in 1:length(vars)) {
    if (!(vars[[i]]$is.factor)) 
      next
    x[, i] <- factor(x[, i], levels = vars[[i]]$levels, exclude = NULL)
  }
  has.se <- !is.null(x$se)
  n.predictors <- ncol(x) - 1 - 3 * has.se
  if (n.predictors == 1) {
    predictor <- names(x)[1]
    if (is.factor(x[, 1])) {
      ci.style <- if (is.null(ci.style)) 
        "bars"
      else ci.style
      range <- if (has.se & ci.style != "none") 
        range(c(x$lower, x$upper), na.rm = TRUE)
      else range(x$fit, na.rm = TRUE)
      ylim <- if (!missing(ylim)) 
        ylim
      else c(range[1] - 0.025 * (range[2] - range[1]), 
             range[2] + 0.025 * (range[2] - range[1]))
      tickmarks <- if (type == "response") 
        make.ticks(ylim, link = trans.link, inverse = trans.inverse, 
                   at = ticks$at, n = ticks$n)
      else make.ticks(ylim, link = I, inverse = I, at = ticks$at, 
                      n = ticks$n)
      levs <- levels(x[, 1])
      plot <- xyplot(eval(parse(text = paste("fit ~ as.numeric(", 
                                             names(x)[1], ")"))),strip = function(...){strip.default(..., strip.names = c(factor.names, TRUE))}, panel = function(x, 
                                                                                                                                                                  y, lower, upper, has.se, ...) {
                                               if (hzline) {
                                                 panel.abline(h=est, reference=TRUE)}
                                               if (grid) 
                                                 panel.grid()
                                               good <- !is.na(y)
                                               if (has.se) {
                                                 if (ci.style == "bars") {
                                                   larrows(x0 = x[good], y0 = lower[good], x1 = x[good], 
                                                           y1 = upper[good], angle = 90, code = 3, 
                                                           col = colors[.modc(2)], length = 0.125 * 
                                                             cex/1.5)
                                                 }
                                                 else if (ci.style == "lines") {
                                                   effect.llines(x[good], lower[good], lty = 2, 
                                                                 col = colors[.modc(2)])
                                                   effect.llines(x[good], upper[good], lty = 2, 
                                                                 col = colors[.modc(2)])
                                                 }
                                                 else {
                                                   if (ci.style == "bands") {
                                                     panel.bands(x[good], y[good], upper[good], 
                                                                 lower[good], fill = band.colors[1], alpha = band.transparency, 
                                                                 use.splines = FALSE)
                                                   }
                                                 }
                                               }
                                               effect.llines(x[good], y[good], lwd = lwd, col = colors[1], 
                                                             type = linetype, pch = 19, cex = cex, ...)
                                               if (has.thresholds) {
                                                 panel.abline(h = thresholds, lty = 3)
                                                 panel.text(rep(current.panel.limits()$xlim[1], 
                                                                length(thresholds)), thresholds, threshold.labels, 
                                                            adj = c(0, 0), cex = 0.75)
                                                 panel.text(rep(current.panel.limits()$xlim[2], 
                                                                length(thresholds)), thresholds, threshold.labels, 
                                                            adj = c(1, 0), cex = 0.75)
                                               }
                                             }, ylim = ylim, ylab = ylab, xlab = if (missing(xlab)) 
                                               names(x)[1]
                     else xlab, scales = list(x = list(at = 1:length(levs), 
                                                       labels = levs, rot = rotx), y = list(at = tickmarks$at, 
                                                                                            labels = tickmarks$labels, rot = roty), alternating = alternating, 
                                              y = roty), main = main, lower = x$lower, upper = x$upper, 
                     has.se = has.se, data = x, ...)
      result <- update(plot, layout = if (missing(layout)) 
        c(0, prod(dim(plot)))
        else layout)
      result$split <- split
      result$more <- more
      class(result) <- c("plot.eff", class(result))
    }
    else {
      effect.llines <- if (use.splines) 
        spline.llines
      else effect.llines
      ci.style <- if (is.null(ci.style)) 
        "bands"
      else ci.style
      range <- if (has.se & ci.style != "none") 
        range(c(x$lower, x$upper), na.rm = TRUE)
      else range(x$fit, na.rm = TRUE)
      ylim <- if (!missing(ylim)) 
        ylim
      else if (is.null(residuals)) 
        c(range[1] - 0.025 * (range[2] - range[1]), range[2] + 
            0.025 * (range[2] - range[1]))
      else c(min(min(residuals), range[1] - 0.025 * (range[2] - 
                                                       range[1])), max(max(residuals), range[2] + 0.025 * 
                                                                         (range[2] - range[1])))
      tickmarks <- if (type == "response") 
        make.ticks(ylim, link = trans.link, inverse = trans.inverse, 
                   at = ticks$at, n = ticks$n)
      else make.ticks(ylim, link = I, inverse = I, at = ticks$at, 
                      n = ticks$n)
      nm <- names(x)[1]
      x.vals <- x.data[, nm]
      if (nm %in% names(ticks.x)) {
        at <- ticks.x[[nm]]$at
        n <- ticks.x[[nm]]$n
      }
      else {
        at <- NULL
        n <- 5
      }
      xlm <- if (nm %in% names(xlim)) {
        xlim[[nm]]
      }
      else range.adj(x[nm])
      tickmarks.x <- if ((nm %in% names(transform.x)) && 
                           !(is.null(transform.x))) {
        trans <- transform.x[[nm]]$trans
        make.ticks(trans(xlm), link = transform.x[[nm]]$trans, 
                   inverse = transform.x[[nm]]$inverse, at = at, 
                   n = n)
      }
      else {
        trans <- I
        make.ticks(xlm, link = I, inverse = I, at = at, 
                   n = n)
      }
      if (is.null(x.var)) {
        if (!is.null(residuals)) {
          x.var <- names(x)[1]
        }
        else x.var <- which.max(levels)
      }
      if (!is.null(residuals)) 
        x.fit <- x.data[, predictor]
      if (is.numeric(x.var)) 
        x.var <- predictor
      plot <- xyplot(eval(parse(text = paste("fit ~ trans(", 
                                             x.var, ")"))),strip = function(...){strip.default(..., strip.names = c(factor.names, TRUE))}, panel = function(x, 
                                                                                                                                                            y, x.vals, rug, lower, upper, has.se, ...) {
                                               if (hzline) {
                                                 panel.abline(h=est, reference=TRUE)}
                                               if (grid) 
                                                 panel.grid()
                                               good <- !is.na(y)
                                               axis.length <- diff(range(x))
                                               effect.llines(x[good], y[good], lwd = lwd, col = colors[1], 
                                                             ...)
                                               if (rug && is.null(residuals)) 
                                                 lrug(trans(x.vals))
                                               if (has.se) {
                                                 if (ci.style == "bars") {
                                                   larrows(x0 = x[good], y0 = lower[good], x1 = x[good], 
                                                           y1 = upper[good], angle = 90, code = 3, 
                                                           col = eval(colors[.modc(2)]), length = 0.125 * 
                                                             cex/1.5)
                                                 }
                                                 else if (ci.style == "lines") {
                                                   effect.llines(x[good], lower[good], lty = 2, 
                                                                 col = colors[.modc(2)])
                                                   effect.llines(x[good], upper[good], lty = 2, 
                                                                 col = colors[.modc(2)])
                                                 }
                                                 else {
                                                   if (ci.style == "bands") {
                                                     panel.bands(x[good], y[good], upper[good], 
                                                                 lower[good], fill = band.colors[1], alpha = band.transparency, 
                                                                 use.splines = use.splines)
                                                   }
                                                 }
                                               }
                                               if (has.thresholds) {
                                                 panel.abline(h = thresholds, lty = 3)
                                                 panel.text(rep(current.panel.limits()$xlim[1], 
                                                                length(thresholds)), thresholds, threshold.labels, 
                                                            adj = c(0, 0), cex = 0.75)
                                                 panel.text(rep(current.panel.limits()$xlim[2], 
                                                                length(thresholds)), thresholds, threshold.labels, 
                                                            adj = c(1, 0), cex = 0.75)
                                               }
                                               if (!is.null(residuals)) {
                                                 lpoints(trans(x.fit), residuals, col = residuals.color, 
                                                         pch = residuals.pch)
                                                 if (show.fitted) 
                                                   lpoints(trans(x.fit), fitted, pch = 16, col = residuals.color)
                                                 llines(loess.smooth(trans(x.fit), residuals, 
                                                                     span = span), lwd = 2, lty = 2, col = residuals.color)
                                               }
                                             }, ylim = ylim, xlim = suppressWarnings(trans(xlm)), 
                     ylab = ylab, xlab = if (missing(xlab)) 
                       names(x)[1]
                     else xlab, x.vals = x.vals, rug = rug, main = main, 
                     lower = x$lower, upper = x$upper, has.se = has.se, 
                     data = x, scales = list(y = list(at = tickmarks$at, 
                                                      labels = tickmarks$labels, rot = roty), x = list(at = tickmarks.x$at, 
                                                                                                       labels = tickmarks.x$labels, rot = rotx), alternating = alternating), 
                     ...)
      result <- update(plot, layout = if (missing(layout)) 
        c(0, prod(dim(plot)))
        else layout)
      result$split <- split
      result$more <- more
      class(result) <- c("plot.eff", class(result))
    }
    return(result)
  }
  predictors <- names(x)[1:n.predictors]
  levels <- sapply(apply(x[, predictors], 2, unique), length)
  if (is.null(x.var)) {
    if (!is.null(residuals)) {
      x.var <- names(x)[1]
    }
    else x.var <- which.max(levels)
  }
  if (!is.null(residuals)) 
    x.fit <- x.data[, x.var]
  if (is.character(x.var)) {
    which.x <- which(x.var == predictors)
    if (length(which.x) == 0) 
      stop(paste("x.var = '", x.var, "' is not in the effect.", 
                 sep = ""))
    x.var <- which.x
  }
  if (is.character(z.var)) {
    which.z <- which(z.var == predictors)
    if (length(which.z) == 0) 
      stop(paste("z.var = '", z.var, "' is not in the effect.", 
                 sep = ""))
    z.var <- which.z
  }
  if (x.var == z.var) 
    z.var <- z.var + 1
  if (multiline) {
    ci.style <- if (is.null(ci.style)) 
      "none"
    else ci.style
    if (ci.style == "lines") {
      cat("Confidence interval style 'lines' changed to 'bars'\n")
      ci.style <- "bars"
    }
    range <- if (has.se && ci.style != "none") 
      range(c(x$lower, x$upper), na.rm = TRUE)
    else range(x$fit, na.rm = TRUE)
    ylim <- if (!missing(ylim)) 
      ylim
    else c(range[1] - 0.025 * (range[2] - range[1]), range[2] + 
             0.025 * (range[2] - range[1]))
    tickmarks <- if (type == "response") 
      make.ticks(ylim, link = trans.link, inverse = trans.inverse, 
                 at = ticks$at, n = ticks$n)
    else make.ticks(ylim, link = I, inverse = I, at = ticks$at, 
                    n = ticks$n)
    zvals <- unique(x[, z.var])
    if (is.factor(x[, x.var])) {
      levs <- levels(x[, x.var])
      key <- list(title = predictors[z.var], 
                  cex.title = 1, 
                  text = list(as.character(zvals)), 
                  lines = list(col = colors[.modc(1:length(zvals))], 
                               lty = lines[.modl(1:length(zvals))], lwd = lwd), 
                  points = list(col = colors[.modc(1:length(zvals))], 
                                pch = symbols[.mods(1:length(zvals))]))
      key <- c(key[!names(key) %in% names(key.args)], key.args)
      plot <- xyplot(eval(parse(text = paste("fit ~ as.numeric(", 
                                             predictors[x.var], ")", if (n.predictors > 2) 
                                               paste(" |", paste(predictors[-c(x.var, z.var)])), 
                                             collapse = "*"))), strip = function(...){strip.default(..., strip.names = c(factor.names, TRUE))}, panel = function(x, 
                                                                                                                                                                 y, subscripts, z, lower, upper, show.se, ...) {
                                               if (hzline) {
                                                 panel.abline(h=est, reference=TRUE)}
                                               if (grid) 
                                                 panel.grid()
                                               for (i in 1:length(zvals)) {
                                                 sub <- z[subscripts] == zvals[i]
                                                 good <- !is.na(y[sub])
                                                 os <- if (show.se) 
                                                   (i - (length(zvals) + 1)/2) * (2/(length(zvals) - 
                                                                                       1)) * 0.01 * (length(zvals) - 1)
                                                 else 0
                                                 effect.llines(x[sub][good] + os, y[sub][good], 
                                                               lwd = lwd, type = linetype, col = colors[.modc(i)], 
                                                               pch = symbols[.mods(i)], lty = lines[.modl(i)], 
                                                               cex = cex, ...)
                                                 if (show.se) {
                                                   larrows(x0 = x[sub][good] + os, y0 = lower[subscripts][sub][good], 
                                                           x1 = x[sub][good] + os, y1 = upper[subscripts][sub][good], 
                                                           angle = 90, code = 3, col = eval(colors[.modc(i)]), 
                                                           length = 0.125 * cex/1.5)
                                                 }
                                               }
                                               if (has.thresholds) {
                                                 panel.abline(h = thresholds, lty = 3)
                                                 panel.text(rep(current.panel.limits()$xlim[1], 
                                                                length(thresholds)), thresholds, threshold.labels, 
                                                            adj = c(0, 0), cex = 0.75)
                                                 panel.text(rep(current.panel.limits()$xlim[2], 
                                                                length(thresholds)), thresholds, threshold.labels, 
                                                            adj = c(1, 0), cex = 0.75)
                                               }
                                             }, ylim = ylim, ylab = ylab, xlab = if (missing(xlab)) 
                                               predictors[x.var]
                     else xlab, z = x[, z.var], scales = list(x = list(at = 1:length(levs), 
                                                                       labels = levs, rot = rotx), y = list(at = tickmarks$at, 
                                                                                                            labels = tickmarks$labels, rot = roty), alternating = alternating), 
                     zvals = zvals, main = main, key = key, lower = x$lower, 
                     upper = x$upper, show.se = has.se && ci.style == 
                       "bars", data = x, ...)
      result <- update(plot, layout = if (missing(layout)) 
        c(0, prod(dim(plot)))
        else layout)
      result$split <- split
      result$more <- more
      class(result) <- c("plot.eff", class(result))
    }
    else {
      effect.llines <- if (use.splines) 
        spline.llines
      else effect.llines
      nm <- names(x)[x.var]
      x.vals <- x.data[, nm]
      if (nm %in% names(ticks.x)) {
        at <- ticks.x[[nm]]$at
        n <- ticks.x[[nm]]$n
      }
      else {
        at <- NULL
        n <- 5
      }
      xlm <- if (nm %in% names(xlim)) {
        xlim[[nm]]
      }
      else range.adj(x[nm])
      tickmarks.x <- if ((nm %in% names(transform.x)) && 
                           !(is.null(transform.x))) {
        trans <- transform.x[[nm]]$trans
        make.ticks(trans(xlm), link = transform.x[[nm]]$trans, 
                   inverse = transform.x[[nm]]$inverse, at = at, 
                   n = n)
      }
      else {
        trans <- I
        make.ticks(xlm, link = I, inverse = I, at = at, 
                   n = n)
      }
      key <- list(title = predictors[z.var], 
                  cex.title = 1, 
                  text = list(as.character(zvals)), 
                  lines = list(col = colors[.modc(1:length(zvals))], 
                               lty = lines[.modl(1:length(zvals))], lwd = lwd))
      key <- c(key[!names(key) %in% names(key.args)], key.args)
      plot <- xyplot(eval(parse(text = paste("fit ~trans(", 
                                             predictors[x.var], ")", if (n.predictors > 2) 
                                               paste(" |", paste(predictors[-c(x.var, z.var)])), 
                                             collapse = "*"))),strip = function(...){strip.default(..., strip.names = c(factor.names, TRUE))}, panel = function(x, 
                                                                                                                                                                y, subscripts, x.vals, rug, z, lower, upper, 
                                                                                                                                                                show.se, ...) {
                                               if (hzline) {
                                                 panel.abline(h=est, reference=TRUE)}
                                               if (grid) 
                                                 panel.grid()
                                               if (rug && is.null(residuals)) 
                                                 lrug(trans(x.vals))
                                               axis.length <- diff(range(x))
                                               for (i in 1:length(zvals)) {
                                                 sub <- z[subscripts] == zvals[i]
                                                 good <- !is.na(y[sub])
                                                 effect.llines(x[sub][good], y[sub][good], lwd = lwd, 
                                                               type = linetype, col = colors[.modc(i)], lty = lines[.modl(i)], 
                                                               cex = cex, ...)
                                                 if (show.se) {
                                                   if (ci.style == "bars") {
                                                     os <- (i - (length(zvals) + 1)/2) * (2/(length(zvals) - 
                                                                                               1)) * 0.01 * axis.length
                                                     larrows(x0 = x[sub][good] + os, y0 = lower[subscripts][sub][good], 
                                                             x1 = x[sub][good] + os, y1 = upper[subscripts][sub][good], 
                                                             angle = 90, code = 3, col = eval(colors[.modc(i)]), 
                                                             length = 0.125 * cex/1.5)
                                                   }
                                                   if (ci.style == "bands") {
                                                     panel.bands(x[sub][good], y[sub][good], 
                                                                 upper[subscripts][sub][good], lower[subscripts][sub][good], 
                                                                 fill = eval(band.colors[.modb(i)]), alpha = band.transparency, 
                                                                 use.splines = use.splines)
                                                   }
                                                 }
                                               }
                                               if (has.thresholds) {
                                                 panel.abline(h = thresholds, lty = 3)
                                                 panel.text(rep(current.panel.limits()$xlim[1], 
                                                                length(thresholds)), thresholds, threshold.labels, 
                                                            adj = c(0, 0), cex = 0.75)
                                                 panel.text(rep(current.panel.limits()$xlim[2], 
                                                                length(thresholds)), thresholds, threshold.labels, 
                                                            adj = c(1, 0), cex = 0.75)
                                               }
                                             }, ylim = ylim, xlim = suppressWarnings(trans(xlm)), 
                     ylab = ylab, xlab = if (missing(xlab)) 
                       predictors[x.var]
                     else xlab, x.vals = x.vals, rug = rug, z = x[, 
                                                                  z.var], zvals = zvals, main = main, key = key, 
                     lower = x$lower, upper = x$upper, show.se = has.se && 
                       ci.style %in% c("bars", "bands"), data = x, 
                     scales = list(y = list(at = tickmarks$at, labels = tickmarks$labels), 
                                   rot = roty, x = list(at = tickmarks.x$at, labels = tickmarks.x$labels, 
                                                        rot = rotx), alternating = alternating), 
                     ...)
      result <- update(plot, layout = if (missing(layout)) 
        c(0, prod(dim(plot)))
        else layout)
      result$split <- split
      result$more <- more
      class(result) <- c("plot.eff", class(result))
    }
    return(result)
  }
  ci.style <- if (is.null(ci.style)) {
    if (is.factor(x[, x.var])) 
      "bars"
    else "bands"
  }
  else ci.style
  range <- if (has.se && ci.style != "none") 
    range(c(x$lower, x$upper), na.rm = TRUE)
  else range(x$fit, na.rm = TRUE)
  if (is.factor(x[, x.var])) {
    ylim <- if (!missing(ylim)) 
      ylim
    else c(range[1] - 0.025 * (range[2] - range[1]), range[2] + 
             0.025 * (range[2] - range[1]))
    tickmarks <- if (type == "response") 
      make.ticks(ylim, link = trans.link, inverse = trans.inverse, 
                 at = ticks$at, n = ticks$n)
    else make.ticks(ylim, link = I, inverse = I, at = ticks$at, 
                    n = ticks$n)
    levs <- levels(x[, x.var])
    plot <- xyplot(eval(parse(text = paste("fit ~ as.numeric(", 
                                           predictors[x.var], ") |", paste(predictors[-x.var], 
                                                                           collapse = "*")))),strip = function(...){strip.default(..., strip.names = c(factor.names, TRUE))}, panel = function(x, 
                                                                                                                                                                                               y, subscripts, lower, upper, has.se, ...) {
                                                                             if (hzline) {
                                                                               panel.abline(h=est, reference=TRUE)}
                                                                             if (grid) 
                                                                               panel.grid()
                                                                             good <- !is.na(y)
                                                                             if (has.se) {
                                                                               if (ci.style == "bars") {
                                                                                 larrows(x0 = x[good], y0 = lower[subscripts][good], 
                                                                                         x1 = x[good], y1 = upper[subscripts][good], 
                                                                                         angle = 90, code = 3, col = colors[.modc(2)], 
                                                                                         length = 0.125 * cex/1.5)
                                                                               }
                                                                               else if (ci.style == "lines") {
                                                                                 effect.llines(x[good], lower[subscripts][good], 
                                                                                               lty = 2, col = colors[.modc(2)])
                                                                                 effect.llines(x[good], upper[subscripts][good], 
                                                                                               lty = 2, col = colors[.modc(2)])
                                                                               }
                                                                               else {
                                                                                 if (ci.style == "bands") {
                                                                                   panel.bands(x[good], y[good], upper[subscripts][good], 
                                                                                               lower[subscripts][good], fill = band.colors[1], 
                                                                                               alpha = band.transparency, use.splines = FALSE)
                                                                                 }
                                                                               }
                                                                             }
                                                                             effect.llines(x[good], y[good], lwd = lwd, type = linetype, 
                                                                                           col = colors[1], pch = 19, cex = cex, ...)
                                                                             if (has.thresholds) {
                                                                               panel.abline(h = thresholds, lty = 3)
                                                                               panel.text(rep(current.panel.limits()$xlim[1], 
                                                                                              length(thresholds)), thresholds, threshold.labels, 
                                                                                          adj = c(0, 0), cex = 0.75)
                                                                               panel.text(rep(current.panel.limits()$xlim[2], 
                                                                                              length(thresholds)), thresholds, threshold.labels, 
                                                                                          adj = c(1, 0), cex = 0.75)
                                                                             }
                                                                           }, ylim = ylim, ylab = ylab, xlab = if (missing(xlab)) 
                                                                             predictors[x.var]
                   else xlab, scales = list(x = list(at = 1:length(levs), 
                                                     labels = levs, rot = rotx), y = list(at = tickmarks$at, 
                                                                                          labels = tickmarks$labels, rot = roty), alternating = alternating), 
                   main = main, lower = x$lower, upper = x$upper, has.se = has.se, 
                   data = x, ...)
    result <- update(plot, layout = if (missing(layout)) 
      c(0, prod(dim(plot)))
      else layout)
    result$split <- split
    result$more <- more
    class(result) <- c("plot.eff", class(result))
  }
  else {
    effect.llines <- if (use.splines) 
      spline.llines
    else effect.llines
    nm <- names(x)[x.var]
    x.vals <- x.data[, nm]
    if (nm %in% names(ticks.x)) {
      at <- ticks.x[[nm]]$at
      n <- ticks.x[[nm]]$n
    }
    else {
      at <- NULL
      n <- 5
    }
    xlm <- if (nm %in% names(xlim)) {
      xlim[[nm]]
    }
    else range.adj(x[nm])
    tickmarks.x <- if ((nm %in% names(transform.x)) && !(is.null(transform.x))) {
      trans <- transform.x[[nm]]$trans
      make.ticks(trans(xlm), link = transform.x[[nm]]$trans, 
                 inverse = transform.x[[nm]]$inverse, at = at, 
                 n = n)
    }
    else {
      trans <- I
      make.ticks(xlm, link = I, inverse = I, at = at, n = n)
    }
    ylim <- if (!missing(ylim)) 
      ylim
    else if (is.null(residuals)) 
      c(range[1] - 0.025 * (range[2] - range[1]), range[2] + 
          0.025 * (range[2] - range[1]))
    else c(min(min(residuals), range[1] - 0.025 * (range[2] - 
                                                     range[1])), max(max(residuals), range[2] + 0.025 * 
                                                                       (range[2] - range[1])))
    tickmarks <- if (type == "response") 
      make.ticks(ylim, link = trans.link, inverse = trans.inverse, 
                 at = ticks$at, n = ticks$n)
    else make.ticks(ylim, link = I, inverse = I, at = ticks$at, 
                    n = ticks$n)
    x.fit <- x.data[, predictors[x.var]]
    use <- rep(TRUE, length(residuals))
    xx <- x[, predictors[-x.var], drop = FALSE]
    plot <- xyplot(eval(parse(text = paste("fit ~ trans(", 
                                           predictors[x.var], ") |", paste(predictors[-x.var], 
                                                                           collapse = "*")))), strip = function(...){strip.default(..., strip.names = c(factor.names, TRUE))}, panel = function(x, 
                                                                                                                                                                                                y, subscripts, x.vals, rug, lower, upper, has.se, 
                                                                                                                                                                                                ...) {
                                                                             if (hzline) {
                                                                               panel.abline(h=est, reference=TRUE)}
                                                                             if (grid) 
                                                                               panel.grid()
                                                                             good <- !is.na(y)
                                                                             effect.llines(x[good], y[good], lwd = lwd, col = colors[1], 
                                                                                           ...)
                                                                             if (rug && is.null(residuals)) 
                                                                               lrug(trans(x.vals))
                                                                             if (has.se) {
                                                                               if (ci.style == "bars") {
                                                                                 larrows(x0 = x[good], y0 = lower[subscripts][good], 
                                                                                         x1 = x[good], y1 = upper[subscripts][good], 
                                                                                         angle = 90, code = 3, col = eval(colors[.modc(2)]), 
                                                                                         length = 0.125 * cex/1.5)
                                                                               }
                                                                               else if (ci.style == "lines") {
                                                                                 effect.llines(x[good], lower[subscripts][good], 
                                                                                               lty = 2, col = colors[.modc(2)])
                                                                                 effect.llines(x[good], upper[subscripts][good], 
                                                                                               lty = 2, col = colors[.modc(2)])
                                                                               }
                                                                               else if (ci.style == "bands") {
                                                                                 panel.bands(x[good], y[good], upper[subscripts][good], 
                                                                                             lower[subscripts][good], fill = band.colors[1], 
                                                                                             alpha = band.transparency, use.splines = use.splines)
                                                                               }
                                                                               if (!is.null(residuals)) {
                                                                                 predictors <- predictors[-x.var]
                                                                                 factors <- sapply(xx, is.factor)
                                                                                 for (predictor in predictors) {
                                                                                   use <- use & if (factors[predictor]) 
                                                                                     x.all[, predictor] == xx[subscripts[1], 
                                                                                                              predictor]
                                                                                   else x.all[, predictor] == xx[subscripts[1], 
                                                                                                                 predictor]
                                                                                 }
                                                                                 n.in.panel <- sum(use)
                                                                                 if (n.in.panel > 0) {
                                                                                   lpoints(trans(x.fit[use]), residuals[use], 
                                                                                           col = residuals.color, pch = residuals.pch)
                                                                                   if (show.fitted) 
                                                                                     lpoints(trans(x.fit[use]), fitted[use], 
                                                                                             pch = 16, col = residuals.color)
                                                                                   if (n.in.panel >= 10) 
                                                                                     llines(loess.smooth(x.fit[use], residuals[use], 
                                                                                                         span = span), lwd = 2, lty = 2, col = residuals.color)
                                                                                 }
                                                                               }
                                                                             }
                                                                             if (has.thresholds) {
                                                                               panel.abline(h = thresholds, lty = 3)
                                                                               panel.text(rep(current.panel.limits()$xlim[1], 
                                                                                              length(thresholds)), thresholds, threshold.labels, 
                                                                                          adj = c(0, 0), cex = 0.75)
                                                                               panel.text(rep(current.panel.limits()$xlim[2], 
                                                                                              length(thresholds)), thresholds, threshold.labels, 
                                                                                          adj = c(1, 0), cex = 0.75)
                                                                             }
                                                                           }, ylim = ylim, xlim = suppressWarnings(trans(xlm)), 
                   ylab = ylab, xlab = if (missing(xlab)) 
                     predictors[x.var]
                   else xlab, x.vals = x.vals, rug = rug, main = main, 
                   lower = x$lower, upper = x$upper, has.se = has.se, 
                   data = x, scales = list(y = list(at = tickmarks$at, 
                                                    labels = tickmarks$labels, rot = roty), x = list(at = tickmarks.x$at, 
                                                                                                     labels = tickmarks.x$labels, rot = rotx), alternating = alternating), 
                   ...)
    result <- update(plot, layout = if (missing(layout)) 
      c(0, prod(dim(plot)))
      else layout)
    result$split <- split
    result$more <- more
    class(result) <- c("plot.eff", class(result))
  }
  return(result)
}

spline.llines <- function (x, y, ...) {
  llines(spline(x, y), ...)
}

make.ticks <- function (range, link, inverse, at, n) 
{
  warn <- options(warn = -1)
  on.exit(warn)
  link <- if (is.null(link)) 
    function(x) nlm(function(y) (inverse(y) - x)^2, mean(range))$estimate
  else link
  if (is.null(n)) 
    n <- 5
  labels <- if (is.null(at)) {
    labels <- pretty(sapply(range, inverse), n = n + 1)
  }
  else at
  ticks <- sapply(labels, link)
  list(at = ticks, labels = format(labels))
}

range.adj <- function (x) 
{
  range <- range(x, na.rm = TRUE)
  c(range[1] - 0.025 * (range[2] - range[1]), range[2] + 0.025 * 
      (range[2] - range[1]))
}

lrug <- function (x) 
{
  if (length(unique(x)) < 0.8 * length(x)) 
    x <- jitter(x)
  grid.segments(x, unit(0, "npc"), x, unit(0.5, "lines"), default.units = "native")
}

panel.bands <- function (x, y, upper, lower, fill, col, subscripts, ..., font, 
                         fontface, use.splines = FALSE) 
{
  if (!missing(subscripts)) {
    upper <- upper[subscripts]
    lower <- lower[subscripts]
  }
  if (use.splines) {
    up <- spline(x, upper)
    down <- spline(x, lower)
    x <- up$x
    upper <- up$y
    lower <- down$y
  }
  panel.polygon(c(x, rev(x)), c(upper, rev(lower)), col = fill, 
                fill = fill, border = FALSE, ...)
}
