wheel <-
function(color, num=12, bg="gray95", border=NULL, 
	init.angle=105, cex=1, lty=NULL, main=NULL, verbose=TRUE, ...)
{
    if (!is.numeric(num) || any(is.na(num) | num < 0)) 
        stop("Sorry =(    'num' value must be positive")
	x <- rep(1, num)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
	# set colors
	col = setColors(color, num)
	labels = col
	# labels color
	labcol = ifelse( mean(col2rgb(bg)) > 127, "black", "white")
	# prepare plot window
	par(bg = bg)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L]) 
        xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    dev.hold()
    on.exit(dev.flush())
    plot.window(xlim, ylim, "", asp = 1)
	# get ready to plot
	border <- rep(border, length.out = nx)
	if (is.null(border[1]))
		border <- rep(bg, length.out = nx)
    lty <- rep(NULL, length.out = nx)
    angle <- rep(45, length.out = nx)
	radius = seq(1, 0, by=-1/num)[1:num]
    twopi <- -2 * pi
    t2xy <- function(t, rad) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = rad * cos(t2p), y = rad * sin(t2p))
    }
	# plot colored segments
    for (i in 1L:nx)
	{
        n <- max(2, floor(200 * dx[i]))
		P <- t2xy(seq.int(x[i], x[i + 1], length.out = n), rad=radius[1])
        polygon(c(P$x, 0), c(P$y, 0), angle = angle[i], 
            border = border[i], col = col[i], lty = lty[i])
        P <- t2xy(mean(x[i + 0:1]), rad=radius[1])
        lab <- labels[i]
        if (!is.na(lab) && nzchar(lab)) {
			adjs = 0.5
			if (P$x > 1e-08) adjs <- 0
			if (P$x < -1e-08) adjs <- 1
            lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
            text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE, 
                adj = adjs, cex=cex, col=labcol, ...)
        }
    }
	# add title
	title(main = main, ...)
	# return color names
	if (verbose)
		col
}
