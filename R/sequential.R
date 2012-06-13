sequential <-
function(color, percentage=5, what="saturation", 
	s=NULL, v=NULL, alpha=NULL, fun="linear", plot=TRUE, verbose=TRUE)
{	
	# convert to HSV
	col_hsv = rgb2hsv(col2rgb(color))[,1]
	# transparency
	if (is.null(alpha))
		alpha = 1
	if (substr(color, 1, 1) == "#" && nchar(color) == 9)
		alpha = substr(color, 8, 9)
	# get hue, saturation, and value
	hue = col_hsv[1]
	if (is.null(s)) s = col_hsv[2]
	if (is.null(v)) v = col_hsv[3]
	# sequence function
	getseq = switch(fun, 
		linear = seq(0, 1, by=percentage/100),
		sqrt = sqrt(seq(0, 1, by=percentage/100)),
		log = log1p(seq(0, 1, by=percentage/100)),
		log10 = log10(seq(0, 1, by=percentage/100))
		)
	# what type of sequence?
	if (what == "saturation") {
		sat = getseq
		fixed = paste("v=", round(v,2), " and alpha=", alpha, sep="")
		if (is.numeric(alpha))
			seq_col = hsv(hue, s=sat, v=v, alpha=alpha)
		if (is.character(alpha)) {
			seq_col = hsv(hue, s=sat, v=v)
			seq_col = paste(seq_col, alpha, sep="")
		}
	}
	if (what == "value") {
		val = getseq
		fixed = paste("s=", round(s,2), " and alpha=", alpha, sep="")
		if (is.numeric(alpha))
			seq_col = hsv(hue, s=s, v=val, alpha=alpha)
		if (is.character(alpha)) {
			seq_col = hsv(hue, s=s, v=val)
			seq_col = paste(seq_col, alpha, sep="")
		}
	}
	if (what == "alpha") {
		alpha = getseq
		fixed = paste("s=", round(s,2), " and v=", round(v,2), sep="")
		seq_col = hsv(hue, s=s, v=v, alpha=alpha)
	}
	# if plot TRUE
	if (plot)
	{
		n = length(seq(0, 1, by=percentage/100))
		fx = unlist(fixed)
		dev.new()
		plot(0, 0, type="n", xlim=c(0,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="")
		rect(0:(n-1)/n, 0, 1:n/n, 1, col=seq_col, border="lightgray")
		mtext(seq_col, side=1, at=0.5:(n)/n, cex=0.8, las=2)
		title(paste("Sequential colors based on ", what, "\n with fixed ", fx, sep=""),
			cex.main=0.9)
	}
	# result
	if (verbose)
		seq_col
}

