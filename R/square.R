square <-
function(color, plot=TRUE, bg="white", labcol=NULL, title=TRUE)
{	
	tmp_cols = setColors(color, 12)
	sqr_colors <- tmp_cols[c(1,4,7,10)]

	# plot
	if (plot)
	{
		# labels color
		if (is.null(labcol)) 
		{
			lab_col = rep("", 12)
			if (mean(col2rgb(bg)) > 127)
			{
				lab_col[c(1,4,7,10)] <- "black"
				lab_col[c(2,3,5,6,8,9,11,12)] <- col2HSV(bg)
			} else {
				lab_col[c(1,4,7,10)] <- "white"
				lab_col[c(2,3,5,6,8,9,11,12)] <- col2HSV(bg)
			}
		} else {
			lab_col = rep(labcol, 12)
			if (mean(col2rgb(bg)) > 127)
			{
				lab_col[c(1,4,7,10)] <- labcol
				lab_col[c(2,3,5,6,8,9,11,12)] <- col2HSV(bg)
			} else {
				lab_col[c(1,4,7,10)] <- labcol
				lab_col[c(2,3,5,6,8,9,11,12)] <- col2HSV(bg)
			}
		}	
		# hide non-adjacent colors
		tmp_cols[c(2,3,5,6,8,9,11,12)] <- paste(substr(tmp_cols[c(2,3,5,6,8,9,11,12)],1,7), "0D", sep="")
		pizza(tmp_cols, labcol=lab_col, bg=bg)
		# title
		if (title)
			title(paste("Square color scheme of: ", tmp_cols[1]), 
				col.main=lab_col[1], cex.main=0.8)
	}
	# result
	sqr_colors
}
