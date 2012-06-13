triadic <-
function(color, plot=TRUE, bg="white", labcol=NULL, title=TRUE)
{	
	tmp_cols = setColors(color, 12)
	triad_colors <- tmp_cols[c(1,5,9)]

	# plot
	if (plot)
	{
		# labels color
		if (is.null(labcol)) 
		{
			lab_col = rep("", 12)
			if (mean(col2rgb(bg)) > 127)
			{
				lab_col[c(1, 5, 9)] <- "black"
				lab_col[c(2,3,4,6,7,8,10,11,12)] <- col2HSV(bg)
			} else {
				lab_col[c(1, 5, 9)] <- "white"
				lab_col[c(2,3,4,6,7,8,10,11,12)] <- col2HSV(bg)
			}
		} else {
			lab_col = rep(labcol, 12)
			if (mean(col2rgb(bg)) > 127)
			{
				lab_col[c(1, 5, 9)] <- labcol
				lab_col[c(2,3,4,6,7,8,10,11,12)] <- col2HSV(bg)
			} else {
				lab_col[c(1, 5, 9)] <- labcol
				lab_col[c(2,3,4,6,7,8,10,11,12)] <- col2HSV(bg)
			}
		}
		# hide non-adjacent colors
		tmp_cols[c(2,3,4,6,7,8,10,11,12)] <- paste(
			substr(tmp_cols[c(2,3,4,6,7,8,10,11,12)],1,7), "0D", sep="")
		pizza(tmp_cols, labcol=lab_col, bg=bg)
		# title
		if (title)
			title(paste("Triadic color scheme of: ", tmp_cols[1]), 
				col.main=lab_col[1], cex.main=0.8)
	}
	# result
	triad_colors
}
