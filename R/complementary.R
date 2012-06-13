complementary <-
function(color, plot=TRUE, bg="white", labcol=NULL, title=TRUE)
{	
	tmp_cols = setColors(color, 12)
	comp_colors <- tmp_cols[c(1, 7)]

	# plot
	if (plot)
	{
		# labels color
		if (is.null(labcol)) 
		{
			lab_col = rep("", 12)
			if (mean(col2rgb(bg)) > 127)
			{
				lab_col[c(1, 7)] <- "black"
				lab_col[c(2:6,8:12)] <- col2HSV(bg)
			} else {
				lab_col[c(1, 7)] <- "white"
				lab_col[c(2:6,8:12)] <- col2HSV(bg)
			}
		} else {
			lab_col = rep(labcol, 12)
			if (mean(col2rgb(bg)) > 127)
			{
				lab_col[c(1, 7)] <- labcol
				lab_col[c(2:6,8:12)] <- col2HSV(bg)
			} else {
				lab_col[c(1, 7)] <- labcol
				lab_col[c(2:6,8:12)] <- col2HSV(bg)
			}
		}	
		# hide non-adjacent colors
		tmp_cols[c(2:6,8:12)] <- paste(substr(tmp_cols[c(2:6,8:12)],1,7), "0D", sep="")
		pizza(tmp_cols, labcol=lab_col, bg=bg)
		# title
		if (title)
			title(paste("Complementary (opposite) color of: ", tmp_cols[1]), 
				col.main=lab_col[1], cex.main=0.8)
	}
	# result
	comp_colors
}
