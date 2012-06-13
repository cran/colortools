pals <- 
function(name=NULL, bg="white")
{
	pal_cols = list(
		c("#69D2E7", "#A7DBD8", "#E0E4CC", "#F38630", "#FA6900"),
		c("#556270", "#4ECDC4", "#C7F464", "#FF6B6B", "#C44D58"),
		c("#0B486B", "#3B8686", "#79BD9A", "#A8DBA8", "#C44D58"),
		c("#4B4452", "#487D76", "#92B55F", "#E8DA5E", "#FF4746"),
		c("#00A0B0", "#6A4A3C", "#CC333F", "#EB6841", "#EDC951"),
		c("#554236", "#F77825", "#D3CE3D", "#F1EFA5", "#60B99A"),
		c("#E8DDCB", "#CDB380", "#036564", "#033649", "#031634"),
		c("#594F4F", "#547980", "#45ADA8", "#9DE0AD", "#E5FCC2"),
		c("#1B676B", "#519548", "#88C425", "#BEF202", "#EAFDE6"),
		c("#B2D9F7", "#487AA1", "#3D3C3B", "#7C8071", "#DDE3CA"))
	pal_names = c("fish", "cheer", "drift", "spec", "ocean",
		"mystery", "terra", "pancake", "dream", "deserve")

	if (is.null(name))
    {
		n = 5
		par(mar = c(2, 5, 2, 2), bg=bg)
		plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
			axes = FALSE, xlab = "", ylab = "")
		k = 0
		h = 1/15
		for (i in 1:10)
		{
			rect(0:(n-1)/n, h-1/15, 1:n/n, h, col=pal_cols[[i]], border=bg, lwd=3)
			h = h + 1/10
		}
		cp = "black"
		if (mean(col2rgb(bg)) <= 127)
			cp = "white"
		mtext(pal_names, side=2, at = seq(0.04, .93, length=10), 
			col=cp, las=2)
		mtext("pals palette names", side=3, at=0.5, line=0, col=cp)
    } else {
		if (name %in% pal_names) {
			pal_cols[[which(pal_names == name)]]
		} else {
			stop("Wrong color name")
		}
	}
}



