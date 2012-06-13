setColors <-
function(color, num)
{
	# convert to RGB
	rgb_col = col2rgb(color)
	# convert to HSV
	hsv_col = rgb2hsv(rgb_col)[,1]
	# get degree
	hue = hsv_col[1]
	sat = hsv_col[2]
	val = hsv_col[3]
	cols = seq(hue, hue + 1, by=1/num)
	cols = cols[1:num]
	cols[cols > 1] <- cols[cols > 1] - 1
	# get colors with hsv
	colors = hsv(cols, sat, val)
	# transparency
	if (substr(color, 1, 1) == "#" && nchar(color) == 9)
	{
		alpha = substr(color, 8, 9)
		colors = paste(colors, alpha, sep="")
	}
	colors
}
