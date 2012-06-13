col2HSV <-
function(color)
{
	# convert to RGB
	rgb_col = col2rgb(color)
	# convert to HSV
	hsv_col = rgb2hsv(rgb_col)
	if (length(color) == 1)
	{
		# get degree
		hue = hsv_col[1]
		sat = hsv_col[2]
		val = hsv_col[3]
		# get colors with hsv
		hex_col = hsv(hue, sat, val)
	}
	if (length(color) > 1)
	{
		hex_col = rep("", length(color))
		for (j in 1:length(color))
		{
			hex_col[j] = hsv(hsv_col[1,j], hsv_col[2,j], hsv_col[3,j])
		}
	}
	hex_col
}
