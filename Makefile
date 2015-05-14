# Google: full moon calendar 1900 2010
#
fullmoon.htm:
	wget 'http://home.hiwaay.net/~krcool/Astro/moon/fullmoon.htm'

FullMoons1900-2100.txt: FullMoons1900-2100.html
	awk '/^[12]/ && NF==6' $^ > $@

centennial_README.rtf:
	wget http://earthquake.usgs.gov/data/centennial/centennial_README.rtf

centennial_Y2K.CAT:
	wget http://earthquake.usgs.gov/data/centennial/centennial_Y2K.CAT