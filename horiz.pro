; horiz.pro

READ_JPEG,'plots_tables_images/100kft/horiz.jpg',image
redc = image[0,*,*]

mask = redc gt 100






stop

end