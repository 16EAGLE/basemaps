# Avoid masking from terra::ext and transform to avoid warnings about 
# default transform to Web Mercator
ext <- sf::st_transform(ext_eur, "epsg:3857")