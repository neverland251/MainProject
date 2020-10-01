library('ggmap')

geo <-
  read.csv("c://users//23//Desktop//경위도.csv", stringsAsFactors = F)
geo <- geo[, c("Long_DD", "Lat_DD")]
colnames(geo) <- c("long", "lat")

water_site <-
  read.csv("C://Users//23//Desktop//Project//조류 예측 심화//데이터//데이터셋//수질자동측정망//수질자동측정망지점정보.csv")

first <- function(x) {
  k <- regexpr("-", x)
  return(k[1])
}

substrs_1 <- function(x) {
  locations <- first(x)
  substr(x, locations, locations) <- "."
  return(x)
}

substrs_2 <- function(x) {
  locations <- first(x)
  x <- gsub("-", "", x)
  return(x)
}

#temp <- sapply(geo[,1],substrs_1,USE.NAMES = F)
#temp <- sapply(temp,substrs_2,USE.NAMES = F)

#geo[,1] <- as.numeric(temp)

#temp <- sapply(geo[,2],substrs_1,USE.NAMES = F)
#temp <- sapply(temp,substrs_2,USE.NAMES = F)

#geo[,2] <- as.numeric(temp)

register_google(key = "AIzaSyDEWl05jlZfuNLEAe1aP_1lEFVlDJlUYVY")

geocode_utf <- function(x) {
  x <- as.character(x)
  return(geocode(enc2utf8(x)))
}

water_site_geo <- sapply(water_site$주소, geocode_utf, USE.NAMES = F)
water_site_geo <- as.data.frame(t(water_site_geo))
water_site_geo$lon <- unlist(water_site_geo$lon)
water_site_geo$lat <- unlist(water_site_geo$lat)


write.csv(water_site_geo, "C://Users//23//Desktop//Project//조류 예측 심화//데이터//데이터셋//수질자동측정망//geocode.csv")

korea_map <-
  get_map(
    location = colMeans(geo),
    zoom = 8,
    scale = 2,
    maptype = "terrain",
    size = c(640, 640)
  )
ggmap(korea_map) + 
  geom_point(data = geo, aes(x = long, y = lat)) + 
  geom_point(data = water_site_geo, aes(x = lon, y = lat), color = "red")
