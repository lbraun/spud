#' @importFrom graphics plot
#' @importFrom utils read.table

# spu: R classes and methods for spatial app usage data

# install.packages("leaflet")

read.spu = function(file, header = TRUE, sep = ",") {
  spu_object = read.table(system.file("extdata", file, package = "spu"), header = header, sep = sep)
  spu_object$datetime = strptime(spu_object$datetime, "%Y-%m-%d %H:%M:%S")
  class(spu_object) = append("spu", class(spu_object))
  spu_object
}

plot.spu = function(x, xlab = "Longitude", ylab = "Latitude", pch = "+", ...) {
  plot(x = x$longitude, y = x$latitude, xlab = xlab, ylab = ylab, pch = pch, ...)
}

data = read.spu("dummy_data.csv")

plot(data$latitude, data$longitude)

# web_mercator = CRS("+init=epsg:3857")

# library(ggmap)
# bgMap = get_map(as.vector(bbox(data)), source = "google", zoom = 13)
# par(mar = rep(0,4))
# plot(data, bgMap = bgMap, pch = 16, cex = .5)


popup = function(datetime, action, user) {
  str_interp("datetime: ${datetime}\naction: ${action}\nuser: ${user}\n")
}

library(leaflet)

m = leaflet(data) %>% addTiles() %>% addCircles(popup = ~action)
m
