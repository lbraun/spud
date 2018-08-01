#' @importFrom graphics plot
#' @importFrom utils read.table
NULL

library(sf)
library(mapview)

# spu: R classes and methods for spatial app usage data

# install.packages("leaflet")


#' Read data from a csv file in the extdata directory.
#'
#' @param file The name of the file you want to read in.
#'
#' @return The resulting spu object.
#' @export
#'
#' @examples
read.spu = function(file, crs = 4326) {
  data = read.table(system.file("extdata", file, package = "spu"), , header = TRUE, sep = ",")
  data$datetime = strptime(data$datetime, "%Y-%m-%d %H:%M:%S")
  spu_object = st_as_sf(data, coords = c("longitude", "latitude"), crs = crs, agr = "constant")
  # class(spu_object) = append("spu", class(spu_object))
  spu_object
}


#' Draw map showing spatial distribution of usage actions
#'
#' @param data An spu oject containing the data to be displayed
#'
#' @export
#'
#' @examples
plot_usage_actions_leaflet = function(data) {
  data$color = replicate(nrow(data), sample(c("red", "green", "blue"), 1))
  data$color = assign_color(data$action)
  m = leaflet(data) %>% addTiles() %>% addCircles(popup = ~action, color = ~color)
  m
}

plot_usage_actions = function(x) {
  mapview(x, zcol = c("user", "action"))
}

assign_colors = function(actions) {
  for (action in actions)
  replicate(nrow(data), assign_color(, 1))
}

assign_color = function(action) {
  sample(c("red", "green", "blue"), 1)
}

x = read.spu("dummy_data.csv")

# web_mercator = CRS("+init=epsg:3857")

# library(ggmap)
# bgMap = get_map(as.vector(bbox(data)), source = "google", zoom = 13)
# par(mar = rep(0,4))
# plot(data, bgMap = bgMap, pch = 16, cex = .5)


# popup = function(datetime, action, user) {
#   str_interp("datetime: ${datetime}\naction: ${action}\nuser: ${user}\n")
# }

# library(leaflet)

# m = leaflet(data) %>% addTiles() %>% addCircles(popup = ~action)
# m
