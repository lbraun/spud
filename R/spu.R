#' @importFrom graphics plot
#' @importFrom utils read.table
NULL

# spu: R classes and methods for spatial app usage data

# install.packages("leaflet")


#' Read data from a csv file in the extdata directory.
#'
#' @param file The name of the file you want to read in.
#' @param crs The coordinate reference system of the data.
#'
#' @return The resulting spu object.
#'
#' @importFrom sf st_as_sf
#' @export
#'
#' @examples
read.spu = function(file, crs = 4326) {
  data = read.table(system.file("extdata", file, package = "spu"), header = TRUE, sep = ",")
  data$datetime = strptime(data$datetime, "%Y-%m-%d %H:%M:%S")
  spu_object = st_as_sf(data, coords = c("longitude", "latitude"), crs = crs, agr = "constant")
  # class(spu_object) = append(class(spu_object), "spu")
  spu_object
}

#' Draw map showing spatial distribution of usage actions using leaflet
#'
#' @param data An spu oject containing the data to be displayed
#'
#' @importFrom leaflet colorFactor leaflet addTiles addCircles addLegend %>%
#' @export
#'
#' @examples
plot_usage_actions_leaflet = function(data) {
  # Create a palette that maps factor levels to colors
  pal = colorFactor(c("navy", "red"), domain = data$action)
  m = leaflet(data) %>%
    addTiles() %>%
    addCircles(popup = ~action, color = ~pal(action)) %>%
    addLegend(pal = pal, values = ~action, opacity = 1)
  m
}

#' Draw map showing spatial distribution of usage actions using mapview
#'
#' @param data An spu oject containing the data to be displayed
#'
#' @importFrom mapview mapview
#' @export
#'
#' @examples
plot_usage_actions = function(data) {
  mapview(data, zcol = "action", legend = TRUE)
}

x = read.spu("dummy_data.csv")
