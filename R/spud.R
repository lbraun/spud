# spud: R classes and methods for spatial app usage data

#' Read data from csv files in the extdata directory.
#'
#' @param file The name of the file you want to read in.
#' @param crs The coordinate reference system of the data.
#'
#' @return The resulting spud object.
#'
#' @importFrom sf st_as_sf
#' @importFrom utils read.table
#' @export
#'
#' @examples
read.spud = function(file, crs = 4326) {
  data = read.table(system.file("extdata", file, package = "spud"), header = TRUE, sep = ",")
  data$datetime = as.POSIXct(strptime(data$datetime, "%Y-%m-%d %H:%M:%S"))
  spud_object = sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = crs, agr = "constant")
  spud_object
}

#' @export
read.spud_app = function(file, crs = 4326, name) {
  spud_object = read.spud(file, crs)
  App$new(name = name, usage_data = spud_object)
}

#' @export
read.spud_user = function(file, crs = 4326, id) {
  spud_object = read.spud(file, crs)
  User$new(id = id, usage_data = spud_object)
}

appify = function(spud_object) {
  App$new(name = "My fancy app", usage_data = spud_object)
}

#' Draw map showing spatial distribution of usage actions using leaflet
#'
#' @param data A spud object containing the data to be displayed
#'
#' @importFrom leaflet colorFactor leaflet addTiles addCircles addLegend %>%
#' @export
#'
#' @examples
plot_usage_actions_leaflet = function(data) {
  # Create a palette that maps actions to colors
  pal = colorFactor(c("navy", "red"), domain = data$action)
  map = leaflet(data) %>%
    addTiles() %>%
    addCircles(popup = ~action, color = ~pal(action)) %>%
    addLegend(pal = pal, values = ~action, opacity = 1)
  map
}

#' Draw map showing spatial distribution of usage actions using mapview
#'
#' @param data A spud object containing the data to be displayed
#'
#' @importFrom mapview mapview
#' @export
#'
#' @examples
plot_usage_actions_mapview = function(data) {
  mapview(data, zcol = "action", legend = TRUE)
}


#' Draw map showing locations where users tried the app for the first time
#'
#' @param data A spud object containing the data to be displayed
#'
#' @importFrom mapview mapview
#' @importFrom dplyr group_by top_n
#' @export
#'
#' @examples
plot_first_actions = function(data) {
  first_actions = data
  first_actions$datetime = as.numeric(first_actions$datetime)
  first_actions = first_actions %>% group_by(user) %>% top_n(1, datetime)
  first_actions$datetime = as.POSIXct(first_actions$datetime, origin='1970-01-01')
  mapview(first_actions, zcol = "action", legend = TRUE)
}


#' Draw map showing a path of a user over time
#'
#' @param data A spud object containing the data to be displayed
#' @param user_id The identifier of one user
#'
#' @importFrom mapview mapview
#' @importFrom dplyr filter arrange
#' @export
#'
#' @examples
plot_user_path = function(data, user_id) {
  user_data = data %>% filter(user == user_id) %>% arrange(datetime)
  user_path = data.frame(user_data %>% st_coordinates())

  # Create a palette that maps datetime to colors
  pal = colorFactor(c("green", "red"), domain = user_data$datetime)

  map = leaflet() %>%
    addTiles() %>%
    addPolylines(data = user_path, lng = ~X, lat = ~Y) %>%
    addCircles(data = user_data, popup = ~action, color = ~pal(datetime))
  map
}

x = read.spud("dummy_data.csv")
app = appify(x)
