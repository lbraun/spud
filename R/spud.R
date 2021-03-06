#' spud: R classes and methods for spatial usage data
#'
#' The spud package aims to give researchers and app developers an easy way to
#' visualize and analyze how and where people use a given service.
#'
#' @section Methods:
#' Get started with spud using the \code{read.spud} method on the dummy data provided
#' with the package. Run \code{read.spud("dummy_data.csv")} and try out any of the
#' main exposed methods:
#'
#' - plot_usage_actions_leaflet
#'
#' - plot_usage_actions_mapview
#'
#' - plot_user_path
#'
#' Alternatively use the data to initialize a spud class.
#'
#' @section Classes:
#' There are two classes included in spud, App and User, which represent an
#' application and its users, respectively. See the relevant documentation
#' pages for more details.
#'
#' @docType package
#' @name spud
NULL

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
read.spud = function(file, crs = 4326) {
  data = read.table(system.file("extdata", file, package = "spud"), header = TRUE, sep = ",")
  data$datetime = as.POSIXct(strptime(data$datetime, "%Y-%m-%d %H:%M:%S"))
  spud_object = sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = crs, agr = "constant")
  spud_object
}

#' Read data from csv files directly into an App object.
#'
#' @param file The name of the file you want to read in.
#' @param crs The coordinate reference system of the data.
#' @param name The name of the app.
#'
#' @return The resulting App object.
#' @export
read.spud_app = function(file, crs = 4326, name) {
  spud_object = read.spud(file, crs)
  App$new(name = name, usage_data = spud_object)
}

#' Read data from csv files directly into a User object.
#'
#' @param file The name of the file you want to read in.
#' @param crs The coordinate reference system of the data.
#' @param id The id of the user.
#'
#' @return The resulting User object.
#' @export
read.spud_user = function(file, crs = 4326, id) {
  spud_object = read.spud(file, crs)
  User$new(id = id, usage_data = spud_object)
}

#' Draw map showing spatial distribution of usage actions using leaflet
#'
#' @param data A spud object containing the data to be displayed
#'
#' @importFrom leaflet colorFactor leaflet addTiles addCircles addLegend %>%
#' @export
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
plot_usage_actions_mapview = function(data) {
  mapview(data, zcol = "action", legend = TRUE)
}


#' Draw map showing a path of a user over time
#'
#' @param data A spud object containing the data to be displayed
#' @param user_id The identifier of one user
#'
#' @importFrom leaflet leaflet addTiles addPolylines addCircles
#' @importFrom dplyr filter arrange
#' @importFrom sf st_coordinates
#' @export
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
