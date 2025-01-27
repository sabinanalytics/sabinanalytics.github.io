#haversine_distance.R

## calculate distance for latitude/longitude
deg2rad <- function(degrees) {
  radians <- degrees * pi / 180
  return(radians)
}
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert degrees to radians
  lat1 <- deg2rad(lat1)
  lon1 <- deg2rad(lon1)
  lat2 <- deg2rad(lat2)
  lon2 <- deg2rad(lon2)
  
  # Radius of the Earth in miles
  earth_radius <- 3959
  
  # Haversine formula
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * asin(sqrt(a))
  
  # Calculate distance
  distance <- earth_radius * c
  
  return(distance)
}