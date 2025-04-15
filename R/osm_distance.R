#' OpenStreetMap distance
#' 
#' Calculated shortest geographic distance to selected OpenStreetMap features 
#' 
#' See osm_distance_roads, osm_distance places
#' 
#' @export
#' @param x SpatRaster or SpatVector for which distances are calculated 
#' @param what Either "roads" or "places"
#' @param ... Additional arguments to osm_distance_roads or osm_distance places
osm_distance <- function(x, what, ...){
  if(what=="roads") return(osm_distance_roads(x, ...))
  if(what=="places") return(osm_distance_places(x, ...))
  return(NULL)
}

#' OpenStreetMap distance to roads
#' 
#' Calculated shortest geographic distance to OpenStreetMap road features 
#' 
#' When a bounding box is not provided in the osm_bbox argument, the maximum required bounding box will be identified through an iterative search. This may be a more efficient option than providing an overly wide bounding box.
#' 
#' @export
#' @importFrom terra ext project extend vect as.points names 
#' @importFrom osmdata opq add_osm_feature osmdata_sf
#' 
#' @param x SpatRaster or SpatVector for which distances are calculated 
#' @param values Character vector of road categories to be included (see https://wiki.openstreetmap.org/wiki/Key:highway) or a single road category with suffix "+" to include all categories including and above the selected category. Set to NULL to include all values. 
#' @param timeout Numeric (seconds)
#' @param osm_bbox Bounding box for OSM feature retrieval, as numeric vector or character string of administrative name (see \code{osmdata::getbb})
#' 
#' @returns SpatRaster or SpatVector
#' 
osm_distance_roads <- function(x, values="residential+", timeout=25, osm_bbox=NULL,...){

  #specifiy values
  road_values <- c("motorway","trunk","primary", "secondary", "tertiary", "unclassified", "residential", 
                   "motorway_link", "trunk_link", "primary_link", "secondary_link" , "tertiary_link",
                   "living_street", "service", "pedestrian" , "track", 
                   "footway" , "path")
  if(!is.null(values))
    if(substr(values, nchar(values), nchar(values))=="+")
      values <- road_values[1:which(road_values==substr(values, 1, nchar(values)-1))]

  do_osm_query <- function(){
    q <- opq(osm_bbox, timeout=timeout)
    q <- add_osm_feature(q, key="highway", value=values)
    ret <- osmdata_sf(q)
    return(ret)
  }

  if(is.null(osm_bbox)){
    #Initial bbox from x
    ext_x <- ext(x)
    osm_bbox <- as.bbox(project(ext_x, x, "epsg:4326"))
    #Query
    bbox_data <- do_osm_query()
    #If no data in initial bbox, iteratively extend and search
    bbox_has_data <- !is.null(bbox_data$osm_lines)
    while(!bbox_has_data){
      ext_x <- extend(ext_x, 0.5*max(ext_x$xmax-ext_x$xmin, ext_x$ymax-ext_x$ymin))
      osm_bbox <- as.bbox(project(ext_x, x, "epsg:4326"))
      bbox_data <- do_osm_query()
      bbox_has_data <- !is.null(bbox_data$osm_lines)
    }
    
    # #Calculate distance by how much initial bbox must be extended to ensure all relevant features are included in distance calculation
    #Switch this out for now as distance always returns m/km, which is problematic of bbox is lat/lon. Could be solved by first transforming bbox to utm, then switching back
    # bbox_data_vect <- vect(bbox_data$osm_lines)
    # dmax <- max(distanceToVector(as.points(ext(x), crs=crs(x)), bbox_data_vect))
    # ext_x <- extend(ext(x), dmax)
    # osm_bbox <- as.bbox(project(ext_x, x, "epsg:4326"))
  } else {
    
    #Final query
    bbox_data <- do_osm_query()
    
  }

  
  #Calculate distances
  bbox_data_vect <- vect(bbox_data$osm_lines) 
  
  if(is.SpatRaster(x)){
    x_dist <- distanceToVector(x, bbox_data_vect)
    names(x_dist) <- "dist_roads"
  } else if (is.SpatVector(x)){
    x_dist <- distanceToVector(centroids(x), bbox_data_vect)
    names(x_dist) <- "dist_roads"
    x_dist <- cbind(x, x_dist)
  } else {
    stop("SpatRaster or SpatVector expected")
  }
  return(x_dist)
}

#' OpenStreetMap distance to places
#' 
#' Calculated shortest geographic distance to OpenStreetMap place features 
#' 
#' When a bounding box is not provided in the osm_bbox argument, the maximum required bounding box will be identified through an iterative search. This may be a more efficient option than providing an overly wide bounding box.
#' 
#' @export
#' @importFrom terra ext project extend vect as.points names 
#' @importFrom osmdata opq add_osm_feature osmdata_sf
#' 
#' @param x SpatRaster or SpatVector for which distances are calculated 
#' @param values Character vector of place categories to be included (see https://wiki.openstreetmap.org/wiki/Key:place) or a single place category with suffix "+" to include all categories including and above the selected category. Set to NULL to include all values. 
#' @param timeout Numeric (seconds)
#' @param name Character of place name
#' @param osm_bbox Bounding box for OSM feature retrieval, as numeric vector or character string of administrative name (see \code{osmdata::getbb})
#' 
#' @returns SpatRaster or SpatVector
#' 
osm_distance_places <- function(x, values="hamlet+", timeout=25, name=NULL, osm_bbox=NULL, ...){
  #add generic option to filter by all available fields
  
  #Specify values
  places_values <- c("city", "borough", "suburb", "quarter", "neighbourhood",  
                     "town", "village", "hamlet", "isolated_dwelling", 	"farm")#ignored: city_block, plot, allotments
  if(!is.null(values))
    if(substr(values, nchar(values), nchar(values))=="+")
      values <- places_values[1:which(places_values==substr(values, 1, nchar(values)-1))]

  do_osm_query <- function(){
    q <- opq(osm_bbox, nodes_only = TRUE, timeout=timeout)
    q <- add_osm_feature(q, key="place", value=values)
    if(!is.null(name)) q <- add_osm_feature(q, key="name", value = name)
    ret <- osmdata_sf(q)
    return(ret)
  }

  #If bbox is not specified in arguments, iteratively extend bbox until maximum required bbox is found
  #TODO?: move this part of bounding box search to new function that can be used for "roads" and "places"
  if(is.null(osm_bbox)){
    #Initial bbox from x
    ext_x <- ext(x)
    osm_bbox <- as.bbox(project(ext_x, x, "epsg:4326"))
    #Query
    bbox_data <- do_osm_query()
    #If no data in initial bbox, iteratively extend and search
    bbox_has_data <- !is.null(bbox_data$osm_points)
    while(!bbox_has_data){
      ext_x <- extend(ext_x, 0.5*max(ext_x$xmax-ext_x$xmin, ext_x$ymax-ext_x$ymin))
      osm_bbox <- as.bbox(project(ext_x, x, "epsg:4326"))
      bbox_data <- do_osm_query()
      bbox_has_data <- !is.null(bbox_data$osm_points)
    }
    #Calculate distance by how much initial bbox must be extended to ensure all relevant features are included in distance calculation
    # bbox_data_vect <- vect(bbox_data$osm_points)
    # dmax <- max(distanceToVector(as.points(ext(x), crs=crs(x)), bbox_data_vect))
    # ext_x <- extend(ext(x), dmax)
    # osm_bbox <- as.bbox(project(ext_x, x, "epsg:4326"))
    
  } else {
    #Final query
    bbox_data <- do_osm_query()
    
  }

  
  #Calculate distances
  bbox_data_vect <- vect(bbox_data$osm_points) 
  if(is.SpatRaster(x)){
    x_dist <- distanceToVector(x, bbox_data_vect)
    names(x_dist) <- "dist_places"
  } else if (is.SpatVector(x)){
    x_dist <- distanceToVector(centroids(x), bbox_data_vect)
    names(x_dist) <- "dist_places"
    x_dist <- cbind(x, x_dist)
  } else {
    stop("SpatRaster or SpatVector expected")
  }
  return(x_dist)
  
}
