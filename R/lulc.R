#' Extract Land Use / Land Cover data
#' 
#' Read a Land Use or Land Cover layer 
#' 
#' Designed to be flexible with regard to STAC endpoint and collection, but for now only tested for io-lulc-9-class on PlanetaryComputer 
#' io-lulc-9-class is only generated for the period 2017-2022, for years outside this range a warning will be returned and the neares year returned
#' 
#' @export
#' @importFrom gdalcubes stac_image_collection cube_view raster_cube
#' @import terra
#' @import rstac
#' 
#' @param x a SpatRaster or SpatExtent object, defining the area for which the LULC should be extracted
#' @param year description
#' @param endpoint description, defaults to Microsoft Planetary Computer
#' @param collection STAC collection, defaults to "io-lulc-9-class"
#' @param assets character defining STAC assets name, defaults to "data"
#' @param authOpt list with STAC authentication options passed to stac_auth (e.g. \code{list(key="abc")}), endpoint-specific
#' 
#' @returns SpatRaster object
#'
#' 
setGeneric("lulc", function(x, year,
                            endpoint="https://planetarycomputer.microsoft.com/api/stac/v1",  collection="io-lulc-9-class", assets="data",
                            authOpt=list(), ...){
  standardGeneric("lulc")
})

setMethod("lulc", signature="SpatRaster",
          function(x, year,
                   endpoint="https://planetarycomputer.microsoft.com/api/stac/v1", collection="io-lulc-9-class", assets="data",
                   authOpt=list()){

            if(collection=='io-lulc-9-class'){
              if(year<2017){
                warning("Land cover product io-lulc-9-class currently has data 2017-2022. 2017 selected.")
                year <- 2017 
              }
              if(year>2022){
                warning("Land cover product io-lulc-9-class currently has data 2017-2022. 2022 selected.")
                year <- 2022 
              }
            }
            
            extent <- terra::ext(x)
            srs <- terra::crs(x)
            dx <- res(x)[1]
            dy <-res(x)[2]
            xmin <- extent[1]
            xmax <- extent[2]
            ymin <- extent[3]
            ymax <- extent[4]
            
            ##  STAC ItemCollection
            searchArgs <- list()
            searchArgs$q <- stac(endpoint)
            searchArgs$collection <- collection
            searchArgs$bbox <- as.bbox(extent$xmin, extent$xmax, extent$ymin, extent$ymax, crs_from=srs, xy=FALSE)
            searchArgs$datetime <- paste0(year,"-01-01T00:00:01Z", "/", year,"-12-31T23:59:59Z")

            items <- do.call(stac_search, searchArgs) |>
              post_request() |>
              stac_auth(endpoint = endpoint, authOpt=authOpt)
            imgCollection <- gdalcubes::stac_image_collection(s=items$features, asset_names=assets)
            
            # Creating cube view
            cv <- gdalcubes::cube_view(extent=list(t0=substr(gdalcubes::extent(imgCollection)$t0,1,10), 
                                                   t1=substr(gdalcubes::extent(imgCollection)$t1,1,10),
                                                   left= xmin, right= xmax, 
                                                   bottom=ymin, top=ymax),
                                       srs=srs,
                                       dx=dx, dy=dy, dt="P1D",
                                       aggregation="first", resampling="near")
            cube <- gdalcubes::raster_cube(imgCollection, cv)
            r <- as.SpatRaster(cube)
            
            cls <- as.data.frame(do.call(rbind, lapply(items$features[[1]]$assets[[assets]]$`file:values`, unlist)))
            cls[,1] <- as.numeric(cls[,1])
            names(cls) <- c("value", "landcover")
            levels(r) <- cls
            
            #fix color table?
            ct <- items$features[[1]]$assets[[assets]]$href |> rast() |> coltab()
            coltab(r) <- ct
            
            return(r)
          }
)

