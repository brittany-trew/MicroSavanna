era5_process <- function(req, pathin, r, tme, out = "grid", lat = NA, long = NA, resampleout = TRUE) {
  if (class(req) == "logical") {
    files<-list.files(pathin)
    n<-length(files)
  } else {
    n<-length(req)
    files <- ""
    for (i in 1:n) files[i]<-req[[i]]$target
  }
  # Get raster template for nc file
  nc<-paste0(pathin,files[1])
  # Grid process
  climdata<-.extract_clima(nc, r, resampleout)
  if (n > 1) {
    for (i in 2:n) {
      nc<-paste0(pathin,files[i])
      climone<-.extract_clima(nc, r, resampleout)
      for (j in 1:9)  climdata[[j]]<-c(climdata[[j]], climone[[j]])
    }
  }
  tc<-climdata[[1]]
  tmenc<-as.POSIXlt(time(tc),tz="UTC")
  if (length(tmenc) > length(tme)) {
    s<-which(tmenc >= tme[1] & tmenc <= tme[length(tme)])
    for (i in 1:9) {
      ro<-climdata[[i]]
      ro<-ro[[s]]
      climdata[[i]]<-ro
    }
  }
  if (out == "point") {
    if (class(r) != "logical") {
      ll<-.latlongfromrast(r)
      lat <- ll$lat
      long <- ll$long
    }
    xy<-data.frame(x=long,y=lat)
    m<-matrix(0,ncol=10,nrow=length(tme))
    for (i in 1:9) {
      v<-as.numeric(extract(climdata[[i]],xy))
      m[,i+1]<-v[-1]
    }
    climdata<-data.frame(m)
    names(climdata)<-c("obs_time","temp","relhum","pres","swdown","difrad","lwdown","windspeed","winddir","precip")
    climdata$obs_time<-tme
  } else {
    for (i in 1:9)  climdata[[i]]<-wrap(climdata[[i]])
  }
  return(climdata)
}
