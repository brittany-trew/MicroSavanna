era5_download<-function(r, tme, credentials, file_prefix, pathout) {
  # set credentials
  sel <- which(credentials$Site == "CDS")
  uid <- credentials$username[sel]
  cds_access_token <- credentials$password[sel]
  ecmwfr::wf_set_key(user = uid,
                     key = cds_access_token)
  # build request
  e<-ext(r)
  ro<-rast(e)
  terra::crs(ro)<-terra::crs(r)
  rll<-project(ro,"EPSG:4326")
  ell<-ext(rll)
  xmn<-floor(ell$xmin*4)/4
  xmx<-ceiling(ell$xmax*4)/4
  ymn<-floor(ell$ymin*4)/4
  ymx<-ceiling(ell$ymax*4)/4
  req <- mcera5::build_era5_request(xmin = xmn, xmax = xmx,
                                    ymin = ymn, ymax = ymx,
                                    start_time = tme[1],
                                    end_time = tme[length(tme)],
                                    outfile_name = file_prefix)
  # check which downloads already exist
  keep<-rep(TRUE,length(req))
  for (i in 1:length(req)) {
    fi<-paste0(pathout,req[[i]]$target)
    if (file.exists(fi)) keep[i]<-FALSE
  }
  s<-which(keep)
  req2<-req[s]
  # download data
  dir.create(pathout,showWarnings=FALSE)
  mcera5::request_era5(request = req2, uid = uid, out_path = pathout, combine = F)
  return(req)
}
