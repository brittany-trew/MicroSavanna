machorn.lad.fixed <- function (leveld.lidar.array, voxel.height, beer.lambert.constant = NULL) 
{
  voxel.N.pulse <- leveld.lidar.array$array[3:dim(leveld.lidar.array$array)[1], 
                                            , ]
  pulse.accum <- array(0, dim = dim(voxel.N.pulse))
  voxel.N.pulse[is.na(voxel.N.pulse)] <- 0
  for (i in (dim(voxel.N.pulse)[1]):1) {
    if (i == (dim(voxel.N.pulse)[1])) {
      pulse.accum[i, , ] <- voxel.N.pulse[i, , ]
    }
    else {
      pulse.accum[i, , ] <- pulse.accum[i + 1, , ] + voxel.N.pulse[i, 
                                                                   , ]
    }
  }
  pulse.all <- array(rep(pulse.accum[1, , ], each = dim(voxel.N.pulse)[1]), 
                     dim = dim(voxel.N.pulse))
  shots.through <- pulse.all - pulse.accum
  shots.in <- array(dim = dim(voxel.N.pulse))
  shots.in[1:(dim(voxel.N.pulse)[1] - 1), , ] <- shots.through[2:(dim(voxel.N.pulse)[1]), 
                                                               , ]
  shots.in[(dim(voxel.N.pulse)[1]), , ] <- pulse.accum[1, , 
  ]
  k <- beer.lambert.constant
  dz <- voxel.height
  if (is.null(k)) {
    rLAD <- log(shots.in/shots.through) * (1/dz)
    rLAD[is.infinite(rLAD) | is.nan(rLAD)] <- NA
  }
  else {
    print("MacArthur-Horn constant is set! k = ")
    print(k)
    rLAD <- log(shots.in/shots.through) * (1/(k * dz))
    rLAD[is.infinite(rLAD) | is.nan(rLAD)] <- NA
  }
  for (r in 1:dim(rLAD)[2]) {
    for (c in 1:dim(rLAD)[3]) {
      na.cut <- ceiling(
        leveld.lidar.array$array[2,r,c] / voxel.height
      ) + 2 # Fix is here so that canopy isn't cut according to 1m voxels only.
      if (is.na(na.cut) == FALSE) {
        if (na.cut > dim(rLAD)[1]) {
        }
        else {
          rLAD[na.cut:dim(rLAD)[1], r, c] <- NA
        }
      }
      else {
      }
    }
  }
  out <- list()
  out$rLAD <- rLAD
  out$shots.in <- shots.in
  gc()
  remove(leveld.lidar.array)
  remove(pulse.accum)
  remove(voxel.N.pulse)
  gc()
  return(out)
}