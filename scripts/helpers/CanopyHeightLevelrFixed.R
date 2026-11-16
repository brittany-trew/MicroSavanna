canopy.height.levelr.fixed <- function(lidar.array)
{
  l.array <- lidar.array$array
  chm.pulses <- array(data = NA, dim = dim(l.array))
  
  for (q in 1:dim(l.array)[2]) {
    for (z in 1:dim(l.array)[3]) {
      
      lidar.col <- l.array[, q, z]
      ground <- lidar.col[1]
      canopy <- lidar.col[2]
      
      if (is.finite(ground)) {
        
        canopy.ht <- canopy - ground
        ch.col <- lidar.col
        ch.col[1] <- 0
        ch.col[2] <- canopy.ht
        
        g.pulse <- which(ch.col[3:length(ch.col)] > 0)
        
        # ** The Main Fix: Handle columns with no positive voxel pulse counts
        if (length(g.pulse) == 0) {
          chm.pulses[, q, z] <- NA
          next
        }
        # ---------------------------------------------
        
        g.ind <- g.pulse[1] + 2
        cht.col <- c(ch.col[1:2], ch.col[g.ind:length(ch.col)])
        
        new.index <- length(cht.col) + 1
        if (new.index > length(ch.col)) {
          chm.pulses[, q, z] <- cht.col
        } else {
          cht.col[new.index:length(ch.col)] <- 0
          chm.pulses[, q, z] <- cht.col
        }
        
      } else {
        chm.pulses[, q, z] <- NA
      }
    }
  }
  
  return.data <- base::list(
    array = chm.pulses,
    x.bin = lidar.array$x.bin,
    y.bin = lidar.array$y.bin,
    z.bin = lidar.array$z.bin
  )
  return(return.data)
}
