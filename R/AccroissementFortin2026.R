# species <- predicted.sp[1, "speciesGr"]
# deltaT <- predicted.sp[1, "correctedDt"]
# BAL <- predicted.sp[1, "BAL"]
# DBH <- predicted.sp[1, "dbhCm_x"]
# BAS <- predicted.sp[1, "G_othersMinusBAL"]
# VPD <- predicted.sp[1, "meanVaporPressureDeficit"]
# CMI <- predicted.sp[1, "meanCMI"]
# FROST <- predicted.sp[1, "meanFrostDay"]
# DD <- predicted.sp[1, "meanDD"]
# SBWoutbreak <- predicted.sp[1, "nbYearsDefoliated"]
# drainageClass <- predicted.sp[1, "drainage"]
# type <- "response"


#'
#' Make population-averaged predictions using the mixed truncated Tobit model
#'
#' @param species
#' @param deltaT Interval de temps  (années)
#' @param BAL surface terrière (m2/ha) des arbres plus grands
#' @param DBH diamètre à hauteur de poitrine (cm)
#' @param BAS surface terrière des arbres plus petits (m2/ha)
#' @param VPD Déficit de pression vapeur juin à août (hPa)
#' @param CMI climate moisture index juin à août (cm)
#' @param DD growning degree-days de plus de 5 degrés Celsius (deg c)
#' @param SBWoutbreak Nombre d'années de défoliation modérées à sévère de TBE
#' @param drainageClass Classe de drainage: xeric, mesic, subhydric, hydric
#' @param type soit original (avec correction Gauss-Hermite quadrature) ou reponse(
#' sans correction Gauss-Hermite quadrature)
#' @return pédiction "population-averaged" de l'accroissement en diamètre (cm)
#'

  makePredictions <- function(Accrois) {
    species=Accrois$species
    deltaT=Accrois$deltaT
    BAL=Accrois$BAL
    DBH=Accrois$DBH
    BAS=Accrois$BAS
    VPD=Accrois$VPD
    CMI=Accrois$CMI
    DD=Accrois$DD
    SBWoutbreak=Accrois$SBWoutbreak
    drainageClass=Accrois$drainageClass
    type = "original"

  parms <- finalParms[which(finalParms$speciesGr == species),]

  s2 <- parms[which(parms$Parameter == "s2"), "Estimate"]
  s2u <- parms[which(parms$Parameter == "s2u_t"), "Estimate"]
  parms <- parms[which(!parms$Parameter %in% c("s2", "s2u_t")),]


  x <-as.matrix(c(1,log(deltaT), deltaT, BAL, BAL^2, DBH, log(DBH + 1), BAS, BAS^2, log(BAS + 1), BAL*DBH, BAS*DBH, VPD, VPD^2, CMI, CMI^2,
        DD/1000,(DD/1000)^2, as.numeric(drainageClass == "xeric"), as.numeric(drainageClass == "subhydric"),as.numeric(drainageClass == "hydric"),
        SBWoutbreak * as.numeric(species %in% c("SAB","EPR","EPB","EPN"))))

  beta<-t(as.matrix(parms$Estimate))


  xBeta<-as.numeric(beta%*%x)


   if (type == "response"){
    return(as.numeric(xBeta))
   }

  truncation <- 0
  offset <- 1
  if (length(s2u) > 0) {
    v <- c(-0.202018287045609E1, -0.958572464613819, 0, 0.958572464613819, 0.202018287045609E1)
    w <- c(0.199532420590459E-1, 0.393619323152241, 0.945308720482942, 0.393619323152241, 0.199532420590459E-1)
    pred <- 0
    sqrtTwiceS2u <- (2 * s2u)^.5
    for (i in 1:5) {
      pred <- pred + w[i] * computeIntegralOriginalScale(xBeta + sqrtTwiceS2u * v[i], s2, truncation, offset)
    }
    pred <-  pred / pi^.5
  } else { # no random effect
    pred <- computeIntegralOriginalScale(xBeta, s2, truncation, offset)
  }

  if (is.na(pred)==TRUE){

    return(0)
  }


  return(as.numeric(pred))
}

##################Corrige les prévisions avec Gauss-Hermite quadrature

computeIntegralOriginalScale <- function(xBeta, sigma2, truncation, offset) {
  sigma <- sigma2^.5
  F_t <- pnorm(as.numeric((truncation - xBeta)/sigma))
  if (F_t < 1E-8) {
    pred <- exp(xBeta + 0.5 * sigma2)
  } else {
    pred <- exp(xBeta + 0.5 * sigma2) * ((1 + erf(as.numeric((xBeta + sigma2 - truncation)/(2*sigma2)^.5)))/(2*(1-F_t)))
  }
  return(pred - offset)
}


erf <- function(x) {
  2 * pnorm(x * sqrt(2)) - 1
}


