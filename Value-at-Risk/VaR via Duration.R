# VaR
VaR.D <- function(P, C, r, y, f = 1, s = 1, VaR = 95, PV = NULL, p = NULL){
  
  B <- P * (C/r * (1 - (1 + r/f)^-(y * f)) + (1 + r/f)^-(y * f)) #Bond Price
  
  P. <- P * (1 + C / f) / (1 + r / f) ^ (y * f) # Principle Part
  
  for (n in 1:(y*f-1)){ PV <- cbind(PV, C * P/f/(1 + r/f)^(n * f)) # Coupon PV
  
  p <- cbind(p, n * PV[n]) } # Coupon part for numerator
  
  D <- (sum(p[seq(y*f - 1)]) + P.*y*f)/(P. + sum(PV[seq(y*f - 1)])) # Duration
  
  B * qnorm(1 - VaR * .01) * (D / (1 + (r - s * .01) / f)) * (r / f) # VaR
}
# Test
VaR.D(1000, .1, .05, 3, 1, 1)
