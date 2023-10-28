# VaR
VaR.D <- function(P, C, r, y, f = 1, s = 1, VaR = 95, PV = NULL, p = NULL){
  
  B <- P*(C*(1/r*(1-1/(1+(1/(f/r)))^(y*f)))+1/(1+(1/(f/r)))^(y*f)) # Bond Price
  
  P. <- P * (1 + C / f) / (1 + r) ^ (y * f) # Principle Part
  
  for (n in 1:(y - 1)){ PV <- cbind(PV, (C * P/f )/(1 + r )^n) # PV of coupons
  
  p <- cbind(p, n * PV[n]) } # Coupon part for numerator
  
  D <- (sum(p[seq(y - 1)]) + P. * y) / (P. + sum(PV[seq(y - 1)])) # Duration
  
  VaR.d <- B * (qnorm(1 - VaR * 0.01) * (D/(1 + (r - s * 0.01)/f))) * r # VaR
  
  return(VaR.d) # Display sentence
}
# Test
VaR.D(1000, 0.1, 0.05, 3, 1, 1)
