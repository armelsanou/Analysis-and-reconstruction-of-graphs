# -----------------
# Partial Correlation
# -----------------

# Our data
X = c(2,4,15,20)
Y = c(1,2,3,4)
Z = c(0,0,1,1)

# Compte the linear regressions
lrXZ = lm(X ~ Z)
lrYZ = lm(Y ~ Z)

# Get the residuals
resXZ = lrXZ$residuals
resYZ = lrYZ$residuals

# Get the partial correlation between X and Y
cor(resXZ, resYZ)
# [1] 0.919145

# Compare with the correlation betwwen X and Y
cor(X, Y)
# [1] 0.9695016

# Using the ppcor function
library(ppcor)
pcorXYZ = pcor(cbind(X, Y, Z))
pcorXYZ$estimate

# Check partial correlation between X and Z, conditioning on Y
lrXY = lm(X ~ Y)
lrZY = lm(Z ~ Y)
resXY = lrXY$residuals
resZY = lrZY$residuals
cor(resXY, resZY)

# -----------------

