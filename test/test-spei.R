# Dongdong, test modified SPEI

# Load data
data(wichita)

# One and tvelwe-months SPEI
wichita$PET <- thornthwaite(wichita$TMED,37.6475)
spei1  <- spei(wichita$PRCP-wichita$PET,1)
spei12 <- spei(wichita$PRCP-wichita$PET,12)

# Extract information from spei object
summary(spei1)
names(spei1)
spei1$call
spei1$fitted
spei1$coefficients
