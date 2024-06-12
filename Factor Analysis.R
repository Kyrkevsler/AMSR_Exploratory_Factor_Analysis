# Set-up Working Directory 
setwd('C:/Users/admin/Documents/Krysler Files/R files/Factor Analysis')

# Set-up working data frame
MMW <- read.csv('MMWSURVEY.csv')[, -1]
MMW <- na.omit(MMW)
head(MMW, 10)

library(ltm)
library(psych)
library(EFAtools)
library(dplyr)

# Preparatory Tests

### ASSESSING SCALE RELIABILITY
cronbach.alpha(MMW)
alpha(MMW)

# The overall internal consistency reliability is estimated at 80.7%.

### Bartlett’s test of sphericity

# Ho: the population correlation matrix is an identity matrix (variables are uncorrelated)
# Ha: the population correlation matrix is not an identity matrix (variables are correlated)

cor <- cor(MMW)
cortest.bartlett(cor, n= nrow(MMW))

# The population correlation matrix is not an identity matrix (variables are correlated).

### Measure of Sampling Adequacy (MSA)

# MSA overall should be 0.50 or higher
# MSA greater than or equal to 0.80 - good
# MSA greater than or equal to 0.90 - excellent
# MSA less than 0.50 - remedial action is needed: delete variable
# with lowest KMO

KMO(cor)
  
# The overall MSA of 0.902 is considered excellent.
# The individual MSA range from mostly good to excellent.

### ASSESSING SAMPLE SIZE
dim(MMW)

# The sample size is 257 objects of 24 variables.   // Originally 278 but disregarded NA values.


### Parallel Analysis 
fa.parallel(MMW, fa= "both", fm= "minres")

# Factor Analysis using fa method
(f_model <- fa(MMW, nfactors= 5))
fa.diagram(f_model, rsize= 0.05, cex= 0.8)

# Variance Accounted 
f_model$Vaccounted

# Factor Correlations
f_model$Phi

# Mean Item Complexity
f_model$complexity
mean(f_model$complexity)

# The null model and objective function
f_model$null.model

# Model
f_model$null.dof

# Chi-square
f_model$null.chisq

# Root mean square error of approximation
f_model$RMSEA

# Tucker Lewis Index
f_model$TLI

# Diagram Varimax
f_diagram.var <- fac(MMW, 5, rotate = "varimax")
fa.diagram(f_diagram.var, main = "Varimax")


### Naming of Factors

# Factor 1 (Problem-Solving Factor)
  # POTS 5 -  I can tackle a challenging MMW problem.
  # POTS 6 - I believe I can get better at math.
  # POTS 8 - I can solve word problems.
  # POTS 17 - I ask questions when I am unsure of a problem.
  # POTS 18 - I’m certain I can figure out how to solve difficult math problems.
  # POTS 19 - Time spent learning why a solution works is time well spent.
  # POTS 21 - Even if the concepts in MMW class are hard, I can learn them.

# Factor 2 (Accomplishment Factor)
  # POTS 2 - I can work with a partner to find a solution to a problem.
  # POTS 3 - I feel accomplished when I solve a problem.
  # POTS 4 - I work hard in my MMW classes.
  # POTS 10 - I complete assignments outside of class.
  # POTS 22 - It’s important for me to do really well in MMW.
  # POTS 24 - I always find other ways to learn MMW like searching on the internet.

# Factor 3 (Real-world Application Factor)
  # POTS 15 - Discussing different solutions is a good way of learning MMW exercises.
  # POTS 16 - Achievement and effort in MMW class are likely to lead to job success later on.
  # POTS 20 - I can use what I learn in MMW class in other subjects.
  # POTS 23 - I can name several ways I use MMW topics in my day-to-day life.

# Factor 4 (Subject Expectations Factor)
  # POTS 1 - My MMW teacher has been unsuccessful with helping me to appreciate math.
  # POTS 7 - I find it difficult to focus during MMW class.
  # POTS 11 - My only interest in MMW is getting a passing score.

  # POTS 12 - I like doing MMW activities.
  # POTS 13 -  I think I will do/have done well in MMW this semester.
  # POTS 14 - MMW can be fun.

# Factor 5 (Complacent Factor)
  # POTS 9 - Getting the right answer is more important than understanding why the answer works.

### Graph of the Selected Rotation
 
# Varimax
plot(f_diagram.var$loadings[, 1],
     f_diagram.var$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1,1),
     xlim = c(-1,1),
     col = "red",
     main = "Varimax Rotation") + 
  abline(h = 0, v = 0)

# Additional guides

# Using Varimax
loading_2 <- f_diagram.var$loadings[, 1:2]
fa.diagram(loading_2)


f1 <- MMW[, c("POTS18", "POTS5", "POTS8", "POTS6", "POTS21", 
             "POTS19", "POTS17", "POTS12", "POTS13", "POTS14", "POTS20")]
alpha(f1)

# The overall internal consistency reliability is estimated at 88%.

f2 <- MMW[, c("POTS4", "POTS22", "POTS3", "POTS10", "POTS24", 
              "POTS2", "POTS15")]
alpha(f2)

# The overall internal consistency reliability is estimated at 76%.

f3 <- MMW[, c("POTS23", "POTS11", "POTS1", "POTS16", "POTS7", 
             "POTS9")]
alpha(f3)

# The overall internal consistency reliability is estimated at 27%.




