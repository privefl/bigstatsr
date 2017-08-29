## Test environments
* local Linux (CentOS 7), R 3.3.3 and 3.4.0
* local Windows (10), R 3.4.0
* Linux on Travis-CI (devel and release)
* Max on Travis-CI (release)
* Windows on Appveyor (release)
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs. 

Overall, there was 6 NOTEs:

* checking package dependencies ... NOTE
  Package suggested but not available for checking: 'RevoUtilsMath'
  
  Package only available with Microsoft R Open (to check if the user is using Microsoft R Open).
  
* checking installed package size ... NOTE

  - 12.7 Mb on Linux
  -  5.3 Mb on Windows
  -  < 5 Mb on Mac

* checking R code for possible problems ... [10s] NOTE
  FBM.code256_RC: no visible global function definition for 'new'
  FBM_RC: no visible global function definition for 'new'

  NOTE when using Reference Classes. 
  
* (on win-builder) Examples with CPU or elapsed time > 10s (x2)

  One example that uses parallelization and takes 12 sec to run.
  
* (on win-builder) * checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Florian Privé <florian.prive.21@gmail.com>'
  New submission
  
  I don't understand this NOTE because I have another package on CRAN (https://cran.r-project.org/package=primefactr).
