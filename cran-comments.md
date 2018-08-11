## Resubmission
This is a resubmission. In this version I have:

* Added single quotations to usage of 'Brandwatch' in the package Title and Description.
* Updated the author description by using the person() function in the Authors@R field, in the DESCRIPTION field.
* Fixed unexecutable code in man/bwr_cat_get.Rd by removing the unnecessary closing bracket.
* Tested the package on Ubuntu as well as OSX and Windows.

## Test environments
* local OS X install, R 3.5.1
* windows 10, R 3.4.3
* ubuntu 18.04.1, R 3.4.3

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Authentication
As Brandwatch is a paid platform, API calls cannot be tested without a valid username / password, which I have not been able to provide within this package due to security restrictions.
Should it be useful, please contact me by email and I'll be happy to share authentication to facilitate testing any functions. Thank you.
