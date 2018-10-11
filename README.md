# US Data maintainer

Set of scripts previously employed to estimate several specifications of the Fed's monetary policy rule across a variety of econometric methods with post-war data. These scripts are now included in another repository and integrated in more general routines. Therefore, this repository will be modified to host routines automating data collection, cleaning, assembling, posting, and reporting.

#### Todo list
	* strip off the unnecessary scripts
	* extend variables included in the DB
	* reduce econometrics to what's essential
	* improve speed with vectorisation where possible














# Previously:
#### UnemplTaylor
Code for TR estimates.

Highly automated, this set of scripts downloads, assembles macroeconomic time series on the US economy and then uses the tidy dataset to estimate a number of TR specifications through simple OLS with stability checks, Markov Switching Models (two or three states), VAR and SVAR. Results are stored and output in rule-specific text files, alongside with all plots produced. In addition, the composed dataset is stored in a proper CSV file with xts time series features, for later use. 



#### Todo list:

* Reda
	+ ~~improve graphs on short series~~
	+ ~~tabs and latex output~~
	+ switch to javaless xlsx import
	+ ~~automate to Pi via chron~~
	+ re-script USreg to output regs of interest-> switch to lapply
	+ extend regressions$formula and regressions$models to accommodate three splits
	+ ~~possibly add bootstrapping~~ -- no: it crashes outcorr?
	+ fork a version with static date so to have more stable estimates: mid august 2018 
	
	
