Working from Home
Raw data, Tidy data, Workfile


Raw data

many data tables
In some, observations are person level
In most, observations are person X week level
There are many variables that we won't use in the analysis
We'll work with the raw data tables that have variables that we'll use

quit_data
 observations: person-level (n=249)
 variables: ID: personid
	x: treatment group, 
	y: whether quit job by end of experiment, performance z-score during experiment, 
	z: performance z-scores pre-experiment, few background variables

tc_comparison.dta
 observations: person-level (n=249)
 variables: ID: personid
	z: all background variables used to check balance except pre-experiment performance z-score


performance_during_exper.dta
 observations: person x week level (n=112,279, 1,934 persons, 86 weeks, unbalanced)
 variables: ID: personid, year_week
	y: phonecallraw 
	+: other potential outcome variables, not used in our case study
 


Tidy data
Two data files

One at person level, wfh_tidy_person
From raw: Two person-level raw files merged 
+ merged aggregated from one person x week level file, aggregated to person level 
 (with one variable, sum phonecallraw before and during experiment

One at person X week level, wfh_tidy_personweek
From raw: One person x week level file, with only the phoencallraw (renamed phonecalls) variable 
 (plus IDs plus treatment vars)
 (kept only 249 persons in experiment from total 1,943 persons; expgroup=0 or 1)



Workfile
Same as wfh_tidy_person
 (In Stata: variables ordered so that order conforms table 20.1 on balance)
