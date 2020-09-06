**********************************************
* Data Analysis textbook
**********************************************


* before running stata code, do this

* user written
ssc install egenmore, replace
ssc install unique, replace
ssc install regsave, replace
ssc install matchit, replace
ssc install binscatter, replace
ssc install vioplot, replace
ssc install estout, replace
*net install tabmiss, replace
ssc install texsave, replace
ssc install outreg2, replace
ssc install listtex, replace
ssc install listtab, replace

ssc install elasticregress, replace
ssc install lassopack, replace
ssc install Rsource, replace
ssc install ftools, replace
ssc install matmap, replace 
ssc install crossfold, replace

* color issues
ssc install palettes, replace
ssc install colrspace, replace
net install scheme_virdis, from(https://raw.github.com/vikjam/stata-scheme-virdis/master/) replace

colorpalette, vertical n(20): viridis

* stata
ssc install wbopendata
* datahelpdesk.worldbank.org/knowledgebase/articles/889464-wbopendata-stata-module-to-access-world-bank-data

*fred
* https://blog.stata.com/2017/08/08/importing-data-with-import-fred/
*key is a valid API key, which is provided by the St. Louis Federal Reserve and may be obtained from
* https://research.stlouisfed.org/docs/api/api key.html.

*set fredkey key, permanently

* faster tools, collapse
* https://github.com/mcaceresb/stata-gtools
local github "https://raw.githubusercontent.com"
net install gtools, from(`github'/mcaceresb/stata-gtools/master/build/)

* settings
set matsize 2000

************************************-
* not yet used
************************************-


* trimming
*net install st0313.pkg, from(http://www.stata-journal.com/software/sj13-3/)
*net get st0313.pkg, from(http://www.stata-journal.com/software/sj13-3/)



* missing ibs
*net install dm91.pkg, from(http://www.stata.com/stb/stb61/)
*net installl spost9_ado.pkg, from( http://www.indiana.edu/~jslsoc/stata/)
