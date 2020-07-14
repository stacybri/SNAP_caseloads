

*  Set name of log file and start log (Automatically add date, hour, minute, and second log file starts)                                  
local time = "$S_TIME"
local time = regexr("`time'", ":", " ")
local time = regexr("`time'", ":", " ")
gl hour = word("`time'",1)
gl minute = word("`time'",2)
gl second = word("`time'",3)
global tdate=subinstr("$S_DATE", " ", "", .)
gl base_yr= 1993
gl base_mo= 03
gl end_yr= 2000
gl end_mo= 07
do 20200528_caseloads_simulation_artificial_low.do
do 20200528_caseloads_simulation_artificial.do
do 20200528_caseloads_simulation.do

gl base_yr= 2000
gl base_mo= 07
gl end_yr= 2007
gl end_mo= 12
do 20200528_caseloads_simulation_artificial_low.do
do 20200528_caseloads_simulation_artificial.do
do 20200528_caseloads_simulation.do

gl base_yr= 2007
gl base_mo= 12
gl end_yr= 2013
gl end_mo= 06
do 20200528_caseloads_simulation_artificial_low.do
do 20200528_caseloads_simulation_artificial.do
do 20200528_caseloads_simulation.do


gl base_yr= 2013
gl base_mo= 06
gl end_yr= 2016
gl end_mo= 12
do 20200528_caseloads_simulation_artificial_low.do
do 20200528_caseloads_simulation_artificial.do
do 20200528_caseloads_simulation.do

gl base_yr= 2000
gl base_mo= 07
gl end_yr= 2016
gl end_mo= 12
do 20200528_caseloads_simulation_artificial_low.do
do 20200528_caseloads_simulation_artificial.do
do 20200528_caseloads_simulation.do
