/* This code was written by Brian Stacy on Jan 14, 2014.  The goal of the code is to simulate how changes in SNAP policies and how changes in the business cycle affect welfare caseloads over time.  In addition, the code produces standard errors for the simulation estimates.

This code is based off of the Specification8_techedup_logs.do file, which was sent to me in January 2014.  Here is the introduction to that do file:

                Food Stamp Caseload Project with MORE Policy Variables AND UPDATED DATA! 
		This version does NOT have state-month effects but, instead, uses a state-specific linear trend 
		Uses both 12 and 24 month lags	
		THIS DIFFERS FROM 3D BY USING FIXED ISSUANCE EBT AND VEHICLE AND SIMPLIFIED REPORTING DATA 
		IT ALSO INCORPORATES COMMENTS AND SUGGESTIONS FROM ERS SEMINAR:
		WELFARE VARIABLE (RATHER THAN JUST TANF), TAKING OUT OUTREACH DOLLARS (AFTER CHANGING IT'S DIVISION), KATRINA DUMMY 

		IT DIFFERS FROM 4 BY FIXING THE EBT VARIABLE BY /100 AND CHANGING THE EXPANDED ELIG BY USING 
		CAROLE TRIPPE'S DATES, SUPPLEMENTED WITH EMAIL BETWEEN MA AND CT 

		IT DIFFERS FROM 5 BY USING STATE FE RATHER THAN STATE TREND. THIS IS BECAUSE WE LOOKED AT THE ZILIAK ET AL AFDC PAPER.
		WE REALIZED THAT FIRST DIFFERENCING STATE TRENDS AMOUNTS TO A STATE FE. THUS, BEFORE WE WERE USING A VARIABLE FOR THE TREND
		IN THE DIFFERENCES 	

		THIS INCLUDES THE 'FIXED' APPLY ONLINE VARIABLE AND THE 'FIXED' VEHICLE VARIABLES.

		THIS INCLUDES THE 'RIGHT' CASELOAD DATA FROM 2003+ TO ADDRESS THE PROBLEM WITH MISSOURI MISREPORTING
		THEIR CASELOAD WHEN THEY IMPLEMENTED A NEW DATA SYSTEM
		
		AND OUTCOMES RELATED TO AVERAGE BENEFIT SIZE, PER HOUSEHOLD. AND AVERAGE # OF PEOPLE PER HOUSEHOLD.
		
*/
		/*	SAME VERSION AS SPECIFICATION15_VERSION_SUBMITTED_SUMMER2016 BUT WITH EXTENDED CASELOAD DATA
		SAME VERSION AS Specification14_techedup_logs_SUBMITTED_DRAFT_VERSION_NEW.log, JUST CLEANER
		DOUBLE CHECKED TO MATCH BRIAN'S SIMULATIONS FROM EMAIL 4/28/2016 AND SIMULATIONS LABELLED 20160419_CASELOADS_SIMULATION.DO AND 20160419_CASELOADS_SIMULATION_ARTIFICIAL.DO 
		ADDED SOME ABAWD VARIABLES FROM CBPP AND CRAIG	
		CHANGED PARAMETERIZATION OF NONCITIZEN RULES AGAIN
		*/
************************************************************************
  *******  Start Logs and set globals for the simulation   **************************


clear all
set matsize 10000
set more off
capture log close





*  Set name of log file and start log (Automatically add date, hour, minute, and second log file starts)

                                       
local time = "$S_TIME"
local time = regexr("`time'", ":", " ")
local time = regexr("`time'", ":", " ")
gl hour = word("`time'",1)
gl minute = word("`time'",2)
gl second = word("`time'",3)



*  Create new directory for logs and results each day (for organizational purposes)
global tdate=subinstr("$S_DATE", " ", "", .)
capture mkdir ${tdate}_logs_${base_mo}_${base_yr}_${end_mo}_${end_yr}
capture mkdir ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}

log using ${tdate}_logs_${base_mo}_${base_yr}_${end_mo}_${end_yr}/${hour}hr_${minute}min_${second}sec_analysis.txt, replace text


* Set number of repetitions done in forming standard errors
gl reps=10
                                       

                                       

***************************************************************************
  ******* Generate variables and form more globals for analysis ********
  
use all_fsp4.dta
	drop if yrmo>201612 /* new snap database only goes up until 12/2016 */
	
tsset st_fips period
/*		Dependent Variables 
h_fsp_toth		Monthly HH Caseloads, NOT logged, Deflated
l_h_fsp_toth	Monthly HH Caseloads, Deflated by #hholds
i_fsp_totp		Monthly Indiv Caseloads, NOT logged, Deflated
l_i_fsp_totp	Logged Monthly Ind. Caseloads, deflated by #people

		Non Policy Covariates
unempr		Unemployment Rate
emp			Total Covered Employment per capita (Just divided total employment by population so didn't adjust for working age pop)
smeff 		State-Month Dummies
st_fips		State
period		time
temp			absolute deviation from average temperature
demgov		Democratic Governor
		
		Policy Variables
ReportingEarnersQuarterly 	
simplify				Year-Month Created Simplified Reporting
CertPeriodEarners0103 		Percent cert. period 1-3 months for earners (QC data generated)
CashAsstTransBenefits		Number of months of transitional benefits to TANF leavers (with missing values filled in)
IssuanceEBT 			Percentage of $ value of FSP benefits issued by EBT
Biometric 				State uses finger imaging and/or facial imaging
outreachdoll			Dollar amount of combined state, federal, and FNS grants towards outreach.
ApplyOnline 			State allows online application, some of these states require signature to be mailed in
car_1notallexempt			Exempts at least one vehicle but doesn't offer full exemption
car_allexempt			Exempts All Vehicles
teitc0mr         			real max for 2 kids fed+state eitc for tax year, in thousands
teitc_pay				real max for 2 kids fed+state eitc for tax year, in thousands * state-month federal income tax payouts
nonfull				Noncitizens under age 65 are fully eligible
nonpart				Noncitizens under age 65 are at least partially eligible
welfare				Implementation of TANF or AFDC Waiver
	*/
	/*FD of Dependent Variables*/
#delimit;
gen dc_ip = d.i_fsp_totp	;	label var dc_ip "FD of Monthly Indiv Caseloads, NOT logged, Deflated";
gen dlc_ip = d.l_i_fsp_totp	;	label var dlc_ip "FD of Logged Monthly Ind. Caseloads, deflated by #people";	
gen dlc_hh = d.l_h_fsp_toth	;	label var dlc_hh "FD of Logged Monthly HH Caseloads, Deflated by #hholds";
#delimit cr

summ yrmo if (yrmo>=199001)
************************************
****	May Need to Install the Following Stata ado file xtscc;
****	http://ideas.repec.org/c/boc/bocode/s456787.html;
*ssc install xtscc;
************************************
#delimit;

tsset st_fips period;

		* FD Variables ;
global dmeff "dm2 dm3 dm4 dm5 dm6 dm7 dm8 dm9 dm10 dm11 dm12"; /* The 1st differences of the month dummies */

levelsof st_fips, local(fips);
  gl st_trend;
foreach i in `fips' {;
    global st_trend $st_trend st_trend`i' ;
  };

tsset st_fips period;
levelsof year if yrmo>199001 & yrmo<201612 , local(yr);
  gl year_dum;
forval y=1990/2016 {;
	gen year_`y'=(year==`y');
	gen dy`y'=d.year_`y' ;
   global year_dum $year_dum dy`y' ;    
};

*global st_trend "st_trend1 st_trend2 st_trend4-st_trend56";
global unemp24 "d1u d2u d3u d4u d5u d6u d7u d8u d9u d10u d11u d12u d13u d14u d15u d16u d17u d18u d19u d20u d21u d22u d23u d24u";
global unemp12 "d1u d2u d3u d4u d5u d6u d7u d8u d9u d10u d11u d12u";
global demgov24 " d1demgov d2demgov d3demgov d4demgov d5demgov d6demgov d7demgov d8demgov d9demgov d10demgov d11demgov d12demgov d13demgov d14demgov d15demgov d16demgov d17demgov d18demgov d19demgov d20demgov d21demgov d22demgov d23demgov d24demgov";
global demgov12 " d1demgov d2demgov d3demgov d4demgov d5demgov d6demgov d7demgov d8demgov d9demgov d10demgov d11demgov d12demgov";
global welf24 " d1welfare d2welfare d3welfare d4welfare d5welfare d6welfare d7welfare d8welfare d9welfare d10welfare d11welfare d12welfare d13welfare d14welfare d15welfare d16welfare d17welfare d18welfare d19welfare d20welfare d21welfare d22welfare d23welfare d24welfare";
global welf12 " d1welfare d2welfare d3welfare d4welfare d5welfare d6welfare d7welfare d8welfare d9welfare d10welfare d11welfare d12welfare";
global iebt24 " d1iebt d2iebt d3iebt d4iebt d5iebt d6iebt d7iebt d8iebt d9iebt d10iebt d11iebt d12iebt d13iebt d14iebt d15iebt d16iebt d17iebt d18iebt d19iebt d20iebt d21iebt d22iebt d23iebt d24iebt";
global iebt12 " d1iebt d2iebt d3iebt d4iebt d5iebt d6iebt d7iebt d8iebt d9iebt d10iebt d11iebt d12iebt";
global cert0103_24 " d1CertPeriodEarners0103 d2CertPeriodEarners0103 d3CertPeriodEarners0103 d4CertPeriodEarners0103 d5CertPeriodEarners0103 d6CertPeriodEarners0103 d7CertPeriodEarners0103 d8CertPeriodEarners0103 d9CertPeriodEarners0103 d10CertPeriodEarners0103 d11CertPeriodEarners0103 d12CertPeriodEarners0103
			d13CertPeriodEarners0103 d14CertPeriodEarners0103 d15CertPeriodEarners0103 d16CertPeriodEarners0103 d17CertPeriodEarners0103 d18CertPeriodEarners0103 d19CertPeriodEarners0103 d20CertPeriodEarners0103 d21CertPeriodEarners0103 d22CertPeriodEarners0103 d23CertPeriodEarners0103 d24CertPeriodEarners0103"; 
global cert0103_12 " d1CertPeriodEarners0103 d2CertPeriodEarners0103 d3CertPeriodEarners0103 d4CertPeriodEarners0103 d5CertPeriodEarners0103 d6CertPeriodEarners0103 d7CertPeriodEarners0103 d8CertPeriodEarners0103 d9CertPeriodEarners0103 d10CertPeriodEarners0103 d11CertPeriodEarners0103 d12CertPeriodEarners0103";
global carex24 " d1car_1ex d2car_1ex d3car_1ex d4car_1ex d5car_1ex d6car_1ex d7car_1ex d8car_1ex d9car_1ex d10car_1ex d11car_1ex d12car_1ex d13car_1ex d14car_1ex d15car_1ex d16car_1ex d17car_1ex d18car_1ex d19car_1ex d20car_1ex d21car_1ex d22car_1ex d23car_1ex d24car_1ex";
global carex12 " d1car_1ex d2car_1ex d3car_1ex d4car_1ex d5car_1ex d6car_1ex d7car_1ex d8car_1ex d9car_1ex d10car_1ex d11car_1ex d12car_1ex";
global carexa24 " d1car_allex d2car_allex d3car_allex d4car_allex d5car_allex d6car_allex d7car_allex d8car_allex d9car_allex d10car_allex d11car_allex d12car_allex d13car_allex d14car_allex d15car_allex d16car_allex d17car_allex d18car_allex d19car_allex d20car_allex d21car_allex d22car_allex d23car_allex d24car_allex";
global carexa12 " d1car_allex d2car_allex d3car_allex d4car_allex d5car_allex d6car_allex d7car_allex d8car_allex d9car_allex d10car_allex d11car_allex d12car_allex";
global res24 " d1res d2res d3res d4res d5res d6res d7res d8res d9res d10res d11res d12res d13res d14res d15res d16res d17res d18res d19res d20res d21res d22res d23res d24res";
global res12 " d1res d2res d3res d4res d5res d6res d7res d8res d9res d10res d11res d12res";
global tran24 " d1tranben d2tranben d3tranben d4tranben d5tranben d6tranben d7tranben d8tranben d9tranben d10tranben d11tranben d12tranben d13tranben d14tranben d15tranben d16tranben d17tranben d18tranben d19tranben d20tranben d21tranben d22tranben d23tranben d24tranben";
global tran12 " d1tranben d2tranben d3tranben d4tranben d5tranben d6tranben d7tranben d8tranben d9tranben d10tranben d11tranben d12tranben";
global bio24 " d1bio d2bio d3bio d4bio d5bio d6bio d7bio d8bio d9bio d10bio d11bio d12bio d13bio d14bio d15bio d16bio d17bio d18bio d19bio d20bio d21bio d22bio d23bio d24bio";
global bio12 " d1bio d2bio d3bio d4bio d5bio d6bio d7bio d8bio d9bio d10bio d11bio d12bio";
global apply24 " d1online d2online d3online d4online d5online d6online d7online d8online d9online d10online d11online d12online d13online d14online d15online d16online d17online d18online d19online d20online d21online d22online d23online d24online";
global apply12 " d1online d2online d3online d4online d5online d6online d7online d8online d9online d10online d11online d12online";
global outreach24 " d1outreachdoll d2outreachdoll d3outreachdoll d4outreachdoll d5outreachdoll d6outreachdoll d7outreachdoll d8outreachdoll d9outreachdoll d10outreachdoll d11outreachdoll d12outreachdoll d13outreachdoll d14outreachdoll d15outreachdoll d16outreachdoll d17outreachdoll d18outreachdoll d19outreachdoll d20outreachdoll d21outreachdoll d22outreachdoll d23outreachdoll d24outreachdoll";
global outreach12 " d1outreachdoll d2outreachdoll d3outreachdoll d4outreachdoll d5outreachdoll d6outreachdoll d7outreachdoll d8outreachdoll d9outreachdoll d10outreachdoll d11outreachdoll d12outreachdoll";
global partbansomeadult24 "d1partban_atleast_nonadult d2partban_atleast_nonadult d3partban_atleast_nonadult d4partban_atleast_nonadult d5partban_atleast_nonadult d6partban_atleast_nonadult d7partban_atleast_nonadult d8partban_atleast_nonadult d9partban_atleast_nonadult d10partban_atleast_nonadult d11partban_atleast_nonadult d12partban_atleast_nonadult d13partban_atleast_nonadult d14partban_atleast_nonadult d15partban_atleast_nonadult d16partban_atleast_nonadult d17partban_atleast_nonadult d18partban_atleast_nonadult d19partban_atleast_nonadult d20partban_atleast_nonadult  d21partban_atleast_nonadult  d22partban_atleast_nonadult  d23partban_atleast_nonadult d24partban_atleast_nonadult";
global partbansomeadult12 "d1partban_atleast_nonadult d2partban_atleast_nonadult d3partban_atleast_nonadult d4partban_atleast_nonadult d5partban_atleast_nonadult d6partban_atleast_nonadult d7partban_atleast_nonadult d8partban_atleast_nonadult d9partban_atleast_nonadult d10partban_atleast_nonadult d11partban_atleast_nonadult d12partban_atleast_nonadult";
global teitc24 " d1teitc_pay d2teitc_pay d3teitc_pay d4teitc_pay d5teitc_pay d6teitc_pay d7teitc_pay d8teitc_pay d9teitc_pay d10teitc_pay d11teitc_pay d12teitc_pay d13teitc_pay d14teitc_pay d15teitc_pay d16teitc_pay d17teitc_pay d18teitc_pay d19teitc_pay d20teitc_pay d21teitc_pay d22teitc_pay d23teitc_pay d24teitc_pay";
global teitc12 " d1teitc_pay d2teitc_pay d3teitc_pay d4teitc_pay d5teitc_pay d6teitc_pay d7teitc_pay d8teitc_pay d9teitc_pay d10teitc_pay d11teitc_pay d12teitc_pay";
global catelig12 " d1CatElig d2CatElig d3CatElig d4CatElig d5CatElig d6CatElig d7CatElig d8CatElig d9CatElig d10CatElig d11CatElig d12CatElig";
global catelig24 " d1CatElig d2CatElig d3CatElig d4CatElig d5CatElig d6CatElig d7CatElig d8CatElig d9CatElig d10CatElig d11CatElig d12CatElig d13CatElig d14CatElig d15CatElig d16CatElig d17CatElig d18CatElig d19CatElig d20CatElig d21CatElig d22CatElig d23CatElig d24CatElig";
global ads12 " d1ads d2ads d3ads d4ads d5ads d6ads d7ads d8ads d9ads d10ads d11ads d12ads";
global ads24 " d1ads d2ads d3ads d4ads d5ads d6ads d7ads d8ads d9ads d10ads d11ads d12ads d13ads d14ads d15ads d16ads d17ads d18ads d19ads d20ads d21ads d22ads d23ads d24ads";
global abawd12 "d1abawd_final d2abawd_final d3abawd_final d4abawd_final d5abawd_final d6abawd_final d7abawd_final d8abawd_final d9abawd_final d10abawd_final d11abawd_final d12abawd_final";
global abawd24 "d1abawd_final d2abawd_final d3abawd_final d4abawd_final d5abawd_final d6abawd_final d7abawd_final d8abawd_final d9abawd_final d10abawd_final d11abawd_final d12abawd_final d13abawd_final d14abawd_final d15abawd_final d16abawd_final d17abawd_final d18abawd_final d19abawd_final d20abawd_final d21abawd_final d22abawd_final d23abawd_final d24abawd_final";
global dlc_ip24 "d1l_i_fsp_totp d2l_i_fsp_totp d3l_i_fsp_totp d4l_i_fsp_totp d5l_i_fsp_totp d6l_i_fsp_totp d7l_i_fsp_totp d8l_i_fsp_totp d9l_i_fsp_totp d10l_i_fsp_totp d11l_i_fsp_totp d12l_i_fsp_totp d13l_i_fsp_totp d14l_i_fsp_totp d15l_i_fsp_totp d16l_i_fsp_totp d17l_i_fsp_totp d18l_i_fsp_totp d19l_i_fsp_totp d20l_i_fsp_totp d21l_i_fsp_totp d22l_i_fsp_totp d23l_i_fsp_totp d24l_i_fsp_totp ";
global dlc_ip12 "d1l_i_fsp_totp d2l_i_fsp_totp d3l_i_fsp_totp d4l_i_fsp_totp d5l_i_fsp_totp d6l_i_fsp_totp d7l_i_fsp_totp d8l_i_fsp_totp d9l_i_fsp_totp d10l_i_fsp_totp d11l_i_fsp_totp d12l_i_fsp_totp";
global dlc_hh24 "d1l_h_fsp_toth d2l_h_fsp_toth d3l_h_fsp_toth d4l_h_fsp_toth d5l_h_fsp_toth d6l_h_fsp_toth d7l_h_fsp_toth d8l_h_fsp_toth d9l_h_fsp_toth d10l_h_fsp_toth d11l_h_fsp_toth d12l_h_fsp_toth d13l_h_fsp_toth d14l_h_fsp_toth d15l_h_fsp_toth d16l_h_fsp_toth d17l_h_fsp_toth d18l_h_fsp_toth d19l_h_fsp_toth d20l_h_fsp_toth d21l_h_fsp_toth d22l_h_fsp_toth d23l_h_fsp_toth d24l_h_fsp_toth ";
global dlc_hh12 "d1l_h_fsp_toth d2l_h_fsp_toth d3l_h_fsp_toth d4l_h_fsp_toth d5l_h_fsp_toth d6l_h_fsp_toth d7l_h_fsp_toth d8l_h_fsp_toth d9l_h_fsp_toth d10l_h_fsp_toth d11l_h_fsp_toth d12l_h_fsp_toth";
global dla_ben24 "d1lavghholdben d2lavghholdben d3lavghholdben d4lavghholdben d5lavghholdben d6lavghholdben d7lavghholdben d8lavghholdben d9lavghholdben d10lavghholdben d11lavghholdben d12lavghholdben d13lavghholdben d14lavghholdben d15lavghholdben d16lavghholdben d17lavghholdben d18lavghholdben d19lavghholdben d20lavghholdben d21lavghholdben d22lavghholdben d23lavghholdben d24lavghholdben";
global dla_ben12 "d1lavghholdben d2lavghholdben d3lavghholdben d4lavghholdben d5lavghholdben d6lavghholdben d7lavghholdben d8lavghholdben d9lavghholdben d10lavghholdben d11lavghholdben d12lavghholdben";
global dla_siz24 "d1avghholdsize d2avghholdsize d3avghholdsize d4avghholdsize d5avghholdsize d6avghholdsize d7avghholdsize d8avghholdsize d9avghholdsize d10avghholdsize d11avghholdsize d12avghholdsize d13avghholdsize d14avghholdsize d15avghholdsize d16avghholdsize d17avghholdsize d18avghholdsize d19avghholdsize d20avghholdsize d21avghholdsize d22avghholdsize d23avghholdsize d24avghholdsize";
global dla_siz12 "d1avghholdsize d2avghholdsize d3avghholdsize d4avghholdsize d5avghholdsize d6avghholdsize d7avghholdsize d8avghholdsize d9avghholdsize d10avghholdsize d11avghholdsize d12avghholdsize";




 *********************************************************************************************** 
 *********************************************************************** 
 ************  Begin Simulation   ******************************* 
 tsset st_fips yrmo; 
 sum dlc_ip dlc_hh if (yrmo>=199001);

 #delimit cr 


*Identify states that are biggest and smallest adopters of SNAP policies

	egen CertPeriodEarners0103_rank=rank(CertPeriodEarners0103) if year>=$base_yr & year<=$end_yr
	su CertPeriodEarners0103_rank if year>=$base_yr & year<=$end_yr
	gen CertPeriodEarners0103_pctl=(CertPeriodEarners0103_rank-1)/(`r(max)'-1)
	su CertPeriodEarners0103_pctl

	egen ebt_rank=rank(IssuanceEBT) if year>=$base_yr & year<=$end_yr
	su ebt_rank if year>=$base_yr & year<=$end_yr
	gen ebt_pctl=(ebt_rank-1)/(`r(max)'-1)
	su ebt_pctl

  
  gen tot_snap_policy=0
  gen tot_snap_policy_eligibility=0
  gen tot_snap_policy_transaction=0
  gen tot_snap_policy_abawds=0
gen car_totexempt=0
replace car_totexempt=.5 if car_1notallexempt==1
replace car_totexempt=1 if car_allexempt==1
foreach var in ebt_pctl CertPeriodEarners0103_pctl car_totexempt  simplify transition Biometric ApplyOnline ads monthdoll CashAsstExpCatEligibility partban_atleast_nonadult abawd_final {
local begin_yrmo=$base_yr*100+$base_mo    
local end_yrmo=$end_yr*100+$end_mo
gl end_yrmo=`end_yrmo'
gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo

  gen tot_snap_policy_temp=0
  gen tot_snap_policy_eligibility_temp=0
  gen tot_snap_policy_transaction_temp=0
  gen tot_snap_policy_abawds_temp=0

while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    if "`var'"=="CertPeriodEarners0103_pctl" | "`var'"=="Biometric" | "`var'"=="partban_atleast_nonadult" | "`var'"=="abawd_final"  {
  qui replace tot_snap_policy_temp= l.tot_snap_policy_temp-`var'  if yrmo==`yrmo'
  if "`var'"=="car_totexempt" | "`var'"=="transition" | "`var'"=="CashAsstExpCatEligibility" | "`var'"=="partban_atleast_nonadult" | "`var'"=="abawd_final"{
  qui replace tot_snap_policy_eligibility_temp= l.tot_snap_policy_eligibility_temp-`var'   if yrmo==`yrmo'
  }
  if "`var'"=="CertPeriodEarners0103_pctl" | "`var'"=="simplify" | "`var'"=="ApplyOnline" | "`var'"=="ebt_pctl" | "`var'"=="Biometric" {
  qui replace tot_snap_policy_transaction_temp= l.tot_snap_policy_transaction_temp-`var'  if yrmo==`yrmo'
  }
  }
  else {
    qui replace tot_snap_policy_temp= l.tot_snap_policy_temp+`var'  if yrmo==`yrmo'
  if "`var'"=="car_totexempt" | "`var'"=="transition" | "`var'"=="CashAsstExpCatEligibility"   | "`var'"=="abawd_final" {
  qui replace tot_snap_policy_eligibility_temp= l.tot_snap_policy_eligibility_temp+`var'   if yrmo==`yrmo'
  }
  if "`var'"=="CertPeriodEarners0103_pctl" | "`var'"=="simplify" | "`var'"=="ApplyOnline" | "`var'"=="ebt_pctl" | "`var'"=="Biometric" {
  qui replace tot_snap_policy_transaction_temp= l.tot_snap_policy_transaction_temp+`var'  if yrmo==`yrmo'
  }
    }
    }

  }
}
replace tot_snap_policy=tot_snap_policy+tot_snap_policy_temp
replace tot_snap_policy_eligibility=tot_snap_policy_eligibility+tot_snap_policy_eligibility_temp
replace tot_snap_policy_transaction= tot_snap_policy_transaction+ tot_snap_policy_transaction_temp
  
drop tot_snap*_temp

	}

foreach var in tot_snap_policy tot_snap_policy_eligibility tot_snap_policy_transaction {
	egen `var'_rank=rank(`var') if yrmo==`end_yrmo'
	egen `var'_r2=max(`var'_rank), by(st_fips)
	replace `var'_rank=`var'_r2
	drop `var'_r2
	}
su tot_snap_policy_rank , de
levelsof state_pc if tot_snap_policy_rank>=`r(p90)' , local(top_states) clean
gl top_states `top_states'
gen top_states=0
su tot_snap_policy_rank , de
replace top_states=1 if tot_snap_policy_rank>=`r(p90)'

su tot_snap_policy_rank , de
levelsof state_pc if tot_snap_policy_rank<=`r(p10)' , local(bottom_states) clean
gl bottom_states `bottom_states'
gen bottom_states=0
su tot_snap_policy_rank, de
replace bottom_states=1 if tot_snap_policy_rank<=`r(p10)'

su tot_snap_policy_transaction_rank , de
levelsof state_pc if tot_snap_policy_transaction_rank>=`r(p90)' , local(top_states) clean
gl top_states_transaction `top_states'
gen top_states_transaction=0
su tot_snap_policy_transaction_rank , de
replace top_states_transaction=1 if tot_snap_policy_transaction_rank>=`r(p90)'

su tot_snap_policy_transaction_rank , de
levelsof state_pc if tot_snap_policy_transaction_rank<=`r(p10)' , local(bottom_states) clean
gl bottom_states_transaction `bottom_states'
gen bottom_states_transaction=0
su tot_snap_policy_transaction_rank , de
replace bottom_states_transaction=1 if tot_snap_policy_transaction_rank<=`r(p10)'

su tot_snap_policy_eligibility_rank, de
levelsof state_pc if tot_snap_policy_eligibility_rank>=`r(p90)' , local(top_states) clean
gl top_states_eligibility `top_states'
gen top_states_eligibility=0
su tot_snap_policy_eligibility_rank , de
replace top_states_eligibility=1 if tot_snap_policy_eligibility_rank>=`r(p90)'

su tot_snap_policy_eligibility_rank , de
levelsof state_pc if tot_snap_policy_eligibility_rank<=`r(p10)' , local(bottom_states) clean
gl bottom_states_eligibility `bottom_states'
gen bottom_states_eligibility=0
su tot_snap_policy_eligibility_rank , de
replace bottom_states_eligibility=1 if tot_snap_policy_eligibility_rank<=`r(p10)'
	
*generate variable for the maximum outreach per person across all states and time periods
	  su monthdoll, de
	  gen monthdoll_max=`r(max)'
	  
	

	*>>>>>>	SECOND SET OF REGRESSIONS ADDING IN FSP VARIABLES


	foreach try1a of varlist dlc_ip {
	d `try1a' $`try1a'24
	/*	MONTH EFFECTS, 24 LAGS	*/
	
	**********Form Estimates of Effect of Various Factors over Time **************

preserve

cap mkdir ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/
        
*xtscc is fixed effects regression with Driscoll and Kraay (1998) standard errors
*Original code used xi command to include dummy variables of st_fips, but since xtscc is fixed effects regression, this is redundant.  I removed the xi code.  Here is original with xi:
* xi: xtscc `try1a' $`try1a'24 $unemp24 $welf24 $teitc24 $iebt24 $cert0103_24 $carex24 $carexa24  $res24 $tran24 $bio24 $apply24 $ads i.st_fips if ( yrmo>=199001), fe



xtset st_fips period

* Fixed effects regression (I substitute xtreg for xtscc in this part of the code, because xtreg has better postestimation features.  Specifically, I needed to use xtreg's predict features.  The standard errors are wrong, but I will use the xtscc standard errors for the second part of the simulation
        
xtreg `try1a' $`try1a'24 $unemp24 $welf24 $teitc24 $iebt24 $cert0103_24 $carex24 $carexa24 $res24 $tran24 $bio24 $apply24 $ads24 $outreach24 $catelig24 $partbansomeadult24 $abawd24 dkatrina $dmeff $st_trend $year_dum if yrmo>=199001, fe


        
predict `try1a'_st_fips_eff if ( yrmo>=199001), u

xtscc `try1a' $`try1a'24 $unemp24 $welf24 $teitc24 $iebt24 $cert0103_24 $carex24 $carexa24 $res24 $tran24 $bio24 $apply24 $ads24 $outreach24 $catelig24 $partbansomeadult24 $abawd24 dkatrina  $year_dum $dmeff $st_trend if ( yrmo>=199001), fe

*generate 24 lags of the predicted dependent variable



**********************************************
  ****Form predicted caseloads based on all  factors

gen `try1a'_other_chgs=_b[_cons]
foreach var in $unemp24 $welf24 $teitc24 $iebt24 $cert0103_24 $carex24 $carexa24  $res24 $tran24 $bio24 $apply24 $ads24 $outreach24 $catelig24 $partbansomeadult24 $abawd24 dkatrina  $year_dum $dmeff $st_trend {
  replace `try1a'_other_chgs=`try1a'_other_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo
xtset st_fips period
while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_o= `try1a'_other_chgs + `try1a'_st_fips_eff if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
      qui replace `try1a'_predicted_o= `try1a'_predicted_o + l`i'.`try1a'_predicted_o*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_o= `try1a'_predicted_o + l`i'.`try1a'_predicted_o*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}


egen `try1a'_tot_pred_o_chg_orig=total(`try1a'_predicted_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)
drop `try1a'_predicted_o `try1a'_other_chgs


		/******************************************************************************************************************/
      /* To form "Artificial USA", I need to replace the SNAP policy variables to be set to 1 in the first period and   */
		/* zero otherwise.  This ensures that the SNAP policies are switched "on" for the duration of the time period.	   */
		/* Remember we are dealing with differenced outcomes, so switching to 1 in first period and zero otherwise		   */
		/* ensures SNAP variables are "on" for full period																					   */
      /******************************************************************************************************************/


		foreach var in iebt CertPeriodEarners0103 car_1ex car_allex res tranben bio online ads outreachdoll CatElig partban_atleast_nonadult abawd_final  {
			forval i=1/24 {
				local mo=$base_mo+`i'
				 if `mo'<=12 local yrmo=$base_yr*100+$base_mo+`i'
				 if `mo'>12 & `mo'<=24 local yrmo=(${base_yr}+1)*100+$base_mo+(`i'-12)
				 if `mo'>24 local yrmo=(${base_yr}+2)*100+$base_mo+(`i'-24) 
				replace d`i'`var'=0
				    if "`var'"=="CertPeriodEarners0103"   {
						replace d`i'`var'=-1*l`i'.`var' if yrmo==`yrmo'
						}
					else if "`var'"=="car_1ex"{
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.car_1notallexempt==0
						}
					else if "`var'"=="car_allex"{
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.car_allexempt==1
						}							
				   else if "`var'"=="iebt"   {
						replace d`i'`var'=(1-l`i'.IssuanceEBT) if yrmo==`yrmo'
						}	
					else if  "`var'"=="bio"   {
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.Biometric==1
						}
					else if  "`var'"=="partban_atleast_nonadult"   {
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.partban_atleast_nonadult==1
						}									
					else if "`var'"=="res" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.simplify==0
						}
					else if "`var'"=="abawd_final" {
						replace d`i'`var'=-1*l`i'.`var' if yrmo==`yrmo' 
						}						
					else if "`var'"=="tranben" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.transition==0
						}			
					else if "`var'"=="online" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.ApplyOnline==0
						}	
					else if "`var'"=="CatElig" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.CashAsstExpCatEligibility==0
						}	
					else if "`var'"=="outreachdoll" {
						replace d`i'`var'=(monthdoll_max-l`i'.monthdoll) if yrmo==`yrmo'
						}			
					else {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.`var'==0
						}
				
				}
			}

			
**********************************************
  ****Form predicted caseloads based on all  factors

gen `try1a'_other_chgs=_b[_cons]
foreach var in $unemp24 $welf24 $teitc24 $iebt24 $cert0103_24 $carex24 $carexa24  $res24 $tran24 $bio24 $apply24 $ads24 $outreach24 $catelig24 $partbansomeadult24 $abawd24 dkatrina  $year_dum $dmeff $st_trend {
  replace `try1a'_other_chgs=`try1a'_other_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo
xtset st_fips period
while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_o= `try1a'_other_chgs + `try1a'_st_fips_eff if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
      qui replace `try1a'_predicted_o= `try1a'_predicted_o + l`i'.`try1a'_predicted_o*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_o= `try1a'_predicted_o + l`i'.`try1a'_predicted_o*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_o_chg=total(`try1a'_predicted_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)

egen `try1a'_total_chg=total(`try1a') if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)

gen `try1a'_fra_exp_o=`try1a'_tot_pred_o_chg/`try1a'_total_chg

        

**********************************************
  ****Form predicted caseloads if only welfare reform doesnt change





gen `try1a'_welfare_chgs=0
foreach var in $welf24 $teitc24  {
  replace `try1a'_welfare_chgs=`try1a'_welfare_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_w=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_w= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_welfare_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_w= `try1a'_predicted_w + l`i'.`try1a'_predicted_w*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_w= `try1a'_predicted_w + l`i'.`try1a'_predicted_w*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_w_chg=total(`try1a'_predicted_w) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing



gen `try1a'_fra_exp_w=`try1a'_tot_pred_w_chg/`try1a'_total_chg
*gen `try1a'_fra_exp_w2=`try1a'_tot_pred_w_chg/`try1a'_tot_pred_o_chg




**********************************************
  ****Form predicted caseloads if only fsp reform doesn't change





gen `try1a'_fsp_all_chgs=0
foreach var in  $iebt24 $cert0103_24 $carex24 $carexa24  $res24 $tran24 $bio24 $apply24 $ads24 $outreach24 $catelig24 $partbansomeadult24 $abawd24{
  replace `try1a'_fsp_all_chgs=`try1a'_fsp_all_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_all=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_all= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_all_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_all= `try1a'_predicted_fsp_all + l`i'.`try1a'_predicted_fsp_all*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_all= `try1a'_predicted_fsp_all + l`i'.`try1a'_predicted_fsp_all*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_all_chg=total(`try1a'_predicted_fsp_all) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_all=`try1a'_tot_pred_fsp_all_chg/`try1a'_total_chg
*gen `try1a'_fra_exp_fsp_all2=`try1a'_tot_pred_fsp_all_chg/`try1a'_tot_pred_o_chg


**********************************************
  ****Form predicted caseloads if only fsp associated with transaction costs reform doesn't change





gen `try1a'_fsp_tc_chgs=0
foreach var in  $cert0103_24 $res24 $apply24 $iebt24 $bio24 {
  replace `try1a'_fsp_tc_chgs=`try1a'_fsp_tc_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_tc=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_tc= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_tc_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_tc= `try1a'_predicted_fsp_tc + l`i'.`try1a'_predicted_fsp_tc*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_tc= `try1a'_predicted_fsp_tc + l`i'.`try1a'_predicted_fsp_tc*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_tc_chg=total(`try1a'_predicted_fsp_tc) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_tc=`try1a'_tot_pred_fsp_tc_chg/`try1a'_total_chg
*gen `try1a'_fra_exp_fsp_tc2=`try1a'_tot_pred_fsp_tc_chg/`try1a'_tot_pred_o_chg


**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility doesn't change





gen `try1a'_fsp_elig_chgs=0
foreach var in  $carex24 $carexa24 $tran24 $catelig24 $partbansomeadult24 $abawd24{
  replace `try1a'_fsp_elig_chgs=`try1a'_fsp_elig_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_elig=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_elig= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_elig_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_elig= `try1a'_predicted_fsp_elig + l`i'.`try1a'_predicted_fsp_elig*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_elig= `try1a'_predicted_fsp_elig + l`i'.`try1a'_predicted_fsp_elig*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_elig_chg=total(`try1a'_predicted_fsp_elig) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_elig=`try1a'_tot_pred_fsp_elig_chg/`try1a'_total_chg
*gen `try1a'_fra_exp_fsp_elig2=`try1a'_tot_pred_fsp_elig_chg/`try1a'_tot_pred_o_chg


**********************************************
  ****Form predicted caseloads if only fsp reform associated with broad based eligibility doesn't change





gen `try1a'_fsp_eligb_chgs=0
foreach var in  $catelig24 {
  replace `try1a'_fsp_eligb_chgs=`try1a'_fsp_eligb_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_eligb=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_eligb= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_eligb_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_eligb= `try1a'_predicted_fsp_eligb + l`i'.`try1a'_predicted_fsp_eligb*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_eligb= `try1a'_predicted_fsp_eligb + l`i'.`try1a'_predicted_fsp_eligb*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_eligb_chg=total(`try1a'_predicted_fsp_eligb) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_eligb=`try1a'_tot_pred_fsp_eligb_chg/`try1a'_total_chg
*gen `try1a'_fra_exp_fsp_eligb2=`try1a'_tot_pred_fsp_eligb_chg/`try1a'_tot_pred_o_chg

**********************************************
  ****Form predicted caseloads if only fsp reform associated with abawds doesn't change





gen `try1a'_fsp_abawd_chgs=0
foreach var in  $abawd24 {
  replace `try1a'_fsp_abawd_chgs=`try1a'_fsp_abawd_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_abawd=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_abawd= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_abawd_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_abawd= `try1a'_predicted_fsp_abawd + l`i'.`try1a'_predicted_fsp_abawd*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_abawd= `try1a'_predicted_fsp_abawd + l`i'.`try1a'_predicted_fsp_abawd*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_abawd_chg=total(`try1a'_predicted_fsp_abawd) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_abawd=`try1a'_tot_pred_fsp_abawd_chg/`try1a'_total_chg
*gen `try1a'_fra_exp_fsp_abawd2=`try1a'_tot_pred_fsp_abawd_chg/`try1a'_tot_pred_o_chg

**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility for non-citizens doesn't change




gen `try1a'_fsp_noncit_chgs=0
foreach var in  $partbansomeadult24 {
  replace `try1a'_fsp_noncit_chgs=`try1a'_fsp_noncit_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_noncit=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_noncit= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_noncit_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_noncit= `try1a'_predicted_fsp_noncit + l`i'.`try1a'_predicted_fsp_noncit*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_noncit= `try1a'_predicted_fsp_noncit + l`i'.`try1a'_predicted_fsp_noncit*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_noncit_chg=total(`try1a'_predicted_fsp_noncit) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_noncit=`try1a'_tot_pred_fsp_noncit_chg/`try1a'_total_chg
*gen `try1a'_fra_exp_fsp_noncit2=`try1a'_tot_pred_fsp_noncit_chg/`try1a'_tot_pred_o_chg

gl addition_var

**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility for non-citizens doesn't change

gl care24 $carex24 $carexa24
gl cres24 $cert0103_24 $res24


foreach set in iebt cert0103_ care cres res tran bio apply ads outreach unemp dmeff {
local set1=substr("`set'",1,5)
gen `try1a'_fsp_`set1'_chgs=0

if "`set'"!="dmeff" {
foreach var in  ${`set'24} {
  replace `try1a'_fsp_`set1'_chgs=`try1a'_fsp_`set1'_chgs+`var'*_b[`var']
}
}

if "`set'"=="dmeff" {
foreach var in  ${`set'} {
  replace `try1a'_fsp_`set1'_chgs=`try1a'_fsp_`set1'_chgs+`var'*_b[`var']
}
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_`set1'=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_`set1'= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_`set1'_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_`set1'= `try1a'_predicted_fsp_`set1' + l`i'.`try1a'_predicted_fsp_`set1'*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_`set1'= `try1a'_predicted_fsp_`set1' + l`i'.`try1a'_predicted_fsp_`set1'*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}
egen `try1a'_tot_pred_fsp_`set1'_chg=total(`try1a'_predicted_fsp_`set1') if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_`set1'=`try1a'_tot_pred_fsp_`set1'_chg/`try1a'_total_chg
*gen `try1a'_fra_exp_fsp_`set1'2=`try1a'_tot_pred_fsp_`set1'_chg/`try1a'_tot_pred_o_chg

gl addition_var $addition_var `try1a'_fra_exp_fsp_`set1' `try1a'_fra_exp_fsp_`set1'2 `try1a'_tot_pred_fsp_`set1'_chg
}


**********************************************
  ****Form predicted caseloads if only $welf reform doesn't change





gen `try1a'_wel_o_chgs=0
foreach var in  $welf24 {
  replace `try1a'_wel_o_chgs=`try1a'_wel_o_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_wel_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_wel_o= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_wel_o_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_wel_o= `try1a'_predicted_wel_o + l`i'.`try1a'_predicted_wel_o*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_wel_o= `try1a'_predicted_wel_o + l`i'.`try1a'_predicted_wel_o*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_wel_o_chg=total(`try1a'_predicted_wel_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_wel_o=`try1a'_tot_pred_wel_o_chg/`try1a'_total_chg
*gen `try1a'_fra_exp_wel_o2=`try1a'_tot_pred_wel_o_chg/`try1a'_tot_pred_o_chg





**********************************************
  ****Form predicted caseloads if only $teitc reform doesn't change





gen `try1a'_veitc_chgs=0
foreach var in  $teitc24 {
  replace `try1a'_veitc_chgs=`try1a'_veitc_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_veitc=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_veitc= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_veitc_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_veitc= `try1a'_predicted_veitc + l`i'.`try1a'_predicted_veitc*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_veitc= `try1a'_predicted_veitc + l`i'.`try1a'_predicted_veitc*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_veitc_chg=total(`try1a'_predicted_veitc) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_veitc=`try1a'_tot_pred_veitc_chg/`try1a'_total_chg
*gen `try1a'_fra_exp_veitc2=`try1a'_tot_pred_veitc_chg/`try1a'_tot_pred_o_chg






**********************************************
  ****Form predicted caseloads based on economic and seasonal factors




gen `try1a'_econ_seas_chgs=0
foreach var in $unemp24  $dmeff {
  replace `try1a'_econ_seas_chgs=`try1a'_econ_seas_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_es=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo

while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_es= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_econ_seas_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
      qui replace `try1a'_predicted_es= `try1a'_predicted_es + l`i'.`try1a'_predicted_es*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_es= `try1a'_predicted_es + l`i'.`try1a'_predicted_es*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_es_chg=total(`try1a'_predicted_es) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)

gen `try1a'_fra_exp_es=`try1a'_tot_pred_es_chg/`try1a'_total_chg
*gen `try1a'_fra_exp_es2=`try1a'_tot_pred_es_chg/`try1a'_tot_pred_o_chg




foreach var in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_noncit wel_o veitc es o fsp_iebt fsp_cert0 fsp_care fsp_cres fsp_res fsp_tran fsp_bio fsp_apply fsp_ads fsp_outre fsp_unemp fsp_dmeff {
  gen `try1a'_fra_exp_`var'2 =`try1a'_tot_pred_`var'_chg/`try1a'_tot_pred_o_chg_orig
}





su `try1a'_fra_exp_w  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb `try1a'_tot_pred_fsp_eligb_chg  `try1a'_fra_exp_fsp_noncit `try1a'_tot_pred_fsp_noncit_chg  `try1a'_fra_exp_wel_o `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es `try1a'_tot_pred_es_chg `try1a'_fra_exp_o `try1a'_tot_pred_o_chg  `try1a'_total_chg, detail

su `try1a'_fra_exp_w `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb `try1a'_tot_pred_fsp_eligb_chg  `try1a'_fra_exp_fsp_noncit `try1a'_tot_pred_fsp_noncit_chg  `try1a'_fra_exp_wel_o `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es `try1a'_tot_pred_es_chg `try1a'_fra_exp_o `try1a'_tot_pred_o_chg  `try1a'_total_chg  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'
bysort st_fips: su `try1a'_fra_exp_w  `try1a'_tot_pred_w_chg  `try1a'_fra_exp_fsp_all `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb `try1a'_tot_pred_fsp_eligb_chg  `try1a'_fra_exp_fsp_noncit `try1a'_tot_pred_fsp_noncit_chg  `try1a'_fra_exp_wel_o `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es `try1a'_tot_pred_es_chg `try1a'_fra_exp_o `try1a'_tot_pred_o_chg `try1a'_total_chg  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'


 corr `try1a'_tot_pred_w_chg   `try1a'_tot_pred_fsp_all_chg `try1a'_tot_pred_es_chg `try1a'_tot_pred_o_chg `try1a'_total_chg  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'

gen beg_year=$base_yr
gen beg_mo=$base_mo
gen end_year=$end_yr
gen end_mo=$end_mo

      if "`try1a'"=="dlc_hh" {
        gen snap_tot=h_fsp_tot
    }
      if "`try1a'"=="dlc_ip" {
        gen snap_tot=i_fsp_tot
    }

su snap_tot

collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg* `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp24 $welf24 $teitc24 $temp24 dkatrina pop (firstnm) snap_tot yrmo  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'   , by(st_fips)

save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_estimates_fsp, replace

collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg* `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp24 $welf24 $teitc24 $temp24 dkatrina pop snap_tot yrmo [aweight=snap_tot]

gen st_fips=-10
replace state_pc="USA"



foreach var in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_noncit wel_o veitc es o fsp_iebt fsp_cert0 fsp_care fsp_cres fsp_res fsp_tran fsp_bio fsp_apply fsp_ads fsp_outre fsp_unemp fsp_dmeff {
  replace `try1a'_fra_exp_`var' =`try1a'_tot_pred_`var'_chg/`try1a'_total_chg
  replace `try1a'_fra_exp_`var'2 =`try1a'_tot_pred_`var'_chg/`try1a'_tot_pred_o_chg_orig
}


save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_estimates_fsp_overall, replace

use ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_estimates_fsp, replace

append using "${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_estimates_fsp_overall.dta"

save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_estimates_fsp, replace

restore


**********Form SE of Effect of Various Factors over Time **************

forv rep=1/$reps {
preserve
xtset st_fips period

*generate state dummy variables (This is done to make it easy to get DK s.e. for the state fixed effect estimates)

tab st_fips if ( yrmo>=199001), gen(dst_fips_)
gl dst_fips
local t
levelsof st_fips if ( yrmo>=199001), local(fips)
foreach x in `fips' {
    local t=`t' + 1
  gl dst_fips $dst_fips dst_fips_`t'
}


qui xtscc `try1a' $`try1a'24 $unemp24 $welf24 $teitc24 $iebt24 $cert0103_24 $carex24 $carexa24  $res24 $tran24 $bio24 $apply24 $ads24 $outreach24 $catelig24 $partbansomeadult24 $abawd24 dkatrina  $year_dum $dmeff $st_trend $dst_fips if ( yrmo>=199001), nocons ase
                                                                               
*Generate normally distributed random draw from distribution of estimator

gl random_draws
foreach var in $`try1a'24  $unemp24 $welf24 $teitc24 $iebt24 $cert0103_24 $carex24 $carexa24  $res24 $tran24 $bio24 $apply24 $ads24 $outreach24 $catelig24 $partbansomeadult24 $abawd24 dkatrina  $year_dum $dmeff $st_trend $dst_fips {
  gl random_draws $random_draws `var'_rd
}
drawnorm $random_draws, means(e(b)) cov(e(V))

*This generates N*K random draws, where N is number of observations and K is number of regressors
*I dont want the draw to be different for each observation.  I want them constant.
*I achieve this by setting all draws to be equal to the first observation.
*This is clunky but effective and was the fastest way I could come up with.


foreach var in $random_draws {
qui replace `var'=`var'[1]
}


**********************************************
  ****Form predicted caseloads based on all  factors


gen `try1a'_other_chgs=0
foreach var in $unemp24 $welf24 $teitc24 $iebt24 $cert0103_24 $carex24 $carexa24  $res24 $tran24 $bio24 $apply24 $ads24 $outreach24 $catelig24 $partbansomeadult24 $abawd24 dkatrina  $year_dum $dmeff $st_trend $dst_fips {
  qui replace `try1a'_other_chgs=`try1a'_other_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo
xtset st_fips period
while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_o= `try1a'_other_chgs if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
      qui replace `try1a'_predicted_o= `try1a'_predicted_o + l`i'.`try1a'_predicted_o*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_o= `try1a'_predicted_o + l`i'.`try1a'_predicted_o*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}


egen `try1a'_tot_pred_o_chg_orig=total(`try1a'_predicted_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)
drop `try1a'_predicted_o `try1a'_other_chgs


		/******************************************************************************************************************/
      /* To form "Artificial USA", I need to replace the SNAP policy variables to be set to 1 in the first period and   */
		/* zero otherwise.  This ensures that the SNAP policies are switched "on" for the duration of the time period.	   */
		/* Remember we are dealing with differenced outcomes, so switching to 1 in first period and zero otherwise		   */
		/* ensures SNAP variables are "on" for full period																					   */
      /******************************************************************************************************************/

		foreach var in iebt CertPeriodEarners0103 car_1ex car_allex res tranben bio online ads outreachdoll CatElig partban_atleast_nonadult abawd_final {
			forval i=1/24 {
				local mo=$base_mo+`i'
				 if `mo'<=12 local yrmo=$base_yr*100+$base_mo+`i'
				 if `mo'>12 & `mo'<=24 local yrmo=(${base_yr}+1)*100+$base_mo+(`i'-12)
				 if `mo'>24 local yrmo=(${base_yr}+2)*100+$base_mo+(`i'-24) 
				replace d`i'`var'=0
				    if "`var'"=="CertPeriodEarners0103"   {
						replace d`i'`var'=-1*l`i'.`var' if yrmo==`yrmo'
						}	
					else if "`var'"=="car_1ex"{
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.car_1notallexempt==0
						}	
					else if "`var'"=="car_allex"{
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.car_allexempt==1
						}							
				   else if "`var'"=="iebt"   {
						replace d`i'`var'=(1-l`i'.IssuanceEBT) if yrmo==`yrmo'
						}	
					else if  "`var'"=="bio"   {
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.Biometric==1
						}
					else if  "`var'"=="partban_atleast_nonadult"   {
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.partban_atleast_nonadult==1
						}							
					else if "`var'"=="res" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.simplify==0
						}
					else if "`var'"=="abawd_final" {
						replace d`i'`var'=-1*l`i'.`var' if yrmo==`yrmo' 
						}						
					else if "`var'"=="tranben" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.transition==0
						}			
					else if "`var'"=="online" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.ApplyOnline==0
						}	
					else if "`var'"=="CatElig" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.CashAsstExpCatEligibility==0
						}
					else if "`var'"=="outreachdoll" {
						replace d`i'`var'=(monthdoll_max-l`i'.monthdoll) if yrmo==`yrmo'
						}							
					else {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.`var'==0
						}
				}
			}
		
			
			

**********************************************
  ****Form predicted caseloads based on all  factors

gen `try1a'_other_chgs=0
foreach var in $unemp24 $welf24 $teitc24 $iebt24 $cert0103_24 $carex24 $carexa24  $res24 $tran24 $bio24 $apply24 $ads24 $outreach24 $catelig24 $partbansomeadult24 $abawd24 dkatrina  $year_dum $dmeff $st_trend $dst_fips {
  qui replace `try1a'_other_chgs=`try1a'_other_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo
xtset st_fips period
while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_o= `try1a'_other_chgs if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
      qui replace `try1a'_predicted_o= `try1a'_predicted_o + l`i'.`try1a'_predicted_o*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_o= `try1a'_predicted_o + l`i'.`try1a'_predicted_o*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_o_chg=total(`try1a'_predicted_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)
egen `try1a'_total_chg=total(`try1a') if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)
gen `try1a'_fra_exp_o=`try1a'_tot_pred_o_chg/`try1a'_total_chg


**********************************************
  ****Form predicted caseloads if only welfare reform doesn't change


gen `try1a'_welfare_chgs=0
foreach var in  $welf24 $teitc24 {
  qui replace `try1a'_welfare_chgs=`try1a'_welfare_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_w=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_w= `try1a'_other_chgs -`try1a'_welfare_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_w= `try1a'_predicted_w + l`i'.`try1a'_predicted_w*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_w= `try1a'_predicted_w + l`i'.`try1a'_predicted_w*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_w_chg=total(`try1a'_predicted_w) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing



gen `try1a'_fra_exp_w=`try1a'_tot_pred_w_chg/`try1a'_total_chg





**********************************************
  ****Form predicted caseloads if only fsp reform doesn't change





gen `try1a'_fsp_all_chgs=0
foreach var in  $iebt24 $cert0103_24 $carex24 $carexa24  $res24 $tran24 $bio24 $apply24 $ads24 $outreach24 $catelig24 $partbansomeadult24 $abawd24{
  replace `try1a'_fsp_all_chgs=`try1a'_fsp_all_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_all=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_all= `try1a'_other_chgs -`try1a'_fsp_all_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_all= `try1a'_predicted_fsp_all + l`i'.`try1a'_predicted_fsp_all*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_all= `try1a'_predicted_fsp_all + l`i'.`try1a'_predicted_fsp_all*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_all_chg=total(`try1a'_predicted_fsp_all) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_all=`try1a'_tot_pred_fsp_all_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp associated with transaction costs reform doesn't change





gen `try1a'_fsp_tc_chgs=0
foreach var in  $cert0103_24 $res24 $apply24 $iebt24 $bio24 {
  replace `try1a'_fsp_tc_chgs=`try1a'_fsp_tc_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_tc=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_tc= `try1a'_other_chgs -`try1a'_fsp_tc_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_tc= `try1a'_predicted_fsp_tc + l`i'.`try1a'_predicted_fsp_tc*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_tc= `try1a'_predicted_fsp_tc + l`i'.`try1a'_predicted_fsp_tc*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_tc_chg=total(`try1a'_predicted_fsp_tc) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_tc=`try1a'_tot_pred_fsp_tc_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility doesn't change





gen `try1a'_fsp_elig_chgs=0
foreach var in  $carex24 $carexa24 $tran24 $catelig24 $partbansomeadult24 $abawd24{
  replace `try1a'_fsp_elig_chgs=`try1a'_fsp_elig_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_elig=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_elig= `try1a'_other_chgs -`try1a'_fsp_elig_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_elig= `try1a'_predicted_fsp_elig + l`i'.`try1a'_predicted_fsp_elig*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_elig= `try1a'_predicted_fsp_elig + l`i'.`try1a'_predicted_fsp_elig*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_elig_chg=total(`try1a'_predicted_fsp_elig) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_elig=`try1a'_tot_pred_fsp_elig_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with broad based eligibility doesn't change





gen `try1a'_fsp_eligb_chgs=0
foreach var in  $catelig24 {
  replace `try1a'_fsp_eligb_chgs=`try1a'_fsp_eligb_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_eligb=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_eligb= `try1a'_other_chgs -`try1a'_fsp_eligb_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_eligb= `try1a'_predicted_fsp_eligb + l`i'.`try1a'_predicted_fsp_eligb*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_eligb= `try1a'_predicted_fsp_eligb + l`i'.`try1a'_predicted_fsp_eligb*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_eligb_chg=total(`try1a'_predicted_fsp_eligb) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_eligb=`try1a'_tot_pred_fsp_eligb_chg/`try1a'_total_chg

**********************************************
  ****Form predicted caseloads if only fsp reform associated with abawds doesn't change





gen `try1a'_fsp_abawd_chgs=0
foreach var in  $abawd24 {
  replace `try1a'_fsp_abawd_chgs=`try1a'_fsp_abawd_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_abawd=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_abawd= `try1a'_other_chgs -`try1a'_fsp_abawd_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_abawd= `try1a'_predicted_fsp_abawd + l`i'.`try1a'_predicted_fsp_abawd*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_abawd= `try1a'_predicted_fsp_abawd + l`i'.`try1a'_predicted_fsp_abawd*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_abawd_chg=total(`try1a'_predicted_fsp_abawd) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_abawd=`try1a'_tot_pred_fsp_abawd_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility for non-citizens doesn't change





gen `try1a'_fsp_noncit_chgs=0
foreach var in  $partbansomeadult24 {
  replace `try1a'_fsp_noncit_chgs=`try1a'_fsp_noncit_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_noncit=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_noncit= `try1a'_other_chgs -`try1a'_fsp_noncit_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_noncit= `try1a'_predicted_fsp_noncit + l`i'.`try1a'_predicted_fsp_noncit*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_noncit= `try1a'_predicted_fsp_noncit + l`i'.`try1a'_predicted_fsp_noncit*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_noncit_chg=total(`try1a'_predicted_fsp_noncit) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_noncit=`try1a'_tot_pred_fsp_noncit_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if other sets of variables change

gl care24 $carex24 $carexa24
gl cres24 $cert0103_24 $res24


foreach set in iebt cert0103_ care cres res tran bio apply ads outreach unemp dmeff {
local set1=substr("`set'",1,5)

gen `try1a'_fsp_`set1'_chgs=0


if "`set'"!="dmeff" {
foreach var in  ${`set'24} {
  replace `try1a'_fsp_`set1'_chgs=`try1a'_fsp_`set1'_chgs+`var'*`var'_rd
}
}

if "`set'"=="dmeff" {
foreach var in  ${`set'} {
  replace `try1a'_fsp_`set1'_chgs=`try1a'_fsp_`set1'_chgs+`var'*`var'_rd
}
}



local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_`set1'=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_`set1'= `try1a'_other_chgs -`try1a'_fsp_`set1'_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_`set1'= `try1a'_predicted_fsp_`set1' + l`i'.`try1a'_predicted_fsp_`set1'*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_`set1'= `try1a'_predicted_fsp_`set1' + l`i'.`try1a'_predicted_fsp_`set1'*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_`set1'_chg=total(`try1a'_predicted_fsp_`set1') if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_`set1'=`try1a'_tot_pred_fsp_`set1'_chg/`try1a'_total_chg

}


**********************************************
  ****Form predicted caseloads if only $welf reform doesn't change





gen `try1a'_wel_o_chgs=0
foreach var in  $welf24 {
  replace `try1a'_wel_o_chgs=`try1a'_wel_o_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_wel_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_wel_o= `try1a'_other_chgs -`try1a'_wel_o_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_wel_o= `try1a'_predicted_wel_o + l`i'.`try1a'_predicted_wel_o*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_wel_o= `try1a'_predicted_wel_o + l`i'.`try1a'_predicted_wel_o*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_wel_o_chg=total(`try1a'_predicted_wel_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_wel_o=`try1a'_tot_pred_wel_o_chg/`try1a'_total_chg






**********************************************
  ****Form predicted caseloads if only $teitc reform doesn't change





gen `try1a'_veitc_chgs=0
foreach var in  $teitc24 {
  replace `try1a'_veitc_chgs=`try1a'_veitc_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_veitc=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_veitc= `try1a'_other_chgs -`try1a'_veitc_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_veitc= `try1a'_predicted_veitc + l`i'.`try1a'_predicted_veitc*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_veitc= `try1a'_predicted_veitc + l`i'.`try1a'_predicted_veitc*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_veitc_chg=total(`try1a'_predicted_veitc) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_veitc=`try1a'_tot_pred_veitc_chg/`try1a'_total_chg







**********************************************
  ****Form predicted caseloads based on economic and seasonal factors


gen `try1a'_econ_seas_chgs=0
foreach var in $unemp24  $dmeff {
  qui replace `try1a'_econ_seas_chgs=`try1a'_econ_seas_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_es=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo

while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_es= `try1a'_other_chgs -`try1a'_econ_seas_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/24 {
      if "`try1a'"=="dlc_hh" {
      qui replace `try1a'_predicted_es= `try1a'_predicted_es + l`i'.`try1a'_predicted_es*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_es= `try1a'_predicted_es + l`i'.`try1a'_predicted_es*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_es_chg=total(`try1a'_predicted_es) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)

gen `try1a'_fra_exp_es=`try1a'_tot_pred_es_chg/`try1a'_total_chg




foreach var in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_noncit wel_o veitc es o fsp_iebt fsp_cert0 fsp_care fsp_cres fsp_res fsp_tran fsp_bio fsp_apply fsp_ads fsp_outre fsp_unemp fsp_dmeff {
  gen `try1a'_fra_exp_`var'2 =`try1a'_tot_pred_`var'_chg/`try1a'_tot_pred_o_chg_orig
}



gen beg_year=$base_yr
gen beg_mo=$base_mo
gen end_year=$end_yr
gen end_mo=$end_mo

      if "`try1a'"=="dlc_hh" {
        gen snap_tot=h_fsp_tot
    }
      if "`try1a'"=="dlc_ip" {
        gen snap_tot=i_fsp_tot
    }

collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg*  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg   `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg* `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp24 $welf24 $teitc24 $temp24 dkatrina pop (firstnm) snap_tot yrmo if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'  , by(st_fips)

save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_se_fsp_rep`rep', replace

collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg*  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg* `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp24 $welf24 $teitc24 $temp24  dkatrina pop  snap_tot yrmo  [aweight=snap_tot]

gen st_fips=-10
replace state_pc="USA"


foreach var in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_noncit wel_o veitc es o fsp_iebt fsp_cert0 fsp_care fsp_cres fsp_res fsp_tran fsp_bio fsp_apply fsp_ads fsp_outre fsp_unemp fsp_dmeff {
  replace `try1a'_fra_exp_`var' =`try1a'_tot_pred_`var'_chg/`try1a'_total_chg
  replace `try1a'_fra_exp_`var'2 =`try1a'_tot_pred_`var'_chg/`try1a'_tot_pred_o_chg_orig
}


save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_se_fsp_overall`rep', replace

use ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_se_fsp_rep`rep', replace

append using "${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_se_fsp_overall`rep'.dta"

save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_se_fsp_rep`rep', replace



restore
}

preserve

clear all

local list: dir "${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/" files "`try1a'_24lags_se_fsp_rep*.dta"
  local rep=1
  gen rep=.

foreach x in `list' {
  di "append using `x'"
  append using ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`x', generate(temp)
  qui replace rep=`rep' if temp==1
  qui drop temp
  local rep=`rep'+1
}


collapse (firstnm) state_pc (sd) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg*  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg* `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp24 $welf24 $teitc24 pop , by(st_fips)

save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_se_combined_fsp, replace

clear
use ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_estimates_fsp
append using ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_se_combined_fsp, generate(se)


label var `try1a'_fra_exp_w "Fraction Explained Actual- no change Welfare Reform"
label var `try1a'_fra_exp_w2 "Fraction Explained Predicted- no change Welfare Reform"
label var `try1a'_tot_pred_w_chg "Predicted Change- no change Welfare Reform"

label var `try1a'_fra_exp_fsp_all "Fraction Explained Actual- no change All FSP Reform Variables"
label var `try1a'_fra_exp_fsp_all2 "Fraction Explained Predicted- no change All FSP Reform Variables"
label var `try1a'_tot_pred_fsp_all_chg "Predicted Change- no change All FSP Reform Variables"

label var `try1a'_fra_exp_fsp_tc "Fraction Explained Actual- no change FSP Reform Transaction Costs"
label var `try1a'_fra_exp_fsp_tc2 "Fraction Explained Predicted- no change FSP Reform Affecting Transaction Costs"
label var `try1a'_tot_pred_fsp_tc_chg "Predicted Change- no change FSP Reform Affecting Transaction Costs"

label var `try1a'_fra_exp_fsp_elig "Fraction Explained Actual- no change FSP Reform Eligibility"
label var `try1a'_fra_exp_fsp_elig2 "Fraction Explained Predicted- no change FSP Reform Eligibility"
label var `try1a'_tot_pred_fsp_elig_chg "Predicted Change- no change FSP Reform Affecting Eligibility"

label var `try1a'_fra_exp_fsp_eligb "Fraction Explained Actual- no change FSP Reform Broad Based Categorical Eligility"
label var `try1a'_fra_exp_fsp_eligb2 "Fraction Explained Predicted- no change FSP Reform Broad Based Categorical Eligility"
label var `try1a'_tot_pred_fsp_eligb_chg "Predicted Change- no change FSP Reform Broad Based Categorical Eligility"

label var `try1a'_fra_exp_fsp_noncit "Fraction Explained Actual - no change FSP Reform Eligility of Non-citizens"
label var `try1a'_fra_exp_fsp_noncit2 "Fraction Explained Predicted- no change FSP Reform Eligility of Non-citizens"
label var `try1a'_tot_pred_fsp_noncit_chg "Predicted Change- no change FSP Reform Eligility of Non-citizens"

foreach set in iebt cert0 care cres res tran bio apply ads outre {
local set1 `set'
  if "`set'"=="care" {
    local set1 carex carexa
  }

  if "`set'"=="cres" {
    local set1 cert0103 res
  }
  
  label var `try1a'_fra_exp_fsp_`set' "Fraction Explained Actual- no change FSP Reform Variables `set1'"
  label var `try1a'_fra_exp_fsp_`set'2 "Fraction Explained Predicted- no change FSP Reform Variables `set1'"
  label var `try1a'_tot_pred_fsp_`set'_chg "Predicted Change- no change FSP Reform Variables `set1' "
  
}

label var `try1a'_fra_exp_wel_o "Fraction Explained Actual- no change only the Welfare Variable (No EITC)"
label var `try1a'_fra_exp_wel_o2 "Fraction Explained Predicted- no change only the Welfare Variable (No EITC)"
label var `try1a'_tot_pred_wel_o_chg "Predicted Change- no change only the Welfare Variable (No EITC)"

label var `try1a'_fra_exp_veitc "Fraction Explained Actual- no change only the VETIC Variable (No Welfare)"
label var `try1a'_fra_exp_veitc2 "Fraction Explained Predicted- no change only the VETIC Variable (No Welfare)"
label var `try1a'_tot_pred_veitc_chg "Predicted Change- no change only the VETIC Variable (No Welfare)"

label var `try1a'_fra_exp_es "Fraction Explained Actual- no change Economic and Seasonal Factors"
label var `try1a'_fra_exp_es2 "Fraction Explained Predicted- no change Economic and Seasonal Factors"
label var `try1a'_tot_pred_es_chg "Predicted Change- no change Economic and Seasonal Factors"

label var `try1a'_fra_exp_o "Fraction Explained Actual -All Observable Factors Changed"
label var `try1a'_fra_exp_o2 "Fraction Explained Predicted -  All Observable Factors Changed"
label var `try1a'_tot_pred_o_chg "Predicted Change if All Observable Factors Changed"

label var `try1a'_total_chg "Total Actual Change"

label define se 0 "Artificial Estimate" 1 "Standard Error"
label values se se
label var se "Artificial Estimate"

label define over -10 "USA Artificial"
label values st_fips over

sort st_fips se
order se
save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_fsp_artificial, replace
export excel using ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_fsp_artificial, firstrow(varlabels) replace
keep in 1/2
save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_24lags_results_fsp/`try1a'_24lags_fsp_artificial_usa, replace
restore
}
	
	
	
	
	
	
	
	
	
	
	
	
	
	/*	MONTH EFFECTS, 12 LAGS	*/


 	
	
	foreach try1a of varlist dlc_ip {
	d `try1a' $`try1a'12

		**********Form Estimates of Effect of Various Factors over Time **************

preserve

cap mkdir ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/
        
*xtscc is fixed effects regression with Driscoll and Kraay (1998) standard errors ;
*Original code used xi command to include dummy variables of st_fips, but since xtscc is fixed effects regression, this is redundant.  I removed the xi code.  Here is original with xi: ;
* xi: xtscc `try1a' $`try1a'12 $unemp12 $welf12 $teitc12 $iebt12 $cert0103_12 $carex12 $carexa12  $res12 $tran12 $bio12 $apply12 $ads i.st_fips if ( yrmo>=199001), fe



xtset st_fips period

* Fixed effects regression (I substitute xtreg for xtscc in this part of the code, because xtreg has better postestimation features.  Specifically, I needed to use xtreg's predict features.  The standard errors are wrong, but I will use the xtscc standard errors for the second part of the simulation ;

xtreg `try1a' $`try1a'12 $unemp12 $welf12 $teitc12 $iebt12 $cert0103_12 $carex12 $carexa12  $res12 $tran12 $bio12 $apply12 $ads12 $outreach12 $catelig12 $partbansomeadult12 $abawd12 dkatrina  $year_dum $dmeff $st_trend if ( yrmo>=199001), fe

predict `try1a'_st_fips_eff if ( yrmo>=199001), u

xtscc `try1a' $`try1a'12 $unemp12 $welf12 $teitc12 $iebt12 $cert0103_12 $carex12 $carexa12  $res12 $tran12 $bio12 $apply12 $ads12 $outreach12 $catelig12 $partbansomeadult12 $abawd12 dkatrina  $year_dum $dmeff $st_trend if ( yrmo>=199001), fe

*generate 12 lags of the predicted dependent variable




		/******************************************************************************************************************/
      /* To form "Artificial USA", I need to replace the SNAP policy variables to be set to 1 in the first period and   */
		/* zero otherwise.  This ensures that the SNAP policies are switched "on" for the duration of the time period.	   */
		/* Remember we are dealing with differenced outcomes, so switching to 1 in first period and zero otherwise		   */
		/* ensures SNAP variables are "on" for full period																					   */
      /******************************************************************************************************************/

		foreach var in iebt CertPeriodEarners0103 car_1ex car_allex res tranben bio online ads outreachdoll CatElig partban_atleast_nonadult abawd_final {
			forval i=1/12 {
				local mo=$base_mo+`i'
				 if `mo'<=12 local yrmo=$base_yr*100+$base_mo+`i'
				 if `mo'>12 & `mo'<=24 local yrmo=(${base_yr}+1)*100+$base_mo+(`i'-12)
				 if `mo'>24 local yrmo=(${base_yr}+2)*100+$base_mo+(`i'-24) 
				replace d`i'`var'=0
				    if "`var'"=="CertPeriodEarners0103"   {
						replace d`i'`var'=-1*l`i'.`var' if yrmo==`yrmo'
						}	
					else if "`var'"=="car_1ex"{
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.car_1notallexempt==0
						}	
					else if "`var'"=="car_allex"{
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.car_allexempt==1
						}	
						else if "`var'"=="iebt"   {
						replace d`i'`var'=(1-l`i'.IssuanceEBT) if yrmo==`yrmo'
						}	
					else if  "`var'"=="bio"   {
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.Biometric==1
						}	
					else if  "`var'"=="partban_atleast_nonadult"   {
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.partban_atleast_nonadult==1
						}							
					else if "`var'"=="res" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.simplify==0
						
					else if "`var'"=="abawd_final" {
						replace d`i'`var'=-1*l`i'.`var' if yrmo==`yrmo' 
						}						
					else if "`var'"=="tranben" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.transition==0
						}			
					else if "`var'"=="online" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.ApplyOnline==0
						}	
					else if "`var'"=="CatElig" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.CashAsstExpCatEligibility==0
						}
					else if "`var'"=="outreachdoll" {
						replace d`i'`var'=(monthdoll_max-l`i'.monthdoll) if yrmo==`yrmo'
						}							
					else {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.`var'==0
						}
				}
			}
		
			}
			

**********************************************
  ****Form predicted caseloads based on all  factors

gen `try1a'_other_chgs=_b[_cons]
foreach var in $unemp12 $welf12 $teitc12 $iebt12 $cert0103_12 $carex12 $carexa12  $res12 $tran12 $bio12 $apply12 $ads12  $outreach12 $catelig12 $partbansomeadult12 $abawd12 dkatrina  $year_dum $dmeff $st_trend {
  replace `try1a'_other_chgs=`try1a'_other_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo
xtset st_fips period
while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_o= `try1a'_other_chgs + `try1a'_st_fips_eff if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
      qui replace `try1a'_predicted_o= `try1a'_predicted_o + l`i'.`try1a'_predicted_o*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_o= `try1a'_predicted_o + l`i'.`try1a'_predicted_o*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_o_chg=total(`try1a'_predicted_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)

egen `try1a'_total_chg=total(`try1a') if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)

gen `try1a'_fra_exp_o=`try1a'_tot_pred_o_chg/`try1a'_total_chg

        
**********************************************
  ****Form predicted caseloads if only welfare reform doesn't change





gen `try1a'_welfare_chgs=0
foreach var in $welf12 $teitc12 {
  replace `try1a'_welfare_chgs=`try1a'_welfare_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_w=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_w= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_welfare_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_w= `try1a'_predicted_w + l`i'.`try1a'_predicted_w*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_w= `try1a'_predicted_w + l`i'.`try1a'_predicted_w*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_w_chg=total(`try1a'_predicted_w) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing


gen `try1a'_fra_exp_w=`try1a'_tot_pred_w_chg/`try1a'_total_chg


**********************************************
  ****Form predicted caseloads if only fsp reform doesn't change





gen `try1a'_fsp_all_chgs=0
foreach var in  $iebt12 $cert0103_12 $carex12 $carexa12  $res12 $tran12 $bio12 $apply12 $ads12  $outreach12 $catelig12 $partbansomeadult12 $abawd12 {
  replace `try1a'_fsp_all_chgs=`try1a'_fsp_all_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_all=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_all= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_all_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_all= `try1a'_predicted_fsp_all + l`i'.`try1a'_predicted_fsp_all*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_all= `try1a'_predicted_fsp_all + l`i'.`try1a'_predicted_fsp_all*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_all_chg=total(`try1a'_predicted_fsp_all) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_all=`try1a'_tot_pred_fsp_all_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp associated with transaction costs reform doesn't change





gen `try1a'_fsp_tc_chgs=0
foreach var in  $cert0103_12 $res12 $apply12 $iebt12 $bio12 {
  replace `try1a'_fsp_tc_chgs=`try1a'_fsp_tc_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_tc=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_tc=`try1a'_other_chgs + `try1a'_st_fips_eff- `try1a'_fsp_tc_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_tc= `try1a'_predicted_fsp_tc + l`i'.`try1a'_predicted_fsp_tc*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_tc= `try1a'_predicted_fsp_tc + l`i'.`try1a'_predicted_fsp_tc*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_tc_chg=total(`try1a'_predicted_fsp_tc) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_tc=`try1a'_tot_pred_fsp_tc_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility doesn't change





gen `try1a'_fsp_elig_chgs=0
foreach var in  $carex12 $carexa12 $tran12 $catelig12 $partbansomeadult12 $abawd12 {
  replace `try1a'_fsp_elig_chgs=`try1a'_fsp_elig_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_elig=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_elig= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_elig_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_elig= `try1a'_predicted_fsp_elig + l`i'.`try1a'_predicted_fsp_elig*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_elig= `try1a'_predicted_fsp_elig + l`i'.`try1a'_predicted_fsp_elig*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_elig_chg=total(`try1a'_predicted_fsp_elig) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_elig=`try1a'_tot_pred_fsp_elig_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with broad based eligibility doesn't change





gen `try1a'_fsp_eligb_chgs=0
foreach var in  $catelig12 {
  replace `try1a'_fsp_eligb_chgs=`try1a'_fsp_eligb_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_eligb=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_eligb= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_eligb_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_eligb= `try1a'_predicted_fsp_eligb + l`i'.`try1a'_predicted_fsp_eligb*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_eligb= `try1a'_predicted_fsp_eligb + l`i'.`try1a'_predicted_fsp_eligb*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_eligb_chg=total(`try1a'_predicted_fsp_eligb) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_eligb=`try1a'_tot_pred_fsp_eligb_chg/`try1a'_total_chg


**********************************************
  ****Form predicted caseloads if only fsp reform associated with abawds doesn't change





gen `try1a'_fsp_abawd_chgs=0
foreach var in  $abawd12 {
  replace `try1a'_fsp_abawd_chgs=`try1a'_fsp_eligb_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_abawd=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_abawd= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_abawd_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_abawd= `try1a'_predicted_fsp_abawd + l`i'.`try1a'_predicted_fsp_abawd*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_abawd= `try1a'_predicted_fsp_abawd + l`i'.`try1a'_predicted_fsp_abawd*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_abawd_chg=total(`try1a'_predicted_fsp_abawd) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_abawd=`try1a'_tot_pred_fsp_abawd_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility for non-citizens doesn't change





gen `try1a'_fsp_noncit_chgs=0
foreach var in  $partbansomeadult12 {
  replace `try1a'_fsp_noncit_chgs=`try1a'_fsp_noncit_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_noncit=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_noncit=`try1a'_other_chgs + `try1a'_st_fips_eff- `try1a'_fsp_noncit_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_noncit= `try1a'_predicted_fsp_noncit + l`i'.`try1a'_predicted_fsp_noncit*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_noncit= `try1a'_predicted_fsp_noncit + l`i'.`try1a'_predicted_fsp_noncit*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_noncit_chg=total(`try1a'_predicted_fsp_noncit) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_noncit=`try1a'_tot_pred_fsp_noncit_chg/`try1a'_total_chg


**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility for non-citizens doesn't change
gl addition_var

gl care12 $carex12 $carexa12
gl cres12 $cert0103_12 $res12


foreach set in iebt cert0103_ care cres res tran bio apply ads outreach unemp dmeff {
local set1=substr("`set'",1,5)

gen `try1a'_fsp_`set1'_chgs=0

if "`set'"!="dmeff" {
foreach var in  ${`set'12} {
  replace `try1a'_fsp_`set1'_chgs=`try1a'_fsp_`set1'_chgs+`var'*_b[`var']
}
}

if "`set'"=="dmeff" {
foreach var in  ${`set'} {
  replace `try1a'_fsp_`set1'_chgs=`try1a'_fsp_`set1'_chgs+`var'*_b[`var']
}
}


local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_`set1'=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_`set1'= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_`set1'_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_`set1'= `try1a'_predicted_fsp_`set1' + l`i'.`try1a'_predicted_fsp_`set1'*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_`set1'= `try1a'_predicted_fsp_`set1' + l`i'.`try1a'_predicted_fsp_`set1'*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_`set1'_chg=total(`try1a'_predicted_fsp_`set1') if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_`set1'=`try1a'_tot_pred_fsp_`set1'_chg/`try1a'_total_chg

gl addition_var $addition_var `try1a'_fra_exp_fsp_`set1' `try1a'_fra_exp_fsp_`set1'2 `try1a'_tot_pred_fsp_`set1'_chg

}


**********************************************
  ****Form predicted caseloads if only $welf reform doesn't change





gen `try1a'_wel_o_chgs=0
foreach var in  $welf12 {
  replace `try1a'_wel_o_chgs=`try1a'_wel_o_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_wel_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_wel_o=`try1a'_other_chgs + `try1a'_st_fips_eff- `try1a'_wel_o_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_wel_o= `try1a'_predicted_wel_o + l`i'.`try1a'_predicted_wel_o*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_wel_o= `try1a'_predicted_wel_o + l`i'.`try1a'_predicted_wel_o*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_wel_o_chg=total(`try1a'_predicted_wel_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_wel_o=`try1a'_tot_pred_wel_o_chg/`try1a'_total_chg






**********************************************
  ****Form predicted caseloads if only $teitc reform doesn't change





gen `try1a'_veitc_chgs=0
foreach var in  $teitc12 {
  replace `try1a'_veitc_chgs=`try1a'_veitc_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_veitc=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_veitc= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_veitc_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_veitc= `try1a'_predicted_veitc + l`i'.`try1a'_predicted_veitc*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_veitc= `try1a'_predicted_veitc + l`i'.`try1a'_predicted_veitc*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_veitc_chg=total(`try1a'_predicted_veitc) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_veitc=`try1a'_tot_pred_veitc_chg/`try1a'_total_chg








**********************************************
  ****Form predicted caseloads based on economic and seasonal factors




gen `try1a'_econ_seas_chgs=0
foreach var in $unemp12 $dmeff {
  replace `try1a'_econ_seas_chgs=`try1a'_econ_seas_chgs+`var'*_b[`var']
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_es=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo

while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_es= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_econ_seas_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
      qui replace `try1a'_predicted_es= `try1a'_predicted_es + l`i'.`try1a'_predicted_es*_b[d`i'l_h_fsp_toth] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_es= `try1a'_predicted_es + l`i'.`try1a'_predicted_es*_b[d`i'l_i_fsp_totp] if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_es_chg=total(`try1a'_predicted_es) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)

gen `try1a'_fra_exp_es=`try1a'_tot_pred_es_chg/`try1a'_total_chg






foreach var in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_abawd fsp_noncit wel_o veitc es o fsp_iebt fsp_cert0 fsp_care fsp_cres fsp_res fsp_tran fsp_bio fsp_apply fsp_ads fsp_outre fsp_unemp fsp_dmeff {
  gen `try1a'_fra_exp_`var'2 =`try1a'_tot_pred_`var'_chg/`try1a'_tot_pred_o_chg
}





su `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg  `try1a'_total_chg, detail

su `try1a'_fra_exp_w `try1a'_fra_exp_w2 `try1a'_tot_pred_w_chg `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg  `try1a'_total_chg  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'
bysort st_fips: su `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg   `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'


 corr `try1a'_tot_pred_w_chg  `try1a'_tot_pred_es_chg `try1a'_tot_pred_o_chg `try1a'_total_chg  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'

gen beg_year=$base_yr
gen beg_mo=$base_mo
gen end_year=$end_yr
gen end_mo=$end_mo

      if "`try1a'"=="dlc_hh" {
        gen snap_tot=h_fsp_tot
    }
      if "`try1a'"=="dlc_ip" {
        gen snap_tot=i_fsp_tot
    }

su snap_tot

collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp24 $welf24 $teitc24 $temp24  dkatrina pop (lastnm) tot_snap_policy tot_snap_policy_eligibility tot_snap_policy_transaction top_states bottom_states top_states_transaction bottom_states_transaction top_states_eligibility bottom_states_eligibility (firstnm) snap_tot yrmo tot_snap_policy_init=tot_snap_policy tot_snap_policy_eligibility_init=tot_snap_policy_eligibility tot_snap_policy_transaction_init=tot_snap_policy_transaction  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'   , by(st_fips)

save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_estimates_fsp, replace

collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp24 $welf24 $teitc24 $temp24 dkatrina pop snap_tot yrmo  [aweight=snap_tot]

gen st_fips=-10
replace state_pc="USA"


foreach var in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_abawd fsp_noncit wel_o veitc es o fsp_iebt fsp_cert0 fsp_care fsp_cres fsp_res fsp_tran fsp_bio fsp_apply fsp_ads fsp_outre fsp_unemp fsp_dmeff {
  replace `try1a'_fra_exp_`var' =`try1a'_tot_pred_`var'_chg/`try1a'_total_chg
  replace     `try1a'_fra_exp_`var'2 =`try1a'_tot_pred_`var'_chg/`try1a'_tot_pred_o_chg
}


save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_estimates_fsp_overall, replace


local counter=0
foreach var in top_states bottom_states top_states_transaction bottom_states_transaction top_states_eligibility bottom_states_eligibility {
local counter=`counter'-1			
use ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_estimates_fsp, replace
keep if `var'==1
collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp12 $welf12 $teitc12 $temp12  dkatrina pop snap_tot yrmo [aweight=snap_tot]

gen st_fips=`counter'
replace state_pc="`var'"



foreach x in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_abawd fsp_noncit wel_o veitc es o fsp_iebt fsp_cert0 fsp_care fsp_cres fsp_res fsp_tran fsp_bio fsp_apply fsp_ads fsp_unemp fsp_dmeff {
  replace `try1a'_fra_exp_`x' =`try1a'_tot_pred_`x'_chg/`try1a'_total_chg
  replace `try1a'_fra_exp_`x'2 =`try1a'_tot_pred_`x'_chg/`try1a'_tot_pred_o_chg
}


save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_estimates_fsp_`var', replace
}

		
use ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_estimates_fsp, replace



		
append using "${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_estimates_fsp_overall.dta"
		foreach var in top_states bottom_states top_states_transaction bottom_states_transaction top_states_eligibility bottom_states_eligibility {
			append using "${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_estimates_fsp_`var'.dta"
			}
save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_estimates_fsp, replace

restore
			
			


**********Form SE of Effect of Various Factors over Time **************

forv rep=1/$reps {
preserve
xtset st_fips period

*generate state dummy variables (This is done to make it easy to get DK s.e. for the state fixed effect estimates)

tab st_fips if ( yrmo>=199001), gen(dst_fips_)
gl dst_fips
local t
levelsof st_fips if ( yrmo>=199001), local(fips)
foreach x in `fips' {
    local t=`t' + 1
  gl dst_fips $dst_fips dst_fips_`t'
}


qui xtscc `try1a' $`try1a'12 $unemp12 $welf12 $teitc12 $iebt12 $cert0103_12 $carex12 $carexa12  $res12 $tran12 $bio12 $apply12 $ads12 $outreach12 $catelig12 $partbansomeadult12 $abawd12 dkatrina  $year_dum $dmeff $st_trend  $dst_fips if ( yrmo>=199001), nocons ase
                                                                               
*Generate normally distributed random draw from distribution of estimator

gl random_draws
foreach var in $`try1a'12 $unemp12 $welf12 $teitc12 $iebt12 $cert0103_12 $carex12 $carexa12  $res12 $tran12 $bio12 $apply12 $ads12  $outreach12 $catelig12 $partbansomeadult12 $abawd12 dkatrina  $year_dum $dmeff $st_trend  $dst_fips {
  gl random_draws $random_draws `var'_rd
}
drawnorm $random_draws, means(e(b)) cov(e(V))

*This generates N*K random draws, where N is number of observations and K is number of regressors
*I dont want the draw to be different for each observation.  I want them constant.
*I achieve this by setting all draws to be equal to the first observation.
*This is clunky but effective and was the fastest way I could come up with.


foreach var in $random_draws {
qui replace `var'=`var'[1]
}



		/******************************************************************************************************************/
      /* To form "Artificial USA", I need to replace the SNAP policy variables to be set to 1 in the first period and   */
		/* zero otherwise.  This ensures that the SNAP policies are switched "on" for the duration of the time period.	   */
		/* Remember we are dealing with differenced outcomes, so switching to 1 in first period and zero otherwise		   */
		/* ensures SNAP variables are "on" for full period																					   */
      /******************************************************************************************************************/

		foreach var in iebt CertPeriodEarners0103 car_1ex car_allex res tranben bio online ads outreachdoll CatElig partban_atleast_nonadult abawd_final {
			forval i=1/12 {
				local mo=$base_mo+`i'
				 if `mo'<=12 local yrmo=$base_yr*100+$base_mo+`i'
				 if `mo'>12 & `mo'<=24 local yrmo=(${base_yr}+1)*100+$base_mo+(`i'-12)
				 if `mo'>24 local yrmo=(${base_yr}+2)*100+$base_mo+(`i'-24) 
				replace d`i'`var'=0
				    if "`var'"=="CertPeriodEarners0103"   {
						replace d`i'`var'=-1*l`i'.`var' if yrmo==`yrmo'
						}	
					else if "`var'"=="car_1ex"{
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.car_1notallexempt==0
						}
					else if "`var'"=="car_allex"{
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.car_allexempt==1
						}							
						else if "`var'"=="iebt"   {
						replace d`i'`var'=(1-l`i'.IssuanceEBT) if yrmo==`yrmo'
						}	
					else if  "`var'"=="bio"   {
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.Biometric==1
						}
					else if  "`var'"=="partban_atleast_nonadult"   {
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.partban_atleast_nonadult==1
						}								
					else if "`var'"=="res" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.simplify==0
						
					else if "`var'"=="abawd_final" {
						replace d`i'`var'=-1*l`i'.`var' if yrmo==`yrmo' 
						}						
					else if "`var'"=="tranben" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.transition==0
						}			
					else if "`var'"=="online" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.ApplyOnline==0
						}	
					else if "`var'"=="CatElig" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.CashAsstExpCatEligibility==0
						}
					else if "`var'"=="outreachdoll" {
						replace d`i'`var'=(monthdoll_max-l`i'.monthdoll) if yrmo==`yrmo'
						}							
					else {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.`var'==0
						}
				}
			}
		}
**********************************************
  ****Form predicted caseloads based on all  factors

gen `try1a'_other_chgs=0
foreach var in $unemp12 $welf12 $teitc12 $iebt12 $cert0103_12 $carex12 $carexa12  $res12 $tran12 $bio12 $apply12 $ads12  $outreach12 $catelig12 $partbansomeadult12 $abawd12 dkatrina  $year_dum $dmeff $st_trend $dst_fips {
  qui replace `try1a'_other_chgs=`try1a'_other_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo
xtset st_fips period
while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_o= `try1a'_other_chgs if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
      qui replace `try1a'_predicted_o= `try1a'_predicted_o + l`i'.`try1a'_predicted_o*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_o= `try1a'_predicted_o + l`i'.`try1a'_predicted_o*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_o_chg=total(`try1a'_predicted_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)
egen `try1a'_total_chg=total(`try1a') if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)
gen `try1a'_fra_exp_o=`try1a'_tot_pred_o_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only welfare reform doesn't change





gen `try1a'_welfare_chgs=0
foreach var in $welf12 $teitc12 {
  replace `try1a'_welfare_chgs=`try1a'_welfare_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_w=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_w= `try1a'_other_chgs-`try1a'_welfare_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_w= `try1a'_predicted_w + l`i'.`try1a'_predicted_w*d`i'l_h_fsp_toth_rd  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_w= `try1a'_predicted_w + l`i'.`try1a'_predicted_w*d`i'l_i_fsp_totp_rd  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_w_chg=total(`try1a'_predicted_w) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing



gen `try1a'_fra_exp_w=`try1a'_tot_pred_w_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform doesn't change





gen `try1a'_fsp_all_chgs=0
foreach var in  $iebt12 $cert0103_12 $carex12 $carexa12  $res12 $tran12 $bio12 $apply12 $ads12  $outreach12 $catelig12 $partbansomeadult12 $abawd12 {
  replace `try1a'_fsp_all_chgs=`try1a'_fsp_all_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_all=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_all= `try1a'_other_chgs-`try1a'_fsp_all_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_all= `try1a'_predicted_fsp_all + l`i'.`try1a'_predicted_fsp_all*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_all= `try1a'_predicted_fsp_all + l`i'.`try1a'_predicted_fsp_all*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_all_chg=total(`try1a'_predicted_fsp_all) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_all=`try1a'_tot_pred_fsp_all_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp associated with transaction costs reform doesn't change





gen `try1a'_fsp_tc_chgs=0
foreach var in  $cert0103_12 $res12 $apply12 $iebt12 $bio12 {
  replace `try1a'_fsp_tc_chgs=`try1a'_fsp_tc_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_tc=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_tc= `try1a'_other_chgs-`try1a'_fsp_tc_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_tc= `try1a'_predicted_fsp_tc + l`i'.`try1a'_predicted_fsp_tc*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_tc= `try1a'_predicted_fsp_tc + l`i'.`try1a'_predicted_fsp_tc*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_tc_chg=total(`try1a'_predicted_fsp_tc) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_tc=`try1a'_tot_pred_fsp_tc_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility doesn't change





gen `try1a'_fsp_elig_chgs=0
foreach var in  $carex12 $carexa12 $tran12 $catelig12 $partbansomeadult12 $abawd12 {
  replace `try1a'_fsp_elig_chgs=`try1a'_fsp_elig_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_elig=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_elig= `try1a'_other_chgs-`try1a'_fsp_elig_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_elig= `try1a'_predicted_fsp_elig + l`i'.`try1a'_predicted_fsp_elig*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_elig= `try1a'_predicted_fsp_elig + l`i'.`try1a'_predicted_fsp_elig*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_elig_chg=total(`try1a'_predicted_fsp_elig) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_elig=`try1a'_tot_pred_fsp_elig_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with broad based eligibility doesn't change





gen `try1a'_fsp_eligb_chgs=0
foreach var in  $catelig12 {
  replace `try1a'_fsp_eligb_chgs=`try1a'_fsp_eligb_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_eligb=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_eligb= `try1a'_other_chgs-`try1a'_fsp_eligb_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_eligb= `try1a'_predicted_fsp_eligb + l`i'.`try1a'_predicted_fsp_eligb*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_eligb= `try1a'_predicted_fsp_eligb + l`i'.`try1a'_predicted_fsp_eligb*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_eligb_chg=total(`try1a'_predicted_fsp_eligb) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_eligb=`try1a'_tot_pred_fsp_eligb_chg/`try1a'_total_chg


**********************************************
  ****Form predicted caseloads if only fsp reform associated with abawds doesn't change





gen `try1a'_fsp_abawd_chgs=0
foreach var in  $abawd12 {
  replace `try1a'_fsp_abawd_chgs=`try1a'_fsp_abawd_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_abawd=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_abawd= `try1a'_other_chgs-`try1a'_fsp_abawd_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_abawd= `try1a'_predicted_fsp_abawd + l`i'.`try1a'_predicted_fsp_abawd*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_abawd= `try1a'_predicted_fsp_abawd + l`i'.`try1a'_predicted_fsp_abawd*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_abawd_chg=total(`try1a'_predicted_fsp_abawd) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_abawd=`try1a'_tot_pred_fsp_abawd_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility for non-citizens doesn't change





gen `try1a'_fsp_noncit_chgs=0
foreach var in  $partbansomeadult12 {
  replace `try1a'_fsp_noncit_chgs=`try1a'_fsp_noncit_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_noncit=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_noncit= `try1a'_other_chgs-`try1a'_fsp_noncit_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_noncit= `try1a'_predicted_fsp_noncit + l`i'.`try1a'_predicted_fsp_noncit*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_noncit= `try1a'_predicted_fsp_noncit + l`i'.`try1a'_predicted_fsp_noncit*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_fsp_noncit_chg=total(`try1a'_predicted_fsp_noncit) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_noncit=`try1a'_tot_pred_fsp_noncit_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only other fsp variable doesn't change


gl care12 $carex12 $carexa12
gl cres12 $cert0103_12 $res12


foreach set in iebt cert0103_ care cres res tran bio apply ads outreach unemp dmeff {

local set1=substr("`set'",1,5)

gen `try1a'_fsp_`set1'_chgs=0


if "`set'"!="dmeff" {
foreach var in  ${`set'12} {
  replace `try1a'_fsp_`set1'_chgs=`try1a'_fsp_`set1'_chgs+`var'*`var'_rd
}
}

if "`set'"=="dmeff" {
foreach var in  ${`set'} {
  replace `try1a'_fsp_`set1'_chgs=`try1a'_fsp_`set1'_chgs+`var'*`var'_rd
}
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_`set1'=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_`set1'= `try1a'_other_chgs-`try1a'_fsp_`set1'_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_fsp_`set1'= `try1a'_predicted_fsp_`set1' + l`i'.`try1a'_predicted_fsp_`set1'*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_fsp_`set1'= `try1a'_predicted_fsp_`set1' + l`i'.`try1a'_predicted_fsp_`set1'*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}
egen `try1a'_tot_pred_fsp_`set1'_chg=total(`try1a'_predicted_fsp_`set1') if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_`set1'=`try1a'_tot_pred_fsp_`set1'_chg/`try1a'_total_chg
}

**********************************************
  ****Form predicted caseloads if only $welf reform doesn't change





gen `try1a'_wel_o_chgs=0
foreach var in  $welf12 {
  replace `try1a'_wel_o_chgs=`try1a'_wel_o_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_wel_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_wel_o= `try1a'_other_chgs-`try1a'_wel_o_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_wel_o= `try1a'_predicted_wel_o + l`i'.`try1a'_predicted_wel_o*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_wel_o= `try1a'_predicted_wel_o + l`i'.`try1a'_predicted_wel_o*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_wel_o_chg=total(`try1a'_predicted_wel_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_wel_o=`try1a'_tot_pred_wel_o_chg/`try1a'_total_chg






**********************************************
  ****Form predicted caseloads if only $teitc reform doesn't change





gen `try1a'_veitc_chgs=0
foreach var in  $teitc12 {
  replace `try1a'_veitc_chgs=`try1a'_veitc_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_veitc=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_veitc= `try1a'_other_chgs-`try1a'_veitc_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
     qui replace `try1a'_predicted_veitc= `try1a'_predicted_veitc + l`i'.`try1a'_predicted_veitc*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_veitc= `try1a'_predicted_veitc + l`i'.`try1a'_predicted_veitc*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_veitc_chg=total(`try1a'_predicted_veitc) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_veitc=`try1a'_tot_pred_veitc_chg/`try1a'_total_chg





**********************************************
  ****Form predicted caseloads based on economic and seasonal factors


gen `try1a'_econ_seas_chgs=0
foreach var in $unemp12 $dmeff {
  qui replace `try1a'_econ_seas_chgs=`try1a'_econ_seas_chgs+`var'*`var'_rd
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_es=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo

while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_es= `try1a'_other_chgs-`try1a'_econ_seas_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    forv i=1/12 {
      if "`try1a'"=="dlc_hh" {
      qui replace `try1a'_predicted_es= `try1a'_predicted_es + l`i'.`try1a'_predicted_es*d`i'l_h_fsp_toth_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
      if "`try1a'"=="dlc_ip" {
      qui replace `try1a'_predicted_es= `try1a'_predicted_es + l`i'.`try1a'_predicted_es*d`i'l_i_fsp_totp_rd if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    }
    }
    

  }
}
}

egen `try1a'_tot_pred_es_chg=total(`try1a'_predicted_es) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)

gen `try1a'_fra_exp_es=`try1a'_tot_pred_es_chg/`try1a'_total_chg


foreach var in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_abawd fsp_noncit wel_o veitc es o fsp_iebt fsp_cert0 fsp_care fsp_cres fsp_res fsp_tran fsp_bio fsp_apply fsp_ads fsp_outre fsp_unemp fsp_dmeff {
  gen `try1a'_fra_exp_`var'2 =`try1a'_tot_pred_`var'_chg/`try1a'_tot_pred_o_chg
}




gen beg_year=$base_yr
gen beg_mo=$base_mo
gen end_year=$end_yr
gen end_mo=$end_mo


      if "`try1a'"=="dlc_hh" {
        gen snap_tot=h_fsp_tot
    }
      if "`try1a'"=="dlc_ip" {
        gen snap_tot=i_fsp_tot
    }

collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp24 $welf24 $teitc24 $temp24  dkatrina pop (lastnm) tot_snap_policy tot_snap_policy_eligibility tot_snap_policy_transaction top_states bottom_states top_states_transaction bottom_states_transaction top_states_eligibility bottom_states_eligibility (firstnm) snap_tot yrmo tot_snap_policy_init=tot_snap_policy tot_snap_policy_eligibility_init=tot_snap_policy_eligibility tot_snap_policy_transaction_init=tot_snap_policy_transaction  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'   , by(st_fips)

save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_se_fsp_rep`rep', replace

collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp24 $welf24 $teitc24 $temp24 dkatrina pop snap_tot yrmo  [aweight=snap_tot]

gen st_fips=-10
replace state_pc="USA"


foreach x in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_abawd fsp_noncit wel_o veitc es o fsp_iebt fsp_cert0 fsp_care fsp_cres fsp_res fsp_tran fsp_bio fsp_apply fsp_ads fsp_outre fsp_unemp fsp_dmeff {
  replace `try1a'_fra_exp_`x' =`try1a'_tot_pred_`x'_chg/`try1a'_total_chg
  replace `try1a'_fra_exp_`x'2 =`try1a'_tot_pred_`x'_chg/`try1a'_tot_pred_o_chg
}


save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_se_fsp_overall`rep', replace

use ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_se_fsp_rep`rep', replace

append using "${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_se_fsp_overall`rep'.dta"

save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_se_fsp_rep`rep', replace



restore
}

preserve

clear all

local list: dir "${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/" files "`try1a'_12lags_se_fsp_rep*.dta"
  local rep=1
  gen rep=.

foreach x in `list' {
  di "append using `x'"
  append using ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`x', generate(temp)
  qui replace rep=`rep' if temp==1
  qui drop temp
  local rep=`rep'+1
}


collapse (firstnm) state_pc  (sd) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg   `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp12 $welf12 $teitc12  pop , by(st_fips)

save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_se_combined_fsp, replace

clear
use ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_estimates_fsp
append using ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_se_combined_fsp, generate(se)

label var `try1a'_fra_exp_w "Fraction Explained Actual- no change Welfare Reform"
label var `try1a'_fra_exp_w2 "Fraction Explained Predicted- no change Welfare Reform"
label var `try1a'_tot_pred_w_chg "Predicted Change- no change Welfare Reform"

label var `try1a'_fra_exp_fsp_all "Fraction Explained Actual- no change All FSP Reform Variables"
label var `try1a'_fra_exp_fsp_all2 "Fraction Explained Predicted- no change All FSP Reform Variables"
label var `try1a'_tot_pred_fsp_all_chg "Predicted Change- no change All FSP Reform Variables"

label var `try1a'_fra_exp_fsp_tc "Fraction Explained Actual- no change FSP Reform Transaction Costs"
label var `try1a'_fra_exp_fsp_tc2 "Fraction Explained Predicted- no change FSP Reform Affecting Transaction Costs"
label var `try1a'_tot_pred_fsp_tc_chg "Predicted Change- no change FSP Reform Affecting Transaction Costs"

label var `try1a'_fra_exp_fsp_elig "Fraction Explained Actual- no change FSP Reform Eligibility"
label var `try1a'_fra_exp_fsp_elig2 "Fraction Explained Predicted- no change FSP Reform Eligibility"
label var `try1a'_tot_pred_fsp_elig_chg "Predicted Change- no change FSP Reform Affecting Eligibility"

label var `try1a'_fra_exp_fsp_eligb "Fraction Explained Actual- no change FSP Reform Broad Based Categorical Eligility"
label var `try1a'_fra_exp_fsp_eligb2 "Fraction Explained Predicted- no change FSP Reform Broad Based Categorical Eligility"
label var `try1a'_tot_pred_fsp_eligb_chg "Predicted Change- no change FSP Reform Broad Based Categorical Eligility"

label var `try1a'_fra_exp_fsp_abawd "Fraction Explained Actual- no change FSP Reform ABAWDs"
label var `try1a'_fra_exp_fsp_abawd2 "Fraction Explained Predicted- no change FSP Reform ABAWDs"
label var `try1a'_tot_pred_fsp_abawd_chg "Predicted Change- no change FSP Reform ABAWDs"

label var `try1a'_fra_exp_fsp_noncit "Fraction Explained Actual - no change FSP Reform Eligility of Non-citizens"
label var `try1a'_fra_exp_fsp_noncit2 "Fraction Explained Predicted- no change FSP Reform Eligility of Non-citizens"
label var `try1a'_tot_pred_fsp_noncit_chg "Predicted Change- no change FSP Reform Eligility of Non-citizens"

foreach set in iebt cert0 care cres res tran bio apply ads outre {
local set1 `set'
  if "`set'"=="care" {
    local set1 carex carexa
  }

  if "`set'"=="cres" {
    local set1 cert0103 res
  }
  
  label var `try1a'_fra_exp_fsp_`set' "Fraction Explained Actual- no change FSP Reform Variables `set1'"
  label var `try1a'_fra_exp_fsp_`set'2 "Fraction Explained Predicted- no change FSP Reform Variables `set1'"
  label var `try1a'_tot_pred_fsp_`set'_chg "Predicted Change- no change FSP Reform Variables `set1' "
  
}

label var `try1a'_fra_exp_wel_o "Fraction Explained Actual- no change only the Welfare Variable (No EITC)"
label var `try1a'_fra_exp_wel_o2 "Fraction Explained Predicted- no change only the Welfare Variable (No EITC)"
label var `try1a'_tot_pred_wel_o_chg "Predicted Change- no change only the Welfare Variable (No EITC)"

label var `try1a'_fra_exp_veitc "Fraction Explained Actual- no change only the VETIC Variable (No Welfare)"
label var `try1a'_fra_exp_veitc2 "Fraction Explained Predicted- no change only the VETIC Variable (No Welfare)"
label var `try1a'_tot_pred_veitc_chg "Predicted Change- no change only the VETIC Variable (No Welfare)"

label var `try1a'_fra_exp_es "Fraction Explained Actual- no change Economic and Seasonal Factors"
label var `try1a'_fra_exp_es2 "Fraction Explained Predicted- no change Economic and Seasonal Factors"
label var `try1a'_tot_pred_es_chg "Predicted Change- no change Economic and Seasonal Factors"

label var `try1a'_fra_exp_o "Fraction Explained Actual -All Observable Factors Changed"
label var `try1a'_fra_exp_o2 "Fraction Explained Predicted -  All Observable Factors Changed"
label var `try1a'_tot_pred_o_chg "Predicted Change if All Observable Factors Changed"

label var `try1a'_total_chg "Total Actual Change"

label define se 0 "Estimate" 1 "Standard Error"
label values se se
label var se "Estimate"



label define over 0 "USA Overall"
label values st_fips over

sort st_fips se
order se
save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_results_fsp/`try1a'_12lags_fsp_artificial, replace
export excel using ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_12lags_fsp_artificial, firstrow(varlabels) replace
restore
}












	

	
	/*	MONTH EFFECTS, Static	*/


 	
	
	foreach try1a of varlist  dlc_ip {
	d `try1a' $`try1a'24

		**********Form Estimates of Effect of Various Factors over Time **************

preserve

cap mkdir ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/
        
*xtscc is fixed effects regression with Driscoll and Kraay (1998) standard errors ;
*Original code used xi command to include dummy variables of st_fips, but since xtscc is fixed effects regression, this is redundant.  I removed the xi code.  Here is original with xi: ;
* xi: xtscc `try1a' d1u d1welfare d1teitc_pay d1iebt d1CertPeriodEarners0103 d1car_1ex d1car_allex  d1res d1tranben d1bio d1online d1ads i.st_fips if ( yrmo>=199001), fe



xtset st_fips period

* Fixed effects regression (I substitute xtreg for xtscc in this part of the code, because xtreg has better postestimation features.  Specifically, I needed to use xtreg's predict features.  The standard errors are wrong, but I will use the xtscc standard errors for the second part of the simulation ;

xtreg `try1a' d1u d1welfare d1teitc_pay d1iebt d1CertPeriodEarners0103 d1car_1ex d1car_allex  d1res d1tranben d1bio d1online d1ads d1outreachdoll d1CatElig d1partban_atleast_nonadult   d1abawd_final dkatrina  $year_dum $dmeff $st_trend if ( yrmo>=199001), fe

predict `try1a'_st_fips_eff if ( yrmo>=199001), u

xtscc `try1a' d1u d1welfare d1teitc_pay d1iebt d1CertPeriodEarners0103 d1car_1ex d1car_allex  d1res d1tranben d1bio d1online d1ads d1outreachdoll d1CatElig d1partban_atleast_nonadult   d1abawd_final dkatrina  $year_dum $dmeff $st_trend if ( yrmo>=199001), fe

*generate 24 lags of the predicted dependent variable



		/******************************************************************************************************************/
      /* To form "Artificial USA", I need to replace the SNAP policy variables to be set to 1 in the first period and   */
		/* zero otherwise.  This ensures that the SNAP policies are switched "on" for the duration of the time period.	   */
		/* Remember we are dealing with differenced outcomes, so switching to 1 in first period and zero otherwise		   */
		/* ensures SNAP variables are "on" for full period																					   */
      /******************************************************************************************************************/

		foreach var in iebt CertPeriodEarners0103 car_1ex car_allex res tranben bio online ads outreachdoll CatElig partban_atleast_nonadult abawd_final {
			forval i=1/1 {
				local mo=$base_mo+`i'
				 if `mo'<=12 local yrmo=$base_yr*100+$base_mo+`i'
				 if `mo'>12 & `mo'<=24 local yrmo=(${base_yr}+1)*100+$base_mo+(`i'-12)
				 if `mo'>24 local yrmo=(${base_yr}+2)*100+$base_mo+(`i'-24) 
				replace d`i'`var'=0
				    if "`var'"=="CertPeriodEarners0103"   {
						replace d`i'`var'=-1*l`i'.`var' if yrmo==`yrmo'
						}	
					else if "`var'"=="car_1ex"{
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.car_1notallexempt==0
						}
					else if "`var'"=="car_allex"{
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.car_allexempt==1
						}							
						else if "`var'"=="iebt"   {
						replace d`i'`var'=(1-l`i'.IssuanceEBT) if yrmo==`yrmo'
						}	
					else if  "`var'"=="bio"   {
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.Biometric==1
						}		
					else if  "`var'"=="partban_atleast_nonadult"   {
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.partban_atleast_nonadult==1
						}							
					else if "`var'"=="res" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.simplify==0
						
					else if "`var'"=="abawd_final" {
						replace d`i'`var'=-1*l`i'.`var' if yrmo==`yrmo' 
						}						
					else if "`var'"=="tranben" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.transition==0
						}			
					else if "`var'"=="online" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.ApplyOnline==0
						}	
					else if "`var'"=="CatElig" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.CashAsstExpCatEligibility==0
						}
					else if "`var'"=="outreachdoll" {
						replace d`i'`var'=(monthdoll_max-l`i'.monthdoll) if yrmo==`yrmo'
						}							
					else {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.`var'==0
						}
				}
			}
		}

**********************************************
  ****Form predicted caseloads based on all  factors

gen `try1a'_other_chgs=_b[_cons]
foreach var in d1u d1welfare d1teitc_pay d1iebt d1CertPeriodEarners0103 d1car_1ex d1car_allex  d1res d1tranben d1bio d1online d1ads d1outreachdoll d1CatElig d1partban_atleast_nonadult   d1abawd_final dkatrina  $year_dum $dmeff $st_trend {
  replace `try1a'_other_chgs=`try1a'_other_chgs+`var'*_b[`var']  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo
xtset st_fips period
while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_o= `try1a'_other_chgs + `try1a'_st_fips_eff if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_o_chg=total(`try1a'_predicted_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)

egen `try1a'_total_chg=total(`try1a') if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)

gen `try1a'_fra_exp_o=`try1a'_tot_pred_o_chg/`try1a'_total_chg

        
**********************************************
  ****Form predicted caseloads if only welfare reform doesn't change





gen `try1a'_welfare_chgs=0
foreach var in d1welfare d1teitc_pay {
  replace `try1a'_welfare_chgs=`try1a'_welfare_chgs+`var'*_b[`var']  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_w=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_w= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_welfare_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_w_chg=total(`try1a'_predicted_w) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing


gen `try1a'_fra_exp_w=`try1a'_tot_pred_w_chg/`try1a'_total_chg


**********************************************
  ****Form predicted caseloads if only fsp reform doesn't change





gen `try1a'_fsp_all_chgs=0
foreach var in  d1iebt d1CertPeriodEarners0103 d1car_1ex d1car_allex  d1res d1tranben d1bio d1online d1ads d1outreachdoll d1CatElig d1partban_atleast_nonadult   d1abawd_final {
  replace `try1a'_fsp_all_chgs=`try1a'_fsp_all_chgs+`var'*_b[`var']  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_all=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_all= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_all_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_fsp_all_chg=total(`try1a'_predicted_fsp_all) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_all=`try1a'_tot_pred_fsp_all_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp associated with transaction costs reform doesn't change





gen `try1a'_fsp_tc_chgs=0
foreach var in  d1CertPeriodEarners0103 d1res d1online d1iebt d1bio {
  replace `try1a'_fsp_tc_chgs=`try1a'_fsp_tc_chgs+`var'*_b[`var']  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_tc=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_tc=`try1a'_other_chgs + `try1a'_st_fips_eff- `try1a'_fsp_tc_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_fsp_tc_chg=total(`try1a'_predicted_fsp_tc) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_tc=`try1a'_tot_pred_fsp_tc_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility doesn't change





gen `try1a'_fsp_elig_chgs=0
foreach var in  d1car_1ex d1car_allex d1tranben d1CatElig d1partban_atleast_nonadult   d1abawd_final {
  replace `try1a'_fsp_elig_chgs=`try1a'_fsp_elig_chgs+`var'*_b[`var']  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_elig=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_elig= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_elig_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_fsp_elig_chg=total(`try1a'_predicted_fsp_elig) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_elig=`try1a'_tot_pred_fsp_elig_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with broad based eligibility doesn't change





gen `try1a'_fsp_eligb_chgs=0
foreach var in  d1CatElig {
  replace `try1a'_fsp_eligb_chgs=`try1a'_fsp_eligb_chgs+`var'*_b[`var']  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_eligb=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_eligb= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_eligb_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_fsp_eligb_chg=total(`try1a'_predicted_fsp_eligb) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_eligb=`try1a'_tot_pred_fsp_eligb_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with abawd doesn't change





gen `try1a'_fsp_abawd_chgs=0
foreach var in  d1abawd_final {
  replace `try1a'_fsp_abawd_chgs=`try1a'_fsp_abawd_chgs+`var'*_b[`var']  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_abawd=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_abawd= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_abawd_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_fsp_abawd_chg=total(`try1a'_predicted_fsp_abawd) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_abawd=`try1a'_tot_pred_fsp_abawd_chg/`try1a'_total_chg


**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility for non-citizens doesn't change





gen `try1a'_fsp_noncit_chgs=0
foreach var in  d1partban_atleast_nonadult   {
  replace `try1a'_fsp_noncit_chgs=`try1a'_fsp_noncit_chgs+`var'*_b[`var']  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_noncit=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_noncit=`try1a'_other_chgs + `try1a'_st_fips_eff- `try1a'_fsp_noncit_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_fsp_noncit_chg=total(`try1a'_predicted_fsp_noncit) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_noncit=`try1a'_tot_pred_fsp_noncit_chg/`try1a'_total_chg


**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility for non-citizens doesn't change
gl addition_var

gl care d1car_1ex d1car_allex
gl cres d1CertPeriodEarners0103 d1res


foreach set in d1iebt d1CertPeriodEarners0103 care cres d1res d1tranben d1bio d1online d1ads d1outreachdoll d1u dmeff {
local set1=substr("`set'",1,5)

gen `try1a'_fsp_`set1'_chgs=0

if "`set'"!="dmeff" &  "`set'"!="care" &  "`set'"!="cres" {

  replace `try1a'_fsp_`set1'_chgs=`try1a'_fsp_`set1'_chgs+`set'*_b[`set']

}

if "`set'"=="dmeff" | "`set'"=="care" |  "`set'"=="cres" {
foreach var in  ${`set'} {
  replace `try1a'_fsp_`set1'_chgs=`try1a'_fsp_`set1'_chgs+`var'*_b[`var']
}
}


local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_`set1'=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_`set1'= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_fsp_`set1'_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_fsp_`set1'_chg=total(`try1a'_predicted_fsp_`set1') if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_`set1'=`try1a'_tot_pred_fsp_`set1'_chg/`try1a'_total_chg

gl addition_var $addition_var `try1a'_fra_exp_fsp_`set1' `try1a'_fra_exp_fsp_`set1'2 `try1a'_tot_pred_fsp_`set1'_chg 

}


**********************************************
  ****Form predicted caseloads if only $welf reform doesn't change





gen `try1a'_wel_o_chgs=0
foreach var in  d1welfare {
  replace `try1a'_wel_o_chgs=`try1a'_wel_o_chgs+`var'*_b[`var']  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_wel_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_wel_o=`try1a'_other_chgs + `try1a'_st_fips_eff- `try1a'_wel_o_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_wel_o_chg=total(`try1a'_predicted_wel_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_wel_o=`try1a'_tot_pred_wel_o_chg/`try1a'_total_chg






**********************************************
  ****Form predicted caseloads if only $teitc reform doesn't change





gen `try1a'_veitc_chgs=0
foreach var in  d1teitc_pay {
  replace `try1a'_veitc_chgs=`try1a'_veitc_chgs+`var'*_b[`var']  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_veitc=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_veitc= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_veitc_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_veitc_chg=total(`try1a'_predicted_veitc) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_veitc=`try1a'_tot_pred_veitc_chg/`try1a'_total_chg








**********************************************
  ****Form predicted caseloads based on economic and seasonal factors




gen `try1a'_econ_seas_chgs=0
foreach var in d1u $dmeff {
  replace `try1a'_econ_seas_chgs=`try1a'_econ_seas_chgs+`var'*_b[`var']  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_es=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo

while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_es= `try1a'_other_chgs + `try1a'_st_fips_eff-`try1a'_econ_seas_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_es_chg=total(`try1a'_predicted_es) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)

gen `try1a'_fra_exp_es=`try1a'_tot_pred_es_chg/`try1a'_total_chg





foreach var in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_abawd fsp_noncit wel_o veitc es o fsp_d1ieb fsp_d1Cer fsp_care fsp_cres fsp_d1res fsp_d1tra fsp_d1bio fsp_d1onl fsp_d1ads fsp_d1out fsp_d1u fsp_dmeff {
  gen `try1a'_fra_exp_`var'2 =`try1a'_tot_pred_`var'_chg/`try1a'_tot_pred_o_chg
}






su `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg  `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg  `try1a'_total_chg, detail 

su `try1a'_fra_exp_w `try1a'_fra_exp_w2 `try1a'_tot_pred_w_chg `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg  `try1a'_total_chg  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'
bysort st_fips: su `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg  `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg   `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'


 corr `try1a'_tot_pred_w_chg  `try1a'_tot_pred_es_chg `try1a'_tot_pred_o_chg `try1a'_total_chg  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'

gen beg_year=$base_yr
gen beg_mo=$base_mo
gen end_year=$end_yr
gen end_mo=$end_mo

      if "`try1a'"=="dlc_hh" {
        gen snap_tot=h_fsp_tot
    }
      if "`try1a'"=="dlc_ip" {
        gen snap_tot=i_fsp_tot
    }

su snap_tot

collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp24 $welf24 $teitc24 $temp24  dkatrina pop (lastnm) tot_snap_policy tot_snap_policy_eligibility tot_snap_policy_transaction top_states bottom_states top_states_transaction bottom_states_transaction top_states_eligibility bottom_states_eligibility (firstnm) snap_tot yrmo tot_snap_policy_init=tot_snap_policy tot_snap_policy_eligibility_init=tot_snap_policy_eligibility tot_snap_policy_transaction_init=tot_snap_policy_transaction  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'   , by(st_fips)

save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_estimates_fsp, replace

collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp24 $welf24 $teitc24 $temp24 dkatrina pop snap_tot yrmo  [aweight=snap_tot]

gen st_fips=-10
replace state_pc="USA"


foreach x in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_abawd fsp_noncit wel_o veitc es o fsp_d1ieb fsp_d1Cer fsp_care fsp_cres fsp_d1res fsp_d1tra fsp_d1bio fsp_d1onl fsp_d1ads fsp_d1out fsp_d1u fsp_dmeff {
  replace `try1a'_fra_exp_`x' =`try1a'_tot_pred_`x'_chg/`try1a'_total_chg
  replace     `try1a'_fra_exp_`x'2 =`try1a'_tot_pred_`x'_chg/`try1a'_tot_pred_o_chg
}


save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_estimates_fsp_overall, replace

local counter=0
foreach var in top_states bottom_states top_states_transaction bottom_states_transaction top_states_eligibility bottom_states_eligibility {
local counter=`counter'-1			
use ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_estimates_fsp, replace
keep if `var'==1
collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg  `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg beg_year beg_mo end_year end_mo d1u d1welfare d1teitc_pay $temp12  dkatrina pop snap_tot yrmo [aweight=snap_tot]

gen st_fips=`counter'
replace state_pc="`var'"



foreach x in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_abawd fsp_noncit wel_o veitc es o fsp_d1ieb fsp_d1Cer fsp_care fsp_cres fsp_d1res fsp_d1tra fsp_d1bio fsp_d1onl fsp_d1ads fsp_d1out fsp_d1u fsp_dmeff {
  replace `try1a'_fra_exp_`x' =`try1a'_tot_pred_`x'_chg/`try1a'_total_chg
  replace `try1a'_fra_exp_`x'2 =`try1a'_tot_pred_`x'_chg/`try1a'_tot_pred_o_chg
}


save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_estimates_fsp_`var', replace
}

		
use ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_estimates_fsp, replace



		
append using "${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_estimates_fsp_overall.dta"
		foreach var in top_states bottom_states top_states_transaction bottom_states_transaction top_states_eligibility bottom_states_eligibility {
			append using "${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_estimates_fsp_`var'.dta"
			}
save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_estimates_fsp, replace

restore


**********Form SE of Effect of Various Factors over Time **************

forv rep=1/$reps {
preserve
xtset st_fips period

*generate state dummy variables (This is done to make it easy to get DK s.e. for the state fixed effect estimates)

tab st_fips if ( yrmo>=199001), gen(dst_fips_)
gl dst_fips
local t
levelsof st_fips if ( yrmo>=199001), local(fips)
foreach x in `fips' {
    local t=`t' + 1
  gl dst_fips $dst_fips dst_fips_`t'
}


qui xtscc `try1a' d1u d1welfare d1teitc_pay d1iebt d1CertPeriodEarners0103 d1car_1ex d1car_allex  d1res d1tranben d1bio d1online d1ads d1outreachdoll d1CatElig d1partban_atleast_nonadult   d1abawd_final dkatrina  $year_dum $dmeff $st_trend  $dst_fips if ( yrmo>=199001), nocons ase
                                                                               
*Generate normally distributed random draw from distribution of estimator

gl random_draws
foreach var in d1u d1welfare d1teitc_pay d1iebt d1CertPeriodEarners0103 d1car_1ex d1car_allex  d1res d1tranben d1bio d1online d1ads d1outreachdoll d1CatElig d1partban_atleast_nonadult   d1abawd_final dkatrina  $year_dum $dmeff $st_trend  $dst_fips  {
  gl random_draws $random_draws `var'_rd
}
drawnorm $random_draws, means(e(b)) cov(e(V)) 

*This generates N*K random draws, where N is number of observations and K is number of regressors
*I dont want the draw to be different for each observation.  I want them constant.
*I achieve this by setting all draws to be equal to the first observation.
*This is clunky but effective and was the fastest way I could come up with.


foreach var in $random_draws {
qui replace `var'=`var'[1]
}



		/******************************************************************************************************************/
      /* To form "Artificial USA", I need to replace the SNAP policy variables to be set to 1 in the first period and   */
		/* zero otherwise.  This ensures that the SNAP policies are switched "on" for the duration of the time period.	   */
		/* Remember we are dealing with differenced outcomes, so switching to 1 in first period and zero otherwise		   */
		/* ensures SNAP variables are "on" for full period																					   */
      /******************************************************************************************************************/

		foreach var in iebt CertPeriodEarners0103 car_1ex car_allex res tranben bio online ads outreachdoll CatElig partban_atleast_nonadult  abawd_final {
			forval i=1/1 {
				local mo=$base_mo+`i'
				 if `mo'<=12 local yrmo=$base_yr*100+$base_mo+`i'
				 if `mo'>12 & `mo'<=24 local yrmo=(${base_yr}+1)*100+$base_mo+(`i'-12)
				 if `mo'>24 local yrmo=(${base_yr}+2)*100+$base_mo+(`i'-24) 
				replace d`i'`var'=0
				    if "`var'"=="CertPeriodEarners0103"   {
						replace d`i'`var'=-1*l`i'.`var' if yrmo==`yrmo'
						}	
					else if "`var'"=="car_1ex"{
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.car_1notallexempt==0
						}	
					else if "`var'"=="car_allex"{
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.car_allexempt==1
						}							
						else if "`var'"=="iebt"   {
						replace d`i'`var'=(1-l`i'.IssuanceEBT) if yrmo==`yrmo'
						}	
					else if  "`var'"=="bio"   {
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.Biometric==1
						}	
					else if  "`var'"=="partban_atleast_nonadult"   {
						replace d`i'`var'=-1 if yrmo==`yrmo' & l`i'.partban_atleast_nonadult==1
						}							
					else if "`var'"=="res" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.simplify==0
						
					else if "`var'"=="abawd_final" {
						replace d`i'`var'=-1*l`i'.`var' if yrmo==`yrmo' 
						}						
					else if "`var'"=="tranben" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.transition==0
						}			
					else if "`var'"=="online" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.ApplyOnline==0
						}	
					else if "`var'"=="CatElig" {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.CashAsstExpCatEligibility==0
						}
					else if "`var'"=="outreachdoll" {
						replace d`i'`var'=(monthdoll_max-l`i'.monthdoll) if yrmo==`yrmo'
						}		
					else {
						replace d`i'`var'=1 if yrmo==`yrmo' & l`i'.`var'==0
						}
				}
			}
		}
**********************************************
  ****Form predicted caseloads based on all  factors

gen `try1a'_other_chgs=0
foreach var in d1u d1welfare d1teitc_pay d1iebt d1CertPeriodEarners0103 d1car_1ex d1car_allex  d1res d1tranben d1bio d1online d1ads d1outreachdoll d1CatElig d1partban_atleast_nonadult   d1abawd_final dkatrina  $year_dum $dmeff $st_trend $dst_fips {
  qui replace `try1a'_other_chgs=`try1a'_other_chgs+`var'*`var'_rd  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo
xtset st_fips period
while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_o= `try1a'_other_chgs if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    
  }
}
}

egen `try1a'_tot_pred_o_chg=total(`try1a'_predicted_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)
egen `try1a'_total_chg=total(`try1a') if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)
gen `try1a'_fra_exp_o=`try1a'_tot_pred_o_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only welfare reform doesn't change





gen `try1a'_welfare_chgs=0
foreach var in d1welfare d1teitc_pay {
  replace `try1a'_welfare_chgs=`try1a'_welfare_chgs+`var'*`var'_rd   
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_w=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_w= `try1a'_other_chgs-`try1a'_welfare_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_w_chg=total(`try1a'_predicted_w) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing



gen `try1a'_fra_exp_w=`try1a'_tot_pred_w_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform doesn't change





gen `try1a'_fsp_all_chgs=0
foreach var in  d1iebt d1CertPeriodEarners0103 d1car_1ex d1car_allex  d1res d1tranben d1bio d1online d1ads d1outreachdoll d1CatElig d1partban_atleast_nonadult   d1abawd_final {
  replace `try1a'_fsp_all_chgs=`try1a'_fsp_all_chgs+`var'*`var'_rd  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_all=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_all= `try1a'_other_chgs-`try1a'_fsp_all_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_fsp_all_chg=total(`try1a'_predicted_fsp_all) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_all=`try1a'_tot_pred_fsp_all_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp associated with transaction costs reform doesn't change





gen `try1a'_fsp_tc_chgs=0
foreach var in  d1CertPeriodEarners0103 d1res d1online d1iebt d1bio {
  replace `try1a'_fsp_tc_chgs=`try1a'_fsp_tc_chgs+`var'*`var'_rd  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_tc=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_tc= `try1a'_other_chgs-`try1a'_fsp_tc_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_fsp_tc_chg=total(`try1a'_predicted_fsp_tc) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_tc=`try1a'_tot_pred_fsp_tc_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility doesn't change





gen `try1a'_fsp_elig_chgs=0
foreach var in  d1car_1ex d1car_allex d1tranben d1CatElig d1partban_atleast_nonadult   d1abawd_final {
  replace `try1a'_fsp_elig_chgs=`try1a'_fsp_elig_chgs+`var'*`var'_rd  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_elig=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_elig= `try1a'_other_chgs-`try1a'_fsp_elig_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_fsp_elig_chg=total(`try1a'_predicted_fsp_elig) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_elig=`try1a'_tot_pred_fsp_elig_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only fsp reform associated with broad based eligibility doesn't change





gen `try1a'_fsp_eligb_chgs=0
foreach var in  d1CatElig {
  replace `try1a'_fsp_eligb_chgs=`try1a'_fsp_eligb_chgs+`var'*`var'_rd  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_eligb=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_eligb= `try1a'_other_chgs-`try1a'_fsp_eligb_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_fsp_eligb_chg=total(`try1a'_predicted_fsp_eligb) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_eligb=`try1a'_tot_pred_fsp_eligb_chg/`try1a'_total_chg


**********************************************
  ****Form predicted caseloads if only fsp reform associated with abawds doesn't change





gen `try1a'_fsp_abawd_chgs=0
foreach var in  d1abawd_final {
  replace `try1a'_fsp_abawd_chgs=`try1a'_fsp_abawd_chgs+`var'*`var'_rd  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_abawd=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_abawd= `try1a'_other_chgs-`try1a'_fsp_abawd_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_fsp_abawd_chg=total(`try1a'_predicted_fsp_abawd) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_abawd=`try1a'_tot_pred_fsp_abawd_chg/`try1a'_total_chg


**********************************************
  ****Form predicted caseloads if only fsp reform associated with eligibility for non-citizens doesn't change





gen `try1a'_fsp_noncit_chgs=0
foreach var in  d1partban_atleast_nonadult   {
  replace `try1a'_fsp_noncit_chgs=`try1a'_fsp_noncit_chgs+`var'*`var'_rd  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_noncit=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_noncit= `try1a'_other_chgs-`try1a'_fsp_noncit_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_fsp_noncit_chg=total(`try1a'_predicted_fsp_noncit) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_noncit=`try1a'_tot_pred_fsp_noncit_chg/`try1a'_total_chg



**********************************************
  ****Form predicted caseloads if only other fsp variable doesn't change


gl care24 d1car_1ex d1car_allex
gl cres24 d1CertPeriodEarners0103 d1res


foreach set in d1iebt d1CertPeriodEarners0103 care cres d1res d1tranben d1bio d1online d1ads d1outreachdoll d1u dmeff {
local set1=substr("`set'",1,5)

gen `try1a'_fsp_`set1'_chgs=0

if "`set'"!="dmeff" &  "`set'"!="care" &  "`set'"!="cres" {

  replace `try1a'_fsp_`set1'_chgs=`try1a'_fsp_`set1'_chgs+`set'*_b[`set']

}

if "`set'"=="dmeff" | "`set'"=="care" |  "`set'"=="cres" {
foreach var in  ${`set'} {
  replace `try1a'_fsp_`set1'_chgs=`try1a'_fsp_`set1'_chgs+`var'*_b[`var']
}
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_fsp_`set1'=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_fsp_`set1'= `try1a'_other_chgs-`try1a'_fsp_`set1'_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}
egen `try1a'_tot_pred_fsp_`set1'_chg=total(`try1a'_predicted_fsp_`set1') if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_fsp_`set1'=`try1a'_tot_pred_fsp_`set1'_chg/`try1a'_total_chg
}

**********************************************
  ****Form predicted caseloads if only $welf reform doesn't change





gen `try1a'_wel_o_chgs=0
foreach var in  d1welfare {
  replace `try1a'_wel_o_chgs=`try1a'_wel_o_chgs+`var'*`var'_rd  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_wel_o=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_wel_o= `try1a'_other_chgs-`try1a'_wel_o_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_wel_o_chg=total(`try1a'_predicted_wel_o) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_wel_o=`try1a'_tot_pred_wel_o_chg/`try1a'_total_chg






**********************************************
  ****Form predicted caseloads if only $teitc reform doesn't change 





gen `try1a'_veitc_chgs=0
foreach var in  d1teitc_pay {
  replace `try1a'_veitc_chgs=`try1a'_veitc_chgs+`var'*`var'_rd  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_veitc=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo



while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_veitc= `try1a'_other_chgs-`try1a'_veitc_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_veitc_chg=total(`try1a'_predicted_veitc) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips) missing

gen `try1a'_fra_exp_veitc=`try1a'_tot_pred_veitc_chg/`try1a'_total_chg





**********************************************
  ****Form predicted caseloads based on economic and seasonal factors


gen `try1a'_econ_seas_chgs=0
foreach var in d1u $dmeff {
  qui replace `try1a'_econ_seas_chgs=`try1a'_econ_seas_chgs+`var'*`var'_rd  
}

local begin_yrmo=$base_yr*100+$base_mo


gen `try1a'_predicted_es=`try1a' if yrmo<=`begin_yrmo'

local end_yrmo=$end_yr*100+$end_mo

gl first_mo=$base_mo+1
local yrmo=$base_yr*100+$base_mo

while `yrmo'<`end_yrmo' {
forv year=$base_yr/$end_yr {
  forv month=1/12 {
    local yrmo=`year'*100+`month'
    qui replace `try1a'_predicted_es= `try1a'_other_chgs-`try1a'_econ_seas_chgs  if yrmo==`yrmo' & yrmo>=`begin_yrmo'
    

  }
}
}

egen `try1a'_tot_pred_es_chg=total(`try1a'_predicted_es) if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo', by(st_fips)

gen `try1a'_fra_exp_es=`try1a'_tot_pred_es_chg/`try1a'_total_chg


foreach var in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_abawd fsp_noncit wel_o veitc es o fsp_d1ieb fsp_d1Cer fsp_care fsp_cres fsp_d1res fsp_d1tra fsp_d1bio fsp_d1onl fsp_d1ads fsp_d1out fsp_d1u fsp_dmeff {
  gen `try1a'_fra_exp_`var'2 =`try1a'_tot_pred_`var'_chg/`try1a'_tot_pred_o_chg
}




gen beg_year=$base_yr
gen beg_mo=$base_mo
gen end_year=$end_yr
gen end_mo=$end_mo


      if "`try1a'"=="dlc_hh" {
        gen snap_tot=h_fsp_tot
    }
      if "`try1a'"=="dlc_ip" {
        gen snap_tot=i_fsp_tot
    }

collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp24 $welf24 $teitc24 $temp24  dkatrina pop (lastnm) tot_snap_policy tot_snap_policy_eligibility tot_snap_policy_transaction top_states bottom_states top_states_transaction bottom_states_transaction top_states_eligibility bottom_states_eligibility (firstnm) snap_tot yrmo tot_snap_policy_init=tot_snap_policy tot_snap_policy_eligibility_init=tot_snap_policy_eligibility tot_snap_policy_transaction_init=tot_snap_policy_transaction  if yrmo>=`begin_yrmo' & yrmo<=`end_yrmo'   , by(st_fips)

save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_se_fsp_rep`rep', replace

collapse (firstnm) state_pc (mean) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg  `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg beg_year beg_mo end_year end_mo $unemp24 $welf24 $teitc24 $temp24 dkatrina pop snap_tot yrmo  [aweight=snap_tot]

gen st_fips=-10
replace state_pc="USA"


foreach x in w fsp_all fsp_tc fsp_elig fsp_eligb fsp_abawd fsp_noncit wel_o veitc es o fsp_d1ieb fsp_d1Cer fsp_care fsp_cres fsp_d1res fsp_d1tra fsp_d1bio fsp_d1onl fsp_d1ads fsp_d1out fsp_d1u fsp_dmeff {
  replace `try1a'_fra_exp_`x' =`try1a'_tot_pred_`x'_chg/`try1a'_total_chg
  replace     `try1a'_fra_exp_`x'2 =`try1a'_tot_pred_`x'_chg/`try1a'_tot_pred_o_chg
}


save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_se_fsp_overall`rep', replace

use ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_se_fsp_rep`rep', replace

append using "${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_se_fsp_overall`rep'.dta"

save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_se_fsp_rep`rep', replace



restore
}

preserve

clear all

local list: dir "${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/" files "`try1a'_static_se_fsp_rep*.dta"
  local rep=1 
  gen rep=.

foreach x in `list' {
  di "append using `x'"
  append using ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`x', generate(temp)
  qui replace rep=`rep' if temp==1
  qui drop temp
  local rep=`rep'+1
}


collapse (firstnm) state_pc  (sd) `try1a'_fra_exp_w `try1a'_fra_exp_w2  `try1a'_tot_pred_w_chg `try1a'_fra_exp_fsp_all* `try1a'_tot_pred_fsp_all_chg  `try1a'_fra_exp_fsp_tc* `try1a'_tot_pred_fsp_tc_chg  `try1a'_fra_exp_fsp_elig `try1a'_fra_exp_fsp_elig2 `try1a'_tot_pred_fsp_elig_chg  `try1a'_fra_exp_fsp_eligb* `try1a'_tot_pred_fsp_eligb_chg  `try1a'_fra_exp_fsp_abawd* `try1a'_tot_pred_fsp_abawd_chg  `try1a'_fra_exp_fsp_noncit* `try1a'_tot_pred_fsp_noncit_chg $addition_var  `try1a'_fra_exp_wel_o* `try1a'_tot_pred_wel_o_chg  `try1a'_fra_exp_veitc* `try1a'_tot_pred_veitc_chg   `try1a'_fra_exp_es* `try1a'_tot_pred_es_chg `try1a'_fra_exp_o* `try1a'_tot_pred_o_chg `try1a'_total_chg beg_year beg_mo end_year end_mo d1u d1welfare d1teitc_pay  pop , by(st_fips)

save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_se_combined_fsp, replace

clear
use ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_estimates_fsp
append using ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_se_combined_fsp, generate(se)

label var `try1a'_fra_exp_w "Fraction Explained Actual- no change Welfare Reform"
label var `try1a'_fra_exp_w2 "Fraction Explained Predicted- no change Welfare Reform"
label var `try1a'_tot_pred_w_chg "Predicted Change- no change Welfare Reform"

label var `try1a'_fra_exp_fsp_all "Fraction Explained Actual- no change All FSP Reform Variables"
label var `try1a'_fra_exp_fsp_all2 "Fraction Explained Predicted- no change All FSP Reform Variables"
label var `try1a'_tot_pred_fsp_all_chg "Predicted Change- no change All FSP Reform Variables"

label var `try1a'_fra_exp_fsp_tc "Fraction Explained Actual- no change FSP Reform Transaction Costs"
label var `try1a'_fra_exp_fsp_tc2 "Fraction Explained Predicted- no change FSP Reform Affecting Transaction Costs"
label var `try1a'_tot_pred_fsp_tc_chg "Predicted Change- no change FSP Reform Affecting Transaction Costs"

label var `try1a'_fra_exp_fsp_elig "Fraction Explained Actual- no change FSP Reform Eligibility"
label var `try1a'_fra_exp_fsp_elig2 "Fraction Explained Predicted- no change FSP Reform Eligibility"
label var `try1a'_tot_pred_fsp_elig_chg "Predicted Change- no change FSP Reform Affecting Eligibility"

label var `try1a'_fra_exp_fsp_eligb "Fraction Explained Actual- no change FSP Reform Broad Based Categorical Eligility"
label var `try1a'_fra_exp_fsp_eligb2 "Fraction Explained Predicted- no change FSP Reform Broad Based Categorical Eligility"
label var `try1a'_tot_pred_fsp_eligb_chg "Predicted Change- no change FSP Reform Broad Based Categorical Eligility"

label var `try1a'_fra_exp_fsp_abawd "Fraction Explained Actual- no change FSP Reform ABAWDs"
label var `try1a'_fra_exp_fsp_abawd2 "Fraction Explained Predicted- no change FSP Reform ABAWDs"
label var `try1a'_tot_pred_fsp_abawd_chg "Predicted Change- no change FSP Reform ABAWDs"

label var `try1a'_fra_exp_fsp_noncit "Fraction Explained Actual - no change FSP Reform Eligility of Non-citizens"
label var `try1a'_fra_exp_fsp_noncit2 "Fraction Explained Predicted- no change FSP Reform Eligility of Non-citizens"
label var `try1a'_tot_pred_fsp_noncit_chg "Predicted Change- no change FSP Reform Eligility of Non-citizens"

foreach set in  d1ieb d1Cer care cres d1res d1tra d1bio d1onl d1ads d1out {
local set1 `set'
  if "`set'"=="care" {
    local set1 carex carexa
  }

  if "`set'"=="cres" {
    local set1 cert0103 res
  }
  
  label var `try1a'_fra_exp_fsp_`set' "Fraction Explained Actual- no change FSP Reform Variables `set1'"
  label var `try1a'_fra_exp_fsp_`set'2 "Fraction Explained Predicted- no change FSP Reform Variables `set1'"
  label var `try1a'_tot_pred_fsp_`set'_chg "Predicted Change- no change FSP Reform Variables `set1' "
  
}

label var `try1a'_fra_exp_wel_o "Fraction Explained Actual- no change only the Welfare Variable (No EITC)"
label var `try1a'_fra_exp_wel_o2 "Fraction Explained Predicted- no change only the Welfare Variable (No EITC)"
label var `try1a'_tot_pred_wel_o_chg "Predicted Change- no change only the Welfare Variable (No EITC)"

label var `try1a'_fra_exp_veitc "Fraction Explained Actual- no change only the VETIC Variable (No Welfare)"
label var `try1a'_fra_exp_veitc2 "Fraction Explained Predicted- no change only the VETIC Variable (No Welfare)"
label var `try1a'_tot_pred_veitc_chg "Predicted Change- no change only the VETIC Variable (No Welfare)"

label var `try1a'_fra_exp_es "Fraction Explained Actual- no change Economic and Seasonal Factors"
label var `try1a'_fra_exp_es2 "Fraction Explained Predicted- no change Economic and Seasonal Factors"
label var `try1a'_tot_pred_es_chg "Predicted Change- no change Economic and Seasonal Factors"

label var `try1a'_fra_exp_o "Fraction Explained Actual -All Observable Factors Changed"
label var `try1a'_fra_exp_o2 "Fraction Explained Predicted -  All Observable Factors Changed"
label var `try1a'_tot_pred_o_chg "Predicted Change if All Observable Factors Changed"

label var `try1a'_total_chg "Total Actual Change"

label define se 0 "Estimate" 1 "Standard Error"
label values se se
label var se "Estimate"



label define over 0 "USA Overall" 
label values st_fips over

sort st_fips se
order se
save ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_results_fsp/`try1a'_static_fsp_artificial, replace
export excel using ${tdate}_results_${base_mo}_${base_yr}_${end_mo}_${end_yr}/`try1a'_static_fsp_artificial, firstrow(varlabels) replace
restore
}	




log close




