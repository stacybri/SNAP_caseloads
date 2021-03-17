# Introduction

This repository contains simulation code to replicate the results in "The Downs and Ups of the SNAP Caseload: What Matters?",  In order to replicate our results, please clone our repository and run the sim_launcher.do file in the Simulation_code folder.  For an overview of the methods, please consult 20191022_Simulation_Overview.docx.

Authors:
Stacy Dickert-Conlin  <dickertc@msu.edu>	
Katie Fitzpatrick <kfitzpat@udel.edu>	
Tiehen, Laura - REE-ERS, Washington, DC <laura.tiehen@usda.gov>	
Brian William Stacy <bstacy@worldbank.org>	

Abstract:

Since the 1990s, states received unprecedented flexibility to determine Supplemental Nutrition Assistance Program (SNAP) eligibility and program administration. We find state SNAP policies accounted for nearly half of the 2000-2016 caseload increase. State economic conditions also play an important role in caseload changes, accounting for almost half of the 2007 through 2013 increase. Within distinct periods of our 1990 through 2016 data, policy and the economy make different contributions to caseload changes. Policy simulations indicate that mandating states to maintain their 2000 SNAP policies, prior to the greatest expansion in latitude, would have lowered 2000-2016 caseload growth by 38 percent.  

# Codebook

**Dependent Variables** 	
h_fsp_toth		Monthly HH Caseloads, NOT logged, Deflated	
l_h_fsp_toth	Monthly HH Caseloads, Deflated by #hholds	
i_fsp_totp		Monthly Indiv Caseloads, NOT logged, Deflated	
l_i_fsp_totp	Logged Monthly Ind. Caseloads, deflated by #people	

**Non Policy Covariates**	
unempr		Unemployment Rate	
emp			Total Covered Employment per capita (Just divided total employment by population so didn't adjust for working age pop)	
smeff 		State-Month Dummies	
st_fips		State	
period		time	
temp			absolute deviation from average temperature	
demgov		Democratic Governor	
		
**Policy Variables**
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
abawd_final   Portion of ABAWDs SUBJECT to work requirement.	
	
## Other details

Many of the policy variables have first differenced versions created.  For instance, d1welfare is the lagged first difference of the welfare variable.  d2welfare is the twice lagged first difference.  These first differenced variables were used in the dynamic regressions.
