*-------------------------------------------------------------------------------------------------------------------------------------------------------
* Policy interactions and electricity generation sector CO2 emissions: A quasi-experimental analysis
*-------------------------------------------------------------------------------------------------------------------------------------------------------
* Codes adapted from:
*        - Sant'anna's DiD Designs Course. https://causal-solutions.com/difference-in-differences-designs/
*        - Asjad Naqvi's GitHub. https://asjadnaqvi.github.io/DiD/docs/01_stata/
*								 https://asjadnaqvi.github.io/DiD/docs/code/06_sdid/
*        - Pietro Santoleri's GitHub. https://github.com/pietrosantoleri/staggered_did

*-------------------------------------------------------------------------------------------------------------------------------------------------------

*-------------------------------------------------------------------------------------------------------------------------------------------------------
* Data load
*-------------------------------------------------------------------------------------------------------------------------------------------------------
* Unbalanced panel data for DID methods 109 (1990-2019 period)
clear
use "https://github.com/Witsonpt/Climate-energy-policies/raw/refs/heads/main/Data%20109_Unbalanced.dta", clear
** Declare the data as a panel data for static TWFE
xtset c_id year

*-------------------------------------------------------------------------------------------------------------------------------------------------------

*-------------------------------------------------------------------------------------------------------------------------------------------------------
* Data manipulation
*-------------------------------------------------------------------------------------------------------------------------------------------------------
* Create variables
******************
** Emissiion per capita (ton/person = (million ton*1000000/million * 1)/population)
g emissionpc=emission*1000000/pop
** Carbon intensity (ton/MWh = million ton/billion kWh)
g carbonintensity=emission/generation
** Energyintensity (kWh / Constant 2017 international $ = billion kWh*1000000000/(Constant 2017 international $/person*population))
g energyintensity=generation*1000000000/(gdp*pop)
** Capacity share of total renewable over capacity (Total renewable capacity / total capacity)
g creshare=recapacity/capacity
** Generation share of total renewable/generation (Total renewable generation / total generation)
g greshare=regeneration/generation 
** Generation ratio of Non-hydroelectric renewables/generation (Non-hydroelectric renewables / total generation)
g nhgreshare=mregeneration/generation 


** Create logs of variables
***************************
g lemissionpc=log(emissionpc)
g lgdp=log(gdp)
g lconsumpc=log(consumpc)
g lpriceg=log(priceg)
g lpricec=log(pricec)

** Create post-treatment dummies from cohort variables (1 if treated by that year, 0 otherwise)
***********************************************************************************************
*** Individual policy post-treatment dummies (4): Auction (posta), Feed-in (postfip), Carbon tax (postct), and Emissions trading system (postets)
*** First treatment period or the treatment cohorts: gets, gct, ga, gfip
g posta = !missing(ga) 
replace posta = 0 if (year < ga)

g postfip = !missing(gfip) 
replace postfip = 0 if (year < gfip)

g postct = !missing(gct) 
replace postct = 0 if (year < gct)

g postets = !missing(gets) 
replace postets = 0 if (year < gets)

** Create policy mix post-treatment dummies
*******************************************
*** Policy-mix post-treatment dummies (11): posta_ct posta_ets postfip_ct postfip_ets post1st post2nd post1st_a post1st_fip post2nd_ct post2nd_ets postmix
g posta_ct = posta*postct
g posta_ets = posta*postets
g postfip_ct = postfip*postct
g postfip_ets = postfip*postets
g post1st = postct*postets
g post2nd = posta*postfip
g post1st_a = post1st*posta
g post1st_fip = post1st*postfip
g post2nd_ct = post2nd*postct
g post2nd_ets = post2nd*postets
g postmix = posta*postfip*postct*postets 

** Create treatment-group policy varibles
*****************************************
*** 1 if the policy was implemented in any year during the whole period and 0 otherwise 
local policies posta postfip postct postets posta_ct posta_ets postfip_ct postfip_ets post1st post2nd post1st_a post1st_fip post2nd_ct post2nd_ets postmix
local labels treat_a treat_fip treat_ct treat_ets treat_a_ct treat_a_ets treat_fip_ct treat_fip_ets treat_1st treat_2nd treat_1st_a treat_1st_fip treat_2nd_ct treat_2nd_ets treat_mix

local n : word count `policies'

forval i = 1/`n' {
    local policy : word `i' of `policies'
    local label : word `i' of `labels'
    
    bysort country: egen `label' = max(`policy')
}

** Label of variables 
*********************
*** Label for income group variable (ic_id)
gen ic = ic_id
label define ic_labels 0 "Not Available" /// Define value labels for ic_id
                       1 "Low-income economies" ///
                       2 "Lower-middle-income economies" ///
                       3 "Upper-middle-income economies" ///
                       4 "High-income economies"
label values ic ic_labels // Apply the value labels to the ic_id variable

*** Label of main variables 
lab var country "Country" 
lab var c_id "Country codes" 
lab var ic "Income classification"
lab var ic_id "Income classification codes" 
lab var region "Region"
lab var region_id "Region codes"
lab var year "Year"
lab var emission "Emission (millions of CO2 metric tons)" 
lab var pop "Population (number of people)" 
lab var emissionpc "Emission per capita (CO2 ton per person)" 
lab var gdp "GDP per capita (constant 2017 international dollars)" 
lab var consumpc "Electric power consumption per capita (kilowatt-hours per person)"
lab var qog "Quality of Government (scaled from 0 to 1)"
lab var rentfo "Fossil fuel rents (% of GDP)"
lab var fd "Financial Development Index (scaled from 0 to 1)"
lab var priceg "Natural gas real price (US$ constant 2017/MMBTU)"
lab var pricec "Coal real price (US$ constant 2017/ton)"
lab var lemissionpc "Log of emission per capita" 
lab var lgdp "Log of GDP per capita" 
lab var lconsumpc "Log of electric power consumption per capita"
lab var lpriceg "Log of natural gas real price"
lab var lpricec "Log of coal real price"
lab var gets "ETS cohorts"
lab var gct "Carbon tax cohorts"
lab var ga "Auctions cohorts"
lab var gfip "Feed-in cohorts"
lab var posta "Auctions"
lab var postfip "Feed-in"
lab var postct "Carbon tax"
lab var postets "ETS"
lab var posta_ct "Auctions x Carbon tax"
lab var posta_ets "Auctions x ETS"
lab var postfip_ct "Feed-in x Carbon tax"
lab var postfip_ets "Feed-in x ETS"
lab var post1st "Carbon tax x ETS"
lab var post2nd "Auctions x Feed-in"
lab var post1st_a "Carbon tax x ETS x Auctions"
lab var post1st_fip "Carbon tax x ETS x Feed-in"
lab var post2nd_ct "Auctions x Feed-in x Carbon tax"
lab var post2nd_ets "Auctions x Feed-in x ETS"
lab var postmix "Auctions x Feed-in x Carbon tax x ETS"
lab var capacity "Total electricity capacity (million kilowatts)"
lab var recapacity "Renewable capacity (million kilowatts)"
lab var mrecapacity "Non-hydro renewable capacity (million kilowatts)"
lab var generation "Total electricity generation (billion kilowatt-hours)"
lab var regeneration "Renewable generation (billion kilowatt-hours)"
lab var mregeneration "Non-hydro renewable generation (billion kilowatt-hours)"
lab var energyintensity "Energy intensity"
lab var carbonintensity "Carbon intensity"
lab var creshare "Renewable capacity share"
lab var greshare "Renewable generation share"
lab var nhgreshare "Non-hydro renewable generation share"

*-------------------------------------------------------------------------------------------------------------------------------------------------------

*-------------------------------------------------------------------------------------------------------------------------------------------------------
* Descriptive Statistics
*-------------------------------------------------------------------------------------------------------------------------------------------------------
* Overview of variable type, stats, number of missing/unique values
*******************************************************************
global varlist emissionpc gdp consumpc qog rentfo fd priceg pricec posta postfip postct postets posta_ct posta_ets postfip_ct postfip_ets post1st post2nd post1st_a post1st_fip post2nd_ct post2nd_ets postmix capacity recapacity mrecapacity generation regeneration mregeneration energyintensity creshare greshare nhgreshare carbonintensity
codebook $varlist
inspect $varlist

* Basic statistics
******************
** Summary of variables 
global varlist emissionpc posta postfip postct postets gdp consumpc qog rentfo fd priceg pricec
asdoc sum $varlist, replace title(Summary Statistics) 

** Correlation of variables 
global varlist emissionpc posta postfip postct postets gdp consumpc qog rentfo fd priceg pricec
asdoc cor $varlist, replace title(Correlation Matrix)

** Summary statistics of CO2 per capita by region, income group and policies 
global varlist region ic treat_a treat_fip treat_ct treat_ets
asdoc, clear 
foreach var of global varlist {
    asdoc tabstat emissionpc, by(`var') statistics(N mean sd min max) columns(statistics) title(Summary statistics of CO2 per capita by `var')
}

** Summarize in a single table tabulations by policy, income-group and region (Tables 2, 7, 8 and 9)
****************************************************************************************************
// Clear any existing estimates
eststo clear

// Define your local varlist
local varlist treat_a treat_fip treat_ct treat_ets treat_a_ct treat_a_ets treat_fip_ct treat_fip_ets treat_1st treat_2nd treat_1st_a treat_1st_fip treat_2nd_ct treat_2nd_ets treat_mix

// Loop over treatment variables
foreach var in treat_a treat_fip treat_ct treat_ets treat_a_ct treat_a_ets treat_fip_ct treat_fip_ets treat_1st treat_2nd treat_1st_a treat_1st_fip treat_2nd_ct treat_2nd_ets treat_mix {

    * Assign corresponding label within the loop
    if "`var'" == "treat_a" label variable `var' "Auctions"
    if "`var'" == "treat_fip" label variable `var' "Feed-in"
    if "`var'" == "treat_ct" label variable `var' "Carbon tax"
    if "`var'" == "treat_ets" label variable `var' "ETS"
    if "`var'" == "treat_a_ct" label variable `var' "Auctions x Carbon tax"
    if "`var'" == "treat_a_ets" label variable `var' "Auctions x ETS"
    if "`var'" == "treat_fip_ct" label variable `var' "Feed-in x Carbon tax"
    if "`var'" == "treat_fip_ets" label variable `var' "Feed-in x ETS"
    if "`var'" == "treat_1st" label variable `var' "Carbon tax x ETS"
    if "`var'" == "treat_2nd" label variable `var' "Auctions x Feed-in"
    if "`var'" == "treat_1st_a" label variable `var' "Carbon tax x ETS x Auctions"
    if "`var'" == "treat_1st_fip" label variable `var' "Carbon tax x ETS x Feed-in"
    if "`var'" == "treat_2nd_ct" label variable `var' "Auctions x Feed-in x Carbon tax"
    if "`var'" == "treat_2nd_ets" label variable `var' "Auctions x Feed-in x ETS"
    if "`var'" == "treat_mix" label variable `var' "Auctions x Feed-in x Carbon tax x ETS"

    * Tabulate by treatment-group
    estpost tab `var'
    eststo `var'_treatment

    * Tabulate by income group
    estpost tab `var' ic
    eststo `var'_ic
    
    * Tabulate by region
    estpost tab `var' region
    eststo `var'_region
}

// Combine the stored estimates into a single table for treatment-group
esttab using summary_table_treatment.rtf, cells(b) ///
    title("Summary of Tabulations by Treatment Group") ///
    nobaselevels nonumbers label ///
    alignment(r) collabels(none) replace


*-------------------------------------------------------------------------------------------------------------------------------------------------------

*-------------------------------------------------------------------------------------------------------------------------------------------------------
* Graphs
*-------------------------------------------------------------------------------------------------------------------------------------------------------

* Distribution of CO2 emissions per capita from electricity generation
**********************************************************************
** Kernel distributions of two variables (Figure 4)
twoway (kdensity emissionpc, lcolor(blue) lwidth(medium)) ///
       (kdensity lemissionpc, lcolor(red) lwidth(medium)), ///
       legend(label(1 "CO2 emissions per capita (tons per person)") label(2 "Log of CO2 emissions per capita")) ///
       title("Kernel density of level and log of CO2 emissions per capita") ///
       ytitle("Density") xtitle("Value")

** Kernel and normal distributions of CO2 emissions per capita (Figure 9A):
kdensity emissionpc, normal  ///  
title("(A) Kernel and normal densities of CO2 emissions per capita") /// 
xtitle("CO2 emissions per capita (tons per person)") /// 
ytitle("Density") /// 
xlabel(0(4)28) ylabel(0(0.1)0.4)

** Overlay graph for Kernel densities of CO2 emissions per capita by income group (Figure 9B)	   
twoway (kdensity emissionpc if ic_id == 1, bwidth(1) lcolor(blue) lpattern(solid)) ///
       (kdensity emissionpc if ic_id == 2, bwidth(1) lcolor(red) lpattern(dash)) ///
       (kdensity emissionpc if ic_id == 3, bwidth(1) lcolor(purple) lpattern(shortdash)) ///
       (kdensity emissionpc if ic_id == 4, bwidth(1) lcolor(orange) lpattern(longdash)), ///
       legend(order(1 "Low-income economies" ///
                    2 "Lower-middle-income economies" ///
                    3 "Upper-middle-income economies" ///
                    4 "High-income economies")) ///
       title("Kernel density of CO2 emissions by income group") ///
	   xtitle("CO2 emissions per capita (tons per person)") ///
	   ytitle("Kernel density") ///
	   xlabel(0(4)28) ylabel(0(0.1)0.4) 

** Overlay graph for Kernel densities of CO2 emissions per capita by region (Figure 9C)
summ emissionpc
twoway (kdensity emissionpc if region_id == 1, bwidth(1) lcolor(blue) lpattern(solid)) ///
       (kdensity emissionpc if region_id == 2, bwidth(1) lcolor(red) lpattern(dash)) ///
       (kdensity emissionpc if region_id == 3, bwidth(1) lcolor(green) lpattern(dot)) ///
       (kdensity emissionpc if region_id == 4, bwidth(1) lcolor(orange) lpattern(longdash)) ///
       (kdensity emissionpc if region_id == 5, bwidth(1) lcolor(purple) lpattern(shortdash)) ///
       (kdensity emissionpc if region_id == 6, bwidth(1) lcolor(cyan) lpattern(dash)) ///
       (kdensity emissionpc if region_id == 7, bwidth(1) lcolor(black) lpattern(solid)), ///
       legend(order(1 "East Asia and Pacific" ///
                    2 "Europe and Central Asia" ///
                    3 "Latin America & the Caribbean" ///
                    4 "Middle East and North Africa" ///
                    5 "North America" ///
                    6 "South Asia" ///
                    7 "Sub-Saharan Africa")) ///
       title("(B) Kernel density of CO2 emissions by region") ///
	   xtitle("CO2 emissions per capita (tons per person)") ///
	   ytitle("Kernel density") /// 
       xlabel(0(4)28) ylabel(0(0.1)0.4)   
	   
** Facet (split) overlay graph for Kernel densities of CO2 emissions per capita by policy (Figure 5)	   
//Panel 1: Policies implemented, dummy variable = 1
twoway (kdensity emissionpc if treat_ct == 1, lcolor(blue) lpattern(solid) ///
        legend(label(1 "Carbon tax = 1"))) ///
       (kdensity emissionpc if treat_ets == 1, lcolor(red) lpattern(solid) ///
        legend(label(2 "ETS = 1"))) ///
       (kdensity emissionpc if treat_a == 1, lcolor(green) lpattern(solid) ///
        legend(label(3 "Auctions = 1"))) ///
       (kdensity emissionpc if treat_fip == 1, lcolor(orange) lpattern(solid) ///
        legend(label(4 "Feed-in policy = 1"))), ///
       title("(A) Policies implemented") ///
       xtitle("CO2 emissions per capita (tons per person)") ///
	   ytitle("Kernel density") ///
	   xlabel(0(4)28) ylabel(0(0.1)0.6) ///
       name(panel1, replace)
//Panel 2: Policies not implemented, dummy variables = 0.
twoway (kdensity emissionpc if treat_ct == 0, lcolor(blue) lpattern(dash) ///
        legend(label(1 "Carbon tax = 0"))) ///
       (kdensity emissionpc if treat_ets == 0, lcolor(red) lpattern(dash) ///
        legend(label(2 "ETS = 0"))) ///
       (kdensity emissionpc if treat_a == 0, lcolor(green) lpattern(dash) ///
        legend(label(3 "Auctions = 0"))) ///
       (kdensity emissionpc if treat_fip == 0, lcolor(orange) lpattern(dash) ///
        legend(label(4 "Feed-in policy = 0"))), ///
       title("(B) Policies not implemented") ///
       xtitle("CO2 emissions per capita (tons per person)") ///
	   ytitle("Kernel density") ///
	   xlabel(0(4)28) ylabel(0(0.1)0.6) ///	   
       name(panel2, replace)
//Combine the two panels
graph combine panel1 panel2, col(2) title("Kernel density of CO2 emissions by policy")

*-------------------------------------------------------------------------------------------------------------------------------------------------------

*-------------------------------------------------------------------------------------------------------------------------------------------------------
*t-test differences of the control variables by policies in 1990 (Table 13)
*-------------------------------------------------------------------------------------------------------------------------------------------------------
// Define the covariates and treatments
local covariates gdp consumpc qog rentfo fd priceg pricec
local treatments treat_a treat_fip treat_ct treat_ets
local year_condition year == 1990

// Loop over each treatment and perform the t-tests
foreach treatment in `treatments' {
    foreach cov in `covariates' {
        ttest `cov' if `year_condition', by(`treatment')
    }
}

*-------------------------------------------------------------------------------------------------------------------------------------------------------

*-------------------------------------------------------------------------------------------------------------------------------------------------------
* Static TWFE 
*-------------------------------------------------------------------------------------------------------------------------------------------------------

* Summary of results (Tables 3 and 14)
*-------------------------------------------------------------------------------------------------------------------------------------------------------
// Clear any existing estimates
eststo clear

// Define local macros for variables
local outcome_variable lemissionpc
local post_policy posta postfip postct postets posta_ct posta_ets postfip_ct postfip_ets post1st post2nd post1st_a post1st_fip post2nd_ct post2nd_ets postmix
local covariates lgdp lconsumpc qog rentfo fd lpriceg lpricec

// Total sample
eststo total_sample: xtreg `outcome_variable' `post_policy' `covariates' i.year, fe cluster(c_id)

// Income subsamples
** Low-income economies
eststo low_income: xtreg `outcome_variable' `post_policy' `covariates' i.year if ic_id == 1, fe cluster(c_id)
** Middle-income economies (aggregation of lower- and upper-middle-income economies)
eststo middle_income: xtreg `outcome_variable' `post_policy' `covariates' i.year if ic_id == 2 | ic_id == 3, fe cluster(c_id)
** High-income economies
eststo high_income: xtreg `outcome_variable' `post_policy' `covariates' i.year if ic_id == 4, fe cluster(c_id)

// Regional subsamples
foreach region in 1 2 3 4 5 6 7 {
    eststo region_`region': xtreg `outcome_variable' `post_policy' `covariates' i.year if region_id == `region', fe cluster(c_id)
}

// Format the outputs:
esttab, ///
    b(%-9.3f 2) se(%-9.3f 2) ///  * Format coefficients and standard errors
    star("*" 0.10 "**" 0.05 "***" 0.01) ///  * Add significance stars
    scalars(r2_o) ///  * Add overall R-square
	nodepvars nomtitles noconstant label compress 
	
// Export to excel format:
esttab using "results_table.csv", replace b(%-9.3f 2) se(%-9.3f 2) star("*" 0.10 "**" 0.05 "***" 0.01) scalars(r2_o) nodepvars nomtitles noconstant label compress

*-------------------------------------------------------------------------------------------------------------------------------------------------------

*-------------------------------------------------------------------------------------------------------------------------------------------------------
* Callaway and Sant'Anna: Figures (7 and 8) and Tables (4, 5, and 17)
*-------------------------------------------------------------------------------------------------------------------------------------------------------
// Install programs
ssc install csdid, replace
ssc install event_plot

// Set for unbalanced panel 
set seed 1
g sample = runiform()<.9

// Estimate C&S DID 
** Use Never treated as control groups (it is the default)
** Outcomes: lemissionpc recapacity regeneration mregeneration, energyintensity, creshare, greshare, nhgreshare, carbonintensity
** For individual policies - first treatment period variables: gets gct ga gfip  
** Covariates: lgdp lconsumpc qog rentfo fd lpriceg lpricec
** Selected covariates: lgdp qog
global outcome_variable lemissionpc 
global gpolicy gfip
global covariates lgdp qog
csdid $outcome_variable $covariates if sample==1, ivar(c_id) ///  name the Panel ID variable
				  time(year)   ///  name the time variable
				  gvar($gpolicy)  ///  name of the first treatment period variable 
				  method(drimp)  ///  estimation method. "drimp" is the default method (only relevant if there are covariates).
				  saverif(rif1) replace // Note that CSDID estimates Asymptotic SE by default. To get Wbootstrap you should "save" the RIFs				  
preserve
	use rif1, clear				
	* Default uses 999 repetitions  
	* To show all types of aggregation (treatment effects across groups, time since treatment, calendar time) use: 
	estat all
	* Make stylized graphs for selected window
    global period 15
	estat event, window (-$period $period) estore(cs) // this produces and stores the estimates at the same time
    event_plot cs, default_look graph_opt(xtitle("Periods since the event") ytitle("Log of CO2 emission per capita") xlabel(-$period(1)$period) ///
	title("(D) Feed-in")) stub_lag(Tp#) stub_lead(Tm#) together

* Honest DID (Table 15 and Figure 10)
**************************************	
// Install programs
ssc install honestdid
honestdid _plugin_check
ssc install coefplot
ssc install ftools, replace
help honestdid

// Produces and store the estimates
csdid_estat event, window(-4 4) estore(csdid)
estat event, revent(-4/-1)
estimates restore csdid
local plotopts xtitle(Mbar) ytitle(95% Robust CI)
honestdid, pre(1/4) post(6/8) mvec(0.5(0.5)2) coefplot `plotopts'
	
*-------------------------------------------------------------------------------------------------------------------------------------------------------

*-------------------------------------------------------------------------------------------------------------------------------------------------------
* Synthetic Difference-in-Differences (Tables 4, 16, 17)
*-------------------------------------------------------------------------------------------------------------------------------------------------------
* https://arxiv.org/abs/2301.11859
*https://docs.iza.org/dp15907.pdf
ssc install sdid
help sdid

* Create a balanced panel data for Synth-DID 103 (2000-2019 period)
*******************************************************************
// Preserve the original unbalanced panel
preserve

// Drop the six unbalancing countries
drop if country == "Congo, Dem. Rep." | country == "Ethiopia" | country == "Iceland" | country == "Paraguay" | country == "Congo, Rep." | country == "Namibia"

// Drop years before 2000
drop if year < 2000

// Set panel structure and check if balanced
xtset c_id year
xtdescribe

// Save the balanced panel to a new file (Optional) 
save "Data 103_Balanced.dta", replace

// Restore the original unbalanced panel
restore

* Estimate SDID 
***************
** Outcome: lemissionpc
** Policy post-treatment: postets
** Selected covariates: lgdp qog
** Methods: sdid (synthetic difference-in-differences, the default) did (standard difference-in-differences), sc (standard synthetic control).
global outcome_variable lemissionpc 
global post_policy postets
global covariates lgdp qog
global method sdid
sdid $outcome_variable country year $post_policy, vce(bootstrap) reps(50) seed(1213) covariates($covariates) method($method) 							

*-------------------------------------------------------------------------------------------------------------------------------------------------------
