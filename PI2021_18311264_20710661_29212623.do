///PI_WiSe2021 Project///
*MN1::18311264 
*MN2::20710661
*MN3::29212623

********************************
*********Exercise 1*************///Inequality Measures
cd "/Users/dejisulola/Documents"
use "Nigeria_HH.dta", clear //Import .dta file for data analysis
describe                                                //EDA for better understanding our dataset
tab hhsize
gen realexpnd = (total_exp_month/povpi)                 //We create a new variable called for the real expenditure, using the food index.
lorenz realexpnd [w=hhsize]                             //Then, we calculate and plot the Lorenz curve, using the Lorenz function
lorenz graph
//We also explore other inequality measure below, using the household size as weights and make a decomposition by regions for in-depth insights.
fastgini realexpnd [w=hhsize] //Gini 0.39180 Theil 0.31951
inequal7 realexpnd [w=hhsize]
ineqdeco realexpnd [w=hhsize] //Atkinson @2 = 0.39846
ineqdeco realexpnd [w=hhsize], by(region) //Theil Index: rural: 0.29405 Urban: 0.32384 Also, within-group + betweeb-group inequality equals the overall theil index 0.31521+0.00430=0.31951
//Plotting of the Lorenz curve using the glcurve function and giving more details for our graph. We plot for the overall Lorenz curve then the decomposed by region version
glcurve realexpnd [w=hhsize], pvar(p) glvar(gl) lorenz nograph
twoway line gl p , sort || line p p, title("Lorenz curve (Abuja)") ytitle("Cumulative incomes share") legend(order(1 "Lorenz Curve (Abuja)" 2 "Line of Perfect equality"))		
//Lorenz curve (Regional comparison)
recode region 1=1 2=2, gen(region1) //first, we recode our regions column to a dummy for more suitable plotting
glcurve realexpnd [w=hhsize] if region1==0 , pvar(p1) glvar(l1) lorenz nograph  //0=rural
glcurve realexpnd [w=hhsize] if region1==1 , pvar(p2) glvar(l2) lorenz nograph  //1=urban	
graph twoway (line l1 p1, sort yaxis(1 2))(line l2 p2, sort yaxis(1 2)) ///
	(function y = x, range(0 1) yaxis(1 2))  ///   
    , aspect(1) xtitle("Cumulative population share") ///    
    title("Lorenz Curve Comparison (Region)") ytitle("Cumulative incomes share") ///    
    legend(order(1 "Region1 (Urban)" 2 "Region0 (Rural)"))

twoway scatter hhsize realexpnd [w=hhsize]    //Exploring the relationship between household size and expenditure

//Then we test our inequality measures to confirm our dataset meets all of the required axiom.
***Properties of inequality measures***

***Replication Invariance*** // we test for Replication invariance by selecting a small sample and multiplying the entries by 8
use "Nigeria_HH.dta", clear
gen realexpnd = (total_exp_month/povpi)
keep if hhID>999820 | hhID<=800440		//only 20 HH
inequal7 realexpnd [w=hhsize] //Gini coefficient 0.17315 and Theil 0.05282
expand 8 
sort hhID
//After making the duuplication, we then measure our new Gini coefficient
inequal7 realexpnd [w=hhsize] //Gini coefficient 0.17315 although the coefficient of variation is different due to correction on degrees of freedom

***Scale Invariance*** // Here we scale up our expenditure by 3 (can be any amount) to see if our inequality remains the same
gen real_expnd3 = realexpnd*3
inequal7 real_expnd3 [w=hhsize] //Gini coefficient 0.17315 and Theil 0.05282
ineqdeco real_expnd3 [w=hhsize]

***Transfer Principle*** //Then, we transfer 300naira in income from the poorest household to the richest and expect an increase in inequality//
sort realexpnd
inequal7 realexpnd [w=hhsize] //Before transfer values - Gini coefficient 0.17315 and Theil 0.05282
replace realexpnd = realexpnd-300 if hhID==999826
replace realexpnd = realexpnd+300 if hhID==800408
inequal7 realexpnd [w=hhsize] //After transfer values from Poor to Rich - Gini coefficient 0.18944 and Theil 0.06863

***Decomposability*** //We'll decompose by region i.e. Urban vs Rural
ineqdeco realexpnd [w=hhsize], by (region) //Within-group + Between-group inequality = 0.06828+ 0.00035 = 0.06863 which sums up to our Theil Index

// we conclude that our dataset meets all the axiom required for an inequality measure.
save "/Users/dejisulola/Documents/Nigeria_HH_Inequaltest.dta", replace

********************************
*********Exercise 2*************//Poverty Measures

use "Nigeria_HH_Abuja.dta", clear    //Import .dta file for data analysis
describe                                                         //EDA for better understanding our dataset
//We calculate our adult equivalence sclae, considering that our dataset is in household level
gen N_Adults = men_cons + women_cons                             //Sum up of all adults per household
gen N_child = boys_cons + girls_cons                             //Sum up of all children per household
gen adultscale = 1 + (N_Adults - 1) * 0.7 + N_child * 0.5        //Then, we compte using the EU defined Adult equivalence scale formular
tab adultscale
//Then, we convert the total monthly expenditure to a monthly level and also standardize with the adult equivalence scale
gen adj_monthlyexp = expyear / (adultscale*12) 
                                
//Share of at risk individuals
sum adj_monthlyexp, detail // To calculate the share of individuals at risk, we first seek for the median of the individual expenditure
return list
scalar median=r(p50)
gen x=1 if adj_monthlyexp<(median*0.60)  //Then, we define the individuals at risk of poverty using the EU definition of <0.60 of median
replace x=0 if adj_monthlyexp>=(median*0.60)
label define x_label 1 "At-risk of poverty" 0 "Non-risk"
label value x x_label
tab x //10.71% is at risk of being poor using monthlyexpenditure
graph pie, over(x) plabel(_all percent) title("Share of individuals At-risk of Poverty") //Data Visualization of share of indiviudals at risk

//Poverty headcount
gen Poverty = 1 if adj_monthlyexp<180500 //
replace Poverty = 0 if adj_monthlyexp>=180500
label define Poverty_label 1 "Poor people" 0 "Non-poor people"
label value Poverty Poverty_label
tab Poverty //6.71% poor
graph pie, over(Poverty) plabel(_all percent) title("Poverty Rate")
tab x Poverty, cell
povdeco adj_monthlyexp, pl(180500) sum //An alternative and simpler function is the povdeco which shows the Headcount and poverty gap combined

//Relative Poor
gen RelPoor=1 if adj_monthlyexp<(median*0.40)
replace RelPoor=0 if adj_monthlyexp>=(median*0.40)
label define Rel_label 1 "Relatively poor" 0 "Non-poor"
label value RelPoor Rel_label
tab RelPoor //0.86% are relatively poor using ttl monthlyexpend

///Poverty incidence for household decomposed by Gender of household head
gen HeadGen = 1 if men_cons>0 //First, create dummy variable for households that have a male household head and hluseholds without a man, is assumed to be headed by a woman
replace HeadGen = 0 if men_cons==0 & women_cons>0  
label define HeadGen_lab 0 "Women" 1 "Men"
label values HeadGen HeadGen_lab
tab HeadGen
povdeco adj_monthlyexp, pl(180500) sum by(HeadGen) //Povdeco estimates poverty incidence and we also summarize using the household head's gender dummy



********************************
*********Exercise 3*************

use "Nigeria_DHS_2018.dta", clear

******* Health *******

des ha40*
tab ha40_01, nol mis

*generate BMI index by dividing the weight ha40* with 100
foreach v of varlist ha40_01-ha40_20 {
    generate bmi`v' = `v'/100
  } 
  
sum bmiha40*
tab bmiha40_01


// Since there is a lot of missing values and 
// we only care about if whether there is at least one household member who have a BMI<18.5  (malnourished), 
// we recode missing values to make it having bmi>18.5 by assigning them value of 20
// this helps us to avoid recode the missing values to having a value of 1

forvalues i = 1(1)9 {
	replace bmiha40_0`i'=20 if bmiha40_0`i'==.
}

forvalues i = 10(1)20 {
	replace bmiha40_`i'=20 if bmiha40_`i'==.
}


*Create a dummy for deprived in health (BMI):
// Assign it to value 1 if there is at least one household member is malnourished (BMI<18.5) and 0 for having BMI>18.5
gen mpi_bmi = 0   

forvalues i = 1(1)9 {
	replace mpi_bmi = 1 if bmiha40_0`i'<18.5 
}

forvalues i = 10(1)20 {
	replace mpi_bmi = 1 if bmiha40_`i'<18.5
}

***** Education ****

des hv108*		
tab hv108_01, mis
tab hv108_01, mis nol

*recode missing values for all 37 variables
forvalues i = 1(1)9 {
	replace hv108_0`i'=. if hv108_0`i'==98
}

forvalues i = 10(1)37 {
	replace hv108_`i'=. if hv108_`i'==98
}

*Create a dummy for deprived in education:
// Assign it to value 1 if there is no HH member has completed 5 years of education (0 otherwise)

gen mpi_educ = 1 

forvalues i = 1(1)9 {
	replace mpi_educ = 0 if hv108_0`i'>=5 & hv108_0`i'!=.
}

forvalues i = 10(1)37 {
	replace mpi_educ = 0 if hv108_`i'>=5 & hv108_`i'!=.
}


****** Living Standards *******

******* DRINKING WATER
des hv201
tab hv201 	
tab hv201, nol mis


*Create dummy for "having no access to safe drinking water":
// Assign it to value 1 when the household has no access to safe drinking water 

gen water = 0
replace water = 1 if inlist(hv201,21,32,42,43,51)

******* FLOOR
des hv213
tab hv213 	
tab hv213, nol mis

*Create dummy for "having dirt floor":
// Assign it to value 1 if there is earth/sand floor (11), or dung (12) 
// Assign value 0 for other floor types

gen floor = 0
replace floor = 1 if inlist(hv213,11,12)


******* SANITATION
des hv205
tab hv205 	
tab hv205, nol mis

*Create dummy for "having no access to adequate sanitation":
// Assign it to value 1 when the household has no access to adequate sanitation

gen sanitation = 0
replace sanitation = 1 if inlist(hv205,23,31,41,42,43)


*Final note about the Variable Properties
// Note that all our variables from above are binary variable, 
// coded 0 if the household is not deprived and 1 if it is deprived


ssc install mpi

*Equal Weight
mpi d1(mpi_bmi) d2(mpi_educ) d3(water floor sanitation), cutoff(0.33)

*Custom Weight
mpi d1(mpi_educ) d2(mpi_bmi) d3(water floor sanitation) w1(0.4) w2(0.3) w3(0.1 0.1 0.1), cutoff(0.33)



******* Regression *******

*Create a new variable to represent the log transformation of the household size:
gen hhsize = hv009
replace hhsize =. if hv009 ==.
gen ln_hhsize = ln(hhsize)

*Create a new variable to represent the age's the household head:
gen headage = hv220
replace headage =. if hv220 ==.

**Create a new variable to represent the Rural
tab hv025, nol
gen regiontype = 1 if hv025==2
replace regiontype=0 if regiontype==.
tab regiontype

**Create a new variable to represent the female household head:
tab hv219, nol
gen headsex = 1 if hv219==2
replace headsex=0 if headsex==.
tab headsex

sum mpi_educ hhsize headage regiontype headsex

** Simple OLS regression
reg mpi_educ ln_hhsize headage regiontype headsex
outreg2 using reducreg, word append   //Outreg function exports our regression results to word document


	
** Plot the scatter plot to show the distribution between Household Size and Age of Household Head 	
twoway scatter hhsize headage, title("Household Size and  Age of Household Head") ytitle("Household Size") xtitle("Age of Household Head")
summarize headage, detail
return list
scalar median=r(p50)
