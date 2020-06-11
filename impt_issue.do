clear

cd "D:\Rati\Blog\Blog 22"

use CB2019_Georgia_response_30Jan2020.dta

******************* DV   *******************

///// IMPISS1 -- What do you think is the most important issue facing Georgia at the moment?  = Dummmy - economic issues vs all other issues
////  1 - economic issues; 0- rest
gen issue1 = IMPISS1  
recode issue1 (-9 / -1 = .) (1=1)	(2=0)	(3=0)	(4=0)	(5=0) (6=1) (7=0) (8=0)	(9=1) (10=0) (11=1)	(12=0)	(13=0)	(14=1)	(15=0)	(16=1)	(17=0)	(18=1)	(19=0)	(20=0)	(21=0)	(22=0)	(23=0)	(24=0)

gen issue2 = IMPISS2 
recode issue2 (-9 / -1 = .) (1=1)	(2=0)	(3=0)	(4=0)	(5=0) (6=1) (7=0) (8=0) (9=1) (10=0) (11=1)	(12=0)	(13=0)	(14=1)	(15=0)	(16=1)	(17=0)	(18=1)	(19=0)	(20=0)	(21=0)	(22=0)	(23=0)	(24=0)

////  Combination of IMPISS1 and IMPISS2
////  1 - both economic 2 - one economic, second not-economic 3- both non-economic / No answer

gen issue = 3
replace issue = 1 if (IMPISS1 == 1 | IMPISS1 == 6  | IMPISS1 == 9  |  IMPISS1 == 11  |  IMPISS1 == 14   |  IMPISS1 == 16  |  IMPISS1 == 18 ) & (IMPISS2 == 1 | IMPISS2 == 6  | IMPISS2 == 9  |  IMPISS2 == 11   |  IMPISS2 == 14 | IMPISS2 == 16   |  IMPISS2 == 18)

replace issue = 2 if (IMPISS1 == 1  | IMPISS1 == 6  | IMPISS1 == 9  |  IMPISS1 == 11 |  IMPISS1 == 14  |  IMPISS1 == 16   |  IMPISS1 == 18 ) & (IMPISS2 != 1  & IMPISS2 != 6  & IMPISS2 != 9  &  IMPISS2 != 11   &  IMPISS2 != 14 &  IMPISS2 != 16   &  IMPISS2 != 18)

replace issue = 2 if (IMPISS1 != 1  & IMPISS1 != 6  & IMPISS1 != 9  &  IMPISS1 != 11  &  IMPISS1 != 14  &  IMPISS1 != 16   &  IMPISS1 != 18 ) & (IMPISS2 == 1  | IMPISS2 == 6  | IMPISS2 == 9  | IMPISS2 == 11   |  IMPISS2 == 14 |  IMPISS2 == 16   |  IMPISS2 == 18)

*************************************** IV   ****************************************

//// demographic vars: sett, age, gender, education, havejob, minority, internet, 
//// wealth index : ownearship of houshold items (utility)


/// =================================================================================
/// recoding demographic variables 
/// =================================================================================

/// STRATUM
gen sett = STRATUM

/// RESPAGE
gen age = RESPAGE

/// gender
/// recoding Female from 2 to 0
gen gender = RESPSEX
recode gender (2=0) /// female = 0 

//// RESPEDU  => education 
/*  1 = secondary or lower 2 = secodanry technical 3 = higher */
gen education = RESPEDU
recode education (1/4 = 1) (5 = 2) (6/8 = 3) (-9 / -1 = .)

//// EMPLSIT => havejob 
/* 1 = empl 0 = no */
gen havejob = EMPLSIT
recode havejob (5/6 = 1) (1/4 = 0) (7/8 = 0) (-9 / -1 = . )

///  ETHNIC -- Ethnicity of the respondent  => minority
/* 0 = Georgian   1 = Non-Georgian   */
gen minority = ETHNIC
recode minority (4 / 7 = 1)  (3 =0) (2=1) (1=1) (-9 / -1 = .)

///// Internet exposure FRQINTR => internet
/* 1 = Every day 2 = Less often 3 = Never	 */
gen internet = FRQINTR
recode internet (1=1) (2/4 =2) (5/6 = 3) (-9 / -1 = .)


//// Wealth Index => utility
foreach var of varlist OWNCOTV OWNDIGC OWNWASH OWNFRDG OWNAIRC OWNCARS OWNCELL OWNCOMP {
gen `var'r = `var' 
}

foreach var of varlist OWNCOTVr OWNDIGCr OWNWASHr OWNFRDGr OWNAIRCr OWNCARSr OWNCELLr OWNCOMPr {
recode `var' (-9 / -1 = .)
}

gen utility = (OWNCOTVr + OWNDIGCr + OWNWASHr + OWNFRDGr + OWNAIRCr + OWNCARSr + OWNCELLr + OWNCOMPr)


/////  Possible co-variates	

///  FOODDBT -- How often has your household borrowed money to buy food?
gen food_br = FOODDBT
recode food_br (-9 / -1 = .) (1/4 = 1 )  (5=0)


/// Party affiliation
// 1 - GD - goverment 2 - oppositon 3 - NP / RA / DK

gen party_aff = 0
replace party_aff = 2 if PARTYSUPP == 302 | PARTYSUPP == 303 | PARTYSUPP == 304 | PARTYSUPP == 305 | PARTYSUPP == 306 | PARTYSUPP == 307   | PARTYSUPP == 308 | PARTYSUPP == 309  | PARTYSUPP == 999 

replace party_aff = 1 if PARTYSUPP == 301

replace party_aff = 3 if PARTYSUPP == -1 |  PARTYSUPP == -2 |  PARTYSUPP == -5 


//// Weighting


svyset PSU [pweight=INDWT], strata(SUBSTRATUM) fpc(NPSUSS) singleunit(certainty) || ID, fpc(NHHPSU) || _n, fpc(NADHH)

stop

/// ============================================================================================================================
/// base demo model. DV: issue IV: i.sett age gender i.education havejob  minority internet utility. + gov_parent
/// ============================================================================================================================


svy: ologit issue  i.sett age gender i.education havejob  minority i.internet utility 
margins, dydx(*) post
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)

/// Predicted probabilities for demo model
svy: ologit issue  i.sett age gender i.education havejob  minority i.internet utility 
margins, at(sett=(1 2 3 ))
marginsplot


svy: ologit issue  i.sett age gender i.education havejob  minority i.internet utility 
margins, at(gender=(0 1 ))
marginsplot


svy: ologit issue  i.sett age gender i.education havejob  minority i.internet utility 
margins, at(education=(1 2 3 ))
marginsplot


svy: ologit issue  i.sett age gender i.education havejob  minority i.internet utility 
margins, at(internet=(1 2 3 ))
marginsplot


// simple demo model + party_aff
svy: ologit issue  i.sett age gender i.education havejob  minority i.internet utility i.party_aff
margins, dydx(*) post
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)

svy: ologit issue  i.sett age gender i.education havejob  minority i.internet utility i.party_aff
margins, at(party_aff=(1 2 3 ))
marginsplot


// simple demo model + food_br
svy: ologit issue  i.sett age gender i.education havejob  minority i.internet utility food_br
margins, dydx(*) post
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)

svy: ologit issue  i.sett age gender i.education havejob  minority i.internet utility food_br
margins, at(food_br=(0 1 ))
marginsplot