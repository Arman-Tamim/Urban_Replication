#delimit;
set more off;
set mem 2000m;
set matsize 1200;
cap log close;

log using cl_new, replace;

quietly do jeanne_lafortune_utoronto_ca_070;
keep if sex==1;
foreach var in age higrade bpl mbpl fbpl statefip{;
rename `var' `var'_male;
};
keep *_male year datanum serial pernum;
sort year datanum serial pernum;
save females_cl, replace;

quietly do jeanne_lafortune_utoronto_ca_070;
keep if sex==2;
rename pernum pernum_male;
rename sploc pernum;
keep year datanum serial pernum pernum_male age higrade bpl mbpl fbpl statefip labforce hrswork2 empstat slwt nativity;
sort year datanum serial pernum;
merge year datanum serial pernum using females_cl, uniqusing;
tab _merge;
drop if _merge==2;
drop _merge;
drop if slwt==0;
save females_cl, replace;

drop if age<25;
gen yob=year-age;
keep if yob>=1901 & yob<=1925;
rename bpld bpl;
rename mbpld mbpl;
rename fbpld fbpl;

drop if bpl>=15000 & bpl<=90000;

gen state=floor(bpl/100);

gen state_male=statefip_male;
replace state_male=floor(bpl_male/100) if bpl_male<15000;

replace higrade_male=. if higrade_male==0;
replace higrade=. if higrade==0;
replace higrade=higrade-3;
replace higrade=0 if higrade<0;
replace higrade_male=higrade_male-3;
replace higrade_male=0 if higrade_male<0;
gen higrade_male_all=higrade_male;
replace higrade_male_all=0 if higrade_male==. & year~=1950;
drop if higrade==.;

gen inlf=labforce==2;
replace hrswork2=. if empstat==1 & hrswork2==0;
gen hours=0 if hrswork2==0;
replace hours=7 if hrswork2==1;
replace hours=22 if hrswork2==2;
replace hours=32 if hrswork2==3;
replace hours=37 if hrswork2==4;
replace hours=40 if hrswork2==5;
replace hours=44 if hrswork2==6;
replace hours=55 if hrswork2==7;
replace hours=65 if hrswork2==8;

compress;


gen yr=year-age_male+14;
rename state_male fips;
sort fips yr;
merge fips yr using comp_llerasmuney, uniqusing;
drop _merge;
rename childcom childcom_male;
rename yr yr_male;
rename fips state_male;

rename state fips;
gen yr=yob+14;
sort fips yr;
merge fips yr using comp_llerasmuney, uniqusing;
drop if _merge==1;
drop _merge;

tab childcom_male, gen(claw_male);
drop claw_male1;
tab childcom, gen(claw_dum);
drop claw_dum1;

gen agesq=age^2;
gen age_malesq=age_male^2;

rename state state_name;
rename fips state;

save females_cl, replace;

xi i.state i.yr;
reg higrade claw_dum* age agesq _I*, cluster(state), [aw=slwt];
testparm claw_dum*;
reg inlf higrade age agesq _I*, cluster(state), [aw=slwt];
estimates store inlf_ols;
reg hours higrade age agesq _I*, cluster(state), [aw=slwt];
estimates store hours_ols;
ivregress 2sls inlf (higrade=claw_dum*) age agesq _I*, cluster(state), [aw=slwt];
estimates store inlf_iv;
ivregress 2sls hours (higrade=claw_dum*) age agesq _I*, cluster(state), [aw=slwt];
estimates store hours_iv;

drop if childcom_male==.;

xi i.state i.yr i.state_male i.yr_male ;

reg higrade claw_dum* claw_male* _I* age agesq, cluster(state), [aw=slwt];
testparm claw_dum* claw_male*;
reg higrade_male claw_dum* claw_male* _I*, cluster(state), [aw=slwt];
testparm claw_dum* claw_male*;

reg inlf higrade higrade_male age agesq _I*, cluster(state), [aw=slwt];
estimates store inlf_ols2;
reg hours higrade higrade_male age agesq _I*, cluster(state), [aw=slwt];
estimates store hours_ols2;
ivregress 2sls inlf (higrade higrade_male=claw_dum* claw_male*) _I* age agesq, cluster(state), [aw=slwt];
estimates store inlf_iv2;
ivregress 2sls hours (higrade higrade_male=claw_dum* claw_male*) _I* age agesq, cluster(state), [aw=slwt];
estimates store hours_iv2;

estout * using tableC5_females.txt, keep(higrade higrade_male) starlevels(* 0.1 ** 0.05 *** 0.01) stats(r2_a N, fmt(%9.3f %9.0g)) cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) style(fixed) replace notype mlabels(, numbers );

estimates clear;

clear;


quietly do jeanne_lafortune_utoronto_ca_070;
keep if sex==2;
foreach var in age higrade bpl mbpl fbpl statefip{;
rename `var' `var'_female;
};
keep *_female year datanum serial pernum sploc;
sort year datanum serial pernum;
save males_cl, replace;

quietly do jeanne_lafortune_utoronto_ca_070;
keep if sex==1;
rename pernum pernum_female;
rename sploc pernum;
keep year datanum serial pernum pernum_female age higrade bpl mbpl fbpl statefip labforce hrswork2 empstat slwt nativity;
sort year datanum serial pernum;
merge year datanum serial pernum using males_cl, uniqusing;
tab _merge;
drop if _merge==2;
drop _merge;
drop if slwt==0;
save males_cl, replace;

drop if age<25;
gen yob=year-age;
keep if yob>=1901 & yob<=1925;

rename bpld bpl;
rename mbpld mbpl;
rename fbpld fbpl;

drop if bpl>=15000 & bpl<=90000;
gen secondgen=(mbpl>=15000 & mbpl<=90000)|(fbpl>=15000 & fbpl<=90000);

gen state=floor(bpl/100);

gen state_female=statefip_female;
replace state_female=floor(bpl_female/100) if bpl_female<15000;

replace higrade_female=. if higrade_female==0;
replace higrade=. if higrade==0;
replace higrade=higrade-3;
replace higrade=0 if higrade<0;
replace higrade_female=higrade_female-3;
replace higrade_female=0 if higrade_female<0;
gen higrade_female_all=higrade_female;
replace higrade_female_all=0 if higrade_female==. & year~=1950;
drop if higrade==.;

gen inlf=labforce==2;
replace hrswork2=. if empstat==1 & hrswork2==0;
gen hours=0 if hrswork2==0;
replace hours=7 if hrswork2==1;
replace hours=22 if hrswork2==2;
replace hours=32 if hrswork2==3;
replace hours=37 if hrswork2==4;
replace hours=40 if hrswork2==5;
replace hours=44 if hrswork2==6;
replace hours=55 if hrswork2==7;
replace hours=65 if hrswork2==8;

compress;

gen yr=year-age_female+14;
rename state_female fips;
sort fips yr;
merge fips yr using comp_llerasmuney, uniqusing;
drop _merge;
rename childcom childcom_female;
rename yr yr_female;
rename fips state_female;

rename state fips;
gen yr=yob+14;
sort fips yr;
merge fips yr using comp_llerasmuney, uniqusing;
drop if _merge==1;
drop _merge;

tab childcom_female, gen(claw_female);
drop claw_female1;
tab childcom, gen(claw_dum);
drop claw_dum1;

gen agesq=age^2;
gen age_femalesq=age_female^2;
rename state state_name;
rename fips state;

save males_cl, replace;

xi i.state i.yr ;
reg higrade claw_dum* _I* age agesq, cluster(state), [aw=slwt];
testparm claw_dum*;
reg inlf higrade age agesq _I*, cluster(state), [aw=slwt];
estimates store inlf_ols;
reg hours higrade age agesq _I*, cluster(state), [aw=slwt];
estimates store hours_ols;
ivregress 2sls inlf (higrade=claw_dum*) _I* age agesq, cluster(state), [aw=slwt];
estimates store inlf_iv;
ivregress 2sls hours (higrade=claw_dum*) _I* age agesq, cluster(state), [aw=slwt];
estimates store hours_iv;

drop if childcom_female==.;

xi i.state i.yr i.state_female i.yr_female ;

reg higrade claw_dum* claw_female* age agesq _I*, cluster(state), [aw=slwt];
testparm claw_dum* claw_female*;
reg higrade_female claw_dum* claw_female* age agesq _I*, cluster(state), [aw=slwt];
testparm claw_dum* claw_female*;

reg inlf higrade higrade_female age agesq _I*, cluster(state), [aw=slwt];
estimates store inlf_ols2;
reg hours higrade higrade_female age agesq _I*, cluster(state), [aw=slwt];
estimates store hours_ols2;
ivregress 2sls inlf (higrade higrade_female=claw_dum* claw_female*) age agesq _I*, cluster(state), [aw=slwt];
estimates store inlf_iv2;
ivregress 2sls hours (higrade higrade_female=claw_dum* claw_female*) age agesq _I*, cluster(state), [aw=slwt];
estimates store hours_iv2;

estout * using tableC5_males.txt, keep(higrade higrade_female) starlevels(* 0.1 ** 0.05 *** 0.01) stats(r2_a N, fmt(%9.3f %9.0g)) cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) style(fixed) replace notype mlabels(, numbers );

estimates clear;

clear;
