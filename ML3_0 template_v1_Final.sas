%include "/proj/gauser/rshyaamp/variable_list.sas";

%let dir=/proj/gauser/rshyaamp/EDA;
libname dir "&dir.";


****************PNP_train*****************************************;
/*PROC IMPORT OUT=dir.PNP_train DATAFILE="&dir./ML3_0_PNP_in_time_train_999Fix_RapToKbs.csv"*/
/*DBMS=csv REPLACE; RUN; */

proc contents data=dir.PNP_Train; run;

data PNP_Train(keep= &retain_list.); set dir.PNP_Train;run;

proc contents data=PNP_Train; run;

**************************************************************************;
%let Dataset = PNP_Train;		*Dataset Name;     
%let Target = odefind_may21;	*Target name binary;
%let var_date = POFFRDT;		*Date field in the Dataset;
%let Scatter_plot_var = PHWT_MAY21 SMPLWT_PROXY1; *Variables required in scatter plot;
%let stability_plot_var =    
VEH_MAKE_ABBR2_DS_ORIG
TRADED_IN
SUBVENTION
NEW_USED_CD_ORIG;				*Variables required in stability plot;

********************Box-tidwell****************;
%let box_vars=FICO_SCR_NB TOT_REVCR_AVAIL_PC 
AVG_TRADE_AGEMO_NB;				*Variables required in Box-tidwell test;
%let weight1=PHWT_MAY21;		*Weight 1 in model;
%let weight2 =SMPLWT_PROXY1;	*weight 2 in model;


*************************Default volume distribution************************;
data temp1(keep= default &var_date.); set &Dataset.;
if &Target. = 1 then default = 'Default';
else default = 'Non-Default';
run;

ods pdf file="&dir./&Dataset._Defaultvolume distribution.pdf" style = meadow;
title1 "Default over the years";
proc sgplot  data=temp1;
vbar &var_date. / group=default GROUPDISPLAY = CLUSTER;
format &var_date. year.;
run;
title;

title1 "Default over the years_quarters";
proc sgplot  data=temp1;
vbar &var_date. / group=default GROUPDISPLAY = CLUSTER;
format &var_date. yyq10.;
run;
title;
ods pdf close;

**************************************************************************************;





********************Listing variables in dataset based on format**********************;
ODS LISTING CLOSE;
ods graphics / MAXOBS=1000000000;
proc sort data=&Dataset. ; 
by &var_date.;
run;

/*proc surveyselect data=&Dataset.*/
/*out = file1(drop= SamplingWeight SelectionProb )*/
/*method=srs*/
/*seed=10 */
/*sampsize=1000; */
/*strata &var_date.;*/
/*format &var_date. yyq10.;*/
/*run;*/


/*IF we need to run for smaller subset , uncomment the above survey select and comment below */
/*data part;*/
data file1 ;set &Dataset.;
format &var_date. yyq10.;
run;

*Listing variables in dataset based on format;
title;

proc contents data=file1 out=contents noprint; run;
proc sql noprint;
select name into :datevar separated by ' '
  from contents
  where 'date'=fmtinfo(format,'cat')
;
quit;

proc sql noprint;
select name into : numvar separated by " "
from contents
where 'date' <> fmtinfo(format,'cat')
and type = 1;
quit;

proc sql noprint;
select name into : charvar separated by " "
from contents
where  type = 2;
quit;

*Listing all variable names;
proc sql noprint;
select name into : var_name separated by " "
from contents;
quit;

%put char_variables = &charvar.;
%put num_variables = &numvar.;
%put data_variables = &datevar.;
%put vars = &var_name.;


%put count_date = %sysfunc(countw(&datevar.));
%put count_char = %sysfunc(countw(&charvar.));
%put count_num = %sysfunc(countw(&numvar.));
%put count_all = %sysfunc(countw(&var_name.));

%let target_var = &Target.;
%let feature_var = %sysfunc(tranwrd(&var_name., &target_var., %str()));
%let feature_num_var = %sysfunc(tranwrd(&numvar., &target_var., %str()));
%put count_features = %sysfunc(countw(&feature_var.));



%let DS=file1;
**********************************************************************************;

****************proc means for numerical variables*******************************;
ods listing close;
ods excel file = "&dir./&Dataset._numeric_proc_means.xlsx" style = meadow;
proc means data=&DS. n nmiss min p1 p5 p25 mean mode median p75 p95 p99 max STACKODSOUTPUT;
var &numvar.;
run;
ods excel close;
**********************************************************************************;


********************************Null Analysis********************************************;
*code from https://www.lexjansen.com/nesug/nesug11/ds/ds12.pdf;
title;

%macro null_analysis(Data_org);
proc format;
value nm . = '0' other = '1';
value $ch ' ' = '0' other = '1';
run;
ods listing close;
ods output onewayfreqs=tables;
proc freq data=&Data_org.;
tables _all_ / missing;
 format _numeric_ nm.
 _character_ $ch.;
run;
ods output close;
ods listing;
proc print data=tables ;
format _all_;
run;

data report;
length var $32;
do until (last.table);
 set tables;
 by table notsorted;
 array names(*) f_: ;
 select (names(_n_)); 
 when ('0') do; 
 miss = frequency;
 p_miss = percent;
 end;
 when ('1') do; 
 ok = frequency;
 p_ok = percent;
 end;
 end;
end;
miss = coalesce(miss,0); 
ok = coalesce(ok,0);
p_miss = coalesce(p_miss,0);
p_ok = coalesce(p_ok,0);
var = scan(table,-1); 
keep var miss ok p_: ; 
format miss ok comma7. p_: 5.1;
label
miss = 'N_MISSING'
ok = 'N_OK'
p_miss = '%_MISSING'
p_ok = '%_OK'
var = 'VARIABLE'
;
run;



proc export data=report
outfile="&dir./&Dataset._missing_count.csv"
dbms=csv replace;
run;
%mend;

ods listing close;
%null_analysis(&DS.);

**********************************************************************************;

*************************Univariate Descriptive analysis***************************************;
%macro uni_analysis_num;
%let n=%sysfunc(countw(&numvar.));
%do i=1 %to &n.;
%let VAR=%scan(&numvar., &i.);
title "&VAR.";
proc univariate data = &DS.;
var &VAR.;
run;
title;
%end;
%mend;

ods listing close;
ods pdf file="&dir./&Dataset._numeric_univariate.pdf" style = meadow;
%uni_analysis_num;
ods pdf close;

**********************************************************************************;
*************************Univariate Histograms************************************;
**it takes long time to run due to all numeric variables;
%macro uni_analysis_histogram;
%let n=%sysfunc(countw(&numvar.));
%do i=1 %to &n.;
%let VAR=%scan(&numvar., &i.);
title "&VAR.";
proc univariate data = &DS. noprint;
var &VAR.;
HISTOGRAM / NORMAL (COLOR=RED);
run;
title;
%end;
%mend;

/*ods pdf file="&dir./&Dataset._numeric_histogram.pdf" style = meadow;*/
/*%uni_analysis_histogram;*/
/*ods pdf close;*/

**********************************************************************************;
*************************Normality Tests******************************************;
/*
%macro normal(input=, vars=, output=);
ods output TestsForNormality = Normal ;
proc univariate data = &input normal;
var &vars;
run;
ods output close;

data &output;
length status $ 15;
set Normal ( where = (Test = 'Kolmogorov-Smirnov'));
if pValue > 0.05 then Status ="Normal";
else Status = "Non-normal";
drop TestLab Stat pType pSign;
run;

proc export data=&output 
outfile="&dir./&Dataset._normality test.csv"
dbms=csv replace;
run;

%mend;

%normal(input=&DS., vars=&numvar., output=&Dataset.Normality);

**********************************************************************************;
*************************Normality QQ plots***************************************;
**it takes long time to run due to all numeric variables;
*Normality_plots;
%macro QQ_PLOT;
%let n=%sysfunc(countw(&numvar.));
%do i=1 %to &n.;
%let VAR=%scan(&numvar., &i.);
title "Normal Quantile-Quantile Plot for &VAR.";
ods graphics on;
proc univariate data=&DS. noprint;
   qqplot &VAR. / odstitle = title;
run;
title;
%end;
%mend;

/*ods pdf file="&dir./&Dataset._QQ_plot.pdf" style = meadow;*/
/*%QQ_PLOT;*/
/*ods pdf close;*/

**********************************************************************************;

*************************Scatter plots***************************************;

*Scatter_plots;
%macro scatter_plot(variables);
data temp_data;
set &DS.;
rownumber_I = _N_;
run;

%let n=%sysfunc(countw(&variables.));
%do i=1 %to &n.;
%let VAR=%scan(&variables., &i.);
title "Scatter Plot for &VAR.";
proc sgplot data=temp_data;
scatter x = rownumber_I  y = &VAR.;
run;
title;
%end;
%mend scatter_plot();

ods listing close;
ods pdf file="&dir./&Dataset._Scatter_plot.pdf" style = meadow;
%scatter_plot(&Scatter_plot_var.);
ods pdf close;

**********************************************************************************;


title1 "&var_date.";
ods listing close;
proc freq data=file1;
table &var_date./list missing plots=FreqPlot;
format &var_date. year.;
run;
title;

**************************************VIF*****************************************;
*Getting nonnull numeric variables from missing value analysis report;
data report1; set report;
if miss = 0 then OUTPUT;
run;

proc sql noprint;
select var into : var_nonnull separated by " "
from report1
quit;


*VIF analysis;
proc contents data=file1(keep= &var_nonnull.) out=contents1 noprint; run;

proc sql noprint;
select name into : numvar_nonnull separated by " "
from contents1
where 'date' <> fmtinfo(format,'cat')
and type = 1;
quit;

%put &target_var.;
%let feature_num_var_nonnull = %sysfunc(tranwrd(&numvar_nonnull., &target_var., %str()));

ods listing close;
ods pdf file="&dir./&Dataset._VIF.pdf" style = meadow;
ods select parameterestimates;
proc reg data=&DS.;
model &target_var. = &feature_num_var_nonnull. /  tol vif collin;
title 'Multicollinearity Investigation of VIF';
run;
ods pdf close;

**********************************************************************************;


********************stability plot against offer_Date******************************;
%macro stability_PLOT;
data Dataset1;
set &Dataset;
format &var_date. yyq10.;
run;

%let n=%sysfunc(countw(&stability_plot_var.));
%do i=1 %to &n.;
%let VAR=%scan(&stability_plot_var., &i.);
title "Stability Plot for &VAR. vs Offer date";
ods graphics on;
ods select freqplot;
proc freq data=Dataset1 order=freq;
tables &VAR.*&var_date. / plots=freqplot(twoway=stacked scale=grouppct); 
run;
title;
%end;
%mend;
ods listing close;
ods pdf file="&dir./&Dataset._stability_plots.pdf" style = meadow;
%stability_PLOT;
ods pdf close;
**********************************************************************************;

****************************Outlier-Check*********************************************;

/*outliers verion_1 - Few variables name might update wrongly due to having length more than 32*/
title;

%macro oulier_check();
proc means data=&Dataset. noprint nway nolabels min max p25 p75 qrange median mean std ;
var _NUMERIC_;
output out=temp1(drop=_type_ _freq_ _LABEL_ ) min= max= p25= p75= qrange= median= mean= std= /autoname;
run;


proc transpose data=temp1 out=out(drop=_LABEL_);
run;
 
/* Create new variables that contain the statistic name and the */
/* original variable name. */
 
data out1;
set out;
stat=scan(_name_,-1,'_');
varname=transtrn(_name_,scan(_name_,-1,'_'),'');
drop _name_;
run;
 
proc sort data=out1;
by varname stat;
run;

proc transpose data=out1 out=want(drop= _name_);
by varname ;
var col1;
id stat;
run;

data final;
set want;
Lowerlimit=  P25 - (1.5*QRange);
Upperlimit = P75 + (1.5*QRange);
if (max > upperlimit) or (min < Lowerlimit) Then Outlier='True';
else Outlier='False';
run;

proc export data=final 
outfile="&dir./&Dataset._outlier.csv"
dbms=csv replace;
run;
%mend outlier_check;

%oulier_check();
**********************************************************************************;
title;
*******************************BOX-tidwell test***********************************;
*Boxtidwell test;
*/The interaction effect should statistically 
insignificant implying that the independent variable 
is linearly related to the logit of the outcome variable and 
the assumption of linearity is satisfied./*;
%macro BT_test(variables);
data DS_box;
set &DS.;
wt=&weight1.*&weight2.;
run;


data BOX_tidwell_test;  
attrib 
	Var_name length=$50 format=$50. label="Variable_name"
	p_value	length=8 format=best12. label="P_value"
	interact_p_val	length=8 format=best12. label="log_interact_p_val"
	Result length=$50 format=$50. label="Result";
stop;
run;

%let n=%sysfunc(countw(&variables.));
%do i=1 %to &n.;
%let VAR=%scan(&variables., &i.);


proc sql;
select min(&VAR.) into :min_val from DS_box;
quit;

%put &min_val.;



data D70_NEW1(keep=&Target. &VAR. base log_base interaction wt);
set DS_box;
base = &VAR. -&min_val. + 1;
log_base =log(base);
interaction = base*log_base;
run;

ODS Output ParameterEstimates = Tab1;
proc logistic data=D70_NEW1;
weight wt;
model &Target. = base;
run;

proc sql;
select ProbChiSq format 12.10 as P into :base_pval from Tab1 where Variable = "base";
quit;


ODS Output ParameterEstimates = Tab2;
proc logistic data=D70_NEW1;
weight wt;
model &Target. = base log_base interaction;
run;

proc sql;
select ProbChiSq format 12.10 as P into :interact_pval from Tab2 where Variable = "interaction";
quit;


proc sql;
insert into BOX_tidwell_test(Var_name,p_value,interact_p_val)
values("&Var.", &base_pval.,&interact_pval.);
quit;

%end;

Data BOX_tidwell_test1;
Set BOX_tidwell_test;
IF (p_value <= 0.05 and interact_p_val > 0.05) THEN Result = "Linear";
ELSE Result = "Non-Linear";
run;

proc export data=BOX_tidwell_test1
outfile="&dir./&Dataset._BOX_tidwell_test_all.csv"
dbms=csv replace;
run;

%mend BT_test;

%BT_test(&numvar_nonnull.);

**********************************************************************************;

title;



***This can be used to get top N count plot for any variable in the dataset(file1)***;
%macro Top_N_bar(N,var,head);
%let TopN = &N;
%let VarName = &var;
proc freq data=file1 ORDER=FREQ noprint;   /* no print. Create output data set of all counts */
  tables &VarName / out=TopOut;
run;
 
data Other;      /* keep the values for the Top categories. Use "Others" for the smaller categories */
set TopOut;
label topCat = "Top &N. Categories of &var. and 'Other'";
topCat = &VarName;       /* name of original categorical var */
if _n_ > &TopN then
    topCat = "Other";    /* merge smaller categories */
run;

proc freq data=Other ORDER=data;   /* order by data and use WEIGHT statement for counts */
  tables TopCat / plots=FreqPlot(scale=percent);
  weight Count;                    
run;
%mend Top_N_bar;



