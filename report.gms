$STitle		GAMS Code for Reporting

$ifthen.undefined not defined fmt

parameter pnum(*)	Numeraire price index;


sets	
	fmt	Report formats	/
		"abs"		Absolute value 
		"pct"		Percentage change from benchmark,				  
		"dif"		Difference between counterfactual and benchmark (absolute values),
		"pct_ref"	Percentage change from reference scenario,				  
		"dif_ref"	Difference between counterfactual and reference scenarios (absolute values)
		/,

	gdpcat	GDP Categories  /
		expend		Expenditure (C + G + I - (X-M)),
		income		Income (Factor income + taxes),
		valueadded	Sectoral factor earnings plus tax payments, 
		total		Total GDP/,

	gdpitem GDP items /
		"X-M",set.g,set.f, 
		revto,revtfd,revtfi,revtf,revtxs,revtms,revCO2,
		expend,income,valueadded,chksum/,

	incitem	Income items /
		c	Private consumption,
		g	Government expenditure, 
		i	Investment demand, 
		vb	Balance of payment (deficit or surplus), 
		set.f	Factors, 
		sd	Subsistence demand, 
		co2rev	CO2 scarcity rents, 
		taxrev	Other tax revenues/,

	qitem	Quantity items /
		Y	Total output,
		D	Domestic supply, 
		X	Export supply , 
		M	Import demand
		CO2	CO2 emissions
		Int	Emission intensity
		set.f	Factor demand
		/,

	pitem	Price items /
		PD	Domestic supply price
		PY	Composite output price
		PE	Export price
		PM	Import price 
		PA	Armington price
		PCO2	CO2 price
		set.f	Factor price 
		/,

	sitem	Summary report items /
			Welfare (Welfare gross of environmental benefits), 
			Emissions, 
			Output, 
			Intensity, 
			'CO2 price', 
			set.f /;

alias (gdpit,gdpitem); alias(rm, rrm);

sets	
	gp(*)		Sectors in report,
	mapg(g,*)	Mapping of sectors,
	rp(*)		Regions in report, 
	mapr(r,*)	Mapping of regions to report regions;

parameters
        gdp(fmt,gdpcat,*,*,*)	GDP accouting,
        vadd(g,gdpitem,*)	GDP on a value-added basis,
        inc(fmt,*,*,*)		Income decomposition,
        quants(fmt,*,*,*,*)	Quantities (sector) at benchmark prices,
        prices(fmt,*,g,*,*)	Prices (sector),
	summary(fmt,*,*,*,*)	Summary report of key indicators,
	transfers(*,*,*)	Compensating transfers;
$endif.undefined

gp(g) = yes; gp("all") = yes; gp("EITE") = yes; gp("non-EITE") = yes;
mapg(i,"EITE") = no; mapg(i,"non-EITE") = no;
mapg(g,g) = yes; mapg(g,"all") = yes; mapg(i,"EITE")$eite(i) = yes; mapg(i,"non-EITE")$(not eite(i)) = yes;

rp(r) = yes; rp("all") = yes;
mapr(r,r) = yes;  mapr(r,"all") = yes;


$ondotl
pnum(r)     = PD.l("c",r);
pnum("all") = PD.l("c",rnum);


gdp("abs","expend","C",r,%1)		=  RA(r)/pnum(r);
gdp("abs","expend","X-M",r,%1)		= -vb(r)*PD("i",rnum)/pnum(r);
gdp("abs","expend","g",r,%1)		=  PD("g",r)*vom("g",r)/pnum(r);
gdp("abs","expend","i",r,%1)		=  PD("i",r)*vom("i",r)/pnum(r);

gdp("abs","income",gdpitem(f),r,%1)$mf(f) =  (PF(f,r)*evom(f,r))/pnum(r);
gdp("abs","income","res",r,%1)		=  sum(g, PS("res",g,r)*vfm("res",g,r))/pnum(r);
gdp("abs","income","revto",r,%1)	=  sum(g, REV_TO(g,r))/pnum(r);
gdp("abs","income","revtfd",r,%1)	=  sum(i, REV_TFD(i,r))/pnum(r);
gdp("abs","income","revtfi",r,%1)	=  sum(i, REV_TFI(i,r))/pnum(r);
gdp("abs","income","revtf",r,%1)	=  sum((f,g), REV_TF(f,g,r))/pnum(r);
gdp("abs","income","revtxs",r,%1)	= -sum((i,s), REV_TXS(i,r,s))/pnum(r);
gdp("abs","income","revtms",r,%1)	=  sum((i,s), REV_TMS(i,s,r))/pnum(r);
gdp("abs","income","revco2",r,%1)	=  sum(g, PCO2(g,r)*CO2(g,r))/pnum(r);

gdp("abs","income","taxrev",r,%1)	=  gdp("abs","income","revto",r,%1)   
					 + gdp("abs","income","revtfd",r,%1)
					 + gdp("abs","income","revtfi",r,%1)
					 + gdp("abs","income","revtf",r,%1)   
					 + gdp("abs","income","revtxs",r,%1)
					 + gdp("abs","income","revtms",r,%1)
					 + gdp("abs","income","revco2",r,%1);

vadd(i,gdpitem(f),r)		=  DFM(f,i,r)*(PF.L(f,r)*mf(f)+PS.L(f,i,r)$sf(f))/pnum(r);
vadd(g,"revto",r)		=  REV_TO(g,r)/pnum(r);
*.	vadd(g,"revtfd",r)		=  sum(i,REV_TFD(i,g,r))/pnum(r);
*.	vadd(g,"revtfi",r)		=  sum(i,REV_TFI(i,g,r))/pnum(r);
vadd(g,"revtfd",r)		=  sum(i, V_AFM(i,g,r)/sum(gg, V_AFM(i,gg,r))*REV_TFD(i,r))/pnum(r);
vadd(g,"revtfi",r)		=  sum(i, V_AFM(i,g,r)/sum(gg, V_AFM(i,gg,r))*REV_TFI(i,r))/pnum(r);
vadd(g,"revtf",r)		=  sum(f, REV_TF(f,g,r))/pnum(r);
vadd(i,"revtxs",r)		= -sum(s, REV_TXS(i,r,s))/pnum(r);
vadd(i,"revtms",r)		=  sum(s, REV_TMS(i,s,r))/pnum(r);
vadd(g,"revco2",r)		=  PCO2(g,r)*CO2(g,r)/pnum(r);

gdp("abs","valueadded",gdpitem(g),r,%1) = sum(gdpit,vadd(g,gdpit,r));
gdp("abs","total","valueadded",r,%1)    = sum((g,gdpitem),vadd(g,gdpitem,r));

gdp("abs","total","expend",r,%1) =   gdp("abs","expend","c",r,%1)
				   + gdp("abs","expend","i",r,%1)
				   + gdp("abs","expend","g",r,%1)
				   + gdp("abs","expend","x-m",r,%1);
gdp("abs","total","income",r,%1) =   sum(gdpitem(f), gdp("abs","income",gdpitem,r,%1))
				   + gdp("abs","income","revto",r,%1)
				   + gdp("abs","income","revtfd",r,%1)
				   + gdp("abs","income","revtfi",r,%1)
				   + gdp("abs","income","revtf",r,%1)
				   + gdp("abs","income","revtxs",r,%1)
				   + gdp("abs","income","revtms",r,%1)
				   + gdp("abs","income","revco2",r,%1);

gdp("abs","total","chksum",r,%1) =   abs(gdp("abs","total","expend",r,%1) - gdp("abs","total","income",r,%1)) 
				   + abs(gdp("abs","total","expend",r,%1) - gdp("abs","total","valueadded",r,%1)) ;

quants("abs","Y",gp,r,%1)	= sum(g$mapg(g,gp),Y(g,r)*vom(g,r));
quants("abs","D",gp,r,%1)	= sum(g$mapg(g,gp),V_VDM(g,r));
quants("abs","X",gp,r,%1)	= sum(g$mapg(g,gp),V_VXM(g,r));
quants("abs","M",gp,r,%1)	= sum(i$mapg(i,gp),M(i,r)*vim(i,r));
quants("abs","CO2",gp,r,%1)	= sum(g$mapg(g,gp),CO2(g,r));
quants("abs","Int",gp,r,%1)$quants("abs","Y",gp,r,%1)	= 1000*quants("abs","CO2",gp,r,%1)/quants("abs","Y",gp,r,%1);	
quants("abs",f,gp,r,%1)		= sum(g$mapg(g,gp), V_FM(f,g,r));


inc("abs","c",r,%1)	   = RA(r)/PD("c",r);
inc("abs","g",r,%1)	   = PD("g",r)*vom("g",r)/PD("c",r);
inc("abs","i",r,%1)	   = PD("i",r)*vom("i",r)/PD("c",r);
inc("abs","vb",r,%1)	   = -vb(r)*PD("i",rnum)/PD("c",r);
inc("abs","co2rev",r,%1)   = sum(g, PCO2(g,r)*CO2(g,r))/PD("c",r);
inc("abs","taxrev",r,%1)   = (  sum(g,REV_TO(g,r)) +  sum(i, REV_TFD(i,r)) + sum(i, REV_TFI(i,r))
			    + sum((f,g), REV_TF(f,g,r)) -sum((i,s), REV_TXS(i,r,s)) + sum((i,s), REV_TMS(i,s,r)) + sum(g, PCO2(g,r)*CO2(g,r)) )/PD("c",r);	
inc("abs",incitem(f),r,%1) = ((PF(f,r)*evom(f,r))$mf(f) + sum(g, PS("res",g,r)*vfm("res",g,r))$sameas(f,"res"))/PD("c",r);

prices("abs","PD",g,r,%1)	= PD(g,r)/pnum(r);
prices("abs","PE",g,r,%1)	= PE(g,r)/pnum(r);
prices("abs","PY",g,r,%1)	= PY(g,r)/pnum(r);
prices("abs","PM",i,r,%1)	= PM(i,r)/pnum(r);
prices("abs","PA",i,r,%1)	= PA(i,r)/pnum(r);
prices("abs",f,i,r,%1)$mf(f)	= PF(f,r)/pnum(r);
prices("abs",f,i,r,%1)$sf(f)	= PS(f,i,r)/pnum(r);
prices("abs","PCO2",g,r,%1)	= PCO2.l(g,r)/pnum(r);

$if %ds%==rebate_11s_6r_3f	summary("abs",f,"all",r,%1)		   = prices("abs",f,"ROI",r,%1);
$if %ds%==emf36_gtap10		summary("abs",f,"all",r,%1)		   = prices("abs",f,"SER",r,%1);
summary("abs","Welfare","all",r,%1)	   = RA(r)/pnum(r);
summary("abs","Emissions","all",r,%1)	   = quants("abs","CO2","all",r,%1);
summary("abs","Emissions","EITE",r,%1)	   = quants("abs","CO2","EITE",r,%1);	
summary("abs","Emissions","non-EITE",r,%1) = quants("abs","CO2","non-EITE",r,%1);	
summary("abs","Output","EITE",r,%1)	   = quants("abs","Y","EITE",r,%1);
summary("abs","Output","non-EITE",r,%1)	   = quants("abs","Y","non-EITE",r,%1);
summary("abs","Intensity","EITE",r,%1)$summary("abs","Output","EITE",r,%1)	      
					   = 1000*summary("abs","Emissions","EITE",r,%1)/summary("abs","Output","EITE",r,%1);
summary("abs","Intensity","non-EITE",r,%1)$summary("abs","Output","non-EITE",r,%1)  
					   = 1000*summary("abs","Emissions","non-EITE",r,%1)/summary("abs","Output","non-EITE",r,%1);
summary("abs","CO2 price","EITE",r,%1)	   = (sum(i$eite(i), PCO2(i,r)*CO2(i,r))/sum(i$eite(i), CO2(i,r)))/pnum(r);
summary("abs","CO2 price","non-EITE",r,%1) = (sum(i$(not eite(i)), PCO2(i,r)*CO2(i,r))/sum(i$(not eite(i)), CO2(i,r)))/pnum(r);

*	Add global accounting  for welfare and emissions (global cost-effectiveness analysis)
summary("abs","Welfare","all","all",%1)	   = sum(rrm,summary("abs","Welfare","all",rrm,%1));
summary("abs","Emissions","all","all",%1)  = sum(rrm,summary("abs","Emissions","all",rrm,%1));
summary("abs","Leakage","all",r,%1)$((not (sum(g, ttax(g,r)) or sum(g,rtax(g,r)))) and sum(rrm$(sum(g, ttax(g, rrm)) or sum(g, rtax(g,rrm))),co2_bmk(rrm) - sum(g, CO2(g,rrm))))
					   = 100*(sum(g,CO2(g,r)) - co2_bmk(r))/sum(rrm$(sum(g, ttax(g,rrm)) or sum(g,rtax(g,rrm))),co2_bmk(rrm) - sum(g, CO2(g,rrm))); 
summary("abs","Leakage","all","all",%1)    = sum(rrm, summary("abs","Leakage","all",rrm,%1));

*.transfers("abs",r,%1)			  = sum(s, TRNSF(s)*trnv(r,s)*PD("c",rnum))/pnum(r);
transfers("abs",r,%1)			  = sum(s, TRNSF(s)*trnv(r,s)*PD("c",rnum));
transfers("abs","all",%1)		  = sum(r, transfers("abs",r,%1));

$offdotl