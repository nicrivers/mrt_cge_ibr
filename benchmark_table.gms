$Title	Generate benchmark_table of Rebating Revenues from Unilateral Emissions Pricing

set	dummy1 /"CO2 intensity", "Trade index", "Fossil fuel net imports", total, EITE/;
$if not set ds $setglobal ds rebate_11s_6r_3f

*	Note to Nic: In the model -- see model.gms -- we only operate with CO2 emissions from fossil fuels
*		     coal (col), gas (gas) and refined oil (oil) while we omit rather negilgible emissions from
*		     crude oil (cru). If we want to fully align statistics in Table 3 of the paper with the model
*		     simulations we need to '$set fe_only yes' below:
$if not set fe_only	$set fe_only yes


set dummy2	Dummy set for ordering of labels /
*	Regions in the dataset 
	USA	United States
	EUR	Europe
	CHN	China and Hong Kong
	IND	India
	OG20	Other G20 (CAN-AUS-NZL-MEX-CHL-KOR-JPN-RUS)
	ROW	Rest of world 

*	Goods in the dataset
*	- Energy carriers
	COL	Coal 
	CRU	Crude oil
	GAS	Natural gas 
	OIL	Refined oil products
	ELE	Electricity and heat 

*	- EITE	Energy-intensive and trade-exposed goods
	CHM	Chemical and pharmacetical and rubber plastics
	I_S	Iron and steel industry
	NFM	Non-ferrous metals
	NMM	Non-metallic minerals
	PPP	Paper-pulp-print
*	- All other goods and services
	ROI	Rest of economy
	/;

$include gtapdata

parameters 
	gdp(r)		GDP accounts (bn USD),
	co2(*,*)	CO2 emissions (Mt),
	co2int(*,r)	CO2 emission intensity (kgCO2 per USD),
	energy(i,r,*,*)	Energy flows (EJ and bnUSD),
	eprd(i,r)	Energy production (EJ),
	eimp(i,r)	Energy imports (EJ),
	eexp(i,r)	Energy exports (EJ);


*	Expenditure-based GDP
gdp(r) = vom("c",r) + vom("g",r) + vom("i",r) - vb(r);


$ifthen.fe_only	%fe_only%==yes
set	fe(i)	Fuels with CO2 emissions from combustion /col, oil, gas/; 
eco2d(i,g,r)$(not fe(i)) = 0;
eco2i(i,g,r)$(not fe(i)) = 0;
$endif.fe_only

*	CO2 emissions from fossil fuel combustion
co2(g,r)         = sum(i, eco2d(i,g,r) +  eco2i(i,g,r));
co2("all",r)	= sum(g, co2(g,r));
display co2;

*	CO2 intensity
co2int(g,r)$vom(g,r)	= co2(g,r)/vom(g,r);
co2int("all",r)		= sum(g, co2(g,r))/sum(g, vom(g,r));
co2int("gdp",r)		= sum(g, co2(g,r))/gdp(r);

display co2int;


*	Energy flows
*	Rescale energy flows from mtoe to EJ 
scalar	mtoetej		EJ per mtoe  /0.041868/;

evt(i,r,s)	= mtoetej*evt(i,r,s);
evd(i,g,r)	= mtoetej*evd(i,g,r);
evi(i,g,r)	= mtoetej*evi(i,g,r);

eimp(i,r) = sum(s$(not sameas(s,r)), evt(i,s,r));
*. Alternatively, we could compute energy imports as:	eimp(i,r) = sum(g, evi(i,g,r));

eexp(i,r) = sum(s$(not sameas(s,r)), evt(i,r,s));

*. Alternatively, we could compute domestic energy production as: eprd(i,r) = sum(g, (evi(i,g,r) + evd(i,g,r)))  - eimp(i,r) + eexp(i,r) ;
eprd(i,r) = sum(g, evd(i,g,r))  + eexp(i,r) ;
display eprd;

energy(i,r,"exports","EJ")	= eexp(i,r);
energy(i,r,"imports","EJ")	= eimp(i,r);
energy(i,r,"production","EJ")	= eprd(i,r);

energy(i,r,"exports","bnUSD")	= sum(s$(not sameas(s,r)), vxmd(i,r,s));
energy(i,r,"imports","bnUSD")	= sum(s$(not sameas(s,r)), vxmd(i,s,r));
energy(i,r,"production","bnUSD")= vom(i,r);

set eite(g)	Emission-intensive and trade-exposed sectors/
$if %ds%==rebate_11s_6r_3f	OIL, CHM, I_S, NFM, NMM, PPP
		/;
parameters	
	trdshr		Import demand shares and export supply shares by EITE good (%),
	trdindex	Trade intensity and trade openeness indicators;

*	Export shares are computed by dividing the value of the export supply by the value of domestic production	
trdshr(r,eite(i),"export")$vom(i,r) = 100*sum(s$(not sameas(s,r)), vxmd(i,r,s))/vom(i,r);
*	Import shares are computed by dividing the value of the import demand by the value of domestic market demand
trdshr(r,eite(i),"import")$sum(g, vifm(i,g,r) + vdfm(i,g,r)) = 100*sum(s$(not sameas(s,r)), vxmd(i,s,r))/sum(g, vifm(i,g,r) + vdfm(i,g,r));	

$ontext
==>	Trade intensity index (TII):
�The intensity of trade with third countries is defined as the ratio between the total value of exports to third countries 
plus the value of imports from third countries and the total market size for the Community (annual turnover plus total imports from third countries). 
		Trade intensity = (imports + exports)/(production+imports)
==>	Trade openess index (TOI):
"The trade openness index is calculated as the ratio of the arithmetic mean of merchandise exports 
(x) and imports (m) to GDP (y)" 
==>	TOI = 0.5 * (x + m)/y;
$offtext

trdindex(r,"TII") = sum((i,s)$(not sameas(r,s)), vxmd(i,r,s) + vxmd(i,s,r))/
				(sum(i, vom(i,r)) + sum((i,s)$(not sameas(s,r)), vxmd(i,s,r)));
trdindex(r,"TOI") = 0.5*sum((i,s)$(not sameas(r,s)), vxmd(i,r,s) + vxmd(i,s,r))/gdp(r);


*	Generate benchmark_table of ERE resubmission
parameter benchmark_table;

set xe(i)	Fossil fuels /cru, col, gas/;

benchmark_table(r,"CO2 intensity","(kg/$)") = co2int("gdp",r);
benchmark_table(r,"Trade index", "total")   = trdindex(r,"TII");
benchmark_table(r, "Trade index","EITE")	   = sum((i,s)$((not sameas(r,s)) and eite(i)), vxmd(i,r,s) + vxmd(i,s,r))/
						(sum(i$eite(i), vom(i,r)) + sum((i,s)$((not sameas(s,r)) and eite(i)), vxmd(i,s,r)));

benchmark_table(r,"Fossil fuel net imports", "(% of GDP)") = 100*sum(i$xe(i), vim(i,r) - vxm(i,r))/ gdp(r);
*.benchmark_table(r,"Fossil fuel net imports", "(% of HEV)") = 100*sum(i$xe(i), vim(i,r) - vxm(i,r))/ vom("c",r);
*.benchmark_table(r,"Fossil fuel net imports (% of domestic fuel consumption)")	
*.					    = 100*sum(i$xe(i), energy(i,r,"imports","EJ") - energy(i,r,"exports","EJ"))/
*.						 sum(g, sum(i$xe(i), (evi(i,g,r) + evd(i,g,r))));

*.benchmark_table(r,"CO2 emissions", "total (Mt)")	 = co2("all",r);
*.benchmark_table(r,"CO2 emissions", "EITE (% of total)") = 100*sum((i,g)$eite(g), eco2d(i,g,r) +  eco2i(i,g,r))/co2("all",r);

benchmark_table(r,"EITE", "CO2 emissions (% of total)")	= 100*sum((i,g)$eite(g), eco2d(i,g,r) +  eco2i(i,g,r))/co2("all",r);
benchmark_table(r,"EITE", "Output (% of total)")		= 100*(sum(g$eite(g), vom(g,r))/sum(g.local, vom(g,r)));
*.benchmark_table(r,"EITE", "Output (% of GDP)")		= 100*(sum(g$eite(g), vom(g,r))/gdp(r));
*.benchmark_table(r,"EITE", "Value-added (% of total)")	= 100*(sum((f,g)$eite(g), vfm(f,g,r))/sum((f,g.local), vfm(f,g,r)));

option dispwidth = 23; 
option benchmark_table:2:1:2;
display benchmark_table;


execute_unload 'benchmark_table.gdx', benchmark_table;
execute 'gdxxrw i=benchmark_table.gdx o=benchmark_table.xlsx par=benchmark_table rng=benchmark_table!a2 cdim=0'

