$STitle	Alternative rebate schemes for CO2 revenues

$ontext
Rebate schemes may help to maintain competitiveness/prevent leakage and may help
to achieve deeper emission reductions than through carbon pricing "stand-alone" 
(i.e. carbon pricing with lump-sum rebating of revenues).

We investigate the implications of alternative rebating schemes as compared to
the reference case of lump-sum rebating in a stylized policy setting where
a region ($set region ...) implements an emissions price ($set co2price ..) 
which is below the social cost of carbon ($set scc ...).

In our analysis we investigate the following rebating schemes
- obr			Output-based rebate	
- abr			Abatement-based rebate
- ibr (alias IBOR)	Intensity-based rebate
- bcr (alias IBER)	"British-Columbia"-type rebate

We report changes in key indicators as compared to a benchmark situation
where no abatement policy is in place (but we could also shift to reporting
changes directly vis-�-vis the reference scenario with lump-sum recycling.
$offtext

*	Select the unilaterally abating region
$if not set region	$set region usa
set	rcalc(r)	Region with emissions abatement policy /%region%/;


*	Select the CO2 price for the abating region (in USD per ton of CO2)
$if not set co2p	$set co2p 0

*	Select the social cost of carbon
$if not set scc		$set scc 0

*	Optional flag to assess a short-horizon variant where we treat capital
*	in EITE sectors as sector specific (for the case: $set ssk yes)
*	By default we assume that capital is mobile across all sectors 
*	(N.B.: Except for capital in fossil fuel primary sectors where we treat
*	 capital inputs as sector-specific throughout to accommodate the calibraton
*	 to exogenous fossil fuel supply elasticities)
$set ssk no
$if not set ssk	$set ssk no

if(%ssk%,
	PS.l("res",g,r)$eite(g)  = PF.l("cap",r);
	evom("cap",r)		 = evom("cap",r) - sum(g$eite(g),  V_FM.l("cap",g,r));
	ks0("res",g,r)$eite(g)	 = V_FM.l("cap",g,r);
	vfm("res",g,r)$eite(g)   = vfm("cap",g,r);
	rtf("res",g,r)$eite(g)   = rtf("cap",g,r);
	rtf0("res",g,r)$eite(g)  = rtf0("cap",g,r);
	vfm("cap",g,r)$eite(g)   = 0;
	rtf("cap",g,r)$eite(g)	 = 0;
	rtf0("cap",g,r)$eite(g)	 = 0; 
);

gtap.iterlim = 0;
$include gtap.gen
solve gtap using mcp;
abort$(abs(gtap.objval) > objtol) "Benchmark replication fails.", gtap.objval;
$batinclude report '"bmk"'

*	Check:	Upper bounds for rebate variants IBOR and IBER set at the
*		level of the BMK scenario
iup(g,r)$(vom(g,r) and eite(g)) = CO2.l(g,r)/(vom(g,r)*Y.l(g,r));



*============================== Policy counterfactuals =========================================
rtax(g,r) = no; ttax(g,r) = 0;

gtap.iterlim = 10000;

loop(r$rcalc(r),
	rtax(g,r)$co2q(g,r)   = yes;
	co2limr(r)	      = sum(g, CO2.l(g,r));
	ptgt(r)		      = %co2p%;
	PCO2.l(g,r)$co2q(g,r) = ptgt(r);
	PCO2R.l(r)	      = ptgt(r);
	PHI.lo(r) = 0; PHI.l(r) = 1; PHI.UP(r) = +inf;
    );	

*	Run the climate policy counterfactuals
file ktitle; ktitle.lw=0;


*	Solve the reference scenario with lump-sum recycling
put_utility ktitle, 'title' /'Solving scenario ','LSR';

if (%co2p%,
$include gtap.gen
solve gtap using mcp;
);
$batinclude report '"lsr"'

set	sc	Alternative revenue recycling scenarios /
		lsr	Lump-sum rebate
		obr	Output-based rebate	
		abr	Abatement-based rebate
		ibr	Intensity-based rebate (alias IBOR)
		bcr 	British-Columbia-type rebate (alias IBER)
		/;

set	runsc(sc)	Selection of scenarios to be processed in the loop;

*	Scenario 'lsr' is already solved
runsc(sc)$(not sameas(sc,"lsr")) = yes;


loop(sc$runsc(sc),

put_utility ktitle, 'title' /'Solving scenario ',sc.tl;

*	Before we assign specific scenario settings in the loop, we must reset all specific policy settings
PSI.fx(g,r) = 0; RHO.fx(g,r) = 0; PHI.l(r)   = 1; obr(r) = no; abr(r) = no; ibr(r) = no; bcr(r) = no;

*	Re-assign scenario settings pending on the specific scenario
if (sameas(sc,"obr"),
	loop(r$rcalc(r), obr(r)  = yes;  PSI.LO(g,r)$eite(g) = 0; PSI.UP(g,r)$eite(g) = +0.99;);
);

if (sameas(sc,"abr"),
	loop(r$rcalc(r), abr(r) = yes; RHO.LO(g,r)$eite(g) = -0.99; RHO.UP(g,r)$eite(g) = +inf;); 
);

if (sameas(sc,"ibr"),
	loop(r$rcalc(r), ibr(r) = yes; RHO.LO(g,r)$eite(g) = -0.99; RHO.UP(g,r)$eite(g) = +inf; PSI.LO(g,r)$eite(g) = 0; PSI.UP(g,r)$eite(g) = +0.99;);
);

if (sameas(sc,"bcr"),
	loop(r$rcalc(r), bcr(r) = yes; RHO.LO(g,r)$eite(g) = -0.99; RHO.UP(g,r)$eite(g) = +inf; PSI.lo(g,r)$eite(g) = 0; PSI.up(g,r)$eite(g) = +0.99;); 
);


if (%co2p%,
$include gtap.gen
solve gtap using mcp; 
abort$(abs(gtap.objval) gt objtol)"Model does not solve.", gtap.objval
);
$batinclude report sc

);

*	Finalize reporting

*	Report differences and percentage changes as compared to benchmark without climate policy 
gdp("pct",gdpcat,gdpitem,r,sc)$gdp("abs",gdpcat,gdpitem,r,"bmk")= 100*(gdp("abs",gdpcat,gdpitem,r,sc)/gdp("abs",gdpcat,gdpitem,r,"bmk") - 1);
gdp("dif",gdpcat,gdpitem,r,sc)					= gdp("abs",gdpcat,gdpitem,r,sc) - gdp("abs",gdpcat,gdpitem,r,"bmk");

quants("pct",qitem,gp,r,sc)$quants("abs",qitem,gp,r,"bmk")	= 100*(quants("abs",qitem,gp,r,sc)/quants("abs",qitem,gp,r,"bmk") - 1);
quants("dif",qitem,gp,r,sc)					= quants("abs",qitem,gp,r,sc) - quants("abs",qitem,gp,r,"bmk");

prices("pct",pitem,g,r,sc)$prices("abs",pitem,g,r,"bmk")	= 100*(prices("abs",pitem,g,r,sc)/prices("abs",pitem,g,r,"bmk") - 1);
prices("dif",pitem,g,r,sc)					= prices("abs",pitem,g,r,sc) - prices("abs",pitem,g,r,"bmk");

summary("pct",sitem,gp,r,sc)$summary("abs",sitem,gp,r,"bmk")	= 100*(summary("abs",sitem,gp,r,sc)/summary("abs",sitem,gp,r,"bmk") - 1);
summary("pct","welfare","all","all",sc)$summary("abs","welfare","all","all","bmk")	
								= 100*(summary("abs","welfare","all","all",sc)/summary("abs","welfare","all","all","bmk") - 1);
summary("pct","emissions","all","all",sc)$summary("abs","emissions","all","all","bmk")	
								= 100*(summary("abs","emissions","all","all",sc)/summary("abs","emissions","all","all","bmk") - 1);
summary("dif",sitem,gp,r,sc)					= summary("abs",sitem,gp,r,sc) - summary("abs",sitem,gp,r,"bmk");

*	Report differences and percentage changes as compared to the reference policy with lump-sum recycling 'lsr'
$ontext
gdp("pct_lsr",gdpcat,gdpitem,r,sc)$gdp("abs",gdpcat,gdpitem,r,"lsr") = 100*(gdp("abs",gdpcat,gdpitem,r,sc)/gdp("abs",gdpcat,gdpitem,r,"lsr") - 1);
gdp("dif_lsr",gdpcat,gdpitem,r,sc)					= gdp("abs",gdpcat,gdpitem,r,sc) - gdp("abs",gdpcat,gdpitem,r,"lsr");

quants("pct_lsr",qitem,gp,r,sc)$quants("abs",qitem,gp,r,"lsr")	= 100*(quants("abs",qitem,gp,r,sc)/quants("abs",qitem,gp,r,"lsr") - 1);
quants("dif_lsr",qitem,gp,r,sc)					= quants("abs",qitem,gp,r,sc) - quants("abs",qitem,gp,r,"lsr");

prices("pct_lsr",pitem,g,r,sc)$prices("abs",pitem,g,r,"lsr")		= 100*(prices("abs",pitem,g,r,sc)/prices("abs",pitem,g,r,"lsr") - 1);
prices("dif_lsr",pitem,g,r,sc)						= prices("abs",pitem,g,r,sc) - prices("abs",pitem,g,r,"lsr");


summary("pct_lsr",sitem,gp,r,sc)$summary("abs",sitem,gp,r,"lsr")	= 100*(summary("abs",sitem,gp,r,sc)/summary("abs",sitem,gp,r,"lsr") - 1);
summary("dif_lsr",sitem,gp,r,sc)					= summary("abs",sitem,gp,r,sc) - summary("abs",sitem,gp,r,"lsr");
$offtext

option summary:3:3:1;
display summary;


*	Here we specify the report arrays that we use for the PIVOT-table/chart report
parameter macro, sector;


sector("Output",g,r,sc)                = quants("pct","Y",g,r,sc);
sector("Price",g,r,sc)                 = prices("pct","PY",g,r,sc);
sector("Emissions",g,r,sc)             = quants("pct","CO2",g,r,sc);
sector("Intensity",g,r,sc)             = quants("pct","Int",g,r,sc);
sector("Emissions","EITE",r,sc)        = quants("pct","CO2","EITE",r,sc);
sector("Emissions","non-EITE",r,sc)    = quants("pct","CO2","non-EITE",r,sc);
sector("Output","EITE",r,sc)           = quants("pct","Y","EITE",r,sc);
sector("Output","non-EITE",r,sc)       = quants("pct","Y","non-EITE",r,sc);
sector("Intensity","EITE",r,sc)        = quants("pct","Int","EITE",r,sc);
sector("Intensity","non-EITE",r,sc)    = quants("pct","Int","non-EITE",r,sc);
sector("CO2 price($)",g,r,sc)          = prices("abs","PCO2",g,r,sc);
sector("Int-IUP(abs)",g,r,sc)          = 1000*iup(g,r);
sector("Int-mdl(abs)",g,r,sc)	       = quants("abs","Int",g,r,sc);
sector("CO2 price($)","EITE",r,sc)     = summary("abs","CO2 price","EITE",r,sc);
sector("CO2 price($)","non-EITE",r,sc) = prices("abs","PCO2","roi",r,sc);
sector("CO2 price($)","c",r,sc)        = prices("abs","PCO2","c",r,sc);

macro(sitem,gp,r,sc)	    	    = summary("pct",sitem,gp,r,sc);
macro(sitem,gp,r,sc)$(not sameas(sitem,"CO2 price")) = summary("pct",sitem,gp,r,sc);
macro("welfare","all","all",sc)     = summary("pct","welfare","all","all",sc);
macro("emissions","all","all",sc)   = summary("pct","emissions","all","all",sc);
macro("CO2 price",gp,r,sc)	    = summary("abs","CO2 price",gp,r,sc);
macro("Leakage","all",r,sc)	    = summary("abs","Leakage","all",r,sc);
macro("Leakage","all","all",sc)	    = summary("abs","Leakage","all","all",sc);
macro("Employment","EITE",r,sc)	    = quants("pct","lab","EITE",r,sc);
macro("Employment","non-EITE",r,sc) = quants("pct","lab","non-EITE",r,sc);
macro("eb(bn$)","all",r,sc)$rcalc(r)    = %scc%*(summary("abs","Emissions","all",r,"BMK") - summary("abs","Emissions","all",r,sc))/pnum(r);
macro("eb(bn$)_global","all",r,sc)$rcalc(r)    = %scc%*(summary("abs","Emissions","all","all","BMK") - summary("abs","Emissions","all","all",sc))/pnum(r);	
macro("netwelf","all",r,sc)$rcalc(r)	= 100*((summary("abs","welfare","all",r,sc) - summary("abs","welfare","all",r,"bmk")+ macro("eb(bn$)","all",r,sc))/
						summary("abs","welfare","all",r,"bmk"));
macro("netwelf_globalscc","all",r,sc)$rcalc(r)	= 100*((summary("abs","welfare","all",r,sc) - summary("abs","welfare","all",r,"bmk")+ macro("eb(bn$)_global","all",r,sc))/
						summary("abs","welfare","all",r,"bmk"));
display macro, sector;

execute_unload 'single.gdx' macro, sector;

$onecho > gdxxrw.txt
par=macro	rng=macro!a2    cdim=0 intastext=n
par=sector	rng=sector!a2	cdim=0 intastext=n
$offecho

execute 'gdxxrw i=single.gdx o=single.xlsx @gdxxrw.txt';


parameter diagnostics;
diagnostics("Emission(%)",g,sc)$eite(g)	= quants("pct","CO2",g,"%region%",sc);
diagnostics("Emission(%)","EITE",sc)	= quants("pct","CO2","EITE","%region%",sc);
diagnostics("Emission(%)","non-EITE",sc)= quants("pct","CO2","non-EITE","%region%",sc);
diagnostics("Emission(%)","c",sc)	= quants("pct","CO2","C","%region%",sc);
diagnostics("Emission(%)","all",sc)	= quants("pct","CO2","all","%region%",sc);
diagnostics("Intensity",g,sc)$eite(g)	= 1000*quants("abs","CO2",g,"%region%",sc)/quants("abs","Y",g,"%region%",sc);
diagnostics("IUP",g,sc)$eite(g)		= 1000*iup(g,"%region%");
diagnostics("PCO2",g,sc)$eite(g)	= sector("CO2 price($)",g,"%region%",sc);
diagnostics("PCO2","non-EITE",sc)	= prices("abs","PCO2","roi","%region%",sc);
diagnostics("PCO2","EITE",sc)		= macro("PCO2","EITE","%region%",sc);
diagnostics("PCO2","c",sc)		= prices("abs","PCO2","c","%region%",sc);
option diagnostics:3:2:1;
display diagnostics;