$STitle	Alternative rebate schemes for CO2 revenues

$ontext
Rebate schemes may help to maintain competitiveness/prevent leakage and may help
to achieve deeper emission reductions than through carbon pricing "stand-alone" 
(i.e. carbon pricing with lump-sum rebating of revenues) also closing the gap
to the social cost of carbon vis implicit additional emsssions pricing (e.g.
with abatement-based rebates).

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

*	Key choices for scenario settings include the abating region, the unilateral
*	CO2 price and the social cost of carbon

*	Select the unilaterally abating region
$if not set region	$set region eur
set	rcalc(r)	Region with emissions abatement policy /%region%/;

*	Select the CO2 price for the abating region (in USD per ton of CO2)
$if not set co2p	$set co2p 0

*	Select the social cost of carbon
$if not set scc		$set scc 0

*	Key elasticities for sensitivity analysis include: etar, esub_e("ele")/esub_kl_e("ele"), and arm (esubd/esubm)
$if not set etar	$set etar ref
$if not set esubele	$set esub_ele ref
$if not set arm		$set arm ref

$if  %etar%==lo		etar(xe,r) = 0.5*etar(xe,r);
$if  %esubele%==lo	esub_e("ele",r) = 0.5*esub_e("ele",r); esub_kl_e("ele",r) = 0.5*esub_kl_e("ele",r);
$if  %arm%==lo		esubd(i)$eite(i) = 0.5*esubd(i); esubd(i)$eite(i) = 0.5*esubd(i);

$if  %etar%==hi		etar(xe) = 2*etar(xe);
$if  %esubele%==hi	esub_e("ele",r) = 2*esub_e("ele",r); esub_kl_e("ele",r) = 2*esub_kl_e("ele",r);
$if  %arm%==hi		esubd(i)$eite(i) = 2*esubd(i); esubd(i)$eite(i) = 2*esubd(i);

*	Need to re-assign fossil fuel elasticities to impute the associated substitution elastcitiy
thetar(xe,r)$vom(xe,r)	     = vfm("res",xe,r)*(1+rtf0("res",xe,r))/(vom(xe,r)*(1-rto0(xe,r)));
esubr(xe,r)$(1-thetar(xe,r)) = etar(xe,r)*thetar(xe,r)/(1-thetar(xe,r));

*	The flag "fixffp" fixes the international fuel prices at BMK levels and thus accommodates
*	some decomposition of leakage through the so-called fossil fuel price channel and the
*	competitiveness channel
$if not set fixffp	$set fixffp no

*	The flag "compensate" imposes that all non-abating regions are kept at their
*	BMK welfare level through compensating transfers (negative or positive).
*	The sum of all compensating transfers (see reporting of terms-of-trade changes ToT below) 
*	should be equivalent to the effective ToT gains/losses by the abating region. This flag serves as 
*	a conceptual handshake on the default reporting of ToT changes for the abating region
*	which could be also calculated as the sum of difference in welfare between the counterfactual
*	policy and the benchmark across the non-abating regions. 
*	If we wanted to see the equivalence we could first run the model without the flag compensate (our
*	default) and just compute the ToT changes for the unilaterally acting/abating region as the
*	sum of BMK-Counterfactual welfare changes in money metric utility across the non-abting regions
*	and then do a second run where we use explicit transfers having $set compensate yes (or on the
*	batch command line - gams scen r=mdl .... --compensate=yes).
*	In doing so we see some deviation between the two modes of calculation which we need still to sort out.

$if not set compensate	$set compensate no

*	In the benchmark we have no binding emission constraints
co2limr(r) = 0; co2limt(r) = 0;
gtap.iterlim = 0;
$include gtap.gen
solve gtap using mcp;
abort$(abs(gtap.objval) > objtol) "Benchmark replication fails.", gtap.objval;
$batinclude report '"bmk"'

*	Check:	Upper bounds for rebate variants IBOR and IBER set at the level of the BMK scenario
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


display trnv;


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
*.runsc(sc)$(sameas(sc,"lsr")) = yes;
*.runsc(sc) = no;
runsc(sc) = yes ;

loop(sc$runsc(sc),

put_utility ktitle, 'title' /'Solving scenario ',sc.tl;

*	Before we assign specific scenario settings in the loop, we must reset all specific policy settings
PSI.fx(g,r) = 0; RHO.fx(g,r) = 0; PHI.l(r)   = 1; obr(r) = no; abr(r) = no; ibr(r) = no; bcr(r) = no;

*	Option to include compensating transfers to keep non-abating regions at their BMK welfare level
if (%compensate%,
	TRNSF.UP(r)$(co2limr(r)=0)		  = +inf;
        TRNSF.LO(r)$(co2limr(r)=0)		  = -inf;
        trnv(r,s)$(co2limr(r)>0 and co2limr(s)=0) = vom("c",r);
        trnv(r,s)$sum(r.local, trnv(r,s))	  = -trnv(r,s)/sum(r.local, trnv(r,s));
        trnv(s,s)$(co2limr(s)=0)		  = 1;
        ctrg(r)					  = quants("abs","Y","C",r,"bmk")/vom("c",r);
   );

*	Option to fix international fuel prices so we can decompose leakage 
if (%fixffp%,
	RS.lo(xe,r)$ks0("res",xe,r) = 0;
	RS.up(xe,r)$ks0("res",xe,r) = inf;
	RS.l(xe,r)$ks0("res",xe,r)  = 1;
   ); 


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
parameter macro, sector,diagnostics;
parameter tot		Terms-of-trade welfare gains or losses;
parameter co2rep	Composite CO2 report (with electricity price change);

loop(sc$runsc(sc),
sector("Output",g,r,sc)			= quants("pct","Y",g,r,sc);
sector("Price",g,r,sc)			= prices("pct","PY",g,r,sc);
sector("Emissions",g,r,sc)		= quants("pct","CO2",g,r,sc);
sector("Intensity",g,r,sc)		= quants("pct","Int",g,r,sc);
sector("Emissions","EITE",r,sc)		= quants("pct","CO2","EITE",r,sc);
sector("Emissions","non-EITE",r,sc)	= quants("pct","CO2","non-EITE",r,sc);
sector("Output","EITE",r,sc)		= quants("pct","Y","EITE",r,sc);
sector("Output","non-EITE",r,sc)	= quants("pct","Y","non-EITE",r,sc);
sector("Intensity","EITE",r,sc)		= quants("pct","Int","EITE",r,sc);
sector("Intensity","non-EITE",r,sc)	= quants("pct","Int","non-EITE",r,sc);
sector("CO2 price($)",g,r,sc)		= prices("abs","PCO2",g,r,sc);
sector("Int-IUP(abs)",g,r,sc)		= 1000*iup(g,r);
sector("Int-mdl(abs)",g,r,sc)		= quants("abs","Int",g,r,sc);
sector("CO2 price($)","EITE",r,sc)	= summary("abs","CO2 price","EITE",r,sc);
sector("CO2 price($)","non-EITE",r,sc)	= prices("abs","PCO2","roi",r,sc);
sector("CO2 price($)","c",r,sc)		= prices("abs","PCO2","c",r,sc);

macro(sitem,gp,r,sc)			= summary("pct",sitem,gp,r,sc);
macro(sitem,gp,r,sc)$(not sameas(sitem,"CO2 price")) = summary("pct",sitem,gp,r,sc);
macro("welfare","all","all",sc)		= summary("pct","welfare","all","all",sc);
macro("welfare$","all",r,sc)		= summary("abs","welfare","all",r,sc);
macro("emissions","all","all",sc)	= summary("pct","emissions","all","all",sc);
macro("CO2 price",gp,r,sc)		= summary("abs","CO2 price",gp,r,sc);
macro("Leakage","all",r,sc)		= summary("abs","Leakage","all",r,sc);
macro("Leakage","all","all",sc)		= summary("abs","Leakage","all","all",sc);
macro("Employment","EITE",r,sc)		= quants("pct","lab","EITE",r,sc);
macro("Employment","non-EITE",r,sc)	= quants("pct","lab","non-EITE",r,sc);
macro("eb(bn$)","all",r,sc)$rcalc(r)    = %scc%*(summary("abs","Emissions","all",r,"BMK") - summary("abs","Emissions","all",r,sc))/pnum(r);
macro("eb(bn$)_global","all",r,sc)$rcalc(r) 
					= %scc%*(summary("abs","Emissions","all","all","BMK") - summary("abs","Emissions","all","all",sc))/pnum(r);	
macro("netwelf","all",r,sc)$rcalc(r)	= 100*((summary("abs","welfare","all",r,sc) - summary("abs","welfare","all",r,"bmk")+ macro("eb(bn$)","all",r,sc))/
						summary("abs","welfare","all",r,"bmk"));
macro("netwelf_globalscc","all",r,sc)$rcalc(r)
					= 100*((summary("abs","welfare","all",r,sc) - summary("abs","welfare","all",r,"bmk")+ macro("eb(bn$)_global","all",r,sc))/
						summary("abs","welfare","all",r,"bmk"));
macro("emissions_abs","all",r,sc)	= summary("abs","Emissions","all",r,sc)						;

diagnostics("Emission(%)",g,sc)$eite(g)	= quants("pct","CO2",g,"%region%",sc);
diagnostics("Emission(%)","EITE",sc)	= quants("pct","CO2","EITE","%region%",sc);
diagnostics("Emission(%)","non-EITE",sc)= quants("pct","CO2","non-EITE","%region%",sc);
diagnostics("Emission(%)","c",sc)	= quants("pct","CO2","C","%region%",sc);
diagnostics("Emission(%)","all",sc)	= quants("pct","CO2","all","%region%",sc);
diagnostics("Intensity",g,sc)$quants("abs","Y",g,"%region%",sc)		
					= 1000*quants("abs","CO2",g,"%region%",sc)/quants("abs","Y",g,"%region%",sc);
diagnostics("IUP",g,sc)$eite(g)		= 1000*iup(g,"%region%");
diagnostics("PCO2",g,sc)$eite(g)	= sector("CO2 price($)",g,"%region%",sc);
diagnostics("PCO2","non-EITE",sc)	= prices("abs","PCO2","roi","%region%",sc);
diagnostics("PCO2","EITE",sc)		= macro("PCO2","EITE","%region%",sc);
diagnostics("PCO2","c",sc)		= prices("abs","PCO2","c","%region%",sc);


if (%compensate%,
*.	RA(r) ==> e:PD("c",rnum)#(s)			q:trnv(r,s)		r:TRNSF(s)
*.transfers("abs",r,sc) = sum(s, TRNSF(s)*trnv(r,s)*PD("c",rnum));
	tot(r,sc)	= - transfers("abs",r,sc);			
	tot("all",sc)	= sum(r, tot(r,sc));
else
*.summary("abs","Welfare","all",r,%1)	   = RA(r)/pnum(r);
	tot(r,sc)$(not sameas(r,"%region%"))= summary("abs","welfare","all",r,sc) - summary("abs","welfare","all",r,"BMK");
	tot("%region%",sc)	= - sum(r$(not sameas(r,"%region%")), tot(r,sc)); 	
	tot("all",sc)		= sum(r, tot(r,sc));
    );	

co2rep(sc,rp,"emission(%)")	= macro("emissions","all",rp,sc);
co2rep(sc,rp,"leakage(%)")	= macro("Leakage","all",rp,sc);
co2rep(sc,r,"CO2price($/tCO2)")	= round(prices("abs","PCO2","c",r,sc),0);
co2rep(sc,r,"ELEprice(%)")	= round(prices("pct","PA","ele",r,sc),1);

);

display macro, sector;

option diagnostics:3:2:1;
display diagnostics;
display tot;

option dispWidth=17;
option co2rep:1:2:1;
display co2rep;

*	By default we run a systematic sensitivity analysis invoked with ssa.gms/runmodel.bat
*	where we pour all results via pivot.bat into results.xlsx
*	If we want to look at one set of scenario runs only without outer-loop sensitivity
*	analysis via ssa.gms we can invoke one.bat and just look at a single simulation run
*	where the results data gets poured into single.gdx/single.xlsx. For this we have to
*	flag one.bat with the environment variable --ssa=no such that we do not exit.
$if not set ssa	$set ssa yes
$if %ssa%==yes	$exit
execute_unload 'single.gdx' macro, co2rep, tot, sector, diagnostics;

$onecho > gdxxrw.txt
par=co2rep	rng=co2rep!a1		cdim=1 rdim=2 intastext=n
par=macro	rng=macro!a2		cdim=0 intastext=n
par=tot		rng=tot!a2		cdim=0 intastext=n
par=sector	rng=sector!a2		cdim=0 intastext=n
par=diagnostics	rng=diagnostics!a2	cdim=0 intastext=n
$offecho

execute 'gdxxrw i=single.gdx o=single.xlsx @gdxxrw.txt';

