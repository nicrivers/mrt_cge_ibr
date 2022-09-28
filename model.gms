$Title	 Multi-Region Trade (MRT) Model for Rebate Analysis Using GTAP data

*	Checks
*	1. Choice of fossil fuel supply elasticity [etar(xe)  = 1;]  versus [etar("col",r) = 4; etar("gas",r) = 1; etar("cru",r) = 1;] 
*	2. Sectors that are eligible for rebate (EIT  -- incl/excl OIL versus ELE)
*	3. Alignment of KLEM substitution elasticities in the power sector
*	4. In reporting we need to update/adjust GDP and INC accounting for potential tax/subsidy payments
*

set dummy /EITE, non-EITE, 'CO2 price', PCO2,Output, Emissions, Intensity, IUP, Unemploy, LSR, OBR, ABR, IBOR, IBER
	   oil, gas, col, cru, roi, ppp, nmm, i_s, nfm, ele, chm, c, g, i, 
	   Y, D, X, M, CO2, Int, Lhi, Llo, Lab, Cap /;	

*	Flag for presolving the model towards a benchmark equilibrium without any intial taxes/subsidies
$if not set tax	$set tax yes


*	Adopt system setting for directory seperator (e.g. "\") which is used in path assignments
$set fs %system.dirsep% 

*	Select the dataset
$if not set ds	$set ds rebate_11s_6r_3f
*.$if not set ds	$set ds	rebate_11s_G20_3f

*	Set the data directory
$set datadir data\

*	Invoke the "gtapdata.gms" routine which reads the dataset
$include gtapdata

*	Assign base-year taxes to rto0 to distinguish benchmark tax rates used
*	for model calibration (rto0) from tax rates (rto) which might be changed
*	in counterfactual policy simulations
parameter rto0	Base-year output tax (subsidy) rates;
rto0(g,r) = rto(g,r);

*	Set deviation tolerance
parameter objtol	Deviation tolerance (for IO accounting in bn USD)  /1e-3/;

*	Check GDP accounting:
parameter byr_gdp	Base year GDP;

*    Income-based GDP includes facor earnings plus tax revenue:
byr_gdp(r,"income")  =
     sum(f, evom(f,r))
    + sum(g,  vom(g,r)*rto(g,r))
    + sum(g,  sum(i, vdfm(i,g,r)*rtfd(i,g,r) + vifm(i,g,r)*rtfi(i,g,r)))
    + sum(g,  sum(f, vfm(f,g,r)*rtf(f,g,r)))
    + sum((i,s), rtms(i,s,r) *  (vxmd(i,s,r) * (1-rtxs(i,s,r)) + sum(j,vtwr(j,i,s,r))))
    - sum((i,s), rtxs(i,r,s) * vxmd(i,r,s));

*    Production-based GDP includes the value of output at market prices plus import
*    tariff revenue less export subsidies:

byr_gdp(r,"production")  =  sum(g, vom(g,r)) -   sum((i,g), vdfm(i,g,r) + vifm(i,g,r))
    + sum((i,s), rtms(i,s,r) *  (vxmd(i,s,r) * (1-rtxs(i,s,r)) + sum(j,vtwr(j,i,s,r))))
    - sum((i,s), rtxs(i,r,s) * vxmd(i,r,s));


*    Expenditure-based GDP:
byr_gdp(r,"expend")  =   vom("c",r) + vom("g",r) + vom("i",r) - vb(r);

display byr_gdp;

*	Differentiation of energy goods
sets	
	xe(g)	Primary (fossil) energy sectors to be calibrated to exogenous supply elasticities /col, gas, cru/,
	fe(i)	Fuels with CO2 emissions from combustion /col, oil, gas/, 
	ele(i)	Electricity /ele/,
	e(i)	Set of fuels with CO2 emissions plus electricity /col, oil, gas, ele/;

*	Fossil fuel sectors xe(g) get calibrated to exogenous supply elasticities
parameters
	esubr(g,r)	Substitution elasticity between specific energy resource and other inputs,
	thetar(g,r)	Resource value share,
	etar(g,r)	Exogenous fossil fuel supply elasticites;

esubr(g,r)    = 0;
etar("col",r) = 4; 
etar("gas",r) = 1; 
etar("cru",r) = 1; 

*	KLEM nesting of production inputs
parameters
        esub_kle_m(g,r)	Elasticity of substitution between KLE and M,
        esub_m(g,r)	Elasticity of substitution within material composite,
        esub_kl_e(g,r)	Elasticity of substitution between KL and E,
        esub_k_l(g,r)	Elasticity of substitution between K and L,
        esub_e(g,r)	Elasticity of susbstituion within energy composite;	

*	Assign KLEM elasticities (preferably based on empirical estimates)
*	The default is zero. We keep the assumption of Leontief technologies
*	for final demand sectors "g" (public good demand) and "i" (investment demand)
set	leontief(g)	Sectors with Leontief KLEM structure /g,i/;

loop(g$(not leontief(g)),
	esub_k_l(g,r)     = esubva(g);
	esub_e(g,r)	  = 0.5;
	esub_kl_e(g,r)	  = 0.5;
	esub_kle_m(g,r)	  = 0.5;
	esub_m(g,r)	  = 0;
     );

*	We increase the CO2 substitution elasticities in the power sector to mimic 
*	easier/cheaper interfuel substitution and RES-E (as well as nuclear) penetration
esub_e("ele",r)	   = 2;
esub_kl_e("ele",r) = 1;

display esubm, esubd;
*	The GTAP dataset features unrealistically high intra-import elasticity of substution for gas (esubm(gas) = 32.579)
*	We set trade elasticities to 3 and 6 as is the case for coal (we would assume coal trade elasticites to be higher
*	in principle since coal trade is not tied to pipelines)

$ontext
PARAMETER esubm  Intra-import elasticity of substitution
oil  4.200,    gas 32.579,    col  6.100,    cru 10.400,	ele 5.600,
PARAMETER esubd  Elasticity of substitution (M versus D)
oil 2.100,     gas 6.438,     col  3.050,    cru 5.200,		ele 2.800
$offtext

esubm("gas") = esubm("col");
esubd("gas") = esubm("col");

parameter resstat(xe,r,*)	Base-year statistics on energy resource sectors;

*	Check: Set fossil fuel resource to zero in case there is no domestic supply from domestic production
vfm("res",xe,r)$(not  (vom(xe,r)-vxm(xe,r)))  = 0;

resstat(xe,r,"vom")	 = vom(xe,r);
resstat(xe,r,"sf")	 = sum(sf, vfm(sf,xe,r));
resstat(xe,r,"mf")	 = sum(mf, vfm(mf,xe,r));
resstat(xe,r,"%sf/vom")$vom(xe,r)  = 100*resstat(xe,r,"sf")/vom(xe,r);

option resstat:3:2:1;
display resstat;

*	In order to reduce the model dimensionality we treat the Armington good as homogeneous across all demands
*	(i.e. the composition of domestic versus imported varieties is the same across all intermediate and final
*	 demand inputs).
parameter vafm(i,g,r), artfd0(i,r), artfi0(i,r), artfd(i,r), artfd_(i,r), artfi(i,r),  avafm(i,r);		

vafm(i,g,r) = vdfm(i,g,r)*(1+rtfd0(i,g,r)) + vifm(i,g,r) * (1+rtfi0(i,g,r));

artfd0(i,r)$sum(g, vdfm(i,g,r))	= sum(g, vdfm(i,g,r)*(1+rtfd0(i,g,r)))/sum(g, vdfm(i,g,r)) - 1;
artfi0(i,r)$sum(g, vifm(i,g,r))	= sum(g, vifm(i,g,r)*(1+rtfi0(i,g,r)))/sum(g, vifm(i,g,r)) - 1;
artfd(i,r) = artfd0(i,r); 		
artfi(i,r) = artfi0(i,r);		
avafm(i,r) = sum(g, vafm(i,g,r));

*	CO2 emissions accounting and regulation
parameters
	eco2(i,g,r)	Base-year CO2 emissions (in Gt),
	co20(i,g,r)	Reference emission flows,
	co2q(g,r)	Sector-specific emission flows,
	co2limr(r)	CO2 emission endowments for regional pricing,
	co2limt(r)	CO2 emission endowments for international pricing (emissions trading),
	glbtgt		Global emission target,		
	ptgt(r)		Targeted CO2 price;

sets	
	rtax(g,r)	Flag for regional emission pricing,
	ttax(g,r)	Flag for international emission pricing,
	obr(r)		Flag for output-based rebating,		
	abr(r)		Flag for abatement based rebating,	
	ibr(r)		Flag for intensity-based rebating,	
	bcr(r)		Flag for British-Columbia rebating;	

parameter	iup(g,r) Upper threshold for intensity rebate;

iup(g,r) = 0; 

*	Scale CO2 emissions from Mt to Gt so we can readily report carbon prices as USD per ton of CO2
*	given that the value units in GTAP are in bn USD
eco2(i,g,r)$vafm(i,g,r)	= (eco2d(i,g,r) + eco2i(i,g,r))/1000;
eco2(i,g,r)$(not fe(i)) = 0;

co20(i,g,r) = eco2(i,g,r); 
co2q(g,r)   = sum(i,co20(i,g,r));
co2limr(r)  = sum((i,g),co20(i,g,r));
co2limt(r)  = sum((i,g),co20(i,g,r));

glbtgt	    = 0;
ptgt(r)	    = 0;

rtax(g,r)   = no; 
ttax(g,r)   = no;
obr(r)	    = no;	
abr(r)	    = no;
ibr(r)	    = no;
bcr(r)	    = no;

set	eite(g)	Set of emission-intensive and trade-exposed sectors /
$if %ds%==rebate_11s_G20_3f	oil,i_s,nfm,nmm,ppp,chm
$if %ds%==rebate_11s_6r_3f	oil,i_s,nfm,nmm,ppp,chm
/;


parameter	ks0(f,g,r)	Sector-specific capital endowments;

ks0(sf,g,r) = vfm(sf,g,r);


$ontext
$model:gtap

$sectors:
	Y(g,r)$vom(g,r)                 ! Output
	DX(g,r)$vom(g,r)                ! Transformation of output into domestic and export supply
	M(i,r)$vim(i,r)                 ! Imports
	YT(j)$vtw(j)                    ! Transportation services
	A(i,r)$avafm(i,r)               ! Armington supply
	CO2(g,r)$co2q(g,r)              ! CO2 emission flows

$commodities:
	PY(g,r)$vom(g,r)		! Output price
	PD(g,r)$(vom(g,r)-vxm(g,r))	! Domestic supply price
	PM(i,r)$vim(i,r)		! Import price
	PE(g,r)$vxm(g,r)		! Export price
	PT(i)$vtw(i)			! Price for Transportation services
	PF(f,r)$(mf(f)*evom(f,r))	! Mobile primary factors rent
	PS(f,g,r)$(sf(f)*vfm(f,g,r))	! Sector-specific primary factors
	PA(i,r)$avafm(i,r)		! Armington supply price 
	PCO2(g,r)$co2q(g,r)		! CO2 emission price by sector and region
	PCO2R(r)$(sum(g, rtax(g,r)))	! Regional CO2 emission price
	PCO2T$card(ttax)		! International CO2 emission price

$consumers:
	RA(r)				! Representative agent

$auxiliary:
	PHI(r)$ptgt(r)			! Rationing of CO2 budget to hit exogenous CO2 price
	RHO(g,r)			! Tax on emission inputs
	PSI(g,r)			! Subsidy on production output
	TAU$glbtgt			! Rationing of emission budget for leakage compensation

*	Transformation of output into domestic and export supply
$prod:DX(g,r)$vom(g,r)	t:etadx(g) 
	o:PD(g,r)	q:(vom(g,r)-vxm(g,r))	
	o:PE(g,r)	q:vxm(g,r)		
	i:PY(g,r)	q:vom(g,r)

*	Production (except for fossil fuels)
$prod:Y(g,r)$(vom(g,r) and not xe(g)) 
+ s:esub_kle_m(g,r)  m(s):esub_m(g,r) vae(s):esub_kl_e(g,r) e(vae):esub_e(g,r) va(vae):esub_k_l(g,r)
+  oil(e):0 col(e):0 gas(e):0  
	o:PY(g,r)	 q:vom(g,r)		p:(1-rto0(g,r))	     a:RA(r) t:rto(g,r)	a:RA(r) n:PSI(g,r)	m:(-1)
	i:PA(i,r)	 q:vafm(i,g,r)								i.tl:$fe(i) m:$(not e(i)) e:$ele(i)
	i:PCO2(g,r)#(fe) q:co20(fe,g,r)		p:1e-6		 			        fe.tl:
	i:PF(mf,r)	 q:vfm(mf,g,r)		p:(1+rtf0(mf,g,r))   a:RA(r) t:rtf(mf,g,r)	va:
	i:PS(sf,g,r)	 q:vfm(sf,g,r)		p:(1+rtf0(sf,g,r))   a:RA(r) t:rtf(sf,g,r)	va:

*	Fossil fuel production
$prod:Y(g,r)$(vom(g,r) and xe(g)) s:esubr(g,r)  id:0
	o:PY(g,r)	q:vom(g,r)	 	 p:(1-rto0(g,r))     a:RA(r) t:rto(g,r)  
	i:PA(i,r)	q:vafm(i,g,r)								id:
	i:PCO2(g,r)	q:(sum(fe,co20(fe,g,r))) p:1e-6		 			        id:
	i:PF(mf,r)	q:vfm(mf,g,r)		 p:(1+rtf0(mf,g,r))     a:RA(r) t:rtf(mf,g,r)	id:
	i:PS(sf,g,r)	q:vfm(sf,g,r)		 p:(1+rtf0(sf,g,r))     a:RA(r) t:rtf(sf,g,r)	

*	International transport service
$prod:YT(i)$vtw(i)  s:1	
	o:PT(i)			 q:vtw(i)
	i:PE(i,r)$vxm(i,r) 	 q:vst(i,r)	
	i:PD(i,r)$(not vxm(i,r)) q:vst(i,r)

*	Import composite
$prod:M(i,r)$vim(i,r)	s:esubm(i)  s.tl:0
	o:PM(i,r)			q:vim(i,r)
	i:PE(i,s)$vxm(i,s)		q:vxmd(i,s,r)	p:pvxmd(i,s,r) s.tl: 
+		a:RA(s) t:(-rtxs(i,s,r)) a:RA(r) t:(rtms(i,s,r)*(1-rtxs(i,s,r)))
	i:PD(i,s)$(not vxm(i,s))	q:vxmd(i,s,r)	p:pvxmd(i,s,r) s.tl:
+		a:RA(s) t:(-rtxs(i,s,r)) a:RA(r) t:(rtms(i,s,r)*(1-rtxs(i,s,r)))
	i:PT(j)#(s)			q:vtwr(j,i,s,r) p:pvtwr(i,s,r) s.tl: a:RA(r) t:rtms(i,s,r)

*	Armington composite
$prod:A(i,r)$avafm(i,r) s:esubd(i)
	o:PA(i,r)	q:avafm(i,r)	
        i:PD(i,r)	q:(sum(g,vdfm(i,g,r)))	   p:(1+artfd0(i,r))  a:RA(r) t:artfd(i,r)  
        i:PM(i,r)	q:(sum(g,vifm(i,g,r)))	   p:(1+artfi0(i,r))  a:RA(r) t:artfi(i,r)  

*	 CO2 emission flows
$prod:CO2(g,r)$co2q(g,r)
	o:PCO2(g,r)            q:1
	i:PCO2R(r)$rtax(g,r)   q:1    p:1e-6	a:RA(r) n:RHO(g,r)  
	i:PCO2T$ttax(g,r)      q:1    p:1e-6	a:RA(r) n:RHO(g,r)
	i:PF("lab",r)          q:1e-6

*	Income-expenditure balance of representative agent
$demand:RA(r)  s:0
	d:PD("c",r)                             q:vom("c",r)
	e:PD("g",r)                             q:(-vom("g",r))
	e:PD("i",r)                             q:(-vom("i",r))
	e:PD("i",rnum)                          q:vb(r)
	e:PF(mf,r)                              q:evom(mf,r)
	e:PS(sf,g,r)                            q:ks0(sf,g,r)
	e:PCO2R(r)$((sum(g, rtax(g,r))))	q:co2limr(r)		r:PHI(r)$ptgt(r)	r:TAU$glbtgt
	e:PCO2T$card(ttax)			q:co2limt(r)		r:PHI(r)$ptgt(r)	r:TAU$glbtgt	

*	Targeting of exogenous CO2 price
$constraint:PHI(r)$ptgt(r)
	PCO2R(r) =e= PD("c",r)*ptgt(r);

*	OBR-IBR-BCR: Implicit output subsidy through revenue recycling
$constraint:PSI(g,r)$((obr(r) or ibr(r) or bcr(r)) and eite(g))
	PSI(g,r)*PY(g,r)*Y(g,r)*vom(g,r) =e= PCO2(g,r)*CO2(g,r);

*	ABR emission pricing
$constraint:RHO(g,r)$(abr(r) and eite(g))
*.	RHO(g) =e= ((eco2(g)/(eco2(g) - CO2(g)))- 1);
	(RHO(g,r) + 1)*(co2q(g,r) - CO2(g,r)) =e= co2q(g,r);

*	IBR emission pricing 
$constraint:RHO(g,r)$(ibr(r) and eite(g))
*.	RHO(g,r) =e= (iup(g,r)/(iup(g,r) - (CO2(g,r)/(Y(g,r)*y0(g,r)))) - 1);
**.	(RHO(g,r) + 1)*(iup(g,r) - (CO2(g,r)/(Y(g,r)*vom(g,r)))) =e= iup(g,r);
	(RHO(g,r) + 1)*((Y(g,r)*vom(g,r))*iup(g,r) - CO2(g,r)) =e= iup(g,r)*Y(g,r)*vom(g,r);

*	BCR emission pricing
$constraint:RHO(g,r)$(bcr(r) and eite(g)) 
*.	RHO(g,r) =e= (((CO2(g,r)/(Y(g,r)*vom(g,r)))/(iup(g,r) - (CO2(g,r))/(Y(g,r)*vom(g,r)))) - 1);
**.	(RHO(g,r) + 1)*(iup(g,r) - (CO2(g,r))/(Y(g,r)*vom(g,r))) =e= (CO2(g,r)/(Y(g,r)*vom(g,r)));
	(RHO(g,r) + 1)*((Y(g,r)*vom(g,r))*iup(g,r) - (CO2(g,r))) =e= CO2(g,r);

*	Leakage-adjusted global emission target
$constraint:TAU$glbtgt
	glbtgt =e= sum(r, sum(g$co2q(g,r), CO2(g,r))); 

$report:
	v:V_AFM(i,g,r)$vafm(i,g,r)			i:PA(i,r)	prod:Y(g,r)
	v:V_FM(f,g,r)$(sf(f)*vom(g,r)*vfm(f,g,r))	i:PS(f,g,r)	prod:Y(g,r)
	v:V_FM(f,g,r)$(mf(f)*vom(g,r)*vfm(f,g,r))	i:PF(f,r)	prod:Y(g,r)
	v:V_VST(i,r)$(vst(i,r) and (not vxm(i,r)))	i:PD(i,r)	prod:YT(i)
	v:V_VST(i,r)$(vst(i,r) and vxm(i,r))		i:PE(i,r)	prod:YT(i)
	v:V_VDM(g,r)$(vom(g,r)-vxm(g,r))		o:PD(g,r)	prod:DX(g,r)
	v:V_VXM(g,r)$(vom(g,r)*vxm(g,r))		o:PE(g,r)	prod:DX(g,r)
	v:V_XMD(i,s,r)$(vxmd(i,s,r)*vxm(i,s))		i:PE(i,s) 	prod:M(i,r)
	v:V_DFM(i,r)$(sum(g, vdfm(i,g,r)))		i:PD(i,r)	prod:A(i,r)
	v:V_IFM(i,r)$(sum(g, vifm(i,g,r)))		i:PM(i,r)	prod:A(i,r)
	v:V_XMD(i,s,r)$(vxmd(i,s,r)*(not vxm(i,s)))	i:PD(i,s) 	prod:M(i,r)
$offtext
$sysinclude mpsgeset gtap

alias (i,i_,j_), (f,f_), (g,gg);

$macro DFM(f,g,r)	(V_FM.L(f,g,r)$(vom(g,r)*vfm(f,g,r)))
$macro DDFM(i,r)	(V_DFM.L(i,r))
$macro DIFM(i,r)	(V_IFM.L(i,r))
$macro REV_TO(g,r)	((rto(g,r)*PY.L(g,r)*Y.L(g,r)*vom(g,r))$(vom(g,r)))		
$macro REV_TFD(i,r)	(artfd(i,r) * PD.L(i,r)  * DDFM(i,r))
$macro REV_TFI(i,r)	(artfi(i,r) * PM.L(i,r) * DIFM(i,r))
$macro REV_TF(f,g,r) 	((rtf(f,g,r)  * V_FM.L(f,g,r) * ((PS.L(f,g,r))$sf(f)+(PF.L(f,r))$mf(f)))$(vom(g,r)*vfm(f,g,r)))
$macro XMD(i,s,r)	((V_XMD.L(i,s,r)/vxmd(i,s,r))$(vim(i,r)*vxmd(i,s,r)) )
$macro PX(i,r)		((PD.L(i,r)$(not vxm(i,r)) + PE.L(i,r)$vxm(i,r)))
$macro REV_TXS(i,s,r)	((XMD(i,s,r)*rtxs(i,s,r)*vxmd(i,s,r) * PX(i,s))$(vxmd(i,s,r)*rtxs(i,s,r)))
$macro REV_TMS(i,s,r)	((XMD(i,s,r)*rtms(i,s,r)*(PX(i,s)*(1-rtxs(i,s,r))*vxmd(i,s,r)  + \
			 sum(j_$vtwr(j_,i,s,r), PT.L(j_)*vtwr(j_,i,s,r))))$(vxmd(i,s,r)*rtms(i,s,r)))

*	Initialize level values
CO2.l(g,r)  = co2q(g,r);
PCO2.l(g,r) = 1e-6; 	
PCO2R.l(r)  = 1e-6;	
PCO2T.l     = 1e-6;
PHI.fx(r)   = 1; 
RHO.fx(g,r) = 0;
PSI.fx(g,r) = 0;
TAU.fx	    = 1;

gtap.workspace = 1024;
gtap.iterlim = 0;
$include GTAP.GEN
solve gtap using mcp;

abort$(abs(gtap.objval) > objtol) "Base-year replication fails.", gtap.objval;
*	Treatment of capital earnngs:
*	1. Move all capital earnings (res, cap) in fossil fuel sectors into sector-specific resource (res)
*	2. Move all capital earnings (res, cap) in other sectors into malleable capital (cap)

*	Calibrate fossil fuel cost function to exogenous supply elasticities
*	etar	=	Elasticity of supply
*	esubr	=	Elasticity of substitution
*	thetar	=	Cost share of specific factor input

*	etar = esubr * thetar/(1-thetar)

*	ad 1.	Move all capital earnings in XE sectors into specific capital and calibrate top-level CES esubr
*		to align locally with an exogenous supply elasticity etar(xe)

rtf0("res",xe,r)$(sum(sf, vfm(sf,xe,r)) + vfm("cap",xe,r)) 
		= (sum(sf, vfm(sf,xe,r)*rtf0(sf,xe,r)) + vfm("cap",xe,r)*rtf0("cap",xe,r))/(sum(sf, vfm(sf,xe,r)) + vfm("cap",xe,r));
rtf0("cap",xe,r)= 0;
rtf0(sf,xe,r)$(not sameas(sf,"res")) = 0;
rtf(sf,xe,r)	= rtf0(sf,xe,r);
rtf(mf,xe,r)	= rtf0(mf,xe,r);
vfm(sf,xe,r)	= vfm(sf,xe,r) + vfm("cap",xe,r);
vfm("cap",xe,r)	= 0;
evom("cap",r)	= sum(i,vfm("cap",i,r));
ks0(sf,i,r)	= vfm(sf,i,r);

thetar(xe,r)$vom(xe,r)	     = vfm("res",xe,r)*(1+rtf0("res",xe,r))/(vom(xe,r)*(1-rto0(xe,r)));
esubr(xe,r)$(1-thetar(xe,r)) = etar(xe,r)*thetar(xe,r)/(1-thetar(xe,r));

display thetar, esubr;

$include GTAP.GEN
solve gtap using mcp;
abort$(abs(gtap.objval) > objtol) "Recalibration of fossil fuel supply sectors fails.", gtap.objval;

*	ad 2.	Move all capital earnings (res, cap) in sectors other than XE into malleable capital (cap)
loop((g,r)$((not xe(g)) and sum(sf, vfm(sf,g,r))),
    rtf0("cap",g,r)$(sum(sf, vfm(sf,g,r)) + vfm("cap",g,r)) 
        = (sum(sf, vfm(sf,g,r)*rtf0(sf,g,r)) + vfm("cap",g,r)*rtf0("cap",g,r))/(sum(sf, vfm(sf,g,r)) + vfm("cap",g,r));
    rtf0(sf,g,r)   = 0;
    rtf(sf,g,r)    = rtf0(sf,g,r);
    rtf(mf,g,r)    = rtf0(mf,g,r);
    vfm("cap",g,r) = vfm("cap",g,r) + sum(sf, vfm(sf,g,r));
    vfm(sf,g,r)	   = 0;
    evom("cap",r)  = sum(i,vfm("cap",i,r));
    ks0(sf,i,r)	   = vfm(sf,i,r);
    PS.l("res",g,r) = 0;
);
$include GTAP.GEN
solve gtap using mcp;
abort$(abs(gtap.objval) > objtol) "Recalibration of capital earnings fails.", gtap.objval;

*	Check base-year CO2 accounting with regional emission pricing (rtax)
rtax(g,r) = yes; 
$include GTAP.GEN
solve gtap using mcp;
abort$(abs(gtap.objval) > objtol) "Base-year replication with rtax fails.", gtap.objval;
rtax(g,r) = no;  

*	Check base-year CO2 accounting with international emission pricing (ttax)
ttax(g,r) = yes; 
$include GTAP.GEN
solve gtap using mcp;
abort$(abs(gtap.objval) > objtol) "Base-year replication with ttax fails.", gtap.objval;
ttax(g,r) = no;  

parameter co2_bmk(r) Bechmark emissions;
co2_bmk(r) = sum(g, CO2.l(g,r));


*	Report GDP and CO2 emissions to compare values in BMK with initial taxes versus BMK without initial taxes
$batinclude report '"bmk"'
parameter gdp_compare, co2_compare;;
gdp_compare("expend","before",r)	       = gdp("abs","total","expend",r,"BMK");
gdp_compare("income","before",r)	       = gdp("abs","total","income",r,"BMK");    
co2_compare(fe,"before",r)$sum(g,vafm(fe,g,r)) = sum(g, (V_AFM.l(fe,g,r)/vafm(fe,g,r))*co20(fe,g,r));
co2_compare("all","before",r)		       = sum(fe,co2_compare(fe,"before",r)); 

display gdp_compare, co2_compare;


if( (not %tax%),
gtap.iterlim = 100000;

*	We need to relax the carbon constraint to make sure that CO2 pricing does not kick in when we
*	set all taxes to zero to provide a benchmark euqilibrium without intial tax distortions
co2limr(r)  = 1000*co2limr(r);
co2limt(r)  = 1000*co2limt(r); 

*	Reset all taxes to zero
rtf(f,g,r)  = 0;

$include gtap.gen
solve gtap using mcp;

artfd(i,r)  = 0;
artfi(i,r)  = 0;
$include gtap.gen
solve gtap using mcp;

rto(g,r)    = 0;
$include gtap.gen
solve gtap using mcp;

rtms(i,s,r) = 0;
rtxs(i,s,r) = 0;
$include gtap.gen
solve gtap using mcp;


abort$(not((gtap.modelstat eq 1) and (gtap.solvestat eq 1)))"Model is not locally optimal or does not terminate";


*	Then make sure that we solve the benchmark at a binding co2lim with a shadow price of zero
*	We also need to update the reference emission flows to sectors:
rtax(g,r)  = yes;
co2q(g,r)  = CO2.l(g,r);
co2limr(r) = sum(g, CO2.l(g,r));
co2limt(r) = sum(g, CO2.l(g,r));
gtap.iterlim = 0;
$include gtap.gen
solve gtap using mcp;
abort$(abs(gtap.objval) > objtol) "Replication fails.", gtap.objval;
co2_bmk(r) = sum(g, CO2.l(g,r));

);


$batinclude report '"bmk"'

gdp_compare("expend","after",r)			= gdp("abs","total","expend",r,"BMK");
gdp_compare("income","after",r)			= gdp("abs","total","income",r,"BMK");    
co2_compare(fe,"after",r)$sum(g,vafm(fe,g,r))	= sum(g, (V_AFM.l(fe,g,r)/vafm(fe,g,r))*co20(fe,g,r));
co2_compare("all","after",r)			= sum(fe,co2_compare(fe,"after",r)); 

display gdp_compare, co2_compare;


gdp_compare("expend","pctdev",r)$gdp_compare("expend","before",r)   = 100*(gdp_compare("expend","after",r)/gdp_compare("expend","before",r)  - 1);     
gdp_compare("income","pctdev",r)$gdp_compare("income","before",r)   = 100*(gdp_compare("income","after",r)/gdp_compare("income","before",r) - 1);     
co2_compare(fe,"pctdev",r)$co2_compare(fe,"before",r)		    = 100*(co2_compare(fe,"after",r)/co2_compare(fe,"before",r)  - 1);	     
co2_compare("all","pctdev",r)$co2_compare("all","before",r)	    = 100*(co2_compare("all","after",r)/co2_compare("all","before",r) - 1);  

display gdp_compare, co2_compare;

parameter intensity	Emission intensity (g CO2 per dollar);

intensity(g,r)$vom(g,r) = 1000*CO2.l(g,r)/(vom(g,r)*Y.l(g,r));

option intensity:3:0:1;

display intensity;


*	Base-year statistics
parameter bmk(*,*,*)	Benchmark statistics;

set label /'Refined petroleum products','Natural gas','Coal','Crude oil','Rest of economy', 'Pulp and paper', 'Non-metal minerals', 'Iron and steel'
	   'Non-ferrous metals', 'Electricity', 'Chemicals', 'Consumption', 'Government', 'Investment'/;

set gl(g,label)	/oil.'Refined petroleum products',
		 gas.'Natural gas',
		 col.'Coal',
		 cru.'Crude oil',
		 roi.'Rest of economy',
		 ppp.'Pulp and paper',
		 nmm.'Non-metal minerals',
		 i_s.'Iron and steel',
		 nfm.'Non-ferrous metals',
		 ele.'Electricity',
		 chm.'Chemicals',
		 c.'Consumption',
		 g.'Government',
		 i.'Investment'	/;

$if not set bmkregion	$set bmkregion usa

bmk(gl(g,label),qitem)	=  round(quants("abs",qitem,g,"%bmkregion%","bmk"),0);
bmk(gl(g,label),"cap")	= 0;
bmk(gl(g,label),"res")	= 0;
bmk(gl(g,label),"CO2")	=  round(1000*quants("abs","CO2",g,"%bmkregion%","bmk"));
bmk(gl(g,label),"Int")$bmk(gl,"Y") =  round(1000*bmk(gl,"CO2")/bmk(gl,"Y"),0);

display bmk;


execute_unload 'bmk_%bmkregion%.gdx' bmk;

$onecho >gdxxrw.txt
par=bmk	  rng=bmk!a1 rdim=2 cdim=1
$offecho
*.execute 'gdxxrw i=bmk_%bmkregion%.gdx o=bmk_%bmkregion%.xlsx @gdxxrw.txt';

