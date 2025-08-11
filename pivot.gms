$title  Produce a Pivot Report Keyed to Input Assumptions
set dummy	/0*2000/;

$onUNDF

*   -------------------------------------------------------------------
*   Model-specific code:

$if not set item     $set item results
$if not set ws       $set ws PivotData
$if not set output   $set output %item%

$if not set inputs   $set inputs mdl,region,co2p,scc,arm,esube
*.$if not set inputs   $set inputs region,co2p,scc,arm,esube,etar,fixffp
$if not set indices  $set indices item 

*   -------------------------------------------------------------------

alias (%inputs%,*);
alias (%indices%,*);
 
set header /%indices%, %inputs%,  value/;

$gdxin 'ssa.gdx'

set     scn(*)  Scenarios indices;
$load scn
set     inputs(scn,%inputs%) Input associations;
$load inputs

parameter       %item%(scn,%indices%)      Model results;

$call 'gdxmerge gdx\*.gdx id=%item%'
$gdxin merged.gdx
$load %item%

parameter       pivotdata   Pivot table data;
loop(inputs(scn,%inputs%),
        pivotdata(%indices%,%inputs%) = %item%(scn,%indices%);
);
execute_unload 'pivotdata.gdx',header,pivotdata;
$onecho >gdxxrw.rsp
set=header    rng=%ws%!a1 cdim=1
par=pivotdata rng=%ws%!a2 cdim=0 intastext=no
$offecho

execute 'gdxxrw i=pivotdata.gdx o=%output%.xlsx @gdxxrw.rsp'

