clear
set more off
capture log close
log using "Result",text replace

//Make sure we are in the right directory.
use Dataset

outreg2 using Summaries,sum(log) tex replace drop(Date)

//To avoid the presence of gaps causing the regression not able to be done, we use ticks as the time.
gen tick = _n
tsset tick

//Generate variables just in case.
gen ln_gold = log(Gold)
gen ln_sp = log(SP500)
gen ln_oil = log(WTI)
gen ln_VIX = log(VIX)
rename K DateNum
replace DateNum = Date
//For financial market, the weekend is closed. So I select 21 days (around) 4 weeks to denote the monthly return.
gen dDate = DateNum-l21.DateNum

//Generate the first order differences to show the changes in percentages.
//Generate the result and times 30 to "normalize" it into 30-day return.
gen dln_gold = (ln_gold-l21.ln_gold)/dDate*30
gen dln_sp = (ln_sp-l21.ln_sp)/dDate*30
gen dln_oil = (ln_oil-l21.ln_oil)/dDate*30
gen dln_VIX = (ln_VIX-l21.ln_VIX)/dDate*30

//Generate differences just in case.
gen dTreasury = (Treasury-l21.Treasury)/dDate*30
gen ln_USINDEX = log(USINDEX)
gen dln_USINDEX = (ln_USINDEX-l21.ln_USINDEX)/dDate*30

//For first 21 entries, we don't have result.
drop if tick<21

//Generate dummys for recession.
//Recession dates according to NBER business cycle.
gen recession = 0
replace recession=1 if DateNum > 17503.5 & DateNum < 18050.5
//Here Recession is for creating a gray area for graphs.
gen Recession = 2000
gen EuroCrisis = 0
//Generate dummys for euro crisis.
gen euro = 0
replace euro = 1 if DateNum > 18383.5 & DateNum < 19355.5

//Visualization
//Visualize the price directly.
gr tw ((area Recession Date if recession==1,color(gs12) base(0))(line Gold Date),name(g1,replace) nodraw)
replace Recession = 4000
gr tw ((area Recession Date if recession==1,color(gs12) base(0))(line SP500 Date),name(g2,replace) nodraw)
replace Recession = 160
gr tw ((area Recession Date if recession==1,color(gs12) base(0))(line WTI Date),name(g3,replace) nodraw)
graph combine g1 g2 g3, col(3) xsize(20) ysize(6.5)
graph export Graphs1.png,height (1200) replace
//Export the graph as Graph1.png

//Visualize the change in returns.
replace Recession = 0.3
gr tw ((area Recession Date if recession==1,color(gs12) base(-0.25))(line dln_gold Date),name(g1,replace) nodraw)
replace Recession = 0.4
gr tw ((area Recession Date if recession==1,color(gs12) base(-0.4))(line dln_sp Date),name(g2,replace) nodraw)
replace Recession = 0.5
gr tw ((area Recession Date if recession==1,color(gs12) base(-0.75))(line dln_oil Date),name(g3,replace) nodraw)
graph combine g1 g2 g3, col(3) xsize(20) ysize(6.5)
graph export Graphs2.png,height (1200) replace 
//Export the graph as Graph2.png

//Visualize the control variables.
replace Recession = 6
gr tw ((area Recession Date if recession==1,color(gs12) base(-0.1))(line Treasury Date),name(g1,replace) nodraw)
replace Recession = 140
gr tw ((area Recession Date if recession==1,color(gs12) base(90))(line USINDEX Date),name(g2,replace) nodraw)
replace Recession = 12
gr tw ((area Recession Date if recession==1,color(gs12) base(3))(line Unemployment Date),name(g3,replace) nodraw)
replace Recession = 12
gr tw ((area Recession Date if recession==1,color(gs12) base(0))(line M2 Date),name(g4,replace) nodraw)
replace Recession = 6
gr tw ((area Recession Date if recession==1,color(gs12) base(-2))(line CPI Date),name(g5,replace) nodraw)
graph combine g1 g2 g3 g4 g5, col(3) xsize(20) ysize(6.5)
graph export Graphs3.png,height (1200) replace
//Export the graph as Graph3.png

//Test if the variables are stationary
//The daily returns of S&P 500
kpss dln_sp
dfgls dln_sp
dfuller dln_sp
//The daily returns of Gold
kpss dln_gold
dfgls dln_gold
dfuller dln_gold
//The daily returns of Crude Oil
kpss dln_oil
dfgls dln_oil
dfuller dln_oil
//Test the stationarity of control variables.
dfuller dTreasury
dfuller dln_USINDEX
dfuller Unemployment
dfuller M2
dfuller CPI

//Visualize the stationary control variables data.
replace Recession = 1
gr tw ((area Recession Date if recession==1,color(gs12) base(-2))(line dTreasury Date),name(g1,replace) nodraw)
replace Recession = 0.1
gr tw ((area Recession Date if recession==1,color(gs12) base(-0.05))(line dln_USINDEX Date),name(g2,replace) nodraw)
replace Recession = 12
gr tw ((area Recession Date if recession==1,color(gs12) base(3))(line Unemployment Date),name(g3,replace) nodraw)
replace Recession = 12
gr tw ((area Recession Date if recession==1,color(gs12) base(0))(line M2 Date),name(g4,replace) nodraw)
replace Recession = 6
gr tw ((area Recession Date if recession==1,color(gs12) base(-2))(line CPI Date),name(g5,replace) nodraw)
graph combine g1 g2 g3 g4 g5, col(3) xsize(20) ysize(6.5)
graph export Graphs4.png,height (1200) replace

//Test if ARCH effect exists before we run the ARCH regression.
reg dln_sp dTreasury dln_USINDEX CPI Unemployment M2
est store StockRet
archlm,lags(1)
reg dln_gold dTreasury dln_USINDEX CPI Unemployment M2
est store GoldRet
archlm,lags(1)
reg dln_oil dTreasury dln_USINDEX CPI Unemployment M2
est store OilRet
archlm,lags(1)
outreg2 [StockRet GoldRet OilRet] using LinearRet,tex replace

//Run the autoregression model for returns.
arch dln_sp dTreasury dln_USINDEX M2,ar(1 2 3)
est store AR_Stock
predict ret_sp_error,resid
arch ret_sp_error,arch(1) garch(1)
est store GARCH_Stock
predict ret_sp_vol,variance

arch dln_gold dTreasury dln_USINDEX M2,ar(1)
est store AR_Gold
predict ret_gold_error,resid
arch ret_gold_error,arch(1) garch(1)
est store GARCH_Gold
predict ret_gold_vol,variance

arch dln_oil dTreasury dln_USINDEX M2,ar(1 2)
est store AR_Oil
predict ret_oil_error,resid
arch ret_oil_error,arch(1) garch(1)
est store GARCH_Oil
predict ret_oil_vol, variance

outreg2 [AR_Gold GARCH_Gold AR_Oil GARCH_Oil AR_Stock GARCH_Stock] using GARCH,tex replace

//Visualize the volatility result for returns.
**Change the value of Recession for plotting the shade area for business cycle.
replace VIX = VIX/21
replace ret_sp_vol = sqrt(ret_sp_vol)*100
replace ret_gold_vol = sqrt(ret_gold_vol)*100
replace ret_oil_vol = sqrt(ret_oil_vol)*100
replace Recession = 8
gr tw ((area Recession Date if recession==1,color(gs12) base(0))(line VIX Date) (line ret_sp_vol Date),name(g1,replace) nodraw)
replace Recession = 8
gr tw ((area Recession Date if recession==1,color(gs12) base(0))(line VIX Date) (line ret_gold_vol Date),name(g2,replace) nodraw)
replace Recession = 8
gr tw ((area Recession Date if recession==1,color(gs12) base(0))(line VIX Date) (line ret_oil_vol Date),name(g3,replace) nodraw)
graph combine g1 g2 g3, col(3) xsize(20) ysize(6.5)
graph export Graphs5.png,height (1200) replace

gr tw ((line VIX Date) (line ret_sp_vol Date),name(g1,replace) nodraw)
gr tw ((line VIX Date) (line ret_gold_vol Date),name(g2,replace) nodraw)
replace Recession = 0.8
gr tw ((line VIX Date) (line ret_oil_vol Date),name(g3,replace) nodraw)
graph combine g1 g2 g3, col(3) xsize(20) ysize(6.5)
graph export Graphs14.png,height (1200) replace

//Calculate the dynamic correlation of returns.
**Change the value of Recession for plotting the shade area for business cycle.
mgarch dcc (dln_sp dln_gold = L.dln_sp L.dln_gold dTreasury dln_USINDEX M2,noconstant),arch(1) garch(1)
est store DCC_Return_Stock_Gold
test _b[/Adjustment:lambda1] = _b[/Adjustment:lambda2] = 0
predict GS*, correlation
replace Recession = 0.6
gr tw ((area Recession Date if recession==1,color(gs12) base(-0.5))(line GS_dln_gold_dln_sp Date),name(g4,replace) nodraw)
mgarch dcc (dln_sp dln_oil = L.dln_sp L.dln_oil dTreasury dln_USINDEX M2,noconstant),arch(1) garch(1)
est store DCC_Return_Stock_Oil
test _b[/Adjustment:lambda1] = _b[/Adjustment:lambda2] = 0
predict OS*, correlation
replace Recession = 1
gr tw ((area Recession Date if recession==1,color(gs12) base(-0.5))(line OS_dln_oil_dln_sp Date),name(g5,replace) nodraw)
mgarch dcc (dln_sp dln_VIX = L.dln_sp L.dln_VIX dTreasury dln_USINDEX M2,noconstant),arch(1) garch(1)
est store DCC_Return_VIX_Stock
test _b[/Adjustment:lambda1] = _b[/Adjustment:lambda2] = 0
replace Recession = 0.5
predict VS*,correlation
gr tw ((area Recession Date if recession==1,color(gs12) base(-1))(line VS_dln_VIX_dln_sp Date),name(g6,replace) nodraw)
replace Recession = 0.6
mgarch dcc (dln_gold dln_oil = l.dln_gold l.dln_oil),arch(1) garch(1)
predict GO*,correlation
gr tw ((area Recession Date if recession==1,color(gs12) base(-0.2))(line GO_dln_oil_dln_gold Date),name(g7,replace) nodraw)
est store DCC_Return_Gold_Oil
graph combine g4 g5 g6 g7, col(4) xsize(20) ysize(5)
graph export Graphs6.png,height (1200) replace
outreg2 [DCC_Return_Stock_Gold DCC_Return_Stock_Oil DCC_Return_Gold_Oil] using DCC_Return,tex replace

gr tw (line GS_dln_gold_dln_sp Date,name(g4,replace) nodraw)
gr tw (line OS_dln_oil_dln_sp Date,name(g5,replace) nodraw)
gr tw (line VS_dln_VIX_dln_sp Date,name(g6,replace) nodraw)
gr tw (line GO_dln_oil_dln_gold Date,name(g7,replace) nodraw)
graph combine g4 g5 g6 g7,col(4) xsize(20) ysize(5)
graph export Graphs13.png,height(1200) replace

//Consider European Sovereign Debt Crisis
**Change the value of Recession and EuroCrisis for plotting the shade area for business cycle.
replace Recession = 0.6
replace EuroCrisis = 0.6
gr tw ((area Recession Date if recession==1,color(gs12) base(-0.5))(area EuroCrisis Date if euro==1,color(gs12) base(-0.5))(line (GS_dln_gold_dln_sp Date)),name(g4,replace) nodraw)
replace Recession = 1
replace EuroCrisis = 1
gr tw ((area Recession Date if recession==1,color(gs12) base(-0.5))(area EuroCrisis Date if euro==1,color(gs12) base(-0.5))(line OS_dln_oil_dln_sp Date),name(g5,replace) nodraw)
replace Recession = 0.5
replace EuroCrisis = 0.5
gr tw ((area Recession Date if recession==1,color(gs12) base(-1))(area EuroCrisis Date if euro==1,color(gs12) base(-1))(line VS_dln_VIX_dln_sp Date),name(g6,replace) nodraw)
replace Recession = 0.6
replace EuroCrisis = 0.6
gr tw ((area Recession Date if recession==1,color(gs12) base(-0.2))(area EuroCrisis Date if euro==1,color(gs12) base(-1))(line GO_dln_oil_dln_gold Date),name(g7,replace) nodraw)
graph combine g4 g5 g6, col(3) xsize(20) ysize(6.5)
graph export Graphs7.png,height (1200) replace

pac ln_gold,name(g1,replace) nodraw
pac ln_oil,name(g2,replace) nodraw
pac ln_sp,name(g3,replace) nodraw
graph combine g1 g2 g3,col(3) xsize(20) ysize(6.5)
graph export Graphs8.png,height(1200) replace

//Test on the relationship between volatility and increment of dynamic correlation.
tsset tick
sum OS_dln_oil_dln_sp GS_dln_gold_dln_sp
gen difference_OS = abs(OS_dln_oil_dln_sp-L.OS_dln_oil_dln_sp)
gen difference_GS = abs(GS_dln_gold_dln_sp-L.GS_dln_gold_dln_sp)
gen GS = abs(GS_dln_gold_dln_sp)
gen OS = abs(OS_dln_oil_dln_sp)
reg difference_GS ret_gold_vol
est store Gold_Corr
reg difference_GS ret_gold_vol l.difference_GS
est store Gold_Corr_AR
reg difference_GS ret_gold_vol ret_sp_vol l.difference_GS
est store Gold_Corr_SP
reg difference_OS ret_oil_vol
est store Oil_Corr
reg difference_OS ret_oil_vol l.difference_OS
est store Oil_Corr_AR
reg difference_OS ret_oil_vol ret_sp_vol l.difference_OS
est store Oil_Corr_SP
outreg2 [Gold_Corr Gold_Corr_AR Gold_Corr_SP Oil_Corr Oil_Corr_AR Oil_Corr_SP] using Coorelation,tex replace
gr tw (line (difference_GS Date),name(g1,replace) nodraw)
gr tw (line (difference_OS Date),name(g2,replace) nodraw)
graph combine g1 g2,col(3) xsize(20) ysize(6.5)
graph export Graphs12.png,height(1200) replace
dfgls difference_OS
dfgls difference_OS

//Do the causality test.
//Regression on the dummy variables.
gen post = 0
replace post = 1 if DateNum > 18050.5
reg GS_dln_gold_dln_sp i.recession i.post
outreg2 using Causality,tex
reg OS_dln_oil_dln_sp i.recession i.post
outreg2 using Causality,tex
reg VS_dln_VIX_dln_sp i.recession i.post
outreg2 using Causality,tex

//Still a safe haven during and post recession?
replace VIX = VIX*3
reg dln_sp VIX dTreasury dln_USINDEX M2
outreg2 using SafeHaven,tex
reg dln_sp VIX dTreasury dln_USINDEX M2 i.recession i.post
outreg2 using SafeHaven,tex
reg dln_gold VIX dTreasury dln_USINDEX M2
outreg2 using SafeHaven,tex
reg dln_gold VIX dTreasury dln_USINDEX M2 i.recession i.post
outreg2 using SafeHaven,tex
reg dln_gold ret_sp_vol dTreasury dln_USINDEX M2 i.recession i.post
outreg2 using SafeHaven,tex
reg dln_oil VIX dTreasury dln_USINDEX M2
outreg2 using SafeHaven,tex
reg dln_oil VIX dTreasury dln_USINDEX M2 i.recession i.post
outreg2 using SafeHaven,tex
reg dln_oil ret_sp_vol dTreasury dln_USINDEX M2 i.recession i.post
outreg2 using SafeHaven,tex

//Single cutoff Regression Discontinuity (use the recession days as discontinuity)
drop if recession == 1
savesome Date DateNum GS_dln_gold_dln_sp OS_dln_oil_dln_sp using RDData,replace
replace tick = _n
rdplot GS_dln_gold_dln_sp tick,c(1413.5) mbw(1400)
graph rename Graph g1,replace
rdplot OS_dln_oil_dln_sp tick,c(1413.5) mbw(1400)
graph rename Graph g2,replace
graph combine g1 g2,col(2) xsize(20) ysize(6.5) title("One Month")
graph save GraphsOM,replace
graph export Graphs9.png,height (1200) replace

//Multi cutoff Regression Discontinuity (use euro sovereign crisis as the other discontinuity)
drop if euro == 1
savesome Date DateNum GS_dln_gold_dln_sp OS_dln_oil_dln_sp using RDMCData,replace
replace tick = _n
gen c = 1413.5
replace c = 1631.5 if tick >= 1526

rdplot GS_dln_gold_dln_sp tick,c(1413.5) mbw(1400)
graph rename Graph g1,replace
rdplot OS_dln_oil_dln_sp tick,c(1413.5) mbw(1400)
graph rename Graph g2,replace
graph combine g1 g2,col(2) xsize(20) ysize(6.5)
graph export Graphs10.png,height (1200) replace

rdmc GS_dln_gold_dln_sp tick,cvar(c)
rdmc OS_dln_oil_dln_sp tick,cvar(c)

rdmcplot GS_dln_gold_dln_sp tick,cvar(c)
graph rename Graph g3,replace
rdmcplot OS_dln_oil_dln_sp tick,cvar(c)
graph rename Graph g4,replace
graph combine g3 g4,col(2) xsize(20) ysize(6.5)
graph export Graphs11.png,height(1200) replace


log close
