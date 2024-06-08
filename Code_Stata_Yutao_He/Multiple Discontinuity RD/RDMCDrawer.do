clear
use RDMC
replace DateNum = Date
sort DateNum
gen Recession = 0.4
gen EuroCrisis = 0.4
gen rece = 0
replace rece = 1 if DateNum>17502.5 & DateNum < 18050.2
gen euro = 0
replace euro = 1 if DateNum>18385.5 & DateNum < 19358.5
gr tw ((area Recession Date if rece==1,color(gs12) base(-0.4) title("Gold"))(area EuroCrisis Date if euro==1,color(gs12) base(-0.4))(scatter Gold_Act Date,color(eltblue))(line Gold_Fit Date,cmissing(n) color(gs1) lw(0.4)),name(g1,replace))
replace Recession =1
replace EuroCrisis = 1
gr tw ((area Recession Date if rece==1,color(gs12) base(-0.5) title("Oil"))(area EuroCrisis Date if euro==1,color(gs12) base(-0.5))(scatter Oil_Act Date,color(eltblue))(line Oil_Fit Date,cmissing(n) color(gs1) lw(0.4)),name(g2,replace))
graph combine g1 g2,col(2) xsize(20) ysize(7) title("Multi-Cutoff Regression Discontinuity") scale(0.6) imargin(small)
graph export RDMC_US.png,height(1200) replace
