clear
use RD
drop DateNum
gen DateNum = Date
sort DateNum
gen rece = 0
replace rece = 1 if DateNum > 17503.5 & DateNum < 18050.5
gen Recession = 0.4
gr tw ((area Recession Date if rece == 1,base(-0.4) legend(off) color(gs12) title("Gold"))(scatter Actual Date,color(eltblue))(line Fitted Date,cmissing(n) lw(0.4) color(gs1)),name(g1,replace))
replace Recession = 1
gr tw ((area Recession Date if rece == 1,base(-0.5) legend(off) color(gs12) title("Oil"))(scatter Actual_O Date,color(eltblue))(line Fitted_O Date,cmissing(n) lw(0.4) color(gs1)),name(g2,replace))
