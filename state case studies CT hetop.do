* estimate CT means and SDs with HETOP

clear all

* SBAC

import excel "state case studies CT hetop data.xlsx" , sheet("Sheet1") cellra(A1:R5) firstrow

keep Subject Gender nlevel?

gen grpid = _n

foreach sub in "ELA" "Math" {
	preserve
	keep if Subject=="`sub'"
	hetop grpid nlevel , numcats(4) modtype(hetop) gaps save("`sub'", star)
	mat G = e(G)
	local d_`sub' = G[1,2]
	restore
}

di `d_ELA'
di `d_Math'

* SAT

import excel "state case studies.xlsx" , sheet("CT_SAT") cellra(A1:R5) firstrow clear

keep Subject Gender nlevel?

gen grpid = _n

foreach sub in "ELA" "Math" {
	preserve
	keep if Subject=="`sub'"
	hetop grpid nlevel , numcats(4) modtype(hetop) gaps save("`sub'", star)
	mat G = e(G)
	local d_`sub' = G[1,2]
	restore
}

di `d_ELA'
di `d_Math'

