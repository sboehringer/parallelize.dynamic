#
#	Rparallel_setEnable_standalone.R
#Mon Mar 25 17:06:14 CET 2013

# enable/disable parallelize.dynamic functionality during standalone use

parallelize_setEnable = function(state) {
	if (!state) {
		assign('Lapply', lapply, envir = .GlobalEnv);
		assign('Sapply', sapply, envir = .GlobalEnv);
		assign('Apply', apply, envir = .GlobalEnv);
		assign('parallelize', parallelize_dummy, envir = .GlobalEnv);
		assign('parallelize_call', parallelize_call_dummy, envir = .GlobalEnv);
	} else {
		assign('Lapply', Lapply_backup, envir = .GlobalEnv);
		assign('Sapply', Sapply_backup, envir = .GlobalEnv);
		assign('Apply', Apply_backup, envir = .GlobalEnv);
		assign('parallelize', parallelize_backup, envir = .GlobalEnv);
		assign('parallelize_call', parallelize_call_backup, envir = .GlobalEnv);
	}
}

#
#	<p> initialize
#

#parallelize_setEnable(F);
