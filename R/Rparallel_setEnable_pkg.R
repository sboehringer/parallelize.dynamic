#
#	Rparallel_setEnable_pkg.R
#Mon Mar 25 17:06:14 CET 2013

# enable/disable parallelize.dynamic functionality during package use

#' @title Turn on/off the parallelization mechanism
#' 
#' This function changes the definition of
#' Apply/Lapply/Sapply/parallelize/parallelize_call to turn the parallelization
#' mechanism on or off.
#' 
#' This function changes the definition of
#' Apply/Lapply/Sapply/parallelize/parallelize_call to turn the parallelization
#' mechanism on or off. The global environment is modified by a call to this
#' function.
#' 
#' @param state TRUE or FALSE to enable or disable parallelization
#' @return The returned value is undefined.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{parallelize}}, \code{\link{parallelize_call}}, \code{\link{parallelize_initialize}}
#' @examples
#' 
#' 	parallelize_setEnable(FALSE);
#' 	Lapply
#' 	parallelize_setEnable(TRUE);
#' 	Lapply
#' 
parallelize_setEnable = function(state) {
	sourceFile = if (!state) {
		system.file('Rscripts/Rparallel_functions_std.R', package = 'parallelize.dynamic')
	} else {
		system.file('Rscripts/Rparallel_functions_parallel.R', package = 'parallelize.dynamic')
	}
	Log(sourceFile, 1);
	source(sourceFile);
}
