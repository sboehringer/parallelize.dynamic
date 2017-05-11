#
#	Rparallel.back.snow.R
#Tue Jun 23 14:16:39 2015

#
#	<p> SNOW execution
#

#' Class \code{"ParallelizeBackendSnow"}
#' 
#' Backend class for parallelization on SNOW clusters
#' 
#' 
#' @name ParallelizeBackendSnow-class
#' @rdname ParallelizeBackendSnow-class
#' @aliases ParallelizeBackendSnow-class
#' initialize,ParallelizeBackendSnow-method
#' lapply_dispatchFinalize,ParallelizeBackendSnow-method
#' @docType class
#'
#' @section Objects from the Class: Objects can be created by calls of the form
#'	\code{new("ParallelizeBackendSnow", config, ...)}.
#' During normal operation you do not have to create objects of this class yourself. Instead, \code{parallelize_initialize} will create such instances for you. The class can be configured with the following field in the \code{Lapply_config} argument of \code{parallelize_initialize}.
#' \itemize{
#'   \item freezerClass: defaults to \code{LapplyPersistentFreezer}
#'   \item stateDir: directory to store results from computations. This location is passed to \code{LapplyPersistentFreezer}. If temporary behavior is desired it can be set to: \code{sprintf('\%s/tmp/remote', tempdir())}.
#'    \item sourceFiles: a vector of files to be sourced prior to parallel execution
#'    \item libraries: a vector of package names to be loaded prior to parallel execution
#'    \item localNodes: an integer number of how many parallel snow jobs are to be created. This should not be larger than the number of (logical) cores available as a general rule. A snow cluster is created using the \code{makePSOCKcluster}
#' }
#' You should be able to run a so-called \code{PSOCKS} cluster to use this package. See the \code{parallel} package for details (see also).
#'
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{makePSOCKcluster}},
#' \code{\linkS4class{ParallelizeBackend}},
#' \code{\linkS4class{ParallelizeBackendLocal}},
#' \code{\linkS4class{ParallelizeBackendSnow}},
#' \code{\linkS4class{ParallelizeBackendOGSremote}}
#' @keywords classes
#' @examples
#' 
#' showClass("ParallelizeBackendSnow")
#' 
#' Lapply_config = list(parallel_count = 24, backends = list(
#'     snow = list(
#'       localNodes = 8, sourceFiles = c('myScript.R'), libraries = c('boot')
#'     )
#' );
setClass('ParallelizeBackendSnow',
	contains = 'ParallelizeBackend',
	representation = list(),
	prototype = list()
);
setMethod('initialize', 'ParallelizeBackendSnow', function(.Object, config, ...) {
	.Object = callNextMethod(.Object, config = config, ...);
	args = List_(config[c('sourceFiles', 'localNodes', 'splitN', 'libraries')], rm.null = T);
	args$libraries = c(args$libraries, 'parallelize.dynamic');
	#args = c(args, list(evalEnvironment = T));
	do.call('specifyCluster', args);
	.Object
});
setMethod('lapply_dispatchFinalize', 'ParallelizeBackendSnow', function(self) { 
	Log(sprintf('Snow dispatch, tmp: %s', self@config$stateDir), 5);
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	freezer = Lapply_executionState__$currentFreezer();
	calls = freezer$getCalls();
	Log(sprintf('Snow dispatch: %d calls', length(calls)), 5);
# 	calls = lapply(calls, function(call) {
# 		call$fct = environment_eval(call$fct, functions = T);
# 		call
# 	});
	r = clapply(calls, function(call) {
		parallelize_setEnable(F);
		parallelize.dynamic:::Lapply_setConfigValue(
			activeDictionary = parallelize.dynamic:::Lapply_getConfig()$backend);
 		#sink('/tmp/debug', append = T);print(Lapply);sink();
		#call = callEvalArgs(call);
# 		sink('/tmp/debug', append = T);print(join(names(as.list(environment(call$fct)))));print(as.list(environment(as.list(environment(call$fct))$f)));print(str(call));sink();
		Do.call(call$fct, call$args)
	});
	freezer$pushResults(r);
	freezer$unlistResults();
	freezer$finalizeResults();
	NULL
});
