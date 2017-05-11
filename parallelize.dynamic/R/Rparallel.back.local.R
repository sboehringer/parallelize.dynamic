#
#	Rparallel.back.local.R
#Tue Jun 23 14:14:40 2015

#
#	<p> local backend
#

#' Class \code{"ParallelizeBackendLocal"}
#'
#' Backend class implementing local execution.
#'
#' @name ParallelizeBackendLocal
#' @rdname ParallelizeBackendLocal-class
#' @aliases ParallelizeBackendLocal-class
#' initialize,ParallelizeBackendLocal-method
#' lapply_dispatchFinalize,ParallelizeBackendLocal-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ParallelizeBackendLocal", config, ...)}.
#' During normal operation you do not have to create objects of this class yourself. Instead, \code{parallelize_initialize} will create such instances for you. The class can be configured with the following field in the \code{Lapply_config} argument of \code{parallelize_initialize}.
#' \itemize{
#'   \item freezerClass: defaults to \code{LapplyPersistentFreezer}
#'   \item stateDir: directory to store results from computations. This location is passed to \code{LapplyPersistentFreezer}. If temporary behavior is desired it can be set to: \code{sprintf('\%s/tmp/remote', tempdir())}.
#'    \item sourceFiles: a vector of files to be sourced prior to parallel execution
#' }
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\linkS4class{ParallelizeBackend}},
#'   \code{\linkS4class{ParallelizeBackendSnow}},
#'   \code{\linkS4class{ParallelizeBackendOGSremote}}
#' @keywords classes
#' @examples
#' 
#' showClass("ParallelizeBackendLocal")
#' 
setClass('ParallelizeBackendLocal',
	contains = 'ParallelizeBackend',
	representation = list(),
	prototype = list()
);
setMethod('initialize', 'ParallelizeBackendLocal', function(.Object, config, ...) {
	.Object = callNextMethod(.Object, config = config, ...);
	Dir.create(config$stateDir, recursive = T);
	# 24.7.2013 -> use stateDir instead
	.Object
});
setMethod('lapply_dispatchFinalize', 'ParallelizeBackendLocal', function(self) { 
	Log(sprintf('Local dispatch, tmp: %s', self@config$stateDir), 5);
	parallelize_setEnable(F);
	parallelize.dynamic:::Lapply_setConfigValue(
		activeDictionary = parallelize.dynamic:::Lapply_getConfig()$backend);
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply__ = get('Lapply__', envir = parallelize_env);
	freezer = Lapply_executionState__$currentFreezer();
	r = lapply(1:freezer$Ncalls(), function(i) {
		mycall = freezer$call(i);
		mycall = callEvalArgs(mycall);
		r = Do.call(mycall$f, mycall$args, envir = mycall$envir);
		freezer$pushResults(r);
		r
	});
	freezer$finalizeResults();
	save(r, file = sprintf('%s/sequence-%d.RData', self@config$stateDir, Lapply__$sequence));
	#parallelize_setEnable(T);
	NULL
});

#' groupd version of local backend
setClass('ParallelizeBackendLocalGrouped',
	contains = 'ParallelizeBackendLocal',
	representation = list(),
	prototype = list()
);
setMethod('initialize', 'ParallelizeBackendLocalGrouped', function(.Object, config, ...) {
	.Object = callNextMethod(.Object, config = config, ...);
	.Object
});
setMethod('lapply_dispatchFinalize', 'ParallelizeBackendLocalGrouped', function(self) { 
	Log(sprintf('Local dispatch, tmp: %s', self@config$stateDir), 5);
	o = Lapply_getConfig();
	Lapply_setConfigValue(activeDictionary = o$backend);
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply__ = get('Lapply__', envir = parallelize_env);
	freezer = Lapply_executionState__$currentFreezer();
	idcs = splitListIndcs(freezer$Ncalls(), o$parallel_count);

	r = lapply(1:nrow(idcs), function(job_index__) {
		mycalls = freezer$callRange(idcs[job_index__, 1], idcs[job_index__, 2]);
		# force evaluation/restriction of environment
		mycalls = lapply(mycalls, function(lc) {
			if (self@config$copy_environments) {
				lc$fct = environment_eval(lc$fct, functions = FALSE, recursive = FALSE);
				lc$arguments = freezeObjectsList(lc$arguments);
			}
			lc
		});

		r0 = lapply(mycalls, function(lc) {
			parallelize_setEnable(F);
			lapply(lc$elements, function(e) {
			try(do.call(lc$fct, c(list(e), as.list(lc$arguments))))
			})
		});
		r = c(results = r0, list(from = idcs[job_index__, 1], to = idcs[job_index__, 2]));
		r
	});
	freezer$pushResults(r);
	#freezer$unlistResults();
	freezer$finalizeResults();
	#save(r, file = sprintf('%s/sequence-%d.RData', self@config$stateDir, Lapply__$sequence));
	parallelize_setEnable(T);
	NULL
});

