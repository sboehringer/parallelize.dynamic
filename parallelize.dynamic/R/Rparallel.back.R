#
#	Rparallel.back.R
#Sun Jul 15 10:48:17 UTC 2012

#
#	<p> general documentation
#

# online vs offline mode: online means that rampUps are computed in one go, whereas offline backends compute one rampUp for a single invocation
# delegating backends are backends that forward execution to another offline backend
#	example: OGSremote -> OGS

#
#	<p> generic interface
#

setGeneric("isSynchroneous", function(self) standardGeneric("isSynchroneous"));
setGeneric("lapply_dispatch",
	function(self, l_parallelize, f_parallelize, ...)standardGeneric("lapply_dispatch"));
setGeneric("lapply_dispatchFinalize", function(self) standardGeneric("lapply_dispatchFinalize"));
setGeneric("lapply_results", function(self, r) standardGeneric("lapply_results"));
# parallelize function as customized by the backend
setGeneric('parallelize_backend', function(self, call_) standardGeneric('parallelize_backend'));
#	scheduling
setGeneric('initScheduling',
	function(self, call_) standardGeneric('initScheduling'));
setGeneric('performParallelizationStep',
	function(self, call_, Lapply_config) standardGeneric('performParallelizationStep'));
setGeneric('finalizeParallelization',
	function(self, r) standardGeneric('finalizeParallelization'));

setGeneric('saveParallelizationState',
	function(self) standardGeneric('saveParallelizationState'));
setGeneric('restoreParallelizationState',
	function(self) standardGeneric('restoreParallelizationState'));
setGeneric('scheduleNextParallelization',
	function(self, call_) standardGeneric('scheduleNextParallelization'));
setGeneric('pollParallelization',
	function(self, options) standardGeneric('pollParallelization'));
setGeneric('getResult',
	function(self) standardGeneric('getResult'));
setGeneric('backendCall',
	function(self) standardGeneric('backendCall'));

#
#	<p> default class
#

#' Class \code{"ParallelizeBackend"}
#' 
#' Base class for parallelization backends. Please refer to documentation of the methods
#' individually for more complete documentation.
#' 
#' 
#' @name ParallelizeBackend-class
#' @aliases ParallelizeBackend-class
#' finalizeParallelization,ParallelizeBackend-method
#' getResult,ParallelizeBackend-method initialize,ParallelizeBackend-method
#' initScheduling,ParallelizeBackend-method
#' isSynchroneous,ParallelizeBackend-method
#' lapply_dispatchFinalize,ParallelizeBackend-method
#' lapply_dispatch,ParallelizeBackend-method
#' lapply_results,ParallelizeBackend-method
#' parallelize_backend,ParallelizeBackend-method
#' performParallelizationStep,ParallelizeBackend-method
#' pollParallelization,ParallelizeBackend-method
#' restoreParallelizationState,ParallelizeBackend-method
#' saveParallelizationState,ParallelizeBackend-method
#' scheduleNextParallelization,ParallelizeBackend-method
#' finalizeParallelization getResult initialize initScheduling isSynchroneous
#' lapply_dispatchFinalize lapply_dispatch lapply_results parallelize_backend
#' performParallelizationStep pollParallelization restoreParallelizationState
#' saveParallelizationState scheduleNextParallelization
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ParallelizeBackend", config, signature)}. %% ~~ describe objects
#' here ~~ Config is a list containing parameters and signature is a character
#' string that uniquely identifies the computation that is to be parallelized.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso %% ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' \code{\linkS4class{ParallelizeBackendLocal}},
#' \code{\linkS4class{ParallelizeBackendSnow}},
#' \code{\linkS4class{ParallelizeBackendOGSremote}}
#' @keywords classes
#' @examples
#' 
#' showClass("ParallelizeBackend")
#' 
setClass('ParallelizeBackend',
	representation = list(
		config = 'list', offline = 'logical', signature = 'character'
	),
	prototype = list(config = list(), offline = F, signature = '')
);
setMethod('initialize', 'ParallelizeBackend', function(.Object, config = list(), signature = '') {
	.Object@config = config;
	.Object@signature = signature;
	if (!is.null(config$offline)) .Object@offline = config$offline;
	.Object
});

#
#	<p> default class implementation
#

setMethod('isSynchroneous', 'ParallelizeBackend', function(self) { return(T); });
# use envir__ to evaluate ...
setMethod('lapply_dispatch', 'ParallelizeBackend', function(self, l_parallelize, f_parallelize, ...,
	envir__ = parent.frame()) { 
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply__ = get('Lapply__', envir = parallelize_env);
	freezer = Lapply_executionState__$currentFreezer();
	args = eval(list(...), envir = envir__);
	Log(sprintf('Pushing @ depth %d', Lapply__$getDepth()), 6);
	freezer$push(Lapply__$sequence,
		f_parallelize = f_parallelize, l_parallelize = l_parallelize, args_parallelize = args);
	NULL
});
setMethod('lapply_dispatchFinalize', 'ParallelizeBackend', function(self) { 
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	freezer = Lapply_executionState__$currentFreezer();
	parallelize_setEnable(F);
	Lapply_setConfigValue(activeDictionary = Lapply_getConfig()$backend);
	r = lapply(1:freezer$Ncalls(), function(i) {
		call = freezer$call(i);
		#call = callEvalArgs(call);
		r = Do.call(call$f, call$args, envir = call$envir);
	});
	freezer$finalizeResults();
	parallelize_setEnable(T);
	r
});
setMethod('lapply_results', 'ParallelizeBackend', function(self, r) { 
	stop('ParallelizeBackend: result retrieval only supported for asynchroneous backends.');
});
setMethod('parallelize_backend', 'ParallelizeBackend', function(self, call_) {
	with(Lapply_getConfig(), if (self@offline) {
		parallelizeOfflineStep(call_, Lapply_config = Lapply_getConfig());
	} else {
		Lapply_initialze_probing();
		for (i in 1:parallel_stack) {
			r = performParallelizationStep(self, call_, Lapply_config = Lapply_getConfig());
			if (all(class(r) != 'Lapply_error')) break;
		}
		r
	});
});
setMethod('performParallelizationStep', 'ParallelizeBackend', function(self, call_, Lapply_config) {
	parallelizeStep(call_, Lapply_config = Lapply_config);
});
setMethod('finalizeParallelization', 'ParallelizeBackend', function(self, r)r);
setMethod('pollParallelization', 'ParallelizeBackend',
	function(self, options = list())list(continue = F, message = '')
);

#
#		<p> parallelization state
#

# <A> running in '.' will not create sub-directory
#	used by remoting computations and already changing to remote stateDir
parallelizationStatePath = function(self, tag = '', ..., ext = '.RData') {
	tagStr = sprintf(tag, ...);
	path = if (self@config$stateDir == '.')
		sprintf('./%s%s', tagStr, ext) else
		sprintf('%s/parallelization_%s/%s%s', self@config$stateDir, self@signature, tagStr, ext);
	Log(sprintf('parallelization path: %s', path), 7);
	path
}
parallelizationStateObjects = c(
	'Lapply_globalConfig__', 'Lapply__', 'Lapply_executionState__', 'Lapply_backend__'
);
saveParallelizationStatePath = function(self, path = NULL) {
	if (is.null(path)) path = parallelizationStatePath(self, 'state');
	Log(sprintf('Saving state to %s', path), 5);
	#parallelizationStateObjects = names(as.list(parallelize_env));
	#Save(parallelizationStateObjects, file = path, symbolsAsVectors = T, envir = parallelize_env);
	Save('parallelize_env', file = path, symbolsAsVectors = T, envir = .GlobalEnv);
}
restoreParallelizationStatePath = function(self, path = NULL) {
	if (is.null(path)) path = parallelizationStatePath(self, 'state');
	#Load(file = path, envir = parallelize_env);
	Load(file = path, envir = .GlobalEnv);
}

setMethod('initScheduling', 'ParallelizeBackend', function(self, call_) {
	stateDir = parallelizationStatePath(self, '', ext = '');
	Log(sprintf('State dir: %s', stateDir), 5);
	Dir.create(stateDir, recursive = T);
	saveParallelizationStatePath(self);
});
setMethod('saveParallelizationState', 'ParallelizeBackend', function(self) {
	saveParallelizationStatePath(self);
});
setMethod('restoreParallelizationState', 'ParallelizeBackend', function(self) {
	restoreParallelizationStatePath(self);
});
setMethod('scheduleNextParallelization', 'ParallelizeBackend', function(self, call_) {
	NULL
});
setMethod('getResult', 'ParallelizeBackend', function(self) {
	if (self@config$doSaveResult)
		r = get(Load(file = parallelizationStatePath(self, 'result'))[1]) else
		stop(sprintf('result was not saved for signature %s', self@signature));
});

setMethod('backendCall', 'ParallelizeBackend', function(self) {
	idcs = splitListIndcs(freezer$Ncalls(), c$parallel_count);

# <i><!>
	backendCall = function(listcalls) {
		parallelize_setEnable(F);
		parallelize.dynamic:::Lapply_setConfigValue(
			activeDictionary = parallelize.dynamic:::Lapply_getConfig()$backend);
		lapply(listcalls, function(lc) {
			lapply(lc$elements, function(e)
				try(do.call(lc$fct, c(list(e), lc$arguments)))
			)
		})
	}
});

