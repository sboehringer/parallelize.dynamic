#
#	Rparallel.R
#Fri Jun 15 12:29:14 CEST 2012
#source('Rparallel.back.R');
library('tools');

#' Automate parallelization of function calls by means of dynamic code analysis
#' 
#' Passing a given function name or a call to the parallelize/parallelize_call
#' functions analyses and executes the code, if possible in parallel. Parallel
#' code execution can be performed locally or on remote batch queuing systems.
#' 
#' \tabular{ll}{ Package: \tab parallelize.dynamic\cr Type: \tab Package\cr
#' Version: \tab 0.9\cr Date: \tab 2012-12-12\cr License: \tab LGPL\cr Depends:
#' \tab methods, tools, parallel\cr }
#' 
#' Use \code{parallelize_initialize} to set up a configuration for performing
#' parallel computations. After that, you can use \code{parallelize} and
#' \code{parallelize_call} to run a dynamic analysis on given functions or
#' function calls and execute parallel jobs resulting from this analysis. For
#' the remote backend OGSremote, the current implmentation is expected to break
#' on machines running operating systems from the Windows family on account of
#' dependencies on system calls. The local backend snow should work on Windows.
#' Patches are welcome to solve any Windows issues.
#' 
#' @name parallelize.dynamic-package
#' @aliases parallelize.dynamic-package parallelize.dynamic
#' @docType package
#' @author Stefan Böhringer
#' 
#' Maintainer: Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link[parallel:parallel-package]{parallel}}
#' @references R Journal article "Dynamic parallelization of R functions",
#' submitted
#' @keywords package

#' @export parallelize parallelize_call parallelize_initialize parallelize_declare parallelize_setEnable parallelize_internal tempcodefile readFile
#' @exportMethod finalizeParallelization
#' @exportMethod getResult
#' @exportMethod initialize
#' @exportMethod initScheduling
#' @exportMethod isSynchroneous
#' @exportMethod lapply_dispatchFinalize
#' @exportMethod lapply_dispatch
#' @exportMethod lapply_results
#' @exportMethod parallelize_backend
#' @exportMethod performParallelizationStep
#' @exportMethod pollParallelization
#' @exportMethod restoreParallelizationState
#' @exportMethod saveParallelizationState
#' @exportMethod scheduleNextParallelization
#' @exportClass LapplyExecutionState
#' @exportClass LapplyFreezer
#' @exportClass ParallelizeBackend
#' @exportClass ParallelizeBackendLocal
#' @exportClass ParallelizeBackendOGSremote
#' @exportClass ParallelizeBackendSnow
#' @exportClass LapplyRNGseedCapsule

#
#	<p> Lapply state reference classes
#

#' @title Class \code{"LapplyState"}
#' 
#' This class is the base class for classes reflecting different stages of the
#' parallelization process: probing and running.
#' 
#' 
#' @name LapplyState-class
#' @docType class
#' @section Extends:
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{LapplyExecutionState-class}},
#' \code{\link{LapplyRunState-class}} %% ~~or \code{\linkS4class{CLASSNAME}}
#' for links to other classes ~~~
#' @keywords classes
#' @examples
#' 
#' showClass("LapplyState")
#' 
LapplyStateClass = setRefClass('LapplyState',
	fields = list( sequence = 'numeric', depth = 'numeric',
	runMode = 'logical', probeMode = 'logical', max_depth = 'numeric'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		sequence <<- 0;
		depth <<- 0;
		runMode <<- F;
		probeMode <<- F;
		.self$initFields(...);
		.self
	},
	# depth is defined by the number of times Lapply is recursively called
	depthInc = function() { depth <<- depth + 1; },
	depthDec = function() { depth <<- depth - 1; },
	# sequence is defined by the number of Lapplys that were started no matter how deeply nested
	sequenceInc = function() { sequence <<- sequence + 1; },
	sequenceDec = function() { sequence <<- sequence - 1; },
	isEqualTo = function(s) { depth == s$depth && sequence == s$sequence }
	#
	#	</p> methods
	#
	)
);
LapplyStateClass$accessors(names(LapplyStateClass$fields()));

#' Class \code{"LapplyProbeState"}
#' 
#' This subclass of \code{"\linkS4class{LapplyState}"} tracks probing runs of
#' the parallelization process.
#' 
#' 
#' @name LapplyProbeState-class
#' @docType class
#' @note See documentation of \code{"\linkS4class{LapplyState}"}.
#' @section Extends: Class \code{"\linkS4class{LapplyState}"}, directly.
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{"\linkS4class{LapplyState}"}
#' @keywords classes
#' @examples
#' 
#' showClass("LapplyProbeState")
#' 
LapplyProbeStateClass = setRefClass('LapplyProbeState',
	fields = list( elements = 'list' ),
	contains = 'LapplyState',
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		# initialize super class
		callSuper(...);
		# defaults
		elements <<- list();
		# overwrites
		.self$setProbeMode(T);
		.self
	},
	pushElements = function(es) {
		elements[[depth]] <<- if (length(elements) < depth) es else c(elements[[depth]], es);
		NULL
	},
	elementsCount = function(atDepth) {
		count = sum(if (atDepth > length(elements)) elements[[length(elements)]] else elements[[atDepth]]);
		count
	}
	#
	#	</p> methods
	#
	)
);
LapplyProbeStateClass$accessors(names(LapplyProbeStateClass$fields()));

#' Class \code{"LapplyRunState"}
#' 
#' This subclass of \code{"\linkS4class{LapplyState}"} tracks running of code
#' of the parallelization process.
#' 
#' 
#' @name LapplyRunState-class
#' @docType class
#' @section Extends: Class \code{"\linkS4class{LapplyState}"}, directly.
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\linkS4class{LapplyState}}
#' @keywords classes
#' @examples
#' 
#' showClass("LapplyRunState")
#' 
LapplyRunStateClass = setRefClass('LapplyRunState',
	fields = list( chunkSize = 'numeric' ),
	contains = 'LapplyState',
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		# initialize super class
		callSuper(...);
		# overwrites
		.self$setRunMode(T);
		.self
	}
	#
	#	</p> methods
	#
	)
);
LapplyRunStateClass$accessors(names(LapplyRunStateClass$fields()));

#
#	<p> random number generation related classes
#

assignRandomSeed = function(v).Random.seed <<- v
LapplyRNGseedCapsuleClass = setRefClass('LapplyRNGseedCapsule',
	fields = list(
		type = 'character',
		seed = 'integer'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		.self$initFields(...);
		.self
	},
	# <!>
	# use .RandomSeed interface
	#
	store = function() {
		seed <<- get('.Random.seed', envir = .GlobalEnv);
		type <<- RNGkind();
		NULL
	},
	restore = function() {
		assignRandomSeed(c(NULL, seed))
		NULL
	}
	#	</p> methods
	#
	)
);
LapplyRNGseedCapsuleClass$accessors(names(LapplyRNGseedCapsuleClass$fields()));

#
#	<p> freezer classes
#


# Freezer class stores individual calls for list elements iterated overwrites
# Also the structure of Lapply calls is stored to be able to re-associate results
#	with Lapply calls
# The isolation of individual calls allows for re-shuffeling of bundling calls for final
#	execution

#' Class \code{"LapplyFreezer"}
#' 
#' This class encapsulates storage of calls and their results. Interaction with
#' this is done from backends and subclassing is only required if a new storage
#' mechanism of unevaluated calls or results thereof is needed. The end user
#' does not interact with this class.
#' 
#' 
#' @name LapplyFreezer-class
#' @docType class
#' @section Extends:
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{LapplyPersistentFreezer-class}} %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' @keywords classes
#' @examples
#' 
#' showClass("LapplyFreezer")
#' 
LapplyFreezerClass = setRefClass('LapplyFreezer',
	fields = list( slots = 'list', calls = 'list', results = 'list', copy_env = 'logical' ),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(...) {
		slots <<- list();
		calls <<- list();
		copy_env <<- FALSE;
		.self$initFields(...);
		.self
	},
	# depth is defined by the number of times Lapply is recursively called
	clear = function() {
		slots <<- list();
		calls <<- list();
		gc();
	},
	push = function(sequence, f_parallelize, l_parallelize, args_parallelize) {
		# store by seqeunce id from LapplyState object
		Log(sprintf('Freezing %d invocations @ seq %d.', length(l_parallelize), sequence), 5);
		slots[[as.character(sequence)]] <<- list(
			# definition of function called
			f = f_parallelize,
			# number of list elements iterated
			N = length(l_parallelize),
			# start index of result list in sequential order of calls
			start = sum(list.key(slots, 'N')) + 1
		);
		Log(sprintf('LapplyFreezer: copy environment: %s', copy_env), 5);
		calls <<- c(calls, lapply(l_parallelize, function(e) {
			callWithFunctionArgs(f__ = f_parallelize, args__ = c(list(e), args_parallelize),
				env_eval = copy_env)
		}));
		NULL
	},
	Ncalls = function()length(calls),
	call = function(i)calls[[i]],
	callRange = function(from, to)stop('callRange not implemented in LapplyFreezer'),

	# <p> results
	# for efficiency use nested structure
	pushResults = function(r){
		results[[length(results) + 1]] <<- r;
	},
	# in case results were stored in chunks (as lists) this method flattens the structure
	unlistResults = function() {
		results <<- unlist(results, recursive = F);
		NULL
	},
	finalizeResults = function() { NULL },
	# only to be called after finalizeResults
	resultsForSequence = function(s) {
		slot = slots[[as.character(s)]];
		results[slot$start : (slot$start + slot$N - 1)];
	}

	#
	#	</p> methods
	#
	)
);
LapplyFreezerClass$accessors(names(LapplyFreezerClass$fields()));

#' Class \code{"LapplyPersistentFreezer"}
#' 
#' Subclass of \code{LapplyFreezer} that stores results on disk. See
#' \code{LapplyFreezer} for more documentation.
#' 
#' 
#' @name LapplyPersistentFreezer-class
#' @docType class
#' @section Extends: Class \code{"\linkS4class{LapplyFreezer}"}, directly.
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{LapplyFreezer-class}}
#' @keywords classes
#' @examples
#' 
#' showClass("LapplyPersistentFreezer")
#' 
LapplyPersistentFreezerClass = setRefClass('LapplyPersistentFreezer',
	contains = 'LapplyFreezer',
	fields = list(),
	methods = list(
	finalizeResults = function() {
		callSuper();
	},
	resultsForSequence = function(s) {
		slot = slots[[as.character(s)]];
		seqCalls = slot$start : (slot$start + slot$N - 1);
		# iterate all chunks in current rampUp (== length(results))
		r = lapply(results[[length(results)]], function(r) {
			i = intersect(r$from:r$to, seqCalls);
			r1 = if (length(i) > 0) {
				r0 = frozenCallResults(r$file);
				# do we have to skip portions of current list?
				from = max(slot$start - r$from + 1, 1);
				# how much is left to grab?
				to = from + min(r$to - r$from + 1 - (from - 1), slot$N - max(r$from - slot$start, 0)) - 1;
				r0 = subListFromRaggedLists(r0, from = from, to = to);
				r0
			} else NULL;
			r1
		});
		r = r[!sapply(r, is.null)];
		r = unlist.n(r, 1);
		r
	}

	)
);

LapplyGroupingFreezerClass = setRefClass('LapplyGroupingFreezer',
	contains = 'LapplyPersistentFreezer',
	methods = list(
	#
	#	<p> methods
	#
	push = function(sequence, f, l, args) {
		# store by seqeunce id from LapplyState object
		Log(sprintf('Freezing %d invocations @ seq %d.', length(l), sequence), 5);
		slots[[as.character(sequence)]] <<- list(
			# definition of function called
			f = f,
			# number of list elements iterated
			N = length(l),
			# start index of result list in sequential order of calls
			start = sum(list.key(slots, 'N')) + 1
		);
		calls <<- c(calls, list(list(elements = l, fct = f, arguments = args)));
		NULL
	},
	call = function(i) {
		# length of groups lapply calls
		Nsegments = sapply(calls, function(e)length(e$elements));
		NsegmentsCS = c(0, cumsum(Nsegments));
		segment = rev(which(i > NsegmentsCS))[1];
		mycall = calls[[segment]];

		callWithFunctionArgs(mycall$fct,
			c(mycall$elements[i - NsegmentsCS[segment]], mycall$arguments, env_eval = copy_env)
		)
	},
	callRange = function(from, to){
		# length of groups lapply calls
		Nsegments = sapply(calls, function(e)length(e$elements));
		sl = subListFromRaggedIdcs(Nsegments, from, to);
		r = lapply(sl, function(e){
			lc = calls[[e$segment]];		# list-call
			r = list(
				elements = lc$elements[e$range$from: e$range$to], fct = lc$fct, arguments = lc$arguments
			);
			r
		});
		r
	},
	Ncalls = function() {
		sum(sapply(calls, function(e)length(e$elements)))
	},
	getCalls = function() {
		r = lapply(1:self$Ncalls(), function(i)self$call(i));
		r
	}
 	, resultsForSequence = function(s) {
 		#r = unlist.n(callSuper(s), 1);
 		r = callSuper(s);
		r
 	}
	#
	#	</p> methods
	#
	)
)
# The sentinel stack records entry points into parallelization for the different sequential rampUps
#	occuring during parallelization the ramp down of a given Lapply is equivalent to the rampUp of
#	the ensueing parallelization
# As probing occurs for successively deeper levels, the first sequence number for a given level is stored
#	write-once and will represent the first lapply-loop to parallelize
#	As the depth of the rampDown is unclear, sequenceStop will be updated to represent the most current
#	sequence number of ongoing Lapply loops
# Recovering will than happen between sequence and sequenceStop at the recorded depth

#' Class \code{"LapplyExecutionState"}
#' 
#' An instance of this class reflects the entire lifetime of a dynamic
#' parallelization.
#' 
#' 
#' @name LapplyExecutionState-class
#' @docType class
#' @section Extends:
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Stefan Böhringer
#' 
#' Maintainer: Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{LapplyFreezer-class}}, \code{\link{LapplyState-class}}
#' @keywords classes
#' @examples
#' 
#' showClass("LapplyExecutionState")
#' 
LapplyExecutionStateClass = setRefClass('LapplyExecutionState',
	fields = list(
		# execution state
		sequenceNos = 'list', rampUp = 'numeric',
		# results
		freezerClass = 'character', freezers = 'list', copy_environments = 'logical',
		# random numbers
		randomSeed = 'list'
	),
	methods = list(
	#
	#	<p> methods
	#
	initialize = function(freezerClass = 'LapplyFreezer', ...) {
		# initialize super class
		copy_environments <<- TRUE;
		callSuper(freezerClass = freezerClass, ...);
		# defaults
		sequenceNos <<- list();
		rampUp <<- 1;
		# copy current random seed
		.self$storeRandomSeed();	#randomSeed <<- list();
		# overwrites
		.self
	},
	# add a new sentinel in the parallelization stack
	addSentinel = function() {
		sequenceNos[[length(sequenceNos) + 1]] <<- list(depth = -1, sequence = -1, sequenceStop = -1);
	},
	# update only last element in stack (previous sentinels are fixed)
	# only record first sequence no for given rampUp and depth
	pushSequenceForRampUp = function(sequenceNo, depth) {
		N = length(sequenceNos);
		# if we probe for bigger depthes we re-record the starting sequence
		#	as parallelization will happen at that deeper level
		if (sequenceNos[[N]]$depth < depth) {
			sequenceNos[[N]] <<- list(depth = depth, sequence = sequenceNo);
		}
		# a new sequence number at the current maximal depth is recored as a potential stop of
		#	the current parallelization
		if (sequenceNos[[N]]$depth <= depth) {
#			sequenceNos[[rampUp + 1]] <<- merge.lists(sequenceNos[[rampUp + 1]],
#				list(sequenceStop = sequenceNo));
			sequenceNos[[N]]$sequenceStop <<- sequenceNo;
			Log(sprintf('new sentinel stop: %d', sequenceNo), 6);
		}
		NULL
	},
	# the currentSentinel is the one to skip, which comes from the previous cursor position
	currentSentinel = function() {
		sequenceNos[[rampUp]]
#		if (rampUp == 1 || rampUp - 1 > length(sequenceNos))
#			list(depth = -1, sequence = -1, sequenceStop = -1) else
#			sequenceNos[[rampUp - 1]]
	},
	incCursor = function() {
		rampUp <<- rampUp + 1;
		.self$currentSentinel()
	},
	resetCursor = function() {
		rampUp <<- 1;
		.self$restoreRandomSeed();
	},

	#
	#	functional methods
	#

	rampUpForeFront = function()length(sequenceNos),
	# detect range where results need to be recovered (thawed from the freezer)
	# the latest state (stack position N) is nascent and ignored
	#	there is currently probing or parallelization going on
	checkAgainstState = function(state) {
		#N = length(sequenceNos);
		N = .self$rampUpForeFront();
		sentinel = .self$currentSentinel();
		r = (N > rampUp &&
			state$sequence >= sentinel$sequence &&
			state$sequence <= sentinel$sequenceStop &&
			state$depth == sentinel$depth);
		#Log(sprintf('sentinel check %d [rampup %d]', r,  Lapply_executionState__$rampUp));
		r
	},
	skipToRampDown = function(state) {
		N = length(sequenceNos);
		sentinel = .self$currentSentinel();
		r = (N > rampUp && state$sequence <= sentinel$sequenceStop);
		#Log(sprintf('sentinel check %d [rampup %d]', r,  Lapply_executionState__$rampUp));
		r
	},
	# <N> after probing length of sequenceNos is increased by one
	#	when reaching this point we want to parallelize
	isLastRampUp = function() {
		rampUp == length(sequenceNos)
	},
	# <N> tb called after the state has been processed
	#	if the last sequence was processed the cursor is advanded to proceed to the next rampUp
	adjustCursor = function(state) {
		sentinel = .self$currentSentinel();
		if (.self$checkAgainstState(state) && state$sequence == sentinel$sequenceStop)
			.self$incCursor();
	},
	#
	#	freezer methods
	#
	currentFreezer = function() {
		if (rampUp > length(freezers)) {
			freezers[[rampUp]] <<- getRefClass(freezerClass)$new(copy_env = copy_environments);
		}
		freezers[[rampUp]]
	},

	#
	#	random numbers
	#
	storeRandomSeed = function() {
		# force first number to be generated if non was so far
		if (!exists('.Random.seed', .GlobalEnv)) runif(1);
		randomSeed <<- list(kind = RNGkind(), seed = get('.Random.seed', envir = .GlobalEnv));
		NULL
	},
	restoreRandomSeed = function() {
		RNGkind(randomSeed$kind[1], randomSeed$kind[2]);
		# assume .Random.seed not to be masked
		# explicit assignment in .GlobalEnv not possible due to R package policies
		.Random.seed <- randomSeed$seed;
	}
	

	#
	#	</p> methods
	#
	)
);
LapplyExecutionStateClass$accessors(names(LapplyExecutionStateClass$fields()));


#
#	<p> core parallize functions
#

#if (!exists('parallelize_env')) parallelize_env <- new.env();

# force_rerun instructs backends to ignore state-retaining files and re-run all computations

#  parallelize_initialize
#' Initialize dynamic parallelization of ensuing parallelize calls
#' 
#' Initialzes the parallelization process. The config argument describes all
#' parameters for as many backends as are available. Remaining arguments select
#' a configuration for the ensuing parallelization from that description.
#' 
#' \code{Lapply_config} is a list with the following elements
#'	\itemize{
#'		\item max_depth: maximal depth to investigate during probing
#'		\item parallel_count: provide default for the number of parallel jobs to generate, overwritten by
#'			the function argument
#'		\item offline: this option determines whether parallelize returns before performing the ramp-down. This is relevant for backends running on remote machines. See especially the \code{OGSremote} backend.
#'		\item backends: a list that contains parameters specific for backends. The name of each element should be the name of a backend without the prefix \code{ParallelizeBackend}. Each of the elements is itself a list with the paramters. If several configurations are required for a certain backend - e.g. a batch-queuing system for which different queues are to be used - the name can be chosen arbitrarily. Then the element must contain an element with name \code{backend} that specifies the backend as above (example below). For the backend specific parameters see the class documentation of the backends.
#'  }
#' 
#' @aliases parallelize_initialize Lapply_initialize
#' @param Lapply_config A list describing possible configurations of the
#' parallelization process. See Details.
#' @param stateClass A class name representing parallelization states. Needs
#' only be supplied if custom extensions have been made to the package.
#' @param backend The name of the backend used. See Details and Examples.
#' @param freezerClass The freezerClass used to store unevaluated calls that
#' are to be executed in parallel. Needs only be supplied if custom extensions
#' have been made to the package.
#' @param \dots Extra arguments passed to the initializer of the stateClass.
#' @param force_rerun So called offline computations are stateful. If a given
#' rampUp has been completed an ensuing call - even a rerun of the script in a
#' new R interpreter - reuses previous result. If set to TRUE force_rerun
#' ignores previous results and recomputes the whole computation.
#' @param sourceFiles Overwrite the \code{sourceFiles} entry in
#' \code{Lapply_config}.
#' @param parallel_count Overwrite the \code{parallel_count} entry in
#' \code{Lapply_config}.
#' @param declare_reset if \code{FALSE}, add sourcefile/libraries to what was declared before
#' @param copy_environments defaults to \code{TRUE} and copies all unbound variables if the
#' backend requires so. \code{...} need to be specified as arguments to \code{Lapply} like so:
#' \code{Lapply(values, function(v, ...)(1 + compute(v, ...)), ...)}. Otherwise an error messages
#' results on 'non-local' backends (everything but \code{local}) at the moment.
#' @return Value \code{NULL} is returned.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{parallelize}}, \code{\link{parallelize_call}}, 
#'	 \code{\linkS4class{ParallelizeBackend}},
#'	 \code{\linkS4class{ParallelizeBackendLocal}},
#'   \code{\linkS4class{ParallelizeBackendSnow}},
#'   \code{\linkS4class{ParallelizeBackendOGSremote}}
#' @examples
#' 
#'   config = list(max_depth = 5, parallel_count = 24, offline = TRUE, backends = list(
#'     snow = list(
#'       localNodes = 1, sourceFiles = c('RgenericAll.R', 'Rgenetics.R', 'RlabParallel.R')
#'     ),
#'     local = list(
#'       path = sprintf('%s/tmp/parallelize', tempdir())
#'     ),
#'     `ogs-1` = list(
#'       backend = 'OGS',
#'       sourceFiles = c('RgenericAll.R', 'RlabParallel.R'),
#'       stateDir = sprintf('%s/tmp/remote', tempdir()),
#'       qsubOptions = sprintf('--queue all.q --logLevel %d', 2),
#'       doNotReschedulde = TRUE
#'     ),
#'     `ogs-2` = list(
#'       backend = 'OGS',
#'       sourceFiles = c('RgenericAll.R', 'RlabParallel.R'),
#'       stateDir = sprintf('%s/tmp/remote', tempdir()),
#'       qsubOptions = sprintf('--queue subordinate.q --logLevel %d', 2),
#'       doSaveResult = TRUE
#'     ),
#'     `ogs-3` = list(
#'       backend = 'OGSremote',
#'       remote = 'user@@localhost:tmp/remote/test',
#'       sourceFiles = c('RgenericAll.R', 'RlabParallel.R'),
#'       stateDir = sprintf('%s/tmp/remote/test_local', tempdir()),
#'       qsubOptions = sprintf('--queue all.q --logLevel %d', 2),
#'       doSaveResult = TRUE
#'     )
#'   ));
#'   # run ensuing parallelizations locally, ignore result produced earlier
#'   parallelize_initialize(config, backend = "local", force_rerun = FALSE);
#'   # run ensuing parallelizations on the snow cluster defined in the snow backend section
#'   parallelize_initialize(config, backend = "local");
#'   # run ensuing parallelizations on a local Open Grid Scheduler
#'   parallelize_initialize(config, backend = "ogs-1");
#'   # run same analysis as above with different scheduling options
#'   parallelize_initialize(config, backend = "ogs-2");
#'   # run same analysis on a remote Opend Grid Scheduler
#'   # user 'user' on machine 'localhost' is used
#'   parallelize_initialize(config, backend = "ogs-3");
#' 
# <!> Lapply_config_default -> get('Parallelize_config__')
parallelize_initialize = Lapply_initialize = function(Lapply_config = get('Parallelize_config__'), 
	stateClass = 'LapplyState', backend = 'local', freezerClass = 'LapplyFreezer', ...,
	force_rerun = FALSE, sourceFiles = NULL, libraries = NULL, parallel_count = NULL,
	copy_environments = TRUE, declare_reset = FALSE, rngSeedCapsules = 'LapplyRNGseedCapsule') {
	# <p> check for turning off
	if (backend == 'off') {
		parallelize_setEnable(F);
		return(NULL);
	} else parallelize_setEnable(T);
	# <p> misc setup
	Log.setLevel(firstDef(Lapply_config$logLevel, Log.level(), 4));
	parallelize_setEnable(T);
	# <p> config
	configPre = Lapply_createConfig();
	sourceFiles = c(
		if (declare_reset) c() else configPre$sourceFiles,
		Lapply_config$sourceFiles, Lapply_config$backends[[backend]]$sourceFiles, sourceFiles
	);
	libraries = unique(c('parallelize.dynamic',
		if (declare_reset) c() else configPre$libraries,
		libraries
	));
	copyFiles = if (declare_reset) c() else configPre$copyFiles;
	backendClass = firstDef(Lapply_config$backends[[backend]]$backend, backend);
	backendConfig = merge.lists(
		Lapply_backendConfig_default,
		Lapply_backendConfigSpecific_default[[backendClass]],	# <i> --> class method
		Lapply_config$backends[[backend]],
		list(force_rerun = force_rerun,
			sourceFiles = sourceFiles, libraries = libraries, copyFiles = copyFiles,
			copy_environments = copy_environments)
	);
	Lapply_config = merge.lists(
		Lapply_config_default,
		Lapply_config,
		list(backend = backend, backendConfig = backendConfig, parallel_count = parallel_count,
			sourceFiles = sourceFiles, libraries = libraries,
			copy_environments = copy_environments,
			seedCapsules = rngSeedCapsules)
	);
	Lapply_setConfig(Lapply_config);
	# <p> backend
	if (exists('Lapply_backend__',  envir = parallelize_env)) rm('Lapply_backend__', envir = parallelize_env);
	# <p> iteration states
	Lapply_initializeState(stateClass, ...);
	freezerClass = firstDef(backendConfig$freezerClass, freezerClass);
	assign('Lapply_executionState__', LapplyExecutionStateClass$new(
		freezerClass = freezerClass, copy_environments = Lapply_config$copy_environments),
		envir = parallelize_env);
	NULL
}

#  parallelize_declare
#' Declare static parallelization environment for parallel calls.
#'
#' This includes files to be 
#' sourced, packages to be loaded and files to be copied. This function performs a subset of
#' \code{parallelize_initialize} and its intended use is to factor out certain parameters from
#' \code{parallelize_initialize} calls.
#'
#' 
#' @param source Overwrite the \code{sourceFiles} entry in
#' \code{Lapply_config}.
#' @param packages Overwrite the \code{library} entry in
#' \code{Lapply_config}.
#' @param copy Vector of pathes that is recursively copied if needed.
#' @param reset If true (the default), values are replaced otherwise values are appended
#' @return Value \code{NULL} is returned.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{parallelize}}, \code{\link{parallelize_call}}, 
#'	 \code{\linkS4class{ParallelizeBackend}},
#'	 \code{\linkS4class{ParallelizeBackendLocal}},
#'   \code{\linkS4class{ParallelizeBackendSnow}},
#'   \code{\linkS4class{ParallelizeBackendOGSremote}}
#' @examples
#'
#'   ## Not run:
#'   # run ensuing parallelizations locally, ignore result produced earlier
#'   parallelize_declare(source = 'mySourceFile.R', packages = 'glmnet');
#' 
parallelize_declare = function(source = NULL, packages = NULL, copy = NULL, reset = TRUE) {
	Lapply_config = Lapply_createConfig();
	# <p> config
	sourceFiles = unique(c(if (!reset) Lapply_config$sourceFiles else c(), source));
	libraries = unique(c(if (!reset) Lapply_config$libraries else c(), packages));
	copyFiles = unique(c(if (!reset) Lapply_config$copyFiles else c(), copy));
	# <!><i> copyFiles

	Lapply_setConfig(merge.lists(
		Lapply_config_default,
		Lapply_config,
		list(sourceFiles = sourceFiles, libraries = libraries, copyFiles = copyFiles)
	));
	NULL
}

parallelize_initializeBackendWithCall = function(call_, Lapply_config) with(Lapply_config, {
	# heuristic to get original function name if not supplied
	#functionName = firstDef(call_$name, deparse(sys.call(-6)[[2]][[1]]));
	functionName = firstDef(call_$name, deparse(sys.call(-6)[[2]]));
	#signature = md5sumString(sprintf('%s%s', tag, deparse(.f)));
	signature = md5sumString(sprintf('%s%s', functionName, backend));
	Log(sprintf('parallelize signature %s', signature), 5);

	backendClass = sprintf('ParallelizeBackend%s', uc.first(firstDef(backendConfig$backend, backend)));
	#backendConfig = rget(sprintf('%s_config__', backendClass), default = list());
	assign('Lapply_backend__', new(backendClass, config = backendConfig, signature = signature),
		envir = parallelize_env);
})

Lapply_initializeState = function(stateClass = 'LapplyState', ...) {
	state = getRefClass(stateClass)$new(...);
	assign('Lapply__', state, envir = parallelize_env);
}
Lapply_initialze_probing = function() {
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply_executionState__$resetCursor();
}

Lapply_setConfig = function(config) {
	assign('Lapply_globalConfig__', config, envir = parallelize_env);
}
Lapply_getConfig = function() {
	get('Lapply_globalConfig__', envir = parallelize_env);
	#Lapply_globalConfig__
}
Lapply_createConfig = function() {
	if (!exists('Lapply_globalConfig__', envir = parallelize_env))
		Lapply_setConfig(Lapply_config_default);
	Lapply_getConfig()
}
Lapply_storeSeeds = function() {
	# make work standalone and from package
	getConfig = if (exists('Lapply_getConfig')) Lapply_getConfig else
		parallelize.dynamic:::Lapply_getConfig;
	o = getConfig();
	capsules = lapply(o$seedCapsules, function(capsule) {
		cap = new(capsule);
		cap$store();
		cap
	})
	assign('seedCapsules', capsules, envir = parallelize_env);
}
Lapply_restoreSeeds = function() {
	capsules = get('seedCapsules', envir = parallelize_env);
	lapply(capsules, function(capsule)capsule$restore());
}

#
#	</p> Lapply state reference classes
#


#
#	<p> S3 classes
#

Lapply_error = function() {
	Lapply__ = get('Lapply__', envir = parallelize_env);
	e = structure(list(state = Lapply__$copy()), class =  c('Lapply_error', 'simpleError'));
	e
}
Lapply_error.as.character = function(e) {
	msg = structure(sprintf('Lapply stopped at sequence %d, depth %d', e$state$sequence, e$state$depth),
		error = e);
	msg
}

#
#	</p> S3 classes
#

#
#	<p> error handling
#

Throw = function(msg, state) {
	assign('Global_error_state__', state, envir = parallelize_env);
	stop(msg);
}

Catch = function(result, errorClass, handler) {
	doCallHandler = class(result) == 'try-error' &&
		any(class(get('Global_error_state__', envir = parallelize_env)) == errorClass);
	r = if (doCallHandler) {
		errorState = get('Global_error_state__', envir = parallelize_env);
		handler(errorState);
	} else NULL;
	r = list(result = r, didCall = doCallHandler);
	r
}

Try = function(expr, catch = list(), silent = T, setClass = F) {
	r = try(expr, silent = silent);
	didCall = F;
	if (exists('Global_error_state__', envir = parallelize_env)) {
		for (i in 1:length(catch)) {
			errorClass = names(catch)[i];
			r0 = Catch(r, errorClass, catch[[i]]);
			if (r0$didCall) {
				r = r0$result;
				if (setClass) {
					if (is.null(r)) r = integer(0);
					class(r) = errorClass;
				}
			}
			didCall = didCall || r0$didCall;
		}
		remove('Global_error_state__', envir = parallelize_env);
	}
	if (!didCall && class(r) == 'try-error') stop(r[1]);
	r
}

#
#	</p> error handling
#

Lapply_config_default = list(
	max_depth = 2, parallel_count = 32, parallel_stack = 10,
	provideChunkArgument = F, offline = F, stateDir = '.',
	wait_interval = 30,
	copy_environments = TRUE
);
Lapply_backendConfig_default = list(
	doNotReschedule = F, doSaveResult = F
);
Lapply_backendConfigSpecific_default = list(
	local = list(freezerClass = 'LapplyFreezer'),
	snow = list(freezerClass = 'LapplyFreezer'),
	OGS = list(freezerClass = 'LapplyGroupingFreezer'),
	OGSremote = list(freezerClass = 'LapplyGroupingFreezer')
);

Lapply_do = function(l, .f, ..., Lapply_config, Lapply_chunk = 1, envir__) {
	#f_ = function(e, ...)do.call(.f, c(list(e), list(...)), envir = envir__);
	f_ = function(e, ...)do.call(.f, list(e, ...), envir = envir__);
	r = if (Lapply_config$provideChunkArgument)
		lapply(l, f_, Lapply_chunk = Lapply_chunk, ...) else
		lapply(l, f_, ...);
	r
}

Lapply_probeDepth = function(l, .f, ..., Lapply_config, Lapply_chunk = 1, envir__) {
	# <p> probe deeper levels if Lapply_depth not yet reached
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply__ = get('Lapply__', envir = parallelize_env);
	Lapply__$pushElements(length(l));
	Log(sprintf('Lapply: Adding %d elements @depth %d.', length(l), Lapply__$depth), 5);
	r = if (Lapply__$max_depth > Lapply__$depth) {
		probeWrapper = function(e, ...) {
			Try(
				.f(e, ...), catch = list(Lapply_error = function(e) {
					Log(sprintf('caught escape from depth %d', e$depth));
					# <N> the escape left the Lapply state stale
					Lapply__$depthDec();
					e
				})
			)
		};
		Lapply_do(l, probeWrapper, ...,
			Lapply_config = Lapply_config, Lapply_chunk = Lapply_chunk, envir__ = envir__);
	} else list(Lapply_error());
	# pushSequenceForRampUp records depending of its current state
	#	see implementation thereof
	Lapply_executionState__$pushSequenceForRampUp(Lapply__$sequence, Lapply__$depth);
	# generate escape from ramp-down
	#Throw('lapply probe escape', Lapply_error());
	if (any(as.vector(sapply(r, class)) == 'Lapply_error')) Throw('lapply probe escape', Lapply_error());
	# reached when no parallelization happened
	r
}

# from determines the starting point of probing
Lapply_probe = function(call_, Lapply_config) with(Lapply_config,  {
 	depths = 1:max_depth;
	Log(sprintf("Lapply_probe: depths to probe: %s", join(depths)), 5);
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	# probing will determine a new sentinel
	Lapply_executionState__$addSentinel();
	r = NULL;
	for (i in depths) {
		Lapply_restoreSeeds();
		# reset state: first rampUp, cursor to beginning
		Lapply_executionState__$resetCursor();
		Lapply_initializeState('LapplyProbeState', max_depth = i);
		Lapply__ = get('Lapply__', envir = parallelize_env);
		# probing function
		Log(sprintf('Lapply: probing depth %d.', i), 5);
		r = Try(Do.call(call_$fct, call_$args, envir = call_$envir),
			catch = list(Lapply_error = function(e) {
				Log('final catch', 6);
				e
			}));
		# no parallelization found, real result already computed
		if (all(class(r) != 'Lapply_error')) break;
		# compute registered parallelization 
		count = Lapply__$elementsCount(i);
		Log(sprintf('Lapply: registered %d parallel jobs @depth %d.', count, i), 5);
		# <p> determine stopping condition
		rampUp = Lapply_executionState__$getRampUp();
		# specific counts per rampUp
		this_parallel_count =
			if (rampUp > length(parallel_count)) parallel_count[1] else parallel_count[rampUp];
		if (count >= this_parallel_count) {
			Log(sprintf('Lapply_probe: %d jobs @depth %d >= %d. Switching to run-mode.',
				count, i, this_parallel_count), 5);
			break;
		}
		# break if no parallelism was found
	}
	# in case of parallelization Lapply_errors were thrown. these are re-thrown at higher levels such
	#	that return values are nested lists of Lapply_errors. We detect this case by probing for a return
	#	list and checking classes of members
	if (any(as.vector(sapply(r, class)) == 'Lapply_error')) {
		r = Lapply_error();
	}
	r
})

Lapply_parallelize = function(l_parallelize, f_parallelize, ..., Lapply_config, envir__) {
	Lapply_backend__ = get('Lapply_backend__', envir = parallelize_env);
	Lapply__ = get('Lapply__', envir = parallelize_env);
	r = if (Lapply__$depth == Lapply__$max_depth) {
		lapply_dispatch(Lapply_backend__,
			l_parallelize = l_parallelize, f_parallelize = f_parallelize, ..., envir__ = envir__);
	} else {
		Log(sprintf('entering parallelization at depth %d', Lapply__$depth), 6);
		Lapply_do(l_parallelize, function(e, ...)Try(f_parallelize(e, ...),
			catch = list(Lapply_error = function(e){
				Log('Lapply_do catch', 6);
				Lapply__$depthDec();	# <A> balance depth
				e
			})),
		..., Lapply_config = Lapply_config, envir__ = envir__);
	}
	# <i><N> raise exception only if asynchroneous
	# synchroneous computations are only possible when parallelizing at level 1
	#	or the "program-counter" can be manipulated
	# re-throw
	Throw('lapply run escape', Lapply_error());
	NULL
}

# excute code for the given rampUp
#	Lapply_depth: depth at which to parallelize
Lapply_run = function(call_, Lapply_depth, Lapply_config) {
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply_executionState__$resetCursor();
	# reset state cursor
	Lapply_initializeState('LapplyRunState', max_depth = Lapply_depth);
	Log(sprintf('Lapply_run: running at depth %d.', Lapply_depth), 5);

	r = Try(Do.call(call_$fct, call_$args, envir = call_$envir),
		catch = list(Lapply_error = function(e)Log('final run catch', 5)));
	Lapply_backend__ = get('Lapply_backend__', envir = parallelize_env);
	lapply_dispatchFinalize(Lapply_backend__);
	r
}

Lapply_recoverState = function(sequence) {
	Lapply__ = get('Lapply__', envir = parallelize_env);
	Log(sprintf('Recovering state for sequence %d, depth %d.', Lapply__$sequence, Lapply__$depth), 5);
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	freezer = Lapply_executionState__$currentFreezer();
	r = freezer$resultsForSequence(sequence);
	r
}

# Lapply should not applied here as it lives in the parallelize.dynamic package and will
#	not be replaced by source
# Lapply will be set to Lapply_backup on activation of parallelization
#Lapply = lapply;
.lapply = Lapply_backup = function(l, .f, ...,
	Lapply_config = Lapply_getConfig(),
	#Lapply_local = rget('Lapply_local', envir = parallelize_env, default = T),
	Lapply_local = Lapply_config$local,
	Lapply_chunk = 1) {

	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply__ = get('Lapply__', envir = parallelize_env);
	# <p> Lapply__ state
	Lapply__$depthInc();
	envir__ = parent.frame();
	#	starting new Lapply
	Lapply__$sequenceInc();
	# <p> handle cases
	Log(sprintf("Sequence %d.", Lapply__$sequence), 6);
	r = if (Lapply_executionState__$checkAgainstState(Lapply__)) {
		r = Lapply_recoverState(Lapply__$sequence);
		Lapply_executionState__$adjustCursor(Lapply__);
		r
	#	<p> Lapply within range of sentinel but not at max_depth
	} else if (Lapply_executionState__$skipToRampDown(Lapply__)) {
		Log(sprintf("Skipping sequence %d.", Lapply__$sequence), 5);
		# <p> either do regular lapply or skip parallelisation to required rampUp
		Lapply_do(l, .f, ..., Lapply_config = Lapply_config, Lapply_chunk = Lapply_chunk, envir__ = envir__);
	#	<p> probe for degree of parallelism
	} else if (Lapply__$probeMode) {
		Lapply_probeDepth(l, .f, ...,
			Lapply_config = Lapply_config, Lapply_chunk = Lapply_chunk, envir__ = envir__);
	#	<p> parallelization
	} else if (Lapply__$runMode) {
		Lapply_parallelize(l, .f, ..., Lapply_config = Lapply_config, envir__ = envir__);
	#	<p> local mode
	} else {
		Lapply_do(l, .f, ..., Lapply_config = Lapply_config, Lapply_chunk = Lapply_chunk, envir__ = envir__);
	};
	# <p> Lapply__ state
	Lapply__$depthDec();

	r
}

#Sapply = sapply;
Sapply_backup = function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
	r = Lapply(X, FUN, ...);
	r0 = sapply(r, identity, simplify = simplify, USE.NAMES = USE.NAMES);
	r0
}

#' Expose an apply-loop to parallelization
#' 
#' Replacing and apply/lapply/sapply call with a Apply/Lapply/Sapply call makes
#' it amenable to analysis by the parallelize function that can determine
#' dynamic parallelism in running code.
#' 
#' Please refer
#' to the documentation of apply/lapply/sapply for further documenation. The
#' semantics of Apply/Lapply/Sapply are identical to apply/lapply/sapply. Using
#' these functions implies that you want the parallelization mechanism to be
#' applied to these loops.
#' 
#' @aliases Apply Lapply Sapply
#' @param X See documentation for \code{apply}.
#' @param MARGIN See documentation for \code{apply}.
#' @param FUN See documentation for \code{sapply}.
#' @param simplify See documentation for \code{sapply}.
#' @param USE.NAMES See documentation for \code{sapply}.
#' @param .f See documentation for \code{lapply}.
#' @param l See documentation for \code{lapply}.
#' @param Lapply_config See documentation for \code{parallelize_intialize}.
#'   Normally, this argument should be ignored.
#' @param Lapply_local Force local execution. Normally, this argument should be
#'   ignored.
#' @param Lapply_chunk Normally, this argument should be ignored.
#' @param \dots See documentation for \code{apply}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{parallelize}}
#' @keywords programming iteration parallel programming
#' @examples
#' 
#' 	r0 = sapply(1:10, function(x)x^2);
#' 	r1 = Sapply(1:10, function(x)x^2);
#' 	print(all(r0 == r1));
#' 
#Apply = function(X, MARGIN, FUN, ...) {}
Apply_margin_error = 'wrong MARGIN argument supplied to Apply';
#Apply = apply;
Apply_backup = function(X, MARGIN, FUN, ...) {
	r = if (length(MARGIN) == 1) {
		extractor = if (MARGIN == 1) function(X, i)X[i, ] else
			if (MARGIN == 2) function(X, i)X[, i] else stop(Apply_margin_error);
		r0 = Lapply(1:dim(X)[MARGIN], function(i, ..., Apply_object__, Apply_FUN__, Apply_extractor__) {
			Apply_FUN__(Apply_extractor__(Apply_object__, i), ...)
		} , ..., Apply_object__ = X, Apply_FUN__ = FUN, Apply_extractor__ = extractor);
		r = sapply(r0, function(e)e);
		r
	} else if (length(MARGIN) == 2 && all(MARGIN == 1:2)) {
		extractor = function(X, tuple)X[tuple[1], tuple[2]];
		els = apply(merge(data.frame(row = 1:dim(X)[1]), data.frame(col = 1:dim(X)[2])), 1, as.list);
		r0 = Lapply(els, function(i, ..., Apply_object__, Apply_FUN__, Apply_extractor__) {
			Apply_FUN__(Apply_extractor__(Apply_object__, unlist(i)), ...)
		} , ..., Apply_object__ = X, Apply_FUN__ = FUN, Apply_extractor__ = extractor);
		r = sapply(r0, identity);
		if (is.vector(r)) r = matrix(r, ncol = dim(X)[1]);
		r
	} else {
		stop(Apply_margin_error);
	}
	r
}

parallelizeStep = function(call_, Lapply_config) {
	Lapply_storeSeeds();
	# probe parallelism
	r = Lapply_probe(call_, Lapply_config = Lapply_config);
	# no parallelization was possible
	if (all(class(r) != 'Lapply_error')) return(r);
	# run computation for this rampUp sequence
	Lapply__ = get('Lapply__', envir = parallelize_env);
	Lapply_restoreSeeds();
	Lapply_run(call_, Lapply_depth = Lapply__$max_depth, Lapply_config = Lapply_config);
	Lapply_error();
}

# tag: allow to uniquify the signature for multiple calls to the same function
parallelizeOfflineStep = function(call_, Lapply_config) with(Lapply_config, {
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply_backend__ = get('Lapply_backend__', envir = parallelize_env);
	Log(sprintf('parallelize rampUp %d', Lapply_executionState__$rampUpForeFront()), 5);
	#statePath = sprintf('%s/.parallelize%s.RData', stateDir, signature);
	if (Lapply_executionState__$rampUpForeFront() == 0) {
		initScheduling(Lapply_backend__, call_);
	} else {
		restoreParallelizationState(Lapply_backend__);
	}
	#r = parallelizeStep(.f, ..., Lapply_config = Lapply_config);
	r = performParallelizationStep(Lapply_backend__, call_, Lapply_config = Lapply_config);
	saveParallelizationState(Lapply_backend__);
	if (any(class(r) == 'Lapply_error')) {
		if (!backendConfig$doNotReschedule)
			scheduleNextParallelization(Lapply_backend__, call_);
	} else {
		r = finalizeParallelization(Lapply_backend__, r);
	}
	r
})

parallelize_dummy = function(.f, ..., Lapply_config = NULL, envir__ = NULL) {
	.f(...)
}
parallelize_call_dummy = function(.call, Lapply_config = NULL, envir__ = parent.frame(n = 2)) {
	base:::eval(.call, envir = envir__)
}

parallelize_internal = function(call_, Lapply_local = rget('Lapply_local', default = F),
	parallelize_wait = T) {
	Lapply_config = merge.lists(Lapply_getConfig(), list(local = Lapply_local));
	r = if (Lapply_local) {
		# <!><i> setEnable(F), re-enable afterwards
		do.call(call_$fct, call_$args, envir = call_$envir);
	} else {
		Lapply_setConfig(Lapply_config);
		parallelize_initializeBackendWithCall(call_, Lapply_config = Lapply_config);
		Lapply_backend__ = get('Lapply_backend__', envir = parallelize_env);
		r = parallelize_backend(Lapply_backend__, call_);
		# this is a delegating backend (only supported in offline mode)
		if (parallelize_wait && Lapply_backend__@offline) {
			while (pollParallelization(Lapply_backend__)$continue) Sys.sleep(Lapply_config$wait_interval);
			r = getResult(Lapply_backend__);
		}
		r
	}
	r
}

#' Subject a function to dynamic parallelization
#' 
#' This function executes all necessary steps to perform a dynamic analysis of
#' parallelism of a given function, create objects that encapsulate code that
#' can be executed in parallel, transfer this to a execution backend which
#' potentially is a remote target, recollect results and resume execution in a
#' transparent way.
#' 
#' Function parallelize and parallelize_call both perform a dynamic
#' parallelization of the computation of .f, i.e. parallelism is determined at
#' run-time. Points of potential parallelism have to be indicated by the use of
#' Apply/Sapply/Lapply (collectively Apply functions) instead of
#' apply/sapply/lapply in existing code. The semantics of the new function is
#' exactly the same as that of the original functions such that a simple
#' upper-casing of these function calls makes existing programs amenable to
#' parallelization. Parallelize will execute the function .f, recording the
#' number of elements that are passed to Apply functions. Once a given
#' threshold (degree of parallelization) is reached computation is stopped and
#' remaining executions are done in parallel. A call parallelize_initialize
#' function determines the precise mechanism of parallel execution and can be
#' used to flexibly switch between different resources.
#' 
#' @aliases parallelize parallelize_call
#' @param .f Function to be parallelized given as a function object.
#' @param \dots Arguments passed to .f.
#' @param Lapply_local Force local execution.
#' @param parallelize_wait Force to poll completion of computation if backend
#' returns asynchroneously
#' @param .call Unevaluated call to be parallelized
#' @return The value returned is the result of the computation of function .f
#' @section Important details: \itemize{
#' \item The package creates files in a
#' given folder. This folder is not temporary as it might be needed across R
#' sessions. md5-fingerprints of textual representations of function calls are
#' used to create subfolders therein per \code{parallelize} call. Conflicts can
#' be avoided by choosing different function names per \code{parallelize} call
#' for parallelizing the same function several times. For example: \code{f0 =
#' f1 = function(){42}; parallelize(f0); parallelize(f1);}
#' \item In view of
#' efficiency the parallize.dynamic package does not copy the whole workspace
#' to all parallel jobs. Instead, a hopefully minimal environment is set up for
#' each parallel job. This includes all parameters passed to the function in
#' question together with variables defined in the closure. This leaves all
#' functions undefined and it is therefore expected that all necessary function
#' defintions are given in separate R-files or libraries which have to be
#' specified in the \code{config} variable. These are then sourced/loaded into
#' the parallel job. A way to dynamically create source files from function
#' definitions is given in the Examples section.
#' }
#' @author Stefan Böhringer, \email{r-packages@@s-boehringer.org}
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' \code{\link{Apply}}, \code{\link{Sapply}}, \code{\link{Lapply}},
#' \code{\link{parallelize_initialize}}
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'   # code to be parallelized
#'   parallel8 = function(e) log(1:e) %*% log(1:e);
#'   parallel2 = function(e) rep(e, e) %*% 1:e * 1:e;
#'   parallel1 = function(e) Lapply(rep(e, 15), parallel2);
#'   parallel0 = function() {
#'     r = sapply(Lapply(1:50, parallel1),
#'       function(e)sum(as.vector(unlist(e))));
#'     r0 = Lapply(1:49, parallel8);
#'     r
#'   }
#' 
#'   # create file that can be sourced containing function definitions
#'   # best practice is to define all needed functions in files that
#'   # can be sourced. The function tempcodefile allows to create a
#'   # temporary file with the defintion of given functions
#'   codeFile = tempcodefile(c(parallel0, parallel1, parallel2, parallel8));
#' 
#'   # definitions of clusters
#'   Parallelize_config = list(max_depth = 5, parallel_count = 24, offline = FALSE,
#'   backends = list(
#'     snow = list(localNodes = 2, splitN = 1, sourceFiles = codeFile),
#'     local = list(
#'       path = sprintf('%s/tmp/parallelize', tempdir())
#'     )
#'   ));
#' 
#'   # initialize
#'   parallelize_initialize(Parallelize_config, backend = 'local');
#' 
#'   # perform parallelization
#'   r0 = parallelize(parallel0);
#'   print(r0);
#' 
#'   # same
#'   r1 = parallelize_call(parallel0());
#'   print(r1);
#' 
#'   # compare with native execution
#'   parallelize_initialize(backend = 'off');
#'   r2 = parallelize(parallel0);
#'   print(r2);
#' 
#'   # put on SNOW cluster
#'   parallelize_initialize(Parallelize_config, backend = 'snow');
#'   r3 = parallelize(parallel0);
#'   print(r3);
#' 
#'   # analyse parallelization
#'   parallelize_initialize(Parallelize_config, backend = 'local');
#'   Log.setLevel(5);
#'   r4 = parallelize(parallel0);
#'   Log.setLevel(6);
#'   r5 = parallelize(parallel0);
#' 
#'   print(sprintf('All results are the same is %s', as.character(
#'     all(sapply(list(r0, r1, r2, r3, r4, r5), function(l)all(l == r0)))
#'   )));
#' 
parallelize = parallelize_backup = function(.f, ..., Lapply_local = rget('Lapply_local', default = FALSE),
	parallelize_wait = TRUE, envir__ = parent.frame()) {
	envir_evaled = envir__;
	call_ = list(fct = .f, args = list(...), envir = envir_evaled, name = as.character(sys.call()[[2]]));
	parallelize_internal(call_, Lapply_local = Lapply_local, parallelize_wait = parallelize_wait);
}


parallelize_call = parallelize_call_backup = function(.call, ...,
	parallelize_wait = TRUE, envir__ = parent.frame()) {
	envir_evaled = envir__;
	call_  = encapsulateCall(sys.call()[[2]], ..., envir__ = envir_evaled);
	parallelize_internal(call_, ..., parallelize_wait = parallelize_wait);
}

#parallelize_mention = function(...)NULL

#
#	<p> utility functions
#

#' @title Create a temporary file that contains the function definition of the
#' argument.
#' 
#' Create a temporary file that contains the function definition of the
#' argument so that this file can be sourced to re-instantiate the function.
#' 
#' Create a temporary file that contains the function definition of the
#' argument so that this file can be sourced to re-instantiate the function.
#' The temporary file is written to the temporary folder of the current R
#' session.
#' 
#' @param fcts function object the code of which is to be written to file
#' @return Returns the path to the file written.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @examples
#' 
#'   # code to be parallelized
#'   parallel8 = function(e) log(1:e) %*% log(1:e);
#'   parallel2 = function(e) rep(e, e) %*% 1:e * 1:e;
#'   parallel1 = function(e) Lapply(rep(e, 15), parallel2);
#'   parallel0 = function() {
#'     r = sapply(Lapply(1:50, parallel1),
#'       function(e)sum(as.vector(unlist(e))));
#'     r0 = Lapply(1:49, parallel8);
#'     r
#'   }
#' 
#'   codeFile = tempcodefile(c(parallel0, parallel1, parallel2, parallel8));
#'   cat(readFile(codeFile));
#' 
tempcodefile = function(fcts) {
	fctNames = as.character(as.list(sys.call()[[2]])[-1]);
	# create source file with code
	code = join(sapply(fctNames,
		function(name) {
			def = join(deparse(get(name)), sep = "\n");
			code = sprintf('%s = %s', name, def);
			code
		}), sep = "\n");
	codeFile = tempfile();
	# windows specific code
	codeFile = gsub('([\\])', '/', codeFile);
	writeFile(codeFile, code);
	codeFile
}
