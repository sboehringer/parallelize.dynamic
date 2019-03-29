#
#	Rparallel.back.R
#Tue Jun 23 14:18:33 2015

#
#	<p> OGS execution
#

#
#	ParallelizeBackendOGS S4 class
#

# steps: jid associated with calls to parallelizationStep
# chunks: jids associated with *Apply calls
# sometimes steps is one more than chunks (during computations)
.ParallelizeBackendOGSstateClass = setRefClass('ParallelizeBackendOGSstate',
	fields = list( steps = 'list', chunks = 'list', logPath = 'character' ),
	methods = list(
	initialize = function(...) {
		steps <<- list();
		chunks <<- list();
		logPath <<- '';
		.self
	},
	log = function() { if (logPath != '') save(.self, file = logPath); },
	pushStep = function(jid) {
		steps[[length(steps) + 1]] <<- jid;
		.self$log();
	},
	pushChunks = function(jids) {
		chunks[[length(chunks) + 1]] <<- jids;
		.self$log();
	},
	chunksJids = function() { if (!length(chunks)) c() else chunks[[length(chunks)]]; },
	setLogPath = function(path) {
		logPath <<-path;
		if (file.exists(logPath)) file.remove(logPath);
	}
	)
);
.ParallelizeBackendOGSstateClass$accessors(names(.ParallelizeBackendOGSstateClass$fields()));

#
#	class ParallelizeBackendOGS is expected to work in the current directory
#	if files are to be setup, ParallelizeBackendOGSremote should be used
#

#' Class \code{"ParallelizeBackendOGS"}
#' 
#' %% ~~ A concise (1-5 lines) description of what the class is. ~~ Backend
#' class implmenting Open Grid Scheduler support
#' 
#' 
#' @name ParallelizeBackendOGS-class
#' @aliases ParallelizeBackendOGS-class
#' finalizeParallelization,ParallelizeBackendOGS-method
#' initialize,ParallelizeBackendOGS-method
#' initScheduling,ParallelizeBackendOGS-method
#' lapply_dispatchFinalize,ParallelizeBackendOGS-method
#' pollParallelization,ParallelizeBackendOGS-method
#' restoreParallelizationState,ParallelizeBackendOGS-method
#' scheduleNextParallelization,ParallelizeBackendOGS-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{}. %% ~~ describe objects here ~~
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso %% ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ %% ~~or
#' \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
#' \code{\linkS4class{ParallelizeBackend}},
#' \code{\linkS4class{ParallelizeBackendLocal}},
#' \code{\linkS4class{ParallelizeBackendSnow}},
#' \code{\linkS4class{ParallelizeBackendOGSremote}}
#' @keywords classes
#' @examples
#' 
#' showClass("ParallelizeBackendOGS")
#' 
setClass('ParallelizeBackendOGS',
	contains = 'ParallelizeBackend',
	representation = list(jids = 'ParallelizeBackendOGSstate'),
	prototype = list(jids = .ParallelizeBackendOGSstateClass$new())
);
.ParallelizeBackendOGSDefaultConfig = list(
	qsubOptions = '--queue all.q'
);

setMethod('initialize', 'ParallelizeBackendOGS', function(.Object, config, ...) {
	# <p> super-class
	config = merge.lists(.ParallelizeBackendOGSDefaultConfig, config);
	.Object = callNextMethod(.Object, config = config, ...);
	# <p> OGS initialization
	Log('initializing OGS', 6);
	.Object@offline = T;
	# <p> RNG
	RNGkind("L'Ecuyer-CMRG");
	set.seed(as.integer(Sys.time()));

	# <p> setup environment
	setupLocalEnv();

	# <p> jid state
	.Object@jids$setLogPath(parallelizationStatePath(.Object, 'jids'));
	.Object
});

setMethod('initScheduling', 'ParallelizeBackendOGS', function(self, call_) {
	callNextMethod(self);
	# <p> dir initialization
	dir = parallelizationStatePath(self, tag = '', ext = '');
	Dir.create(dir, recursive = T);
	# <p> initialize files
	sentinelPath = parallelizationStatePath(self, 'sentinel');
	if (file.exists(sentinelPath)) file.remove(sentinelPath);
});

.parallelizationStepOGS = function(call_, pathHandover) {
	# <!> potential race condition with scheduleNextParallelization
	r0 = get(Load(file = pathHandover, Load_sleep = 5)[1]);
	Lapply_backend__ = get('Lapply_backend__', envir = parallelize_env);
	Lapply_backend__@jids$pushStep(r0$jid);
	parallelize_setEnable(T);	# default is off
	parallelizeOfflineStep(call_, Lapply_config = Lapply_getConfig());
}

shellEnvString = function(env, sep = '+++', prefix = '') {
	join(kvlapply(env, function(k, v)Sprintf('%{prefix}s%{k}s=%{v}s')), sep)
}
qsubEnvOptions = function(env) {
	qsubOptions = join(c('--setenv', shellEnvString(env, '+++'), '--setenvsep=+++'), ' ');
	#Logs('QsubEnvOptions: %{qsubOptions}s', level = 6);
	qsubOptions
}
remoteEnvAdd = function(vars = list(
	PATH = "echo `echo 'cat(system.file(package = \"parallelize.dynamic\"))' | Rscript -`/Perl",
	PERL5LIB = "echo `echo 'cat(system.file(package = \"parallelize.dynamic\"))' | Rscript -`/Perl"),
	userhost = 'localhost') {
	env = kvlapply(vars, function(name, cmd) {
		valueNew = trimString(System(cmd,
			return.output = T, patterns = 'ssh', ssh_host = userhost)$output);
		valueOld = trimString(System(Sprintf("echo $%{name}s"),
			return.output = T, patterns = 'ssh', ssh_host = userhost)$output);
		Sprintf('%{valueNew}s:%{valueOld}s')
	});
	env
}
remoteEnvSetup = function(remoteDir) {
	sp = splitPath(remoteDir, ssh = T);
	env = remoteEnvAdd(userhost = sp$userhost);
	remoteEnvProfile = Sprintf('%{remoteDir}s/remoteProfile.sh');
	envAsString = shellEnvString(env, "\n", prefix = 'export ');
	writeFile(remoteEnvProfile, envAsString, ssh = T);
	Logs("Remote env: %{envAsString}s", level = 6);
	splitPath(remoteEnvProfile, ssh = T)$path
}

freezeCallOGS = function(self, ..f, ...,
	freeze_file = tempfile(), freeze_control = list(), waitForJids = c(),
	patterns = 'qsub', cwd = NULL, ssh_host = 'localhost', ssh_source_file = NULL,
	qsubPath = parallelizationStatePath(self, 'qsub', ext = ''),
	qsubMemory = '4G', qsubOptionsAdd = '',
	envir = parent.frame(), thaw_transformation = identity, freeze_env_eval = F,
	freeze_objects = NULL) {

	path = freezeCall(freeze_f = ..f, ...,
		freeze_file = freeze_file, freeze_save_output = T, freeze_control = freeze_control,
		freeze_envir = envir, freeze_env_eval = freeze_env_eval,
		#freeze_objects = 'parallelize_env', thaw_transformation = thaw_transformation);
		#freeze_objects = NULL, thaw_transformation = thaw_transformation);
		freeze_objects = freeze_objects,
		thaw_transformation = thaw_transformation);
	wrap = frozenCallWrap(path, freeze_control);
	wait = if (!length(waitForJids)) '' else sprintf('--waitForJids %s', paste(waitForJids, collapse = ','))
	qsubOptions = Sprintf('%{options}s --outputDir %{qsubPath}Q %{wait}s %{qsubOptionsAdd}s',
		options = self@config$qsubOptions
	);
	qsubOptions = mergeDictToString(list(QSUB_MEMORY = qsubMemory), qsubOptions);
	Logs("qsubOptions: %{qsubOptions}s", level = 5)
	r = System(wrap, 5, patterns = patterns, qsubOptions = qsubOptions, cwd = cwd,
		ssh_host = ssh_host, ssh_source_file = ssh_source_file, return.cmd = T);
	r
}

# we use the freeze/thaw mechanism and a handover such that restoring the state would
#	destroy handover changes, the saving still occurs for tracking purposes
setMethod('restoreParallelizationState', 'ParallelizeBackendOGS', function(self) {
	NULL
});

setMethod('scheduleNextParallelization', 'ParallelizeBackendOGS', function(self, call_) {
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	c = Lapply_getConfig();
	remoteConfig = lapply_dispatch_config(self);
	freeze_control = list(
		sourceFiles = remoteConfig$sourceFiles,
		libraries = unique(c('parallelize.dynamic', remoteConfig$libraries)),
		#objects = parallelizationStateObjects,
		logLevel = Log.level(),
		rng = RNGuniqueSeed(self@signature)
	);
	# 	print('scheduleNextParallelization');print(class(self));print(freeze_control);
	path = parallelizationStatePath(self, 'rampUp:%03d', Lapply_executionState__$rampUp);
	pathHandover = parallelizationStatePath(self, 'rampUp:%03d_handover', Lapply_executionState__$rampUp);
	# <i> gather information from previous step
	#qacct -j 257
	# new path for each rampUp due to potential race condition
	r0 = freezeCallOGS(self, ..f = .parallelizationStepOGS, call_,
		# .parallelizationStepOGS
		pathHandover = pathHandover,
		# freeze
		freeze_file = path, freeze_control = freeze_control,
		freeze_objects = list(parallelize_env = parallelizationStateObjects),
		qsubMemory = remoteConfig$backendConfig$qsubParallelMemory,
		waitForJids = self@jids$chunksJids()
	)
	save(r0, file = pathHandover);
	r0
});

setMethod('lapply_dispatchFinalize', 'ParallelizeBackendOGS', function(self) { 
	Log(sprintf('OGS Dispatching, tmp: %s', self@config$stateDir), 5);
	Lapply_executionState__ = get('Lapply_executionState__', envir = parallelize_env);
	Lapply__ = get('Lapply__', envir = parallelize_env);
	freezer = Lapply_executionState__$currentFreezer();

	# <p> setup
	lcfg = Lapply_getConfig();
	remoteConfig = lapply_dispatch_config(self);
	freeze_control = list(
		sourceFiles = remoteConfig$sourceFiles,
		libraries = unique(c('parallelize.dynamic', remoteConfig$libraries)),
		objects = parallelizationStateObjects,
		logLevel = Log.level()
	);
	# <p> split up calls into 'parallel_count' no of slots
	idcs = splitListIndcs(freezer$Ncalls(), lcfg$parallel_count);

	ogs_frozen_call__ = function(listcalls, Lapply_config) {
		Lapply_setConfig(Lapply_config);
		parallelize_setEnable(F);
		Lapply_setConfigValue(activeDictionary = Lapply_getConfig()$backend);
		lapply(listcalls, function(lc) {
			lapply(lc$elements, function(e)
				try(do.call(lc$fct, c(list(e), lc$arguments)))
			)
		})
	}
	r = lapply(1:dim(idcs)[1], function(job_index__) {
		path = parallelizationStatePath(self, 'sequence:%03d_chunk:%05d', Lapply__$sequence, job_index__);
		mycalls = freezer$callRange(idcs[job_index__, 1], idcs[job_index__, 2]);
		# force evaluation/restriction of environment
		mycalls = lapply(mycalls, function(lc) {
			# < 24.6.2015
			#lc$fct = environment_eval(lc$fct, functions = remoteConfig$copy_environments);
			if (remoteConfig$copy_environments) {
				lc$fct = environment_eval(lc$fct, functions = FALSE, recursive = FALSE);
			}
			lc
		});
		freeze_control_chunk = c(freeze_control, list(rng = RNGuniqueSeed(c(self@signature, job_index__))));
		Log(sprintf("Unique seed for job %d: %d", job_index__, freeze_control_chunk$rng$seed), 5);
		r = freezeCallOGS(self, ogs_frozen_call__, listcalls = mycalls, Lapply_config = lcfg,
			freeze_file = path, freeze_control = freeze_control_chunk,
			cwd = getwd(),
			qsubMemory = self@config$qsubParallelMemory,
			#qsubMemory = remoteConfig$qsubParallelMemory,
			thaw_transformation = thaw_object
		);
		r = c(r, list(file = path, from = idcs[job_index__, 1], to = idcs[job_index__, 2]));
		r
	});
	self@jids$pushChunks(list.kp(r, 'jid', do.unlist = T));
	freezer$pushResults(r);
	#freezer$unlistResults();
	freezer$finalizeResults();
	NULL
});

setMethod('finalizeParallelization', 'ParallelizeBackendOGS', function(self, r) {
	Log(sprintf('OGS finalizing parallelization %s', self@signature), 5);
	if (self@config$doSaveResult)
		save(r, file = parallelizationStatePath(self, 'result'));
	sentinel = list(signature = self@signature);
	save(sentinel, file = parallelizationStatePath(self, 'sentinel'));
	r
});

progressStatJids = function(jids, jidsRunning) {
	jidsPending = intersect(jids, jidsRunning);
	N = length(jids);
	Npending = length(jidsPending);
	r = list(N = N, Npending = Npending, Ncomplete = N - Npending, complete = 1 - Npending / N);
	r
}
.progressStat = function(jidsTasks, i, jidsRunning) {
	jidsTask = if (nif(length(jidsTasks) < i)) NULL else jidsTasks[[i]];
	progressStatJids(jidsTask, jidsRunning)
}

.stdProgressFormat = list(
	title = '%-20s ', Ncomplete = '%4d/', N = '%d', progress = ' [%25s] ', Perc = '%3.0f%%'
);
progressString = function(stat, title = 'Task', format = .stdProgressFormat, NanString = '----') {
	format = merge.lists(.stdProgressFormat, format);
	L = nchar(sprintf(format$progress, '-'));	# length progress bar
	progressBar = if (is.nan(stat$complete)) sprintf('%-*s', L, 'count pending') else
		paste(c(rep('#', round(stat$complete * L, 0)),
			rep('.', round((1 - stat$complete) * L, 0))), collapse = '');
		values = list(title = title,
			Perc = floor(100 * stat$complete),
			Ncomplete = stat$Ncomplete, N = stat$N, progress = progressBar);
		r = unlist(nlapply(format, function(n) {
			if (is.nan(values[[n]])) NanString else sprintf(format[[n]], values[[n]])
	}));
	r = paste(r, collapse = '');
	r
		
}

.pollJids = function(...) {
	qstat = System("qstat -u \\* -xml | xml sel -t -m '//JB_job_number' -v 'text()' -o ' '",
		logLevel = 6, ..., return.output = T);
	jids = fetchRegexpr('(\\d+)', qstat$output, captures = T);
	jids
}

.pollMessageRaw_table = function(jids, qstat_jids) {
	N = max(length(jids$steps), length(jids$chunks));
	msg = as.vector(sapply(1:N, function(i) {
		psc = .progressStat(jids$chunks, i, qstat_jids);
		pss = .progressStat(jids$steps, i, qstat_jids);
		c(
			progressString(psc, title = sprintf('  Parallelization %d', i)),
			progressString(pss, title = sprintf('Rampdown %d', i))
		)
	}));
	msg
}
.pollMessage_table = function(msg, continue) {
	header = paste(rep('-', 79), collapse = '');
	conclusion = if (continue) 'Further scheduling pending' else 'Computation complete';
	#messageRaw = paste(msg, collapse = "\n");
	#message = paste(c(header, messageRaw, header, conclusion, '', ''), collapse = "\n");
	message = c(header, msg, header, conclusion);
	message
}

.pollMessageRaw = function(jids, qstat_jids) {
	Nsteps = length(jids$steps);
	Nchunks = length(jids$chunks);
	jids = if (Nsteps >= Nchunks) jids$steps[[Nsteps]] else jids$chunks[[Nchunks]];
	ps = progressStatJids(jids, qstat_jids);
	progressString(ps, title = if (Nsteps >= Nchunks)
		Sprintf('Rampdown %{Nsteps}d') else Sprintf('Parallelization %{Nchunks}d'));
}

.pollMessage = function(msg, continue) {
	Sprintf('%{msg}s | [%{cont}s]', cont = if (continue) 'continued' else 'done');
}

setMethod('pollParallelization', 'ParallelizeBackendOGS', function(self, options = list()) {
	continue = !file.exists(parallelizationStatePath(self, 'sentinel'));
	# <p> fetch jids
	qstat_jids = .pollJids();
	# <p> restore state locally
	Load(file = parallelizationStatePath(self, 'state'));
	# <p> raw message
	Lapply_backend__ = get('Lapply_backend__', envir = parallelize_env);
	message = .pollMessageRaw(Lapply_backend__@jids, qstat_jids);
	# <p> refine
	message = .pollMessage(message, continue);
	#			=~ m{(\d+)}sog)
	r = list(continue = continue, message = message);
	r
});
