#
#	Rparallel.back.ogsremote.R
#Tue Jun 23 14:20:45 2015

#
#	ParallelizeBackendOGSremote S4 class
#

.ParallelizeBackendOGSremoteDefaultConfig = list(
	remote = 'localhost:parallelize_projects'
);

#' Class \code{"ParallelizeBackendOGSremote"}
#' 
#' Backend class supporting Open Grid Scheduler use from remote machines
#' 
#' 
#' @name ParallelizeBackendOGSremote-class
#' @aliases ParallelizeBackendOGSremote-class
#' getResult,ParallelizeBackendOGSremote-method
#' initialize,ParallelizeBackendOGSremote-method
#' initScheduling,ParallelizeBackendOGSremote-method
#' lapply_dispatchFinalize,ParallelizeBackendOGSremote-method
#' performParallelizationStep,ParallelizeBackendOGSremote-method
#' pollParallelization,ParallelizeBackendOGSremote-method
#' @docType class
#'
#' @section Objects from the Class:
#' Objects can be created by calls of the form
#'	\code{new("ParallelizeBackendOGSremote", config, ...)}.
#' During normal operation you do not have to create objects of this class yourself. Instead, \code{parallelize_initialize} will create such instances for you. The class can be configured with the following field in the \code{Lapply_config} argument of \code{parallelize_initialize}.
#' \itemize{
#'   \item freezerClass: defaults to \code{LapplyPersistentFreezer}. It is recommended to use \code{LapplyGroupingFreezer} for this backend as it is the most efficient freezer. Currently, \code{LapplyGroupingFreezer} is only supported for this backend.
#'   \item stateDir: directory to store results from computations. This location is passed to \code{LapplyPersistentFreezer}. If temporary behavior is desired it can be set to: \code{sprintf('\%s/tmp/remote', tempdir())}.
#'    \item sourceFiles: a vector of files to be sourced prior to parallel execution
#'    \item libraries: a vector of package names to be loaded prior to parallel execution
#'    \item remote: a scp path to a folder on the server that can be used to store temporary files, e.g. 'user@@localhost:tmp/remote/test'. A unique subfolder per computation is created within this folder to store files (unique tempfolder).
#'     \item qsubOptions: extra options that are passed to the \code{qsub.pl} utility included in the package that is used to submit jobs. Execute \code{./qsub.pl --help} in the \code{inst/Perl} folder of the package to see all options and examples. Important options include \code{--queue} to specify the queue, \code{--memory} to set an upper bound for the needed memory (.e.g. \code{--memory 4G}) and \code{--logLevel} to set verbosity of output (level 5 produces detailed output).
#' }
#' To use this backend you have to have access password-less ssh access to a linux server running the Open Grid Scheduler (OGS) or the Sun Grid engine (SGE). You can install OGS locally (see \link{http://gridscheduler.sourceforge.net/CompileGridEngineSource.html}). \code{ssh} and \code{scp} have to be installed on the local machine.
#' Job output (stdout, stderr) as well as \code{qsub.pl} output is stored in subfolder of the unique tempfolder starting with 'qsubOutput'.
#'
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
#' showClass("ParallelizeBackendOGSremote")
#' 
setClass('ParallelizeBackendOGSremote',
	contains = 'ParallelizeBackend',
	representation = list(jids = 'ParallelizeBackendOGSstate'),
	prototype = list(jids = .ParallelizeBackendOGSstateClass$new())
);
setMethod('initialize', 'ParallelizeBackendOGSremote', function(.Object, config, ...) {
	# <p> super-class
	config = merge.lists(.ParallelizeBackendOGSDefaultConfig, config);
	.Object = callNextMethod(.Object, config = config, ...);
	# <p> OGS initialization
	Log('initializing OGS for remote execution', 6);
	.Object@offline = T;
	# restart on other host
	.Object
});

.remoteConfigForOGSremote = function(stateDir = '.') {
	Lapply_remote_config = Lapply_getConfig();
	backendConfig = merge.lists(
		Lapply_remote_config$backendConfig,
		list(backend = 'OGS', stateDir = stateDir, logLevel = Log.level())
	);
	Lapply_remote_config$backends[[Lapply_remote_config$backend]] = 
		Lapply_remote_config$backendConfig = backendConfig;
	Lapply_remote_config
}
.OGSremoteFile = function(self, tag = '', ext = '.RData') {
	Lapply_remote_config = .remoteConfigForOGSremote(stateDir = self@config$remote);
	remoteDummy = new('ParallelizeBackendOGS', config =
		Lapply_remote_config$backendConfig, signature = self@signature);
	remoteDir = parallelizationStatePath(remoteDummy, tag = tag, ext = ext);
	remoteDir
}
.OGSremoteWorkingDir = function(self).OGSremoteFile(self, tag = '', ext = '')

# patch source file pathes to local versions: assumed to be copied to working directory on server
setMethod('lapply_dispatch_config', 'ParallelizeBackend', function(self) {
	config = self@config;
	config$sourceFiles = list.kpu(lapply(config$sourceFiles, splitPath), 'file');
	config
})

setMethod('initScheduling', 'ParallelizeBackendOGSremote', function(self, call_) {
	callNextMethod(self);
	Log('ParallelizeBackendOGSremote:initScheduling', 6);
	r = with(self@config, {
	# <p> check starting sentinel
	sentinelPath = parallelizationStatePath(self, 'OGSremote_sentinel');
	if (file.exists(sentinelPath) && !self@config$force_rerun) {
		Log(sprintf('Signature %s already scheduled.', self@signature), 5);
		return(NULL);
	}
# 	# prevent further parallelize calls from re-initializing
# 	c = Lapply_getConfig();
# 	c$backendConfig$force_rerun = F;
# 	Lapply_setConfig(c);

	# <p> establish start sentinel
	sentinel = list(signature = self@signature);
	save(sentinel, file = sentinelPath);

	# <p> setup remote environment
	#remoteDir = sprintf('%s/%s', remote, self@signature);
	remoteDir = .OGSremoteWorkingDir(self);
	sp = splitPath(remoteDir, ssh = T);
	Log(sprintf('setting up parallelization step in dir %s', remoteDir), 5);
	ignore.shell = Log.level() < 5;
	Dir.create(remoteDir, recursive = T, ignore.shell = ignore.shell);
	# either copy source files or explicitely spcified copyFiles (important for dirs);
	if (is.null(self@config$sourceFiles)) sourceFiles = c();
	if (is.null(self@config$copyFiles)) copyFiles = c();
	copyFiles =  unique(union(copyFiles, sourceFiles));
	Log(sprintf('Copying files: %s', join(copyFiles, ', ')), 5);
	File.copy(copyFiles, remoteDir, ignore.shell = ignore.shell, recursive = T, symbolicLinkIfLocal = T);
	# clear jids
	File.remove(.OGSremoteFile(self, 'jids'));
	# <p> remote environment: environment variables
	remoteProfile = remoteEnvSetup(remoteDir);

	# <p> create remote wrappers
	parallelize_remote = function(call_, Lapply_config) {
		parallelize_initialize(Lapply_config = Lapply_config,
			backend = Lapply_config$backend, copy_environments = Lapply_config$copy_environments);
		r = parallelize_internal(call_, parallelize_wait = F);
	};
	# <p> start rampup on remote host
	#remoteSourceFiles = list.kpu(lapply(self@config$sourceFiles, splitPath), 'file')
	#remoteSourceFiles = sapply(self@config$sourceFiles, function(path)splitPath(path)$file);
	remoteConfig = lapply_dispatch_config(self);
	freeze_control = list(
		sourceFiles = remoteConfig$sourceFiles,	#remoteSourceFiles
		libraries = remoteConfig$libraries,
		logLevel = Log.level(),
		freeze_relative = T
	);
	remoteConfig = .remoteConfigForOGSremote(stateDir = '.');
	Log('ParallelizeBackendOGSremote:initScheduling:callEvalArgs', 7);
	call_ = callEvalArgs(call_, env_eval = remoteConfig$copy_environments);
	Log('ParallelizeBackendOGSremote:initScheduling:freezeCallOGS', 7);
	r = freezeCallOGS(self, parallelize_remote,
		# parallelize_remote
		call_, Lapply_config = remoteConfig,
		# freeze
		freeze_control = freeze_control,
		freeze_file = sprintf('%s/rampUp:000.RData', remoteDir),
		# System
		patterns = c('cwd', 'qsub', 'ssh'),
		cwd = sp$path, ssh_host = sp$userhost,
		qsubPath = sprintf('%s/qsub', sp$path), qsubMemory = remoteConfig$qsubRampUpMemory,
		ssh_source_file = c(remoteConfig$ssh_source_file, remoteProfile), qsubOptionsAdd = '--exports=-'
#		ssh_source_file = c(remoteConfig$ssh_source_file, remoteProfile)
	);
	# end with
	});
	Log('ParallelizeBackendOGSremote:initScheduling:freezeCallOGS:after', 7);
	self@jids$pushStep(r$jid);
	r
});

# instead of doing something here, we poll the remote backend
setMethod('performParallelizationStep', 'ParallelizeBackendOGSremote',
	function(self, call_, Lapply_config) {
	# prevent from completing computation, result has to be gathered by polling
	Lapply_error();

	if (0) {
	stop('ParallelizeBackendOGSremote backend is a delegating backend and does not perform parallelization itself. Use the following to monitor this backend in a loop:
		r = NULL;
		p = pollParallelization(self);
		if (!p$continue) r = getResult(Lapply_backend__);
		r
	');
	}
});

.catVectorAsLine = function(message, width = options('width')$width) {
	messagePadded = sapply(message, function(line) sprintf('%s%*s', line, width - nchar(line) - 1, ' '));
	messageInALine = paste(messagePadded, collapse = '');
	cat(messageInALine);
	flush.console();
	cat("\r");
}

.catVector = function(message, width = options('width')$width, clear = T, padLines = 40) {
	if (clear) cat(paste(rep("\n", 100), collapse = ''));
	cat(paste(c(message, ''), collapse = "\n"));
	if (padLines > 0) cat(paste(rep("\n", padLines), collapse = ''));
}
catCr = function(message) {
	cat("\r");
	cat(message);
}


setMethod('pollParallelization', 'ParallelizeBackendOGSremote', function(self,
	options = list(printProgress = T)) {
	# <p> overwrite backend configuration
	remote_config = .remoteConfigForOGSremote();
	jidFile = .OGSremoteFile(self, 'jids');
	jids = get(Load(file = jidFile, Load_sleep = 30, Load_retries = 60)[[1]]);
	qstat_jids = .pollJids(patterns = 'ssh',
		ssh_host = splitPath(jidFile, ssh = T)$userhost, ssh_source_file = self@config$ssh_source_file);
	#print(jids); print(qstat_jids);
	message = .pollMessageRaw(jids, qstat_jids);
	# <p> check for completion
	continue = !File.exists(.OGSremoteFile(self, 'sentinel'));
# 	# <p> add rampup
# 	message = c(
# 		progressString(.progressStat(self@jids$steps, 1, qstat_jids), title = 'Rampup 1'),
# 		message
# 	);
	# <p> refine
	message = .pollMessage(message, continue);
	#.catVector(message);
	catCr(message);
	if (!continue) cat("\n");
	r = list(message = message, continue = continue);
	r
});

setMethod('lapply_dispatchFinalize', 'ParallelizeBackendOGSremote',
	function(self) { NULL });

setMethod('getResult', 'ParallelizeBackendOGSremote', function(self) {
	r = get(Load(file = .OGSremoteFile(self, 'result'))[1]);
	r
});
