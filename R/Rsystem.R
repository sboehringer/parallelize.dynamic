#
#	Rsystem.R
#Mon 27 Jun 2005 10:51:30 AM CEST 

#
#	<par> file handling
#

# <!><N> works only on atomic path
splitPath = function(path, removeQualifier = T, ssh = F, skipExists = F) {
	if (is.null(path)) return(NULL);
	if (removeQualifier) {
		q = fetchRegexpr('(?<=^\\[).*?(?=\\]:)', path);
		if (length(q) > 0) path = substr(path, nchar(q) + 4, nchar(path));
	}
	sshm = list(user = '', host = '', userhost = '');
	if (ssh) {
		sshm = fetchRegexpr('^(?:(?:([a-z]\\w*)(?:@))?([a-z][\\w.]*):)?(.*)', path,
			ignore.case = T, captureN = c('user', 'host', 'path'))[[1]];
		sshm$userhost = if (sshm$user != '') sprintf('%s@%s', sshm$user, sshm$host) else sshm$host;
		path = sshm$path;
	}

	#path = "abc/def.ext";
	#r.base = basename(path);
	#re = "([^.]*$)";
	#r = gregexpr(re, r.base)[[1]];
	#ext = substr(r.base, r[1], r[1] + attr(r, "match.length")[1] - 1);
	#ext = firstDef(fetchRegexpr('(?<=\\.)[^/.]+\\Z', path), '');
	ext = fetchRegexpr('(?<=\\.)[^/.]+\\Z', path);
	# take everything before ext and handle possible absence of '.'
	#base = substr(r.base, 1, r[1] - 1 - (ifelse(substr(r.base, r[1] - 1, r[1] - 1) == '.', 1, 0)));
	# reduce to file.ext
	base = basename(path);
	# chop off extension if present
	if (length(fetchRegexpr('\\.', base)) > 0) base = fetchRegexpr('\\A.*(?=\\.)', base);
	
	#pieces = regexpr(re, path, perl = T);
	pieces = fetchRegexpr('([^.]+)', path);
	isAbsolute = nchar(path) != 0 && substr(path, 1, 1) == '/';
	# <N> disk is accessed
	exists = if (!skipExists) File.exists(path, host = sshm$userhost, ssh = F) else NA;
	nonempty = exists && (file.info(path)$size > 0);
	ret = list(
		dir = dirname(path),
		base = base,
		path = path,
		fullbase = sprintf("%s/%s", dirname(path), base),
		ext = ext,
		file = basename(path),
		isAbsolute = isAbsolute,
		absolute = if (isAbsolute) path else sprintf('%s/%s', getwd(), path),
		# fs properties
		exists = exists, nonempty = nonempty,
		# remote
		is.remote = !(sshm$user == '' && sshm$host == ''),
			user = sshm$user, host = sshm$host, userhost = sshm$userhost
	);
	ret
}
path.absolute = absolutePath = function(path, home.dir = T) {
	if (home.dir && nchar(path) >= 2 && substr(path, 1, 2) == "~/")
		path = sprintf("%s/%s", Sys.getenv('HOME'), substr(path, 3, nchar(path)));
	if (nchar(path) > 0 && substr(path, 1, 1) == "/") path else sprintf("%s/%s", getwd(), path)
}
tempFileName = function(prefix, extension = NULL, digits = 6, retries = 5, inRtmp = F) {
	ext = if (is.null(extension)) '' else sprintf('.%s', extension);
	path = NULL;
	if (inRtmp) prefix = sprintf('%s/%s', tempdir(), prefix);
	for (i in 1:retries) {
		path = sprintf('%s%0*d%s', prefix, digits, floor(runif(1) * 10^digits), ext);
		if (!File.exists(path)) break;
	}
	if (File.exists(path))
		stop(sprintf('Could not create tempfile with prefix "%s" after %d retries', prefix, retries));
	# potential race condition <N>
	writeFile(path, '', mkpath = T, ssh = T);
	# # old implementation
	#path = tempfile(prefix);
	#cat('', file = path);	# touch path to lock name
	#path = sprintf("%s%s%s", path, ifelse(is.null(extension), "", "."),
	#	ifelse(is.null(extension), "", extension));
	Log(sprintf('Tempfilename:%s', path), 5);
	path
}
dirList = function(dir, regex = T, case = T) {
	base = splitPath(dir)$dir;
	files = list.files(base);
	if (regex) {
		re = splitPath(dir)$file;
		files = files[grep(re, files, perl = T, ignore.case = !case)];
	}
	files
}


write.csvs = function(t, path, semAppend = "-sem", ...) {
	s = splitPath(path);
	write.csv(t, path);
	pathSem = sprintf("%s%s.%s", s$fullbase, semAppend, s$ext);
	# make sure t is a data.frame or dec option will not take effect <A>
	#write.csv2(t, pathSem);
	write.table(t, file = pathSem, row.names = F, col.names = T, dec = ",", sep = ";");
}

#
#	<p> file manipulation
#

File.exists = function(path, host = '', agent = 'ssh', ssh = T) {
	if (ssh) {
		sp = splitPath(path, skipExists = T, ssh = T);
		host = sp$userhost;
		path = sp$path;
	}
	r = if (!is.null(host) && host != '') {
		ret = system(sprintf('%s %s stat %s >/dev/null 2>&1', agent, host, qs(path)));
		ret == 0
	} else file.exists(path);
	r
}

File.copy = function(from, to, ..., recursive = F, agent = 'scp', logLevel = 5, ignore.shell = T,
	symbolicLinkIfLocal = T) {
	spF = splitPath(from, ssh = T);
	spT = splitPath(to, ssh = T);
	is.remote.f = !spF$is.remote || spF$host == 'localhost';
	is.remote.t = !spT$is.remote || spT$host == 'localhost';

	r = if (!is.remote.f && !is.remote.t) {
		if (symbolicLinkIfLocal) {
			file.symlink(spF$path, spT$path, ...);
		} else file.copy(spF$path, spT$path, recursive = recursive, ...);
	} else {
		# <A> assume 'to' to be atomic
		System(sprintf('%s %s %s %s %s',
			agent,
			ifelse(recursive, '-r', ''),
			paste(sapply(from, qs), collapse = ' '),
			qs(to),
			ifelse(ignore.shell, '>/dev/null', '')
		), logLevel);
	}
	r
}

File.remove = function(path, ..., agent = 'ssh', ssh = T, logLevel = 5) {
	r = if (ssh) {
		sp = splitPath(path, skipExists = T, ssh = T);
		host = sp$userhost;
		rpath = sp$path;
		if (File.exists(path, ssh = T))
			System(sprintf('rm %s', join(sapply(rpath, qs))), pattern = agent,
				ssh_host = host, logLevel = logLevel);
	} else if (file.exists(path)) file.remove(path, ...);
	r
}

# <i> remote operations
File.symlink = function(from, to, replace = T, agent = 'ssh', ssh = F, logLevel = 5) {
	r = if (ssh) {
		sp = splitPath(from, skipExists = T, ssh = T);
		host = sp$userhost;
		rpath = sp$path;
		# <!><i>
		stop('not implmenented');
	} else {
		Log(sprintf('symlink %s -> %s', qs(from), qs(to)), logLevel);
		if (replace && file.exists(to)) file.remove(to);
		file.symlink(from, to);
	}
	r
}


# <!> only atomic path
#	treatAsFile: causes Dir.create to split off last path-component
Dir.create = function(path, ..., recursive = F, agent = 'ssh', logLevel = 5,
	ignore.shell = T, allow.exists = T, treatPathAsFile = F) {
	sp = splitPath(path, ssh = T);
	# ignore last path-component
	if (treatPathAsFile) {
		sp$path = sp$dir;
		Log(sprintf('creating path %s', sp$path), 4);
	}
	if (sp$is.remote) {
		System(sprintf('ssh %s mkdir %s %s %s',
			sp$userhost,
			if (recursive) '--parents' else '',
			paste(sapply(sp$path, qs), collapse = ' '),
			if (ignore.shell) '2>/dev/null' else ''
		), logLevel);
	} else {
		if (allow.exists && !file.exists(sp$path)) dir.create(sp$path, ..., recursive = recursive);
	}
}

Save = function(..., file = NULL, symbolsAsVectors = F, mkpath = T, envir = parent.frame(1)) {
	sp = splitPath(file, ssh = T);
	localPath = if (sp$is.remote) tempfile() else file;
	if (mkpath) { Dir.create(file, recursive = T, treatPathAsFile = T); }
	r = if (symbolsAsVectors) {
		do.call('save', c(as.list(c(...)), list(file = localPath)), envir = envir);
	} else save(..., file = localPath, envir = envir);
	if (sp$is.remote) File.copy(localPath, file);
	r
}
Load = function(..., file = NULL, Load_sleep = 0, Load_retries = 3, envir = parent.frame(1)) {
	sp = splitPath(file, ssh = T);
	localPath = if (sp$is.remote) tempfile() else file;
	r = NULL;
	for (i in 1:Load_retries) {
		if (sp$is.remote) {
			if (!File.exists(file)) {
				Sys.sleep(Load_sleep);
				next;
			}
			File.copy(file, localPath);
		}
		r = try(load(..., file = localPath, envir = envir));
		if (class(r) == 'try-error' && Load_sleep > 0) Sys.sleep(Load_sleep) else break;
	}
	if (is.null(r)) stop(sprintf('could not Load %s', file));
	if (class(r) == 'try-error') stop(r[1]);
	r
}

#
#	create output file names
# output = list(prefix = "results/pch", extension = "pdf", tag = "20100727");
fileName = function(output, extension = NULL, subtype = NULL) {
	if (is.null(output)) return(NULL);
	if (is.null(output$prefix)) return(NULL);
	subtype = firstDef(subtype, output$subtype, "");
	if (subtype != "") subtype =  sprintf("%s-", subtype);
	r = sprintf("%s-%s%s.%s", output$prefix, subtype, output$tag,
		firstDef(extension, output$extension, ""));
	Log(r, 4);
	r
}
#.globalOutput = list(prefix = 'results/20120126-');
#save(r, file = .fn('simulation', 'RData'))
.globalOutputDefault = .globalOutput = list(prefix = '', tag = NULL, tagFirst = F);
GlobalOutput_env__ = new.env();
# .fn.set(prefix = 'results/predictionTesting-')
.fn.set = function(...) {
	.globalOutput = merge.lists(.globalOutputDefault, list(...));
	assign('.globalOutput', .globalOutput, envir = GlobalOutput_env__);
}
# create output file name on globalOptions
.fn = function(name, extension = '', options = NULL) {
	o = merge.lists(.globalOutputDefault, .globalOutput,
		get('.globalOutput', envir = GlobalOutput_env__), options);
	# construct plain filename
	sp = splitPath(sprintf('%s%s%s%s', o$prefix, name, ifelse(extension == '', '', '.'), extension));
	# <p> dir
	if (!file.exists(sp$dir)) dir.create(sp$dir);
	# <p> tag
	ext = firstDef(sp$ext, '');
	fn = if (!is.null(o$tag)) {
		if (o$tagFirst) {
			sprintf('%s/%s-%s%s%s', sp$dir, o$tag, sp$base, ifelse(ext == '', '', '.'), ext)
		} else { sprintf('%s/%s-%s%s%s', sp$dir, sp$base, o$tag, ifelse(ext == '', '', '.'), ext) };
	} else sprintf('%s/%s%s%s', sp$dir, sp$base, ifelse(ext == '', '', '.'), ext);
	fn
}
.fn.pushPrefix = function(prefix) {
	output = merge.lists(.globalOutput, list(prefix = sprintf('%s%s', .globalOutput$prefix, prefix)));
	assign('.globalOutput', output, envir = GlobalOutput_env__);
	.globalOutput
}
.fn.popPrefix = function(prefix) {
	output = merge.lists(.globalOutput, list(prefix = sprintf('%s/', splitPath(.globalOutput$prefix)$dir)));
	assign('.globalOutput', output, envir = GlobalOutput_env__);
	.globalOutput
}

#
#	command argument handling
#

# default args: command line call minus command
evaluateArgs = function(c = commandArgs()[-1]) {
	is.no.option = is.na(as.integer(sapply(c, function(a)grep("^--", a))));
	#c = c[!(c == "--vanilla")];	# eliminate '--vanilla' arguments
	c = c[is.no.option];
	if (length(c) > 0) {
		eval.parent(parse(text = c[1]));
		argListString = gsub(";", ",", gsub(";$", "", c[1]));
		print(argListString);
		return(eval(parse(text = sprintf("list(%s)", argListString))));
	}
	return(NULL);
}

# default args: command line call minus command
getCommandOptions = function(c = commandArgs()[-1]) {
	is.no.option = is.na(as.integer(sapply(c, function(a)grep("^--", a))));
	#c = c[!(c == "--vanilla")];	# eliminate '--vanilla' arguments
	c = c[is.no.option];
	o = lapply(c, function(e) {
		eval(parse(text = e));
		nlapply(setdiff(ls(), 'e'), function(n)get(n))
	});
	o = unlist.n(o, 1);
	o
}

# R.pl interface

handleTriggers = function(o, triggerDefinition = NULL) {
	if (is.null(triggerDefinition)) triggerDefinition = rget('.globalTriggers');
	if (!is.list(o) || is.null(triggerDefinition)) return(NULL);
	for (n in names(triggerDefinition)) {
		if (!is.null(o[[n]])) triggerDefinition[[n]](o$args, o);
	}

}


#
#	level dependend logging
#
#Global..Log..Level = 4;
#Default..Log..Level = 4;
#assign(Default..Log..Level, 4, envir = .GlobalEnv);
Log_env__ <- new.env();
assign('DefaultLogLevel', 4, envir = Log_env__);

#' Log a message to stderr.
#' 
#' Log a message to stderr. Indicate a logging level to control verbosity.
#' 
#' This function prints a message to stderr if the condition is met that a
#' global log-level is set to greater or equal the value indicated by
#' \code{level}. \code{Log.level} returns the current logging level.
#' 
#' @aliases Log Log.setLevel Log.level
#' @param o Message to be printed.
#' @param level If \code{Log.setLevel} was called with this value, subsequent
#' calls to \code{Log} with values of \code{level} smaller or equal to this
#' value will be printed.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{Log.setLevel}}, ~~~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' 	Log.setLevel(4);
#' 	Log('hello world', 4);
#' 	Log.setLevel(3);
#' 	Log('hello world', 4);
#' 
Log = function(o, level = get('DefaultLogLevel', envir = Log_env__)) {
	if (level <= get('GlobalLogLevel', envir = Log_env__)) {
		cat(sprintf("R %s: %s\n", date(), as.character(o)));
	}
}
Log.level = function()get('GlobalLogLevel', envir = Log_env__);
Log.setLevel = function(level = get('GlobalLogLevel', envir = Log_env__)) {
	assign("GlobalLogLevel", level, envir = Log_env__);
}
Log.setLevel(4);	# default

# quote if needed
qs = function(s) {
	# <N> better implementation possible: detect unquoted white-space
	if (length(fetchRegexpr('[ \t]', s)) > 0) {
		s = gsub('([\\"])', '\\\\\\1', s);
		s = sprintf('"%s"', s);
	} else {
		s0 = gsub("([\\'])", '\\\\\\1', s);
		if (s0 != s) s = sprintf("$'%s'", s0);
	}
	s
}
.System.fileSystem = list(
	#tempfile = function(prefix, ...)tempfile(splitPath(prefix)$base, tmpdir = splitPath(prefix)$dir, ...),
	tempfile = function(prefix, ...)tempFileName(prefix, ...),
	readFile = function(...)readFile(...)
);
.System.patterns = list(
	default = list(pre = function(cmd, ...)cmd, post = function(spec, ret, ...)list()	),
	qsub = list(pre = function(cmd, spec,
		jidFile = spec$fs$tempfile(sprintf('/tmp/R_%s/qsub_pattern', Sys.getenv('USER'))),
		qsubOptions = '',
		waitForJids = NULL, ...) {
		Dir.create(jidFile, treatPathAsFile = TRUE);
		waitOption = if (is.null(waitForJids)) '' else
			sprintf('--waitForJids %s', join(waitForJids, sep = ','));
		print(cmd);
		ncmd = sprintf('qsub.pl --jidReplace %s %s --unquote %s -- %s',
			jidFile, waitOption, qsubOptions, qs(cmd));
		print(ncmd);
		spec = list(cmd = ncmd, jidFile = jidFile);
		spec
	},
	post = function(spec, ret, ...) { list(jid = as.integer(spec$fs$readFile(spec$jidFile))) }
	),
	
	cwd = list(pre = function(cmd, spec, ..., cwd = '.') {
		ncmd = sprintf('cd %s ; %s', qs(cwd), cmd);
		spec = list(cmd = ncmd);
		spec
	},
	post = function(spec, ret, ...) { list() }
	),
	# <i> stdout/stderr handling
	ssh = list(pre = function(cmd, spec, ssh_host = 'localhost', ssh_source_file = NULL, ...) {
		if (!is.null(ssh_source_file)) {
			cmd = sprintf('source %s ; %s', qs(ssh_source_file), cmd);
		}
		ncmd = sprintf('ssh %s %s', ssh_host, qs(cmd));
		spec = list(cmd = ncmd);
		spec
	},
	fs = function(fs, ..., ssh_host) {
		list(
			tempfile = function(prefix, ...) {
				Log(sprintf('tempfile ssh:%s', prefix), 1);
				r = splitPath(tempFileName(sprintf('%s:%s', ssh_host, prefix), ...), ssh = T)$path;
				Log(sprintf('tempfile sshr:%s', r), 1);
				r
			},
			readFile = function(path, ...)readFile(sprintf('%s:%s', ssh_host, path), ..., ssh = T)
		);
	},
	post = function(spec, ret, ...) { list() }
	)
);
#
#	a system call (c.f. privatePerl/TempFilenames::System)
#
System_env__ <- new.env();
System = function(cmd, logLevel = get('DefaultLogLevel', envir = Log_env__),
	doLog = TRUE, printOnly = NULL, return.output = F,
	pattern = NULL, patterns = NULL, ..., return.cmd = F) {
	# prepare
	if (!exists(".system.doLogOnly", envir = System_env__))
		assign(".system.doLogOnly", F, envir = System_env__);
	doLogOnly = ifelse (!is.null(printOnly), printOnly, get('.system.doLogOnly', envir = System_env__));

	# pattern mapping
	fs = .System.fileSystem;
	if (!is.null(patterns)) {
		spec = list();
		# map file accesses
		for (pattern in rev(patterns)) {
			fsMapper = .System.patterns[[pattern]]$fs;
			if (!is.null(fsMapper)) fs = fsMapper(fs, ...);
			spec[[length(spec) + 1]] = list(fs = fs);
		}
		# wrap commands into each other
		for (i in 1:length(patterns)) {
			spec[[i]] = merge.lists(spec[[i]], .System.patterns[[patterns[[i]]]]$pre(cmd, spec[[i]], ...));
			cmd = spec[[i]]$cmd;
		}
	} else if (!is.null(pattern)) {
		spec = .System.patterns[[pattern]]$pre(cmd, list(fs = fs), ...);
		spec$fs = fs;	# manually install fs
		cmd = spec$cmd;
	}
	# redirection (after patterns) <A>
	if (return.output & !doLogOnly) {
		tmpOutput = tempfile();
		cmd = sprintf("%s > %s", cmd, tmpOutput);
	}
	# logging
	if (doLog){ Log(sprintf("system: %s", cmd), logLevel); }
	# system call
	ret = NULL;
	if (!doLogOnly) ret = system(cmd);
	# return value
	r = list(error = ret);
	if (return.output & !doLogOnly) {
		r = merge.lists(r, list(error = ret, output = readFile(tmpOutput)));
	}
	# postprocess
	if (!doLogOnly) if (!is.null(patterns)) {
		for (i in rev(1:length(patterns))) {
			r = merge.lists(r, .System.patterns[[patterns[[i]]]]$post(spec[[i]], ret, ...));
		}
	} else if (!is.null(pattern)) {
		r = merge.lists(r, .System.patterns[[pattern]]$post(spec, ret, ...));
	}
	if (return.cmd) r$command = cmd;
	# simplified output
	if (!return.output && !return.cmd && is.null(pattern)) r = r$error;
	r
}

# wait on job submitted by system
.System.wait.patterns = list(
	default = function(r, ...)(NULL),
	qsub = function(r, ...) {
		ids = if (is.list(r[[1]]) & !is.null(r[[1]]$jid)) list.kp(r, 'jid', do.unlist = T) else r$jid;
		idsS = if (length(ids) == 0) '' else paste(ids, collapse = ' ');
		System(sprintf('qwait.pl %s', idsS), ...);
	}
);
System.wait = function(rsystem, pattern = NULL, ...) {
	r = if (!is.null(pattern)) .System.wait.patterns[[pattern]](rsystem, ...) else NULL;
	r
}

System.SetDoLogOnly = function(doLogOnly = F) {
	assign(".system.doLogOnly", doLogOnly, envir = System_env__);
}

ipAddress = function(interface = "eth0") {
	o = System(sprintf("/sbin/ifconfig %s", interface), logLevel = 6, return.output = T);
	ip = fetchRegexpr("(?<=inet addr:)[^ ]+", o$output);
	ip
}


#
#	<p> cluster abstraction
#
# Example:
#specifyCluster(localNodes = 8, sourceFiles = c('RgenericAll.R', 'dataPreparation.R'));
#.clRunLocal = F;
#data.frame.types(clapply(l, f, arg1 = 1), rbind = T, do.transpose = T);

# default cluster configuration
.defaultClusterConfig = list(
	hosts = list(list(host = "localhost", count = 2, type = "PSOCK")), local = F,
	provideChunkArgument = F, reverseEvaluationOrder = T, splitN = 4, reuseCluster = F,
	nestingLevel = 0,	# records the nesting of clapply calls
	splittingLevel = 1,	# specifies at which level clapply should parallelize
	evalEnvironment = F	# call environment_eval on function before passing on
);
Snow_cluster_env__ = new.env();
specifyCluster = function(localNodes = 8, sourceFiles = NULL, cfgDict = list(), hosts = NULL,
	.doSourceLocally = F, .doCopy = T, splitN = NULL, reuseCluster = F, libraries = NULL,
	evalEnvironment = F) {
	cfg = merge.lists(.defaultClusterConfig,
		cfgDict,
		list(splitN = splitN, reuseCluster = reuseCluster, evalEnvironment = evalEnvironment),
		list(local = F, source = sourceFiles, libraries = libraries, hosts = (if(is.null(hosts))
			list(list(host = "localhost", count = localNodes, type = "PSOCK", environment = list())) else
				hosts)
	));
	assign(".globalClusterSpecification", cfg, envir = Snow_cluster_env__);
	.globalClusterSpecification = get('.globalClusterSpecification', envir = Snow_cluster_env__);
	if (.doCopy) {
		for (h in .globalClusterSpecification$hosts) {
			if (h$host != "localhost" & !is.null(h$env$setwd)) {
				System(sprintf("ssh %s mkdir '%s' 2>/dev/null", h$host, h$env$setwd), 5);
				System(sprintf("scp '%s' %s:'%s' >/dev/null", paste(sourceFiles, collapse = "' '"),
					h$host, h$env$setwd), 5);
			}
		}
	}
	if (.doSourceLocally) {
		sourceFiles = setdiff(sourceFiles, "RgenericAll.R");	# assume we have been sourced
		eval(parse(text =
			paste(sapply(sourceFiles, function(s)sprintf("source('%s', chdir = TRUE);", s)), collapse = "")));
	}
}

#<!> might not be available/outdated
library('parallel');
# l: list, f: function, c: config
# <i><!> test clCfg$reverseEvaluationOrder before uncommenting
clapply_cluster = function(l, .f, ..., clCfg = NULL) {
	#if (clCfg$reverseEvaluationOrder) l = rev(l);

	# only support SOCK type right now <!><i>
	hosts = unlist(sapply(clCfg$hosts, function(h){
		if (h$type == "PSOCK") rep(h$host, h$count) else NULL}));
	master = ifelse(all(hosts == "localhost"), "localhost", ipAddress("eth0"));
	establishEnvironment = T;
	cl = if (clCfg$reuseCluster) {
		if (!exists(".globalClusterObject")) {
			assign(".globalClusterObject", makeCluster(hosts, type = "PSOCK", master = master),
				envir = Snow_cluster_env__);
		} else establishEnvironment = FALSE;
		get('.globalClusterObject', envir = Snow_cluster_env__)
	} else makeCluster(hosts, type = "PSOCK", master = master);
	#clusterSetupRNG(cl);	# snow
	clusterSetRNGStream(cl, iseed = NULL);	# parallel

	clusterExport(cl, clCfg$vars);

	# <p> establish node environment
	envs = listKeyValue(list.key(clCfg$hosts, "host"), list.key(clCfg$hosts, "environment", unlist = F));
	if (establishEnvironment) r = clusterApply(cl, hosts, function(host, environments, cfg){
		env = environments[[host]];
		if (!is.null(env$setwd)) setwd(env$setwd);
		if (!is.null(cfg$source)) for (s in cfg$source) source(s, chdir = TRUE);
		if (!is.null(cfg$libraries)) for (package in cfg$libraries) library(package, character.only = TRUE);
		# <!> as of 3.4.2013: stop support of exporting global variables to enable CRAN submission
		#if (!is.null(env$globalVars))
		#	for (n in names(env$globalVars)) assign(n, env$globalVars[[n]], pos = .GlobalEnv);
		#sprintf("%s - %s - %s", host, hapmap, getwd());
		NULL
	}, environments = envs, cfg = clCfg);

	# <p> iterate
	N = clCfg$splitN * length(hosts);	# No of splits
	idcs = splitListIndcs(length(l), N);
	exportNames = c();
	iterator__ = if (clCfg$provideChunkArgument) {
		function(.i, ...) {
			r = lapply(idcs[.i, 1]:idcs[.i, 2], function(j)try(.f(l[[j]], .i, ...)));
			if (class(r) == "try-error") r = NULL;
			r
		}
	} else {
		function(.i, ...){
			r = lapply(idcs[.i, 1]:idcs[.i, 2], function(j)try(.f(l[[j]], ...)));
			if (class(r) == "try-error") r = NULL;
			r
		}
	}
	if (clCfg$evalEnvironment) {
		iterator__ = environment_eval(iterator__, functions = T);
		#clusterExport(cl, varlist = names(as.list(environment(iterator__))), envir = environment(iterator__));
	}
	r = clusterApplyLB(cl, 1:dim(idcs)[1], iterator__, ...);
	# <p> finish up
	if (!clCfg$reuseCluster) stopCluster(cl)
	r = unlist(r, recursive = F);
	#if (clCfg$reverseEvaluationOrder) r = rev(r);
	r
}

# wrapper (as of 3.12.8: I seem to have lost a previous change)
clapply = function(l, .f, ..., clCfg = NULL, .clRunLocal = rget(".clRunLocal", F, envir = .GlobalEnv)) {
	# <p> get cluster specification
	clCfg = merge.lists(
		rget(".globalClusterSpecification", default = list(), envir = Snow_cluster_env__),
		firstDef(clCfg, list())
	);
	# <p> update cluster specification
	clCfg$nestingLevel = clCfg$nestingLevel + 1;
	assign(".globalClusterSpecification", clCfg, envir = Snow_cluster_env__);

	# <p> choose/decline parallelization
	r = if (firstDef(.clRunLocal, clCfg$local, F) || clCfg$nestingLevel != clCfg$splittingLevel) {
		if (clCfg$provideChunkArgument) lapply(X = l, FUN = .f, 1, ...)
		else lapply(X = l, FUN = .f, ...)
	} else {
		clapply_cluster(l, .f, ..., clCfg = clCfg);
	};

	# <p> update cluster specification
	clCfg$nestingLevel = clCfg$nestingLevel - 1;
	assign(".globalClusterSpecification", clCfg, envir = Snow_cluster_env__);
	r
}

#
#	<p> Meta-functions
#

#
#		Environments
#

# copy functions code adapted from restorepoint R package
object.copy = function(obj) {
	# Dealing with missing values
	if (is.name(obj)) return(obj);
	obj_class = class(obj);

	copy =
		if ('environment' %in% obj_class) environment.copy(obj) else
		if (all('list' == class(obj))) list.copy(obj) else
		#if (is.list(obj) && !(is.data.frame(obj))) list.copy(obj) else
		obj;
	return(copy)
}
list.copy = function(l)lapply(l, object.copy);
environment.restrict = function(envir__, restrict__= NULL) {
	if (!is.null(restrict__)) {
		envir__ = as.environment(List_(as.list(envir__), min_ = restrict__));
	}
	envir__
}
environment.copy = function(envir__, restrict__= NULL) {
	as.environment(eapply(environment.restrict(envir__, restrict__), object.copy));
}

bound_vars = function(f, functions = F) {
	fms = formals(f);
	# variables bound in default arguments
	vars_defaults = unique(unlist(sapply(fms, function(e)all.vars(as.expression(e)))));
	# variables used in the body
	vars_body = setdiff(all.vars(body(f)), names(fms));
	vars = setdiff(unique(c(vars_defaults, vars_body)), c('...', '', '.GlobalEnv'));
	if (functions) {
		vars = vars[!sapply(vars, function(v)is.function(rget(v, envir = environment(f))))];
	}
	vars
}
bound_fcts_std_exceptions = c('Lapply', 'Sapply', 'Apply');
bound_fcts = function(f, functions = F, exceptions = bound_fcts_std_exceptions) {
	fms = formals(f);
	# functions bound in default arguments
	fcts_defaults = unique(unlist(sapply(fms, function(e)all.vars(as.expression(e), functions = T))));
	# functions bound in body
	fcts = union(fcts_defaults, all.vars(body(f), functions = T));
	# remove variables
	#fcts = setdiff(fcts, c(bound_vars(f, functions), names(fms), '.GlobalEnv', '...'));
	fcts = setdiff(fcts, c(bound_vars(f, functions = functions), names(fms), '.GlobalEnv', '...'));
	# remove functions from packages
	fcts = fcts[
		sapply(fcts, function(e) {
			f_e = rget(e, envir = environment(f));
			!is.null(f_e) && environmentName(environment(f_e)) %in% c('R_GlobalEnv', '') && !is.primitive(f_e)
	})];
	fcts = setdiff(fcts, exceptions);
	fcts
}


environment_evaled = function(f, functions = F) {
	vars = bound_vars(f, functions);
	e = nlapply(vars, function(v) rget(v, envir = environment(f)));
	#Log(sprintf('environment_evaled: vars: %s', join(vars, ', ')), 7);
	#Log(sprintf('environment_evaled: functions: %s', functions), 7);
	if (functions) {
		fcts = bound_fcts(f, functions = T);
		fcts_e = nlapply(fcts, function(v){
			#Log(sprintf('environment_evaled: fct: %s', v), 7);
			v = rget(v, envir = environment(f));
			#if (!(environmentName(environment(v)) %in% c('R_GlobalEnv')))
			v = environment_eval(v, functions = T);
		});
		#Log(sprintf('fcts: %s', join(names(fcts_e))));
		e = c(e, fcts_e);
	}
	#Log(sprintf('evaled: %s', join(names(e))));
	r = new.env();
	lapply(names(e), function(n)assign(n, e[[n]], envir = r));
	#r = if (!length(e)) new.env() else as.environment(e);
	parent.env(r) = .GlobalEnv;
	#Log(sprintf('evaled: %s', join(names(as.list(r)))));
	r
}
environment_eval = function(f, functions = F) {
	environment(f) = environment_evaled(f, functions = functions);
	f
}

#
#		Freeze/thaw
#

delayed_objects_env = new.env();
delayed_objects_attach = function() {
	attach(delayed_objects_env);
}
delayed_objects_detach = function() {
	detach(delayed_objects_env);
}

thaw_list = function(l)lapply(l, thaw_object, recursive = T);
thaw_environment = function(e) {
	p = parent.env(e);
	r = as.environment(thaw_list(as.list(e)));
	parent.env(r) = p;
	r
}

# <i> sapply
thaw_object_internal = function(o, recursive = T, envir = parent.frame()) {
	r = 		 if (class(o) == 'ParallelizeDelayedLoad') thaw(o) else
	#if (recursive && class(o) == 'environment') thaw_environment(o) else
	if (recursive && class(o) == 'list') thaw_list(o) else o;
	r
}

thaw_object = function(o, recursive = T, envir = parent.frame()) {
	if (all(search() != 'delayed_objects_env')) delayed_objects_attach();
	thaw_object_internal(o, recursive = recursive, envir = envir);
}

#
#	<p> backend classes
#

setGeneric('thaw', function(self, which = NA) standardGeneric('thaw'));

setClass('ParallelizeDelayedLoad',
	representation = list(
		path = 'character'
	),
	prototype = list(path = NULL)
);
setMethod('initialize', 'ParallelizeDelayedLoad', function(.Object, path) {
	.Object@path = path;
	.Object
});

setMethod('thaw', 'ParallelizeDelayedLoad', function(self, which = NA) {
	if (0) {
	key = sprintf('%s%s', self@path, ifelse(is.na(which), '', which));
	if (!exists(key, envir = delayed_objects_env)) {
		Log(sprintf('Loading: %s; key: %s', self@path, key), 4);
		ns = load(self@path);
		object = get(if (is.na(which)) ns[1] else which);
		assign(key, object, envir = delayed_objects_env);
		gc();
	} else {
		#Log(sprintf('Returning existing object: %s', key), 4);
	}
	#return(get(key, envir = delayed_objects_env));
	# assume delayed_objects_env to be attached
	return(as.symbol(key));
	}

	delayedAssign('r', {
		gc();
		ns = load(self@path);
		object = get(if (is.na(which)) ns[1] else which);
		object
	});
	return(r);
});

RNGuniqueSeed = function(tag) {
	md5 = md5sumString(join(c(.Random.seed, tag), ''));
	r = list(
		kind = RNGkind(),
		seed = hex2int(substr(md5, 1, 8))
	);
	r
}

RNGuniqueSeedSet = function(seed) {
	RNGkind(seed$kind[1], seed$kind[2]);
	#.Random.seed = freeze_control$rng$seed;
	set.seed(seed$seed);
}

FreezeThawControlDefaults = list(
	dir = '.', sourceFiles = c(), libraries = c(), objects = c(), saveResult = T,
	freeze_relative = F, freeze_ssh = T, logLevel = Log.level()
);

thawCall = function(
	freeze_control = FreezeThawControlDefaults,
	freeze_tag = 'frozenFunction', freeze_file = sprintf('%s/%s.RData', freeze_control$dir, freeze_tag)) {

	load(freeze_file, envir = .GlobalEnv);
	r = with(callSpecification, {
		for (library in freeze_control$libraries) {
			eval(parse(text = sprintf('library(%s)', library)));
		}
		for (s in freeze_control$sourceFiles) source(s, chdir = T);
		Log.setLevel(freeze_control$logLevel);
		if (!is.null(freeze_control$rng)) RNGuniqueSeed(freeze_control$rng);

		if (is.null(callSpecification$freeze_envir)) freeze_envir = .GlobalEnv;
		# <!> freeze_transformation must be defined by the previous source/library calls
		transformation = eval(parse(text = freeze_control$thaw_transformation));
		r = do.call(eval(parse(text = f)), transformation(args), envir = freeze_envir);
		#r = do.call(f, args);
		if (!is.null(freeze_control$output)) save(r, file = freeze_control$output);
		r
	});
	r
}

frozenCallWrap = function(freeze_file, freeze_control = FreezeThawControlDefaults,
	logLevel = Log.level(), remoteLogLevel = logLevel)
	with(merge.lists(FreezeThawControlDefaults, freeze_control), {
	sp = splitPath(freeze_file, ssh = freeze_ssh);
	file = if (freeze_relative) sp$file else sp$path;
	#wrapperPath = sprintf("%s-wrapper.RData", splitPath(file)$fullbase);
	r = sprintf("R.pl --template raw --no-quiet --loglevel %d --code 'eval(get(load(\"%s\")[[1]]))' --",
		logLevel, file);
	r
})

frozenCallResults = function(file) {
	callSpecification = NULL;	# define callSpecification
	load(file);
	get(load(callSpecification$freeze_control$output)[[1]]);
}

freezeCallEncapsulated = function(call_,
	freeze_control = FreezeThawControlDefaults,
	freeze_tag = 'frozenFunction', freeze_file = sprintf('%s/%s.RData', freeze_control$dir, freeze_tag),
	freeze_save_output = F, freeze_objects = NULL, thaw_transformation = identity)
	with(merge.lists(FreezeThawControlDefaults, freeze_control), {

	sp = splitPath(freeze_file, ssh = freeze_ssh);
	outputFile = if (freeze_save_output)
		sprintf("%s_result.RData", if (freeze_relative) sp$base else sp$fullbase) else
		NULL;

	callSpecification = list(
		f = deparse(call_$fct),
		#f = freeze_f,
		args = call_$args,
		freeze_envir = if (is.null(call_$envir)) new.env() else call_$envir,
		freeze_control = list(
			sourceFiles = sourceFiles,
			libraries = libraries,
			output = outputFile,
			rng = freeze_control$rng,
			logLevel = freeze_control$logLevel,
			thaw_transformation = deparse(thaw_transformation)
		)
	);
	thawFile = if (freeze_relative) sp$file else sp$path;
	callWrapper = call('thawCall', freeze_file = thawFile);
	#Save(callWrapper, callSpecification, thawCall, file = file);
	#Save(c('callWrapper', 'callSpecification', 'thawCall', objects),
	#	file = freeze_file, symbolsAsVectors = T);
	#Save(c(c('callWrapper', 'callSpecification', 'thawCall'), objects),
	Save(c('callWrapper', 'callSpecification', 'thawCall', freeze_objects),
		file = freeze_file, symbolsAsVectors = T);
	freeze_file
})

# <!> assume matched call
# <A> we only evaluate named args
callEvalArgs = function(call_, env_eval = F) {
	#if (is.null(call_$envir__) || is.null(names(call_$args))) return(call_);
	#if (is.null(call_$envir) || !length(call_$args)) return(call_);

	# <p> evaluate args
	if (length(call_$args)) {
		args = call_$args;
		callArgs = lapply(1:length(args), function(i)eval(args[[i]], envir = call_$envir__));
		# <i> use match.call instead
		names(callArgs) = setdiff(names(call_$args), '...');
		call_$args = callArgs;
	}

	if (env_eval) {
		call_$fct = environment_eval(call_$fct, functions = T);
	}
	# <p> construct return value
	#callArgs = lapply(call_$args, function(e){eval(as.expression(e), call_$envir)});
	call_
}

#callWithFunctionArgs = function(f, args, envir__ = parent.frame(), name = NULL) {
callWithFunctionArgs = function(f, args, envir__ = environment(f), name = NULL, env_eval = F) {
	if (env_eval) f = environment_eval(f, functions = T);
	call_ = list(
		fct = f,
		envir = environment(f),
		args = args,
		name = name
	);
	call_
}

freezeCall = function(freeze_f, ...,
	freeze_control = FreezeThawControlDefaults,
	freeze_tag = 'frozenFunction', freeze_file = sprintf('%s/%s.RData', freeze_control$dir, freeze_tag),
	freeze_save_output = F, freeze_envir = parent.frame(), freeze_objects = NULL, freeze_env_eval = F,
	thaw_transformation = identity) {

	# args = eval(list(...), envir = freeze_envir)
	call_ = callWithFunctionArgs(f = freeze_f, args = list(...),
		envir__ = freeze_envir, name = as.character(sys.call()[[2]]), env_eval = freeze_env_eval);

	freezeCallEncapsulated(call_,
		freeze_control = freeze_control, freeze_tag = freeze_tag,
		freeze_file = freeze_file, freeze_save_output = freeze_save_output, freeze_objects = freeze_objects,
		thaw_transformation = thaw_transformation
	);
}


encapsulateCall = function(.call, ..., envir__ = environment(.call), do_evaluate_args__ = FALSE,
	unbound_functions = F) {
	# function body of call
	name = as.character(.call[[1]]);
	fct = get(name);
	callm = if (!is.primitive(fct)) {
		callm = match.call(definition = fct, call = .call);
		as.list(callm)[-1]
	} else as.list(.call)[-1];
	args = if (do_evaluate_args__) {
		nlapply(callm, function(e)eval(callm[[e]], envir = envir__))
	} else nlapply(callm, function(e)callm[[e]])
	# unbound variables in body fct
	unbound_vars = 

	call_ = list(
		fct = fct,
		envir = envir__,

		#args = as.list(sys.call()[[2]])[-1],
		args = args,

		name = name
	);
	call_
}

evalCall = function(call) {
	call = callEvalArgs(call);
	do.call(call$f, call$args, envir = call$envir)
}

# envirArgs: non-functional, depracated
Do.call = function(what, args, quote = FALSE, envir = parent.frame(),
	defaultEnvir = .GlobalEnv, envirArgs = NULL, do_evaluate_args = F) {
	if (is.null(envir)) envir = defaultEnvir;
	if (do_evaluate_args) args = nlapply(args, function(e)eval(args[[e]], envir = envir));
	do.call(what = what, args = args, quote = quote, envir = envir)
}


#
#	</p> freeze/thaw functions
#

#
#	<p> file operations
#

#' Return absolute path for name searched in search-pathes
#'
#' Search for pathes.
#'
#' @param as.dirs assume that prefixes are pathes, i.e. a slash will be put between path and prefix
#' @param force enforces that path and prefix are always joined, otherwise if path is absolute no prefixing is performed
file.locate = function(path, prefixes = NULL, normalize = T, as.dirs = T, force = F) {
	if (!force && substr(path, 1, 1) == '/') return(path);
	if (is.null(prefixes)) prefixes = if (as.dirs) '.' else '';
	sep = ifelse(as.dirs, '/', '');
	for (prefix in prefixes) {
		npath = sprintf('%s%s%s', prefix, sep, path);
		if (normalize) npath = path.absolute(npath);
		if (file.exists(npath)) return(npath);
	}
	NULL
}

#' Read content of file and return as character object.
#' 
#' Read content of file and return as character object.
#' 
#' Read content of file and return as character object.
#' 
#' @param path Path to the file to be read.
#' @param prefixes Search for file by prepending character strings from
#' prefixes.
#' @param normalize Standardize pathes.
#' @param ssh Allow pathes to remote files in \code{scp} notation.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'   parallel8 = function(e) log(1:e) %*% log(1:e);
#'   cat(readFile(tempcodefile(parallel8)));
#' 
# prefixes only supported locally <!>
readFile = function(path, prefixes = NULL, normalize = T, ssh = F) {
	s = splitPath(path, ssh = ssh);
	r = if (s$is.remote) {
		tf = tempfile();
		File.copy(path, tf);
		readChar(tf, nchars = as.list(file.info(tf)[1,])$size);
	} else {
		if (!is.null(prefixes)) path = file.locate(path, prefixes, normalize);
		readChar(path, nchars = as.list(file.info(path)[1,])$size);
	}
	r
}

writeFile = function(path, str, mkpath = F, ssh = F) {
	s = splitPath(path, ssh = ssh);
	if (s$is.remote) {
		Dir.create(sprintf('%s:%s', s$userhost, s$dir), recursive = mkpath);
		tf = tempfile();
		out = file(description = tf, open = 'w', encoding='UTF-8');
			cat(str, file = out, sep = "");
		close(con = out);
		File.copy(tf, path);
	} else {
		if (mkpath) {
			if (!file.exists(s$dir)) dir.create(s$dir, recursive = T);
		}
		out = file(description = path, open = 'w', encoding='UTF-8');
			cat(str, file = out, sep = "");
		close(con = out);
	}
	path
}

# <!> local = T does not work
Source = function(file, ...,
	locations = c('.', sprintf('%s/src/Rscripts', Sys.getenv('HOME')))) {
	file0 = file.locate(file, prefixes = locations);
	source(file = file0, ...);
}

# complete: return only complete data with respect to specified colums
# NA: specify 'NA'-values
optionParser = list(
	SEP = function(e)list(T = "\t", S = ' ', C = ',', `;` = ';', `S+` = '')[[e]],
	QUOTE = function(e)(if (e == 'F') '' else e),
	HEADER = function(e)list(T = T, F = F)[[e]],
	NAMES = function(e)splitString(';', e),
	PROJECT = function(e)splitString(';', e),
	`NA` = function(e)splitString(';', e),
	complete = function(e)splitString(';', e),
	CONST = function(e){ r = lapply(splitString(';', e), function(e){
			r = splitString(':', e);
			v = if (length(fetchRegexpr('^\\d+$', r[2])) > 0) r[2] = as.integer(r[2]) else r[2];
			listKeyValue(r[1], v)
		});
		unlist.n(r, 1)
	},
	HEADERMAP = function(e){ r = lapply(splitString(';', e), function(e){
			r = splitString(':', e);
			listKeyValue(r[1], r[2])
		});
		unlist.n(r, 1)
	},
	COLNAMESFILE = identity
);

splitExtendedPath = function(path) {
	q = fetchRegexpr('(?<=^\\[).*?(?=\\]:)', path);
	options = list();
	if (length(q) > 0 && nchar(q) > 0) {
		path = substr(path, nchar(q) + 4, nchar(path));
		os = sapply(splitString(',', q), function(e)splitString('=', e));
		os = listKeyValue(os[1, ], os[2, ]);
		os = nlapply(names(os), function(n)optionParser[[n]](os[[n]]));
		options = merge.lists(options, os);
	}
	r = list(path = path, options = options)
}

readTable.csv.defaults = list(HEADER = T, SEP = "\t", `NA` = c('NA'), QUOTE = '"');
readTable.csv = function(path, options = readTable.csv.defaults, headerMap = NULL, setHeader = NULL, ...) {
	options = merge.lists(readTable.csv.defaults, options);
	t = read.table(path, header = options$HEADER, sep = options$SEP, as.is = T,
		na.strings = options$`NA`, comment.char = '', quote = options$QUOTE, ...);
	if (!is.null(options$NAMES)) names(t)[1:length(options$NAMES)] = options$NAMES;
	if (!is.null(headerMap)) names(t) = vector.replace(names(t), headerMap);
	if (!is.null(setHeader)) names(t) =  c(setHeader, names(t)[(length(setHeader)+1): length(names(t))]);
	t
}

readTable.sav = function(path, options = NULL, headerMap = NULL) {
	#library.ifavailable('foreign');
	# <N> appease R CMD CHECK
	if (!exists('read.spss')) read.spss = NULL;
	#package = 'foreign';
	require(package = 'foreign');
	# read file
	read.spss(path, to.data.frame = T);
}

readTable.RData = function(path, options = NULL, headerMap = NULL) {
	t = as.data.frame(get(load(path)[1]), stringsAsFactors = F);
	#print(t);
	t
}

readTable = function(path, autodetect = T, headerMap = NULL, extendedPath = T, colnamesFile = NULL,...) {
	path = join(path, '');
	o = list();
	if (extendedPath) {
		r = splitExtendedPath(path);
		path = r$path;
		o = r$options;
	}
	sp = splitPath(path);
	r = if (autodetect && !is.null(sp$ext)) {
		name = sprintf('readTable.%s', sp$ext);
		f = if (exists(name)) get(name) else readTable.csv;
		f(path, options = o, ...)
	} else readTable.csv(path, options = o, ...);
	headerMap = firstDef(headerMap, o$HEADERMAP);
	if (!is.null(headerMap)) names(r) = vector.replace(names(r), headerMap);
	if (!is.null(o$NAMES) && length(o$NAMES) <= ncol(r)) names(r)[1:length(o$NAMES)] = o$NAMES;
	colnamesFile = firstDef(o$COLNAMESFILE, colnamesFile);
	if (!is.null(colnamesFile)) {
		ns = read.table(colnamesFile, header = F, as.is = T)[, 1];
		names(r)[1:length(ns)] = ns;
	}
	if (!is.null(o$PROJECT)) r = r[, o$PROJECT];
	if (!is.null(o$complete)) r = r[apply(r[, o$complete], 1, function(e)!any(is.na(e))), ];
	if (!is.null(o$CONST)) { for (n in names(o$CONST)) r[[n]] = o$CONST[[n]]; }
	r
}

#
#	<p> swig
#

swigIt = function(interface, code, moduleName = NULL) {
	dir = tempdir();	# will be constant across calls
	if (is.null(moduleName)) {
		t = tempFileName("swig");
		moduleName = splitPath(t)$base;
	}

	ifile = sprintf("%s/%s.%s", dir, moduleName, "i");
	interface = sprintf("
		%%module %s
		%%inline %%{
			%s;
		%%}
	", moduleName, paste(interface, collapse = ";\n\t\t\t"));

	ifile = sprintf("%s/%s.%s", dir, moduleName, "i");
	base = splitPath(ifile)$fullbase;
	writeFile(ifile, interface);
	cfile = sprintf("%s.c", base);
	writeFile(cfile, code);
	#print(list(i = ifile, c = cfile, so = sprintf("%s.so", base)));
	system(sprintf("swig -r %s", ifile));
	#cat(code);

	system(sprintf("cd %s ; gcc -O2 -D__USE_BSD -D__USE_GNU -std=c99 -c -fpic %s.c %s_wrap.c -I/usr/local/lib64/R/include -lm ",
		splitPath(ifile)$dir, base, base));
	system(sprintf("cd %s ; gcc -shared %s.o %s_wrap.o -o %s.so", splitPath(ifile)$dir, base, base, base));
	#dyn.unload(sprintf("%s.so", base));
	dyn.load(sprintf("%s.so", base));
	source(sprintf("%s/%s.R", splitPath(ifile)$dir, moduleName));
}

#
#	<p> print
#

fprint = function(..., file = NULL, append = F) {
	if (!is.null(file)) sink(file = file, append = append);
	r = print(...);
	if (!is.null(file)) sink();
	r
}

stdOutFromCall = function(call_) {
	tf = tempfile();
	sink(tf);
		eval.parent(call_, n = 2);
	sink();
	readFile(tf)
}

#
#	crypotgraphy/checksumming
#

md5sumString = function(s, prefix = 'md5generator') {
	path = tempfile('md5generator');
	writeFile(path, s);
	md5 = avu(md5sum(path));
	md5
}

#
#	<p> package documentation
#

#	docFile = sprintf('%s/tmp/docOut.Rd', Sys.getenv('HOME'));
#	docDir = sprintf('%s/src/Rpackages/parallelize.dynamic/parallelize.dynamic/man', Sys.getenv('HOME'));
#	docs = RdocumentationSkeleton('Rparallel.back.R', 'parallelize.dynamic', output = docFile);
#	writeRdocumentationToDir(docFile, docDir);

RdocumentationForObjects = function(items, envir, unparser = function(item, envir)item) {
	files = suppressMessages({
		sapply(items, function(item)unparser(item, envir));
	});
	docs = lapply(files, readFile);
	names(docs) = sapply(files, function(f)splitPath(f)$base);
	docs
}
RdocumentationForFunctions = function(items, envir) {
	docs = RdocumentationForObjects(items, envir, unparser = function(item, envir) {
		file = file.path(tempdir(), sprintf("%s.Rd", item));
		prompt(get(item, envir = envir), name = item, filename = file);
		file
	});
	docs
}
RdocumentationForClasses = function(items, envir) {
	docs = RdocumentationForObjects(items, envir, unparser = function(item, envir) {
		file = file.path(tempdir(), sprintf("%s-class.Rd", item));
		methods::promptClass(item, filename = file, where = envir);
		file
	});
	docs
}
RdocumentationForMethods = function(items, envir) {
	docs = RdocumentationForObjects(items, envir, unparser = function(item, envir) {
		file = file.path(tempdir(), sprintf("%s-methods.Rd", item));
		methods::promptMethods(item, filename = file, findMethods(item, where = envir));
		file
	});
	docs
}


# code from packages.skeleton
objectsFromCodeFiles = function(R_files, packageName = 'generic') {
	e = new.env(hash = T);
	methods::setPackageName(packageName, e);
	for (f in R_files) sys.source(f, envir = e);
	classes = getClasses(e);
	methods = getGenerics(e);
	others = ls(e, all.names = T);
	others = others[grep('^\\.', others, invert = T)];

	r = list(envir = e, classes = classes, methods = methods,
		others = setdiff(setdiff(others, classes), methods));
	r
}

RdocumentationSkeleton = function(R_files, output = NULL, packageName = 'generic') {
	os = objectsFromCodeFiles(R_files, packageName = packageName);
	docs = c(
		RdocumentationForFunctions(os$others, os$envir),
		RdocumentationForClasses(os$classes, os$envir),
		RdocumentationForMethods(os$methods, os$envir)
	);

	doc = join(nlapply(docs, function(n) {
		sprintf("\nDOCUMENTATION_BEGIN:%s\n%s\nDOCUMENTATION_END\n", n, docs[[n]])
	}), "\n");
	if (!is.null(output)) {
		if (File.exists(output)) {
			Log(sprintf("Move away file '%s' before writing new skeleton", output), 2);
		} else {
			writeFile(output, doc);
		}
	}
	doc
}

writeRdocumentationToDir = function(pathesIn, pathOut, cleanOut = F) {
	doc = sapply(pathesIn, readFile, USE.NAMES = F);
	r = unlist.n(getPatternFromStrings(doc, '(?s)(?:\\nDOCUMENTATION_BEGIN:)([^\\n]+)\\n(.*?)(?:\\nDOCUMENTATION_END\\n)'), 1);
	Dir.create(pathOut, recursive = T);
	if (cleanOut) {
		files = list_files_with_exts(pathOut, 'Rd');
		file.remove(files);
	}
	nlapply(r, function(n) {
		output = file.path(pathOut, sprintf('%s.Rd', n));
		Log(sprintf('Writing to %s', output), 3);
		writeFile(output, r[[n]]);
	});
	names(r)
}

reDoc = function(package = 'parallelize.dynamic',
	docFile = sprintf('./%s.doc.Rd', package), docDir = sprintf('./%s/man', package)) {
	writeRdocumentationToDir(docFile, docDir, cleanOut = T);
	install.packages(sprintf('./%s', package), repos = NULL);
	#detach(package);
	#library(package)
}

#
#	<p> Rcpp helpers
#

createModule = function(name, libpathes = c(), headers = c(), output = NULL) {
	require('Rcpp');
	require('inline');
	dirs = sapply(libpathes, function(e)splitPath(e)$dir);
	libs = sapply(libpathes, function(e)fetchRegexpr('(?<=lib)(.*)(?=.so)', splitPath(e)$file));
	.libPaths(c(.libPaths(), dirs));
	libincludes = join(sapply(seq_along(dirs), function(i)sprintf('-L"%s" -l%s', splitPath(dirs[i])$absolute, libs[i])), ' ');
	Sys.setenv(`PKG_LIBS` = sprintf('%s %s', Sys.getenv('PKG_LIBS'), libincludes));
	Sys.setenv(`PKG_CXXFLAGS` = sprintf('%s %s', Sys.getenv('PKG_LIBS'), stdOutFromCall(Rcpp:::CxxFlags())));


	for (lib in libpathes) { dyn.load(lib, local = F) }
	moduleRegex = '(?s:(?<=// -- begin inline Rcpp\n)(.*?)(?=// -- end inline Rcpp))';
	inc = join(sapply(headers, function(f) fetchRegexpr(moduleRegex, readFile(f))), "\n");

	rcpp = cxxfunction( signature(), '' , includes = inc, plugin = 'Rcpp', verbose = T );
	mod = Module( name,  getDynLib(rcpp) );
	if (!is.null(output)) {
		libfiles = sapply(libpathes, function(lib) {
			File.copy(lib, sprintf('%s/%s', output, splitPath(lib)$file));
			splitPath(lib)$file
		});
		glue = sprintf('%s/%s.so', output, name);
		File.copy(getDynLib(rcpp)[['path']], glue);
		module_descriptor = list(
			name = name,
			libs = c(libfiles, splitPath(glue)$file)
		);
		save(module_descriptor, file = sprintf('%s/module.RData', output));
	}
	mod
}

activateModule = function(path) {
	require('Rcpp');
	module_descriptor = get(load(sprintf('%s/module.RData', path))[1]);
	r = lapply(module_descriptor$libs, function(lib)try(dyn.unload(sprintf('%s/%s', path, lib)), silent = T));
	r = lapply(module_descriptor$libs, function(lib)dyn.load(sprintf('%s/%s', path, lib), local = F));
	mod = Module( module_descriptor$name, rev(r)[[1]] );
	mod
}

#
#	<p> sqlite
#

sqlCreateTable = function(columns, types = list, index = NULL, createAt = NULL) {
	# <p> create database
	types = merge.lists(listKeyValue(columns, rep('text', length(columns))), types);
	createDbSql = join(sep = "\n", c(
		sprintf('CREATE TABLE data (%s);',
			join(sep = ', ', sapply(columns, function(e)sprintf('%s %s', e, types[e])))),
		if (is.null(index)) c() else sapply(1:length(index), function(i)
			sprintf('CREATE INDEX index_%d ON data (%s);', i, join(index[[i]], sep = ', '))),
		'.quit', ''
	));
	if (!is.null(createAt)) System(sprintf('echo \'%s\' | sqlite3 %s', createDbSql, qs(createAt)), 1);
	createDbSql
}

sepMap = list(T = '\\t', S = ' ', C = ',', `;` = ';', `S+` = '');
sepMapCut = list(T = '\\t', S = '" "', C = ',', `;` = ';', `S+` = '');
csv2sqlite = function(url, output = tempfile(), header = NULL, skip = NULL, selectColumns = NULL,
	index = NULL, sep = 'T',
	NULLs = NULL, types = list()) {
	# <p> determine header
# 	tmp1 = tempfile();
# 	ret = download.file(url, tmp1, method, quiet = FALSE, mode = "w", cacheOK = TRUE);
	#if (ret) stop(sprintf("Download of '%s' failed.", url));
	tmp1 = '/home/pingu/Downloads/gene2accession.gz';
	if (is.null(header)) {
		tmpHeader = tempfile();
	}

	# <p> select columns
	cut = if (!is.null(selectColumns)) {
		skipColumnsIds = which.indeces(selectColumns, header);
		sprintf('| cut %s -f %s ',
			if (sep == 'T') '' else sprintf('-d %s', sepMapCut[[sep]]),
			join(skipColumnsIds, ',')
		)
	} else '';
	columns = if (is.null(selectColumns)) header else selectColumns;

	sqlCreateTable(columns, types, index, createAt = output);

	# <p> import data
	skipCommand = if (is.null(skip)) '' else sprintf('| tail -n +%d ', skip + 1);
	reader = if (splitPath(url)$ext == 'gz') 'zcat' else 'cat';
	importSql = join(sep = "\n", c(
		sprintf(".separator %s\n", sepMap[[sep]]),
		sprintf(".import \"/dev/stdin\" data")
	));

	sepText = sepMap[[sep]];
	filter = if (is.null(NULLs)) '' else
		sprintf("| perl -pe 's/((?<=%s)(?:%s)(?=%s|$)|(?<=^)(?:%s)(?=%s|$))//g'",
			sepText, join(NULLs, '|'), sepText, sepText, sepText);
	cmd = sprintf("%s %s %s %s %s | sqlite3 -init %s %s",
		reader, qs(tmp1), skipCommand, cut, filter, qs(writeFile(tempfile(), importSql)), qs(output));
	System(cmd, 1);
	output
}
qq = function(s)as.character(fetchRegexpr('([^ ]+)', s, captures = T))

sqlite2sqlite = function(dbS, dbD, query, cols, types = list(), index = NULL) {
	sqlCreateTable(cols, types, index, createAt = dbD);
	cmd = sprintf("echo %s | sqlite3 -init %s %s | sqlite3 -init %s %s",
		qs(query),
		qs(writeFile(tempfile(), ".mode csv")),
		qs(dbS),
		qs(writeFile(tempfile(), ".separator ,\n.import \"/dev/stdin\" data")),
		qs(dbD)
	);
	System(cmd, 1);
	dbD
}
