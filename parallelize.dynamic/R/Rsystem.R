#
#	Rsystem.R
#Mon 27 Jun 2005 10:51:30 AM CEST 

#
#	<par> file handling
#

# <!><N> works only on atomic path
# <!> 5.1.2016: trailing slash leads to basename of ""
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
	Nchar = nchar(path);
	if (Nchar != 0 && substr(path, Nchar, Nchar) == '/') {
		base = '';
		dir = substr(path, 1, Nchar - 1);
	} else {
		base = basename(path);
		dir = dirname(path);
	}
	# base as yet still contains the file extension
	file = base;
	# chop off extension if present
	if (length(fetchRegexpr('\\.', base)) > 0) base = fetchRegexpr('\\A.*(?=\\.)', base);
	
	#pieces = regexpr(re, path, perl = T);
	pieces = fetchRegexpr('([^.]+)', path);
	isAbsolute = Nchar != 0 && substr(path, 1, 1) == '/';
	# <N> disk is accessed
	exists = if (!skipExists) File.exists(path, host = sshm$userhost, ssh = F) else NA;
	nonempty = exists && (file.info(path)$size > 0);
	ret = list(
		dir = dir,
		base = base,
		path = path,
		fullbase = sprintf("%s/%s", dir, base),
		ext = ext,
		file = file,
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
path.absolute = absolutePath = function(path, home.dir = T, ssh = T) {
	path = splitPath(path, ssh = ssh)$path;
	if (home.dir && nchar(path) >= 2 && substr(path, 1, 2) == "~/")
		path = sprintf("%s/%s", Sys.getenv('HOME'), substr(path, 3, nchar(path)));
	if (nchar(path) > 0 && substr(path, 1, 1) == "/") path else sprintf("%s/%s", getwd(), path)
}
tempFileName = function(prefix, extension = NULL, digits = 6, retries = 5, inRtmp = F,
	createDir = F, home.dir = T, doNotTouch = F) {
	ext = if (is.null(extension)) '' else sprintf('.%s', extension);
	path = NULL;
	if (inRtmp) prefix = sprintf('%s/%s', tempdir(), prefix);
	if (home.dir) prefix = path.absolute(prefix, home.dir = home.dir);
	for (i in 1:retries) {
		path = sprintf('%s%0*d%s', prefix, digits, floor(runif(1) * 10^digits), ext);
		if (!File.exists(path)) break;
	}
	if (File.exists(path))
		stop(sprintf('Could not create tempfile with prefix "%s" after %d retries', prefix, retries));
	# potential race condition <N>
	if (createDir)
		Dir.create(path, recursive = T) else
		if (!doNotTouch) writeFile(path, '', mkpath = T, ssh = T);
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
list_files_with_exts = function(path, exts, full.names = T)
	list.files(path, pattern = Sprintf('.(%{Exts}s)$', Exts = join(exts, '|')), full.names = full.names);

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

File.copy_raw = function(from, to, ..., recursive = F, agent = 'scp', logLevel = 6, ignore.shell = T,
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

File.copy = function(from, to, ..., recursive = F, agent = 'scp', logLevel = 6, ignore.shell = T,
	symbolicLinkIfLocal = T) {
	if (is.null(from)) return(NULL);
	pairs = cbind(from, to);
	r = apply(pairs, 1, function(r) {
		File.copy_raw(r[1], r[2], ...,
			recursive = recursive, agent = agent, logLevel = logLevel,
			ignore.shell = ignore.shell, symbolicLinkIfLocal = symbolicLinkIfLocal)
	})
	r
}

File.remove = function(path, ..., agent = 'ssh', ssh = T, logLevel = 6) {
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
File.symlink = function(from, to, replace = T, agent = 'ssh', ssh = F, logLevel = 6) {
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
Dir.create = function(path, ..., recursive = F, agent = 'ssh', logLevel = 6,
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
Load = function(..., file = NULL, Load_sleep = 0, Load_retries = 3, envir = parent.frame(1), logLevel = 6) {
	sp = splitPath(file, ssh = T);
	localPath = if (sp$is.remote) tempfile() else file;
	r = NULL;
	for (i in 1:Load_retries) {
		if (sp$is.remote) {
			if (!File.exists(file)) {
				Sys.sleep(Load_sleep);
				next;
			}
			File.copy(file, localPath, logLevel = logLevel);
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
# @par prefix character, start path name with this character string
# @par tag character, add dashed string to all files (defaults to appending to filename)
# @par tagFirst boolean, put tag as a prefix to the file name instead
.fn.set = function(...) {
	.globalOutput = merge.lists(.globalOutputDefault, list(...));
	assign('.globalOutput', .globalOutput, envir = GlobalOutput_env__);
}
# create output file name on globalOptions
.fn = function(name, extension = '', options = NULL) {
	o = merge.lists(.globalOutputDefault, .globalOutput,
		get('.globalOutput', envir = GlobalOutput_env__), options);
	# construct plain filename
	pathes = sprintf('%s%s%s%s', o$prefix, name, ifelse(extension == '', '', '.'), extension);
	fn = sapply(pathes, function(path) {
		sp = splitPath(path);
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
	});
	avu(fn)
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
#	<p> extended system call
#

# Example of patterns:
# 	System(cmd, 5, patterns = c('cwd', 'qsub', 'ssh'),
# 		cwd = sp$path, ssh_host = sp$userhost,
# 		qsubPath = sprintf('%s/qsub', sp$path), qsubMemory = self@config$qsubRampUpMemory);


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
	
	cwd = list(pre = function(cmd, spec, cwd = '.', ...) {
		ncmd = sprintf('cd %s ; %s', qs(cwd), cmd);
		spec = list(cmd = ncmd);
		spec
	},
	post = function(spec, ret, ...) { list() }
	),
	# <i> stdout/stderr handling
	ssh = list(pre = function(cmd, spec, ssh_host = 'localhost', ssh_source_file = NULL, ...,
		ssh_single_quote = T) {
		if (!is.null(ssh_source_file)) {
			cmd = sprintf('%s ; %s',
				join(paste('source', qs(ssh_source_file), sep = ' '), ' ; '), cmd);
		}
		fmt = if (ssh_single_quote) 'ssh %{ssh_host}s %{cmd}q' else 'ssh %{ssh_host}s %{cmd}Q';
		spec = list(cmd = Sprintf(fmt));
		spec
	},
	fs = function(fs, ..., ssh_host) {
		list(
			tempfile = function(prefix, ...) {
				Log(sprintf('tempfile ssh:%s', prefix), 1);
				r = splitPath(tempFileName(sprintf('%s:%s', ssh_host, prefix), ...), ssh = T)$path;
				Log(sprintf('tempfile ssh-remote:%s', r), 1);
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
assign(".system.doLogOnly", FALSE, envir = System_env__);

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
	#<!> might not be available/outdated
	Require('parallel');
	cfg = merge.lists(.defaultClusterConfig,
		cfgDict,
		list(splitN = splitN, reuseCluster = reuseCluster, evalEnvironment = evalEnvironment),
		list(local = F, source = sourceFiles, libraries = libraries, hosts = (if(is.null(hosts))
			list(list(host = "localhost", count = localNodes, type = "PSOCK",
				environment = list(setwd = getwd()))) else
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

# l: list, f: function, c: config
# <i><!> test clCfg$reverseEvaluationOrder before uncommenting
clapply_cluster = function(l, .f, ..., clCfg = NULL) {
	#if (clCfg$reverseEvaluationOrder) l = rev(l);

	# only support SOCK type right now <!><i>
	hosts = as.vector(unlist(sapply(clCfg$hosts, function(h){
		if (h$type == "PSOCK") rep(h$host, h$count) else NULL})));
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
	if (Log.level() >= 7) print(clCfg);

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
#	<p> file operations
#

#' Return absolute path for name searched in search-pathes
#'
#' Search for pathes.
#'
#' @param as.dirs assume that prefixes are pathes, i.e. a slash will be put between path and prefix
#' @param force enforces that path and prefix are always joined, otherwise if path is absolute no prefixing is performed
file.locate = function(path, prefixes = NULL, normalize = T, as.dirs = T, force = F, home = T) {
	if (!force && substr(path, 1, 1) == '/') return(path);
	if (substr(path, 1, 1) == '~' && home) {
		path = path.absolute(path, home = TRUE);
		if (!force) return(path);
	}
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

isURL = function(path)(length(grep("^(ftp|http|https|file)://", path)) > 0L)

Source_url = function(url, ...) {
	Require('RCurl');
	request = getURL(url, followlocation = TRUE,
		cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"));
	tf = tempfile();
	writeFile(tf, request);
    source(tf, ...)
}

# <!> local = T does not work
Source = function(file, ...,
	locations = c('', '.', sprintf('%s/src/Rscripts', Sys.getenv('HOME')))) {
	sapply(file, function(file) {
		if (isURL(file)) Source_url(file, ...) else {
		file0 = file.locate(file, prefixes = locations);
			source(file = file0, ...)
		}
	})
}

#
#	<p> helper functions readTable/writeTable
#

compressPathBz2 = function(pathRaw, path, doRemoveOrig = TRUE) {
	cmd = Sprintf("cat %{pathRaw}q | bzip2 -9 > %{path}q");
	r = System(cmd, 5);
	if (doRemoveOrig && !get('.system.doLogOnly', envir = System_env__)) file.remove(pathRaw);
	r
}
compressPath = function(pathRaw, path, extension = NULL, doRemoveOrig = TRUE) {
	if (is.null(extension)) return(path);
	compressor = get(Sprintf('compressPath%{extension}u'));
	r = compressor(pathRaw, path, doRemoveOrig = doRemoveOrig);
	r
}
decompressPathBz2 = function(path, pathTmp, doRemoveOrig = FALSE) {
	cmd = Sprintf("cat %{path}q | bunzip2 > %{pathTmp}q");
	r = System(cmd, 5);
	if (doRemoveOrig && !get('.system.doLogOnly', envir = System_env__)) file.remove(pathRaw);
	r
}
decompressPath = function(path, pathTmp, extension = NULL, doRemoveOrig = FALSE) {
	if (is.null(extension)) return(path);
	decompressor = get(Sprintf('decompressPath%{extension}u'));
	r0 = decompressor(path, pathTmp, doRemoveOrig = doRemoveOrig);
	r = list(destination = pathTmp, pathOrig = path, return = r0);
	r
}

compressedConnectionBz2 = function(path, mode = '') {
	#r = Sprintf('%{path}s.bz2');
	bzfile(path, open = mode)
}
compressedConnectionGz = function(path, mode = '') {
	gzfile(path, open = mode)
}
compressedConnection = function(path, extension = NULL, mode = '') {
	if (is.null(extension)) return(path);
	compressor = get(Sprintf('compressedConnection%{extension}u'));
	compressor(path, mode = mode)
}
compressedConnectionPath = function(conn) {
	if ('connection' %in% class(conn)) summary(conn)$description else conn
}

#
#	<p> readTable
#

# complete: return only complete data with respect to specified colums
# NA: specify 'NA'-values
readTableSepMap = list(T = "\t", S = ' ', C = ',', `;` = ';', `S+` = '');
optionParser = list(
	SEP = function(e)readTableSepMap[[e]],
	DEC = identity,
	QUOTE = function(e)(if (e == 'F') '' else e),
	HEADER = function(e)list(T = T, F = F)[[e]],
	ROW.NAMES = function(e)list(T = T, F = F)[[e]],
	NAMES = function(e)splitString(';', e),
	FACTORS = function(e)splitString(';', e),
	DATE = function(e)splitString(';', e),
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
	# tb implemented: <i>: merge.lists recursive
	VALUEMAP = function(e){ r = lapply(splitString(';', e), function(e){
			r = splitString(':', e);
			listKeyValue(r[1], r[2])
		});
		unlist.n(r, 1)
	},
	COLNAMESFILE = identity,
	SHEET = as.integer,
	SKIP = as.integer,
	DELETE = Eval
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
path2simple = function(pathes)sapply(pathes, function(path)splitExtendedPath(path)$path);

readTable.ods = function(path, options = NULL) {
	Require('readODS');
	sheet = firstDef(options$SHEET, 1);
	read_ods(path, sheet = sheet, col_names = options$HEADER);
}

# <!> changed SEP default "\t" -> ",", 20.5.2015
#readTable.csv.defaults = list(HEADER = T, SEP = "\t", `NA` = c('NA'), QUOTE = '"');
readTable.csv.defaults = list(HEADER = T, SEP = ",", `NA` = c('NA'), QUOTE = '"', SKIP = 0);
readTable.txt = readTable.csv = function(
	path, options = readTable.csv.defaults, headerMap = NULL, setHeader = NULL, ...) {

	options = merge.lists(readTable.csv.defaults, options);
	t = read.table(path, header = options$HEADER, sep = options$SEP, as.is = T,
		na.strings = options$`NA`, comment.char = '', quote = options$QUOTE, skip = options$SKIP, ...);
	if (!is.null(options$NAMES)) names(t)[1:length(options$NAMES)] = options$NAMES;
	if (!is.null(headerMap)) names(t) = vector.replace(names(t), headerMap);
	if (!is.null(setHeader)) names(t) =  c(setHeader, names(t)[(length(setHeader)+1): length(names(t))]);
	if (!is.null(options$FACTORS)) t = Df_(t, as_factor = options$FACTORS);
	t
}

spssDate = function(date, tz = 'MET', spss.origin = as.POSIXct('2003/02/11', tz = tz) - 13264300800)
	(spss.origin + date)

readTable.SAV = readTable.sav = function(path, options = NULL, headerMap = NULL, stringsAsFactors = F) {
	Require('foreign');
	# read file
	r0 = read.spss(path);
	r1 = as.data.frame(r0, stringsAsFactors = stringsAsFactors);
	if (notE(options$DATE))
		r1[, options$DATE] = do.call(data.frame, lapply(r1[, options$DATE, drop = F], spssDate));
	r1
}

readTable.RData = function(path, options = NULL, headerMap = NULL) {
	t = as.data.frame(get(load(path)[1]), stringsAsFactors = F);
	#print(t);
	t
}

readTable.xls = function(path, options = NULL, ..., row.names = NULL, sheet = 1) {
	Require('gdata', quietly = TRUE);
	read.xls(path, sheet = sheet, ..., row.names = row.names, verbose = FALSE);
}

tableFunctionConnect = c('csv', 'RData');

tableFunctionForPathMeta = function(path, template = 'readTable.%{ext}s', default = readTable.csv,
	forceReader = NULL) {
	sp = splitPath(path);
	compression = NULL;
	tmpFile = NULL;
	if (firstDef(forceReader, sp$ext) %in% c('bz2', 'gz')) {
		compression = sp$ext;
		sp = splitPath(sp$fullbase);
		tmpFile = Sprintf('%{file}s.%{ext}s', file = tempfile(), ext = sp$ext);
	}
	name = Sprintf(template, ext = firstDef(forceReader, sp$ext));
	f = if (exists(name)) get(name) else default;
	r = list(
		fct = f, name = name, ext = sp$ext,
		compression = compression, tempfile = tmpFile, path = path
	);
	r
}
tableFunctionForPath = function(path, template = 'readTable.%{ext}s',
	default = readTable.csv, forceReader = NULL) {
	tableFunctionForPathMeta(path, template, default, forceReader)$fct
}

# forceReader: force readerFunction
tableFunctionForPathReader = function(path, template = 'readTable.%{ext}s', default = readTable.csv,
	forceReader = NULL) {
	m = m0 = tableFunctionForPathMeta(path, template = 'readTable.%{ext}s', default = default, forceReader);
	if (!is.null(m$compression)) {
		path = if (m0$compression %in% tableFunctionConnect)
			compressedConnection(m0$path, m0$compression) else
			decompressPath(m0$path, m0$tempfile, m0$compression)$destination
		m = merge.lists(m0, list(path = path));
	}
	m
}

readTable.defaultOptions = list(HEADER = TRUE);
# <!> as of 23.5.2014: headerMap after o$NAMES assignment
# <i> use tableFunctionForPath
readTable = function(path, autodetect = T, headerMap = NULL, extendedPath = T, colnamesFile = NULL, ...,
	as_factor = NULL, stringsAsFactors = F, defaultReader = readTable.csv, doRemoveTempFile = TRUE,
	forceReader = NULL, ssh = F) {
	# <p> preparation
	pathOrig = path = join(path, '');
	o = readTable.defaultOptions;
	if (extendedPath) {
		r = splitExtendedPath(path);
		path = r$path;
		o = merge.lists(o, r$options);
	}

	sp = splitPath(path, ssh = ssh);
	# <p> copy remote files
	if (sp$host != '') {
		tmpPath = tempFileName('readTable', sp$ext, inRtmp = T);
		File.copy(path, tmpPath);
		sp = splitPath(tmpPath);
		path = tmpPath;
	}
	# <p> read table raw
	reader = if (autodetect && !is.null(sp$ext)) 
		tableFunctionForPathReader(path, 'readTable.%{ext}s', readTable.csv, forceReader) else
		list(fct = defaultReader, path = path);
	r = reader$fct(reader$path, options = o, ...);

	# <p> cleanup
	if (doRemoveTempFile && !get('.system.doLogOnly', envir = System_env__) && !is.null(reader$tempfile))
		file.remove(reader$tempfile);

	# <p> table transformations
	if (!is.null(o$NAMES) && length(o$NAMES) <= ncol(r)) names(r)[1:length(o$NAMES)] = o$NAMES;
	colnamesFile = firstDef(o$COLNAMESFILE, colnamesFile);
	headerMap = c(headerMap, o$HEADERMAP);
	if (!is.null(headerMap)) names(r) = vector.replace(names(r), headerMap);
	if (!is.null(colnamesFile)) {
		ns = read.table(colnamesFile, header = F, as.is = T)[, 1];
		names(r)[1:length(ns)] = ns;
	}
	if (!is.null(o$PROJECT)) r = r[, o$PROJECT, drop = FALSE];
	if (!is.null(o$complete)) r = r[apply(r[, o$complete], 1, function(e)!any(is.na(e))), ];
	if (!is.null(o$CONST)) { for (n in names(o$CONST)) r[[n]] = o$CONST[[n]]; }
	if (!is.null(as_factor)) r = Df_(r, as_factor = as_factor);
	if (!is.null(o$DELETE)) r = r[-o$DELETE, , drop = F];
	r
}

#
#	<p> writeTable
#

writeTable.defaults = list(
	global = list(
	SEP = ' ', DEC = '.',
	ROW.NAMES = FALSE,
	HEADER = TRUE,
	QUOTE = TRUE
	),
	csv = list(SEP = ',', DEC = '.'),
	csv2 = list(SEP = ';', DEC = ',')
);
writeTable.table = function(dataFrame, path, ..., doCompress = NULL, row.names = TRUE, options = list()) {
	o = merge.lists(writeTable.defaults$global, list(ROW.NAMES = row.names), options);
	conn = compressedConnection(path, doCompress, mode = 'w');
	with(o, write.table(dataFrame, file = conn, ...,
		row.names = ROW.NAMES, col.names = HEADER, sep = SEP, quote = (QUOTE != '')));
}

writeTable.compression = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list(), writerFunction) {

	pathRaw = if (!is.null(doCompress)) Sprintf('%{path}s_raw_') else path;
	writerFunction(object, pathRaw, row.names = row.names, options = options);
	r1 = compressPath(pathRaw, path, doCompress, doRemoveOrig);
	r1
}

writeTable.sav_raw = function(object, path, doCompress = NULL, row.names = TRUE, doRemoveOrig = TRUE,
	options = list()) {
	# <p> required package
	options( java.parameters = "-Xmx4g" );
	Require('haven');

	# sanitize row names
	d0 = as.data.frame(object);
	names(d0) = gsub("[.\\$@#]", "\\_", names(d0))
	write_sav(d0, path);
}

writeTable.sav = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list()) {
	writeTable.compression(object, path, writerFunction = writeTable.sav_raw,
		doCompress = doCompress, row.names = row.names, doRemoveOrig = doRemoveOrig, options = options);
}

writeTable.xlsx = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list()) {
	options( java.parameters = "-Xmx4g" );
	Require('xlsx');
	r = path;
	# <p> save non-data frame-constrained column names
	colNames = if (is.matrix(object)) dimnames(object)[[2]] else NULL;
	dataFrame = as.data.frame(object);
	r0 = pathRaw = if (!is.null(doCompress)) Sprintf('%{path}s_raw_') else path;

	# <p> construct spreadsheet
	wb = createWorkbook();
	cs = CellStyle(wb, dataFormat = DataFormat("#,##0.00"));
	styles = listKeyValue(1:ncol(dataFrame), rep.list(cs, ncol(dataFrame)));
    sheet = createSheet(wb, 'dataFrame')
    addDataFrame(dataFrame, sheet, col.names = TRUE, row.names = row.names, 
        startRow = 1, startColumn = 1, colStyle = styles, colnamesStyle = NULL, 
        rownamesStyle = NULL);
    saveWorkbook(wb, pathRaw);

    #r0 = write.xlsx2(dataFrame, file = pathRaw, row.names = row.names, showNA = FALSE,
	#	colStyle = styles);
	r1 = compressPath(pathRaw, path, doCompress, doRemoveOrig);
	r0
}

writeTable.xls = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list()) {
	Require('WriteXLS');
	r = path;
	dataFrame = as.data.frame(object);
	pathRaw = if (!is.null(doCompress)) Sprintf('%{path}s_raw_') else path;
	r0 = WriteXLS(dataFrame, ExcelFileName = pathRaw, row.names = row.names);
	r1 = compressPath(pathRaw, path, doCompress, doRemoveOrig);
	r0
}

writeTable.csv2 = writeTable.csv = function(dataFrame, path, ..., doCompress = NULL,
	row.names = TRUE, options = list()) {
	conn = compressedConnection(path, doCompress, mode = 'w');
	with(options, write.table(dataFrame, file = conn, ...,
		sep = SEP, col.names = HEADER, row.names = ROW.NAMES, quote = (QUOTE != ''), dec = DEC));
}
# doCompress = 'bz2' to write bz2
# <i><!> determine from path
writeTableRaw = function(object, path, ..., doCompress = NULL, row.names = TRUE, autodetect = TRUE,
	defaultWriter = writeTable.csv, options = list()) {
	sp = splitPath(path);
	if (is.null(doCompress) && !is.null(sp$ext) && sp$ext %in% c('bz2', 'gz')) {
		doCompress = sp$ext;
	}
	writer = if (autodetect && !is.null(sp$ext)) 
		tableFunctionForPath(path, 'writeTable.%{ext}s', writeTable.csv) else defaultWriter;
	if (is.null(writer))
		stop(Sprintf("Writing table to extension '%{ext}s' not supported", ext = sp$ext));
	options = merge.lists(writeTable.defaults$global,
		writeTable.defaults[[firstDef(sp$ext, 'global')]], options);
	r0 = writer(object, path = path, ..., doCompress = doCompress, row.names = row.names, options = options);
	r = list(path = path, return = r0);
	r
}

writeTable = function(object, path, ..., doCompress = NULL, row.names = TRUE, autodetect = TRUE,
	defaultWriter = writeTable.csv, simplify = TRUE, extendedPath = TRUE) {
	o = list();
	if (extendedPath) {
		r = splitExtendedPath(path);
		path = r$path;
		o = r$options;
		defaultWriter = writeTable.table;
	}
	if (!is.null(o$PROJECT)) object = object[, o$PROJECT, drop = FALSE];
	r = lapply(path, function(p)
		writeTableRaw(object, p, ...,
			doCompress = doCompress, row.names = row.names, autodetect = autodetect,
			defaultWriter = defaultWriter, options = o)
	);
	if (simplify && length(path) == 1) r = r[[1]];
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
	Require('tools');
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
	Require('Rcpp');
	Require('inline');
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
		Dir.create(output, recursive = T);
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
	Require('Rcpp');
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

# Create sqlite database with contents of csv-file
# @par index: list of columns to index
# @par type: sqlite types: integer, real, text, blob, not specified assumes text

csv2sqlitSepMap = readTableSepMap;
sepMap = list(T = '\\t', S = ' ', C = ',', `;` = ';', `S+` = '');
sepMapCut = list(T = '\\t', S = '" "', C = ',', `;` = ';', `S+` = '');
csv2sqlite = function(path, output = tempfile(),
	columnsNames = NULL, columnsSelect = NULL,
	index = NULL,
	inputSep = 'T', inputHeader = T, inputSkip = NULL,
	NULLs = NULL, types = list()) {

	# <!> cave: does not heed skip
	if (!inputHeader && is.null(columnsNames)) {
		columnsNames = read.table(path, header = F, nrows = 1, sep = csv2sqlitSepMap[[inputSep]]);
	}
	# <p> select columns
	cut = if (!is.null(columnsSelect)) {
		skipColumnsIds = which.indeces(columnsSelect, columnsNames);
		sprintf('| cut %s -f %s ',
			if (inputSep == 'T') '' else sprintf('-d %s', sepMapCut[[inputSep]]),
			join(skipColumnsIds, ',')
		)
	} else '';
	columns = if (is.null(columnsSelect)) columnsNames else columnsSelect;
	types = merge.lists(listKeyValue(columns, rep('text', length(columns))), types);
	sqlCreateTable(columns, types, index, createAt = output);

	# <p> import data
	skipCommand = if (is.null(inputSkip)) '' else sprintf('| tail -n +%d ', inputSkip + 1);
	reader = if (splitPath(path)$ext == 'gz') 'zcat' else 'cat';
	importSql = writeFile(tempfile(), join(sep = "\n", c(
		sprintf(".separator %s\n", sepMap[[inputSep]]),
		sprintf(".import \"/dev/stdin\" data")
	)));

	sepText = sepMap[[inputSep]];
	filter = if (is.null(NULLs)) '' else
		sprintf("| perl -pe 's/((?<=%s)(?:%s)(?=%s|$)|(?<=^)(?:%s)(?=%s|$))//g'",
			sepText, join(NULLs, '|'), sepText, sepText, sepText);
	cmd = Sprintf(con(
		"%{reader}s %{path}Q %{skipCommand}s %{cut}s %{filter}s",
		" | sqlite3 -init %{importSql}Q %{output}Q"));
	System(cmd, 1);
	output
}
# <!> unfinished, siphones code from old csv2sqlite function
url2sqlite = function(url, output = tempfile(), header = NULL, skip = NULL, selectColumns = NULL,
	index = NULL, sep = 'T',
	NULLs = NULL, types = list()) {
	
	# <p> determine header
 	tmp1 = tempfile();
 	ret = download.file(url, tmp1, method, quiet = FALSE, mode = "w", cacheOK = TRUE);
	#if (ret) stop(sprintf("Download of '%s' failed.", url));
	if (is.null(header)) {
		tmpHeader = tempfile();
	}
}

# <!> 7.1.2015: was qq, but conflicts with QQ-plot function
qquote = function(s)as.character(fetchRegexpr('([^ ]+)', s, captures = T))

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

sqliteOpen = function(path) {
	Require('RSQLite');
	dbConnect(SQLite(), dbname = path);
}
sqliteQuery = function(db, query, table = NULL) {
	if (is.null(table)) table = dbListTables(db)[1];
	query = con(sapply(names(query), function(n)Sprintf('%{n}Q = %{v}s', v = qs(query[[n]], force = T))));
	query1 = Sprintf('SELECT * FROM %{table}Q WHERE %{query}s');
	Log(query1, 5);
	dbGetQuery(db, query1);
}

#
#	<p> publishing
#

# if (1) {
#	.fn.set(prefix = 'results/201404/expressionMonocytes-')
# 	initPublishing('expressionMonocytes201404', '201405');
# 	publishFile('results/expressionMonocytesReportGO.pdf');
# }
# # force as subdir in the reporting dir
# publishDir('results/BAP1IHCgekoppeldaanWelofGeenMonosomie3', asSubdir = T);

Publishing_env__ <- new.env();
initPublishing = function(project, currentIteration, publicationPath = '/home/Library/ProjectPublishing') {
	assign('project', project, Publishing_env__);
	assign('projectMd5', md5sumString(project), Publishing_env__);
	assign('currentIteration', currentIteration, Publishing_env__);
	assign('publicationPath', publicationPath, Publishing_env__);
}
publishFctEnv = function(path, into = NULL, as = NULL) with(as.list(Publishing_env__), {
	if (!exists('project')) stop('Publishing system not yet initialized.');

	projectFolder = Sprintf('%{publicationPath}s/%{projectMd5}s');
	prefix = if (is.null(into)) '' else Sprintf('%{into}s/');
	destinationPrefix = Sprintf('%{projectFolder}s/%{currentIteration}s/%{prefix}s');
	destination = Sprintf('%{destinationPrefix}s%{path}s',
		path = if (is.null(as)) splitPath(path)$file else as);
	r = list(projectFolder = projectFolder, prefix = prefix, destination = destination,
		destinationPrefix = destinationPrefix);
	r
})


publishFileRaw = function(file, into = NULL, as = NULL) with(publishFctEnv(file, into, as), {
	if (!is.null(into)) Dir.create(destination, treatPathAsFile = T, recursive = T);
	Logs('Publishing %{file} --> "%{destination}s', 3);
	Dir.create(splitPath(destination)$dir, recursive = T);
	System(Sprintf("chmod -R a+rX %{dir}s", dir = qs(projectFolder)), 4);
	r = file.copy(file, destination, overwrite = T);
	if (any(!r)) Logs("Copying of '%{file}s' failed.", 3);
	Sys.chmod(destination, mode = '0755', use_umask = F);
	destination
})

publishUnit = function(u, into = NULL, as = NULL, asSubdir = TRUE) {
	if (file.info(u)$isdir)
		publishDir(u, into, as, asSubdir) else
		publishFileRaw(u, into, as)
}

publishFiles = publishFile = function(files, into = NULL, as = NULL) {
	lapply(files, publishUnit, into = into, as = as)
}

publishCsv = function(table, as, ..., into = NULL) {
	file = tempfile('publish', fileext = 'csv');
	write.csv(table, file = file, ...);
	publishFile(file, into, as);
}

publishDir = function(dir, into = NULL, as = NULL, asSubdir = FALSE) with(publishFctEnv('', into, as), {
	sp = splitPath(dir);
	# if 'dir' is a slashed dir itself, use the last dir-component
	if (asSubdir) into = if (sp$file == '') splitPath(sp$dir)$file else sp$file;
	if (!is.null(into)) {
		destination = splitPath(Sprintf('%{destination}s/%{into}s/'))$fullbase;	# remove trailing slash
	}
	Dir.create(destination);
	Logs('Publishing %{dir} --> %{destination}s', 3);
	Dir.create(destination, recursive = T);
	System(Sprintf("chmod -R a+rX %{projectFolder}Q"), 4);
	System(Sprintf("cp -r %{dir}Q/* %{destination}Q"), 4);
	System(Sprintf("chmod -R a+rX %{projectFolder}Q"), 4);
	destination
})

publishAsZip = function(files, as, into = NULL, recursive = FALSE) {
	tmp = tempFileName('publishAsZip', createDir = T, inRtmp = T);
	output = tempFileName('publishAsZip', 'zip', inRtmp = T, doNotTouch = T);
	sapply(files, function(file) {
		File.symlink(splitPath(file)$absolute, Sprintf("%{tmp}s"), replace = F);
		NULL
	});
	recursiveOption = ifelse(recursive, '-r', '');
	System(Sprintf("zip -j %{recursiveOption}s %{output}s %{tmp}s/*"), 2);
	publishFile(output, into = into, as = as);
}


#
#	<p> quick pdf generation
#

print2pdf = function(elements, file) {
	es = elements;
	tf = tempfile();
	sink(tf);
		nlapply(es, function(n) {
			cat(n);
			cat('\n-------------------------------------------\n');
			print(es[[n]]);
			cat('\n\n');
		})
	sink();
	System(Sprintf('a2ps %{tf}s --columns 1 --portrait --o - | ps2pdf - - > %{output}s', output = qs(file)));
}

#
#	<p> workarounds
#

# fix broken install from dir: create tarball -> install_local
Install_local = function(path, ...) {
	pkgPath = Sprintf('%{dir}Q/%{base}Q.tgz', dir = tempdir(), base = splitPath(path)$base);
	System(Sprintf('tar czf %{pkgPath}Q %{path}Q'), 2);
	install_local(pkgPath, ...);
}

# misc

clearWarnings = function()assign('last.warning', NULL, envir = baseenv())

#
#	<p> packages
#

# name has to be character of length 1
Library = function(name, ...,
	repos = getOption('repos'), repoNoInteraction = TRUE, repoDefault = "http://cran.rstudio.com",
	quietly = TRUE) {
	# force evaluation
	#if (!Eval(Sprintf('require(%{name}s)'))) {
	if (!Require(name, character.only = TRUE, quietly = TRUE)) {
		if (repoNoInteraction) repos[repos == '@CRAN@'] = repoDefault[1];
		#expr = Sprintf('install.packages(%{name}s)');
		r = try(install.packages(name, repos = repos, ...));
		# if installation from CRAN fails, try bioconductor
		if (class(r) == 'try-error') {
			if (!exists('biocLite')) source("http://bioconductor.org/biocLite.R");
			biocLite(name, suppressUpdates = TRUE, suppressAutoUpdate = TRUE)
		}
		#Eval(Sprintf('library(%{name}s)'));
		library(name, character.only = TRUE, quietly = quietly);
	}
}
Require = function(..., quietly = TRUE) {
	if (quietly)
		suppressPackageStartupMessages(require(..., quietly = quietly)) else
		require(..., quietly = quietly)
}

DumpPackageNames = function(path = '~/Documents/AdminComputer/R/packages-%{version}s.csv',
	version = with(R.Version(), Sprintf('%{major}s.%{minor}s')), n = 1) {
	Logs('Writing package names to file:%{path}s', path = Sprintf(path), logLevel = 2);
	lib = .libPaths()[n];
	writeTable(Df(pkg = row.names(installed.packages(lib))), path = Sprintf(path));
}
ReadPackageNames = function(path = '~/Documents/AdminComputer/R/packages-%{version}s.csv',
	version = R.Version()$version.string) {
	Logs('Reading package names to file:%{path}s', path = Sprintf(path), logLevel = 2);
	readTable(Sprintf(path))$pkg;
}
ReinstallPackages = function(packages, n = 1, type = 'source') {
	lib = lib <- .libPaths()[n];
	install.packages(lib  = lib, pkgs = packages, type = type)
}
Reinstall = function(n = 1, type = 'source') {
	ReinstallPackages(row.names(installed.packages(lib)), n = n , type = type);
}
ReinstallFromVersion = function(version, n = 1, type = 'source') {
	ReinstallPackages(ReadPackageNames(version = version), n = n , type = type);
}

#
#	<p> misc linux system stuff
#

agenda = function()system('bash -l', input = c('shopt -s expand_aliases', 'agenda'))
agenda_stop = function()system('bash -l', input = c('shopt -s expand_aliases', 'agenda-stop'))
runTests = function()system('RrunTests')
