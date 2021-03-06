#!/usr/bin/env perl
#	qsub.pl
#Mon Sep 27 17:29:22 CEST 2010


use TempFileNames;
#use KeyRing;
use Data::Dumper;
use Set;

my $helpText = <<HELP_TEXT;
	Simplified job submission to the Sun Grid Engine (defaults in brackets [])

	Options:
	--outputDir=path	write job stdout/stderr into dir
				[./qsub_outputDir]
	--jid=path		append jid to file
	--jidReplace=path	write jid to file
	--priority number	set priority to number
				(-1000 to 1000 for SGE) [0]
	--memory size	set memory limit (e.g. 8G)
	# Options to qsub.pl have to be terminated by --

	# Environment variables
	QSUB_LOG_DIR	redirect diagnostic files to this directory
	QSUB_QUEUE		default queue to use in job submissions
	QSUB_OPTIONS	default options passed to include
	QSUB_PRIORITY	default priority for jobs
	QSUB_SOURCEFILES	colon separated list of files to source
				prior to running the command
	QSUB_EXCLUDENODES	comma separated lists of nodes not to use

	# Examples:
	# simply prepend qsub.pl to your command
	qsub.pl echo hello world
	# redirect stdout/stderr
	#	commands are unquoted if only a single, quoted argument is given
	qsub.pl 'echo hello world > /tmp/redirect'
	#	use --no-unquote to overwrite this behavior
	qsub.pl --no-unquote -- 'command name with spaces'
	#	use --unquote to enforce unquoting for multiple arguments
	qsub.pl --unquote -- 'echo hello world > /tmp/redirect'

	# environment
	# do not use default PATH export
	qsub.pl --exports -
	# exactly export PERL5LIB
	qsub.pl --exports -,PERL5LIB

	# job dependencies
	# Append the job id of the submitted job to file /tmp/myJobIds
	qsub.pl --jid /tmp/myJobIds -- echo hello world --echoOption
	# Wait for jobs to finish prior to starting new job
	qsub.pl --waitForJids /tmp/myJobIds -- echo hello world --echoOption
	qsub.pl --waitForJids 100,200,230 -- echo hello world --echoOption

	# debugging
	# print and run the generated qsub script
	qsub.pl --loglevel 5 -- 'echo hello world'
	# print, not run the generated qsub script
	qsub.pl --doLogOnly --loglevel 5 -- 'echo hello world'


$TempFileNames::GeneralHelp
HELP_TEXT

#$TEMP_DIR = '/tmp/qsub_pl_'.$ENV{USER};
my $HEADER = '#!/bin/bash
# Options
OGS_OPTIONS

# Explicit ENV exports
OGS_EXPORTS
OGS_SOURCE

PREPARE_CMDS

# Command
CMD
';

my %Options = (
	'-S' => '/bin/sh', '-cwd' => '', '-q' => 'options_QUEUE',
	'-hold_jid' => undef,
	'-V' => '',	# pass environment variables
	'-e' => 'QSUB_OUT', '-o' => 'QSUB_OUT',
	'-p' => 'options_PRIORITY',
	'-l' => 'h_vmem=options_MEMORY',
	'-lhost' => undef,
	'-pe' => sub { return $_[0]->{Ncpu} == 1? undef: sprintf('BWA %d', $_[0]->{Ncpu}) },
	'-r' => 'yes',	# job re-runnable
);
# allow for double keys
my %OptionsKeyRenames = ('-lhost' => '-l');
my %OptionsOnOff = (
	checkpointing => [ '-ckpt' =>  'check_userdefined']
);

my $stringRE='(?:([_\/\-a-zA-Z0-9.]+)|(?:\"((?:\\\\.)*(?:[^"\\\\]+(?:\\\\.)*)*)\"))';

sub qsS { my $p = $_[0];
	$p =~ s{'}{\\'}sog if ($p =~ m{\s}sog);
	return "'$p'";
}

sub submitCommand { my ($cmd, $o) = @_;
	my ($cmdname) = ($cmd =~ m{^\s*'?([^\s']+)'?}so);
	# don't delete
	my $tf = tempFileName($o->{tmpPrefix}. "/job_$cmdname", '.sh', undef, 1);
	#my $env = ''; #join("\n", map { "$_=$ENV{$_}" } keys %ENV);

	# <p> prepare environment
	my @envKeys = grep { $_ ne '' } split(/\s*,\s*/, $o->{exports});
	my @sourceFiles = grep { !($_ =~ m{^\s*$}sog) } unique(split(/\s*:\s*/, $o->{sourceFiles}));
	my @envReset = which_indeces(['-'], [@envKeys]);
	@env = @envKeys[($envReset[0] + 1) .. $#envKeys] if (defined($envReset[0]));
	my @env = map { "$_=$ENV{$_}" } grep { !/$\s*^/ } @envKeys;
	my $setenv = join("\n", split(/\Q$o->{setenvsep}\E/, $o->{setenv}));

	# <p> generic options
	my $mergeDict = makeHash([map { 'options_'. uc($_) } keys %$o], [values %$o]);
	my %opts = (%{makeHash([keys %Options], [map { mergeDictToString($mergeDict, $_)} values %Options])});
	# add fixed options based on
	%opts = (%opts, map { (defined($o->{$_})? @{$OptionsOnOff{$_}}: ()) } keys %OptionsOnOff);
	# evaluate functions
	%opts = (%opts, map { ($_, ref($opts{$_}) eq 'CODE'? $opts{$_}->($o): $opts{$_}) } keys %opts);
	
	# job dependencies
	if (defined($o->{waitForJids})) {
		my @jids = grep { !!$_ } (($o->{waitForJids} =~ m{^\d+\s*(,\s*\d+\s*)*$}so))
			? split(/\s*,\s*/, $o->{waitForJids})
			: split("\n", readFile($o->{waitForJids}));
		$opts{'-hold_jid'} = join(',', @jids) if (!!@jids);
	}
	# exclude nodes
	$opts{'-lhost'} = ('h=!('. join('|', split(/\s*,\s*/, $o->{excludeNodes})). ')')
		if ($o->{excludeNodes} ne '');

	# <p> preparatory commands
	my $prep = '';
	if ($o->{outputDir} ne '' && $o->{moveOutputDir}) {
		$prep = "if [ -e \"$o->{outputDir}\" ]; then\n"
			. "\tmv $o->{outputDir} $o->{outputDir}-`cat /dev/urandom | tr -cd 'a-f0-9' | head -c 8` ; mkdir $o->{outputDir}\n"
			. "fi";
	}
	# <p> construct script
	# remove empty options
	%opts = %{dict2defined({%opts})};
	my @options = map { mergeDictToString(\%OptionsKeyRenames, "#\$ $_ $opts{$_}") } keys %opts;
	my $script = $HEADER;
	$script = mergeDictToString({
		'QSUB_OUT' => $o->{outputDir},
		'OGS_OPTIONS' => join("\n", @options),
		'OGS_EXPORTS' => join("\n", ((map { "export $_" } @env), $setenv)),
		'OGS_SOURCE' => join("\n", (map { ". $_" } @sourceFiles)),
		'PREPARE_CMDS' => $prep,
		'CMD' => $cmd
	}, $script, { sortKeys => 'YES' });

	# <p> prepare file system
	Mkpath($o->{outputDir}) if (!-e $o->{outputDir});
	writeFile($tf, $script, { doMakePath => 1 });

	Log("qsub script:\n-- Start of script --\n$script\n-- End of script --\n", 5);
	my $r = System("qsub $tf", 4, undef, { returnStdout => 'YES' } );
	#Stdout:
	#Your job 710 ("job_echo34686.sh") has been submitted
	my ($jid) = ($r->{output} =~ m{Your job (\d+)}so);
	writeFile($o->{jid}, "$jid\n", { append => 'YES', doMakePath => 1 }) if (defined($o->{jid}));
	writeFile($o->{jidReplace}, "$jid\n", { doMakePath => 1 } ) if (defined($o->{jidReplace}));
}

#main $#ARGV @ARGV %ENV
#	initLog(2);
	my $o = {
		config => 'config.cfg',
		outputDir => firstDef($ENV{QSUB_LOG_DIR}, 'qsub_jobOutputs'),
		moveOutputDir => 0,
		queue => firstDef($ENV{QSUB_QUEUE}, 'default'),
		priority => firstDef($ENV{QSUB_PRIORITY}, 0),
		tmpPrefix => firstDef($ENV{QSUB_TMPPREFIX}, '/tmp/qsub_pl_'.$ENV{USER}),
		exports => 'PATH',
		sourceFiles => firstDef($ENV{QSUB_SOURCEFILES}, ''),
		setenvsep => '+++',
		memory => firstDef($ENV{QSUB_MEMORY}, '4G'),
		Ncpu => 1,
		excludeNodes => firstDef($ENV{QSUB_EXCLUDENODES}, undef),
	};
	my $optionsPresent = int(grep { $_ eq '--' } @ARGV) > 0;
	# <!><i> proper command line splitting
	if ($ENV{QSUB_OPTIONS} ne '') {
		splice(@ARGV, 0, 0, (split(/\s+/, $ENV{QSUB_OPTIONS}), $optionsPresent? (): '--'));
		$optionsPresent = 1;
	}
	my $result = !$optionsPresent? 1
	: GetOptionsStandard($o,
		'help', 'jid=s', 'jidReplace=s', 'exports:s',
		'waitForJids=s', 'outputDir=s', 'unquote!', 'queue=s', 'priority=i', 'cmdFromFile=s', 'checkpointing',
		'memory=s', 'Ncpu=i', 'setenv=s', 'setenvsep=s', 'sourceFiles=s', 'excludeNodes=s',
	);
	# <!> heuristic for unquoting
	$o->{unquote} = 1 if (!defined($o->{unquote}) && @ARGV == 1);
	if ((!@ARGV && !defined($o->{cmdFromFile}))
		|| !$result || $o->{help} || (@ARGV == 1 && $ARGV[0] =~ m{^(--help|-h)$})) {
		printf("USAGE: %s command arg1 ...\n$helpText", ($0 =~ m{/?([^/]*)$}o));
		exit(!$result);
	}
#	$c = readConfigFile($o->{config});
#	$cred = KeyRing->new()->handleCredentials($o->{credentials}, '.this_cookie') || exit(0);
	my $cmd = $o->{unquote}? join(' ', @ARGV): join(' ', map { qsS($_) } @ARGV);
	$cmd .= readFile($o->{cmdFromFile}) if (defined($o->{cmdFromFile}));
	#Log($_) foreach (@ARGV);
	Log("Command to run:\n$cmd", 3);
	submitCommand($cmd, $o);
exit(0);

