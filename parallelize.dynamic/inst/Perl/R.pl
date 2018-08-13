#!/usr/bin/env perl
#
#DATE

use TempFileNames;
#use KeyRing;
use Statistics::R;
use Data::Dumper;
use Set;

$helpText = <<HELP_TEXT;
	R.pl scriptFile [--optionFor_R=o ...] argsToR
	R.pl --optionFor_R_pl 1 -- scriptFile argsToR

	Options:
	--code : directly type R code (turns of --trigger option)
	--instantiate : instantiate options as variables instead of creating list .o.
	--header : code to include into the beginning of driver file
		defaults to "source('RgenericAll.R');"
	--discard-stderr
	--no-quiet: do not sink initialization to /dev/null
	--no-trigger:	do not automatically call handleTriggers
	--includes : source the given files (':' separated)
	--allPerl : consider all options to be perl options
	--template name : use the template with the given name
		standard: a script including files from R_INCLUDE and instantiating options as variables
		raw: run code as given (--code or file), implies --no-instantiate

	Environment variables:
	RSCRIPTS  ':'-separated list of folders to be searched for the script file
	RINCLUDES ':'-separated list of files to be sourced

	Examples:
	R.pl --code "a +b" -- --a 1 --b 5 d.txt --some option
	R.pl --no-quiet --code "a +b" -- --a 1 --b 5 d.txt --some option

	Triggers:
	Define global R-variable .globalTriggers with key/values: option-name = function-name, e.g:
	.globalTriggers = list(help = print_help);
	R.pl --help will call function print_help

	Suggested aliases:
	alias Rq='RINCLUDES= R.pl --allPerl --code '
	alias Rq2='R.pl --allPerl --code '

	Examples:
	Rq 'exp(1)'

$TempFileNames::GeneralHelp
HELP_TEXT

$R_TEMPLATE_standard = <<'R_TEMPLATE',
	R_SINK_ON
	R_STANDARD_HEADER
	.o. = R_ARGUMENTS;
	args = .o.$args;
	R_SINK_OFF
	R_CODE
	R_INPUT
	R_TRIGGER
R_TEMPLATE

$R_TEMPLATE_raw = <<'R_TEMPLATE',
	R_CODE
R_TEMPLATE

$R_TEMPLATE_source = <<'R_TEMPLATE',
	R_INPUT
R_TEMPLATE

%R_TEMPLATES = (
	standard => $R_TEMPLATE_standard, raw => $R_TEMPLATE_raw, source => $R_TEMPLATE_source
);

my @R_STD_INCLUDES = ( 'RgenericAll.R' );

#main $#ARGV @ARGV %ENV
	initLog(2);
	$o = { config => 'config.cfg',
		o => 'Routput.txt',
		template => 'standard',
		instantiate => 1,
		header => '',
		'quiet' => 1, trigger => 1, vsize => '1024M', psize => '50k'
	};
#	$o = { output => '/tmp/default.txt' };
#	$result = GetOptionsStandard($o, 'simple', 'filter|f=s', 'int=i', 'onOff!', 'output|o=s');
#	$result = GetOptionsStandard($o, 'credentials');
	$result = 1;
	$result = GetOptionsStandard($o, 'code=s', 'instantiate!', 'header=s', 'quiet!',
		'trigger!', 'rscripts=s', 'includes=s', 'allPerl', 'template=s', 'vsize=s', 'psize=s', 'RlogLevel=s'
	) if ((grep { $_ eq '--' } @ARGV) || (grep { $_ eq '--allPerl' } @ARGV));
	if (!$result
		|| $o->{help}
		|| (!@ARGV && $o->{code} eq '')
		|| (@ARGV == 1 && $ARGV[0] eq '--help')) {
		printf("USAGE: %s\n$helpText", ($0 =~ m{/?([^/]*)$}o));
		exit(!$result);
	}

	# options
	my @srcDirs = (split(/:/, $ENV{RSCRIPTS}), split(/:/, $o->{rscripts}));
	my @R_INCLUDES = (split(/:/, $ENV{RINCLUDES}), split(/:/, $o->{includes}));
	$o->{header} .= join("\n", map {
		"source('". firstFileLocation($_, @srcDirs). "');"
	} @R_INCLUDES). "\n";
	$o->{header} .= "\tLog.setLevel($o->{RlogLevel});\n" if (defined($o->{RlogLevel}));
	#$c = readConfigFile($o->{config});
	#$cred = KeyRing->new()->handleCredentials($o->{credentials}, '.this_cookie') || exit(0);
	$o->{trigger} = 0 if ($o->{code});
	$o->{instantiate} = 0 if ($o->{template} eq 'raw');

	my $script;
	if (!defined($o->{code})) {
		$script = shift;
		Log("Script: $script", 5);
		Log("Search Path: ". join(';', ('', '.', @srcDirs)), 5);
		$script = firstFileLocation($script, ('', '.', @srcDirs));
		Log("Script Path: $script", 5);
	}
	my %dict;
	for (my $i = 0; $i < @ARGV; $i++) {
		my $a = $ARGV[$i];
		if ($a =~ m{^--(.*)}o) {
			my $opt = $1;
			if ($opt =~ m{^[^=]+=(.*)}sog) {
				$dict{$opt} = $1;
			} else {
				$dict{$opt} = ($ARGV[$i + 1] =~ m{^--(.*)}o)? 1: $ARGV[++$i];
			}
		} else { push(@{$dict{args}}, $a); }
	}
	#print unparseRdata(\%dict);
	my $tf = tempFileName("/tmp/R_wrapper_$ENV{USER}/r_cmd", '.R', {doTouch => 1});
	my $code = firstDef($o->{code}, '');
	$code = "with(.o., { $code })" if ($o->{instantiate});
	#$code = "instantiate.list(.o.)\n\t$code";

	Log("Using template $o->{template}.", 5);
	Log("Template: ". $R_TEMPLATES{$o->{template}}, 5);
	my $Rcode = mergeDictToString({
		SRC_DIR => defined($o->{code})? '.': splitPathDict($script)->{dir},
		R_ARGUMENTS => unparseRdata(\%dict),
		R_INPUT => defined($script)? "source('$script', chdir = T)": '',
		R_CODE => $code,
		R_STANDARD_HEADER => $o->{header},
		R_SINK_ON => $o->{quiet}? "sink(file = file('/dev/null', 'w'), type = 'message');": '',
		R_SINK_OFF => $o->{quiet}? "sink();": '',
		R_TRIGGER => $o->{trigger}? "handleTriggers(.o.);": '',
	}, $R_TEMPLATES{$o->{template}}, { iterate => 'YES' });
	Log($Rcode, 5);
	writeFile($tf, $Rcode);
	my $psize = mergeDictToString({'k' => '000'}, $o->{psize});
	my $exit = System("R --min-vsize=$o->{vsize} --max-ppsize=$psize --vanilla --slave < $tf", 5);
exit($exit);
