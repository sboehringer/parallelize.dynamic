#
#	Rsession.pm
#Tue 31 Jan 2006 10:07:01 AM EST 
#

package Statistics::Rsession;
require 5.000;
#@ISA = qw(SuperClass);

#
#	<§> dependencies
#

#use SuperClass;
use IPC::Open3;
#use POSIX ":sys_wait_h";
use TempFileNames;

#
#	<§> data structures
#

$sessionCounter = 0;	# provide unique file handle names

#
#	<§> standard implementation
#

sub new { my ($class, @args) = @_;
	#my $self = bless($class->SUPER::new(splice(@_, 1)), $class);
	# if no superclass is in place use the following instead
	my $self = bless({}, $class);
	# own initialization follows
	$self->{session} = ++$sessionCounter;
	return $self;
}

#
#	<§> method implementation
#

sub open { my ($self) = @_;
	$self->{in} = sprintf('Rinhandle%d', $self->{session});
	$self->{out} = sprintf('Routhandle%d', $self->{session});
	$self->{err} = sprintf('Rerrhandle%d', $self->{session});

	$self->{pid} = open3($self->{in}, $self->{out}, $self->{err}, "R --slave");
}

sub close { my ($self) = @_;
	close($self->{out});
	close($self->{in});
	close($self->{err});
	waitpid($self->{pid}, 0);
}

# <i>: handle execution halted gracefully
sub execute { my ($self, $code) = @_;
	$self->open() if (!defined($self->{pid}));
	my ($output, $error, $line, $errline) = ('');
	print { $self->{in} } "try({$code});\ncat(\"\n\");print(\"Rsession:end of output\");\n";
	for (;;) {
		$line = readline($self->{out});
		while (($errline = readline($self->{errline})) ne '') { $error .= $errline; };
		last if ($line =~ m{(?:^\[1\] "Rsession:end of output"\n$)}o);
		last if ($error =~ m{(?:Execution halted\n)}os);
		$output .= $line;
		last if (!defined($line) && !defined($errline));
	}
	#$error = $line if ($line =~ /Execution halted/o);
	#$error = readFileHandle($self->{err});
	# old code for 'R --vanilla --silent'
	# $output =~ s{(?:^> \Q$code\E\n)|(?:\Q> print("end of commands")\E\n)}{}gs;
	return { out => $output, err => $error };
}

1;
