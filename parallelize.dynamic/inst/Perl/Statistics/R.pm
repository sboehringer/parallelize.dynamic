#
#	Statistics/R.pm
#Thu 21 Jul 2005 06:35:48 PM CEST 
#
# <!> renamed from Statistics::R -> Statitics::Rbridge as of 18.8.2016 to avoid conflict with CPAN package
# <!> renamed from Statitics::Rbridge -> Statistics::R as of 2.8.2016 to avoid conflict with CPAN package

package Statistics::R;
require Exporter;

@ISA       = qw(Exporter);
@EXPORT    = qw(&RcmdWithArgs &unparseRdata &RsaveData);

#
#	<§> dependencies
#

use	TempFileNames;
use Set;
use IPC::Open3;
use Statistics::Rsession;

#
#	<§> data structures
#

#
#	<§> standard implementation
#

#
#	<§> method implementation
#

sub mapArg2R { my ($v) = @_;
	if (ref($v) eq 'ARRAY') {
		my $v2s = 'c('. join(',', map { mapArg2R($_) } @$v).')';
		$v = $v2s;
	} elsif (ref($v) eq 'HASH') {
		my $v2s = 'list('. join(',', map { sprintf("%s=%s", $_, mapArg2R($v->{$_})) } keys %$v).')';
		$v = $v2s;
	}
	# quoting exceptions
	return (
	   $v =~ /^-?\d+(:\d+)?$/o
	|| $v =~ /^(TRUE|FALSE|T|F)$/
	|| $v =~ /^c\(|^list\(/o	 # '))'
	)
	? $v
	: (($v =~ /"/o)? qs($v): "\"$v\"")

}

sub RcmdWithArgs { my ($rfile, $argDict, $output) = @_;
	my $c = {};
	if (ref($output) eq 'HASH') {
		$c = $output;
		$output = $c->{output};
	}

	my $binary = firstDef($c->{binary}, 'R');
	my $cmd = "$binary --vanilla --silent --args ". (defined($rfile)? "< $rfile": '');
	# quote by default, scan for exceptions
	my $args = join(';', map {
		$_. '='. mapArg2R($argDict->{$_})
	} keys(%{$argDict}));
	return $cmd. " '$args;'". (defined($output)? " > $output" :'');
}

sub RcodeWithArgs { my ($code, $argDict) = @_;
	my $s = Statistics::Rsession->new($argDict);
	$s->open();
		my $out = $s->execute($code);
	$s->close();
	chop($out->{out});
	return $out;
}

sub Rfunction { my ($fct, @args) = @_;
	my $args = join(', ', map(mapArg2R($_), @args));
	my $rCode = (defined($fct->{module})? "source(\"$fct->{module}\");\n": '')
		. "r = $fct->{fct}($args); ". 'cat(r)';
	my $out = Statistics::R::RcodeWithArgs($rCode);
	return $out->{out};
}

sub unparseRList { my ($h, $c) = @_;
	my @elements = map {
		"$_ = ". unparseRdata($h->{$_}, { keyPath => $c->{keyPath}. ".$_", %{$c} } )
	} keys %{$h};
	return 'list('. join(', ', @elements). ')';
}

# seems to be too slow in parsing <N>
sub unparseDataFrame { my ($v, $header) = @_;
	my $ret = 'data.frame(';
	my ($nrow, $ncol) = (int(@{$v}), int(@{$v->[0]}));
	my ($i, $j);
	for ($j = 0; $j < $ncol; $j++) {
		$ret .= ($j? ', ': ''). "$header->[$j] = c(";
		for ($i = 0; $i < $nrow; $i++) {
			$ret .= unparseRdata($v->[$i][$j], $c). ", ";
		}
		$ret .= ")\n";
	}
	$ret .= ")\n";
	return $ret;
}

sub unparseRVector { my ($v, $c) = @_;
	my $ret = '';
	my $isMatrix = 0;
	# detect data frame case
	if (defined($c->{dataFrames}{$c->{keyPath}})) {
		# assume a nested array structure
		my $tempDfPath = tempFileName('/tmp/rDataConvDataFrame', '.txt');
		writeHeadedTable($tempDfPath, $v, $c->{dataFrames}{$c->{keyPath}}{header});
		#$ret = unparseDataFrame($v, $c->{dataFrames}{$c->{keyPath}}{header});
		$ret = "read.delim(\"$tempDfPath\")";
		return $ret;
	}
	# detect matrix case
	if ($#$v >= 0 && ref($v->[0]) eq 'ARRAY') {
		my $l = int(@{$v->[0]});
		$isMatrix = 1;
		foreach $e (@{$v}) {
			if (ref($e) ne 'ARRAY' || int(@{$e}) != $l) {
				$isMatrix = 0;
				last;
			}
		}
		if ($isMatrix) {
			my $l = int(@{$v->[0]});
			my ($i, $j);
			$ret = "matrix(c(";
			for ($i = 0; $i < @{$v}; $i++) {
				for ($j = 0; $j < @{$v->[0]}; $j++) {
					$ret .= unparseRdata($v->[$i][$j], $c). ", ";
				}
			}
			$ret .= "), byrow = T, ncol = $l)";
			return $ret;
		}
	}
	# automagically switch to list if first element is not atomic
	my @elements = map { unparseRdata($_, $c) } @$v;
	return (ref($v->[0]) eq 'HASH'? 'list(': 'c('). join(', ', @elements).")\n";
}

sub unparseRdata { my ($d, $c) = @_;
	my $ret = '';
	if (ref($d) eq 'HASH') {
		$ret = unparseRList($d, $c);
	} elsif (ref($d) eq 'ARRAY') {
		$ret = unparseRVector($d, $c);
	} else { $ret = mapArg2R($d, $c); }
	return $ret;
}

# Example for saving a data-frame
#	my $Rdata = { data => $d->{data}, desc => $d->{desc} };
#	my $Rcfg = {dataFrames => { '.data' => { header => $d->{header} } } };
#	RsaveData($oPath, 'dataset', $Rdata, $Rcfg);

sub RsaveData { my ($path, $name, $var, $cfg) = @_;
	my $str = unparseRdata($var, $cfg);
	my $tpath = tempFileName('/tmp/rDataConv', '.R');
	writeFile($tpath, "$name = $str\nsave($name, file = \"$path\");\n");
	my $bin = firstDef($cfg->{binary}, 'R');
	System("$bin --vanilla --silent < $tpath > /dev/null", 4);
}

1;
