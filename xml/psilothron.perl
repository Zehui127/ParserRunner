#!/usr/bin/perl

 ##############################################################################
 # Copyright 2012 Oeistein Andersen                                           #
 #                                                                            #
 # This file is part of RASP.                                                 #
 #                                                                            #
 # RASP is free software: you can redistribute it and/or modify it            #
 # under the terms of the GNU Lesser General Public License as published      #
 # by the Free Software Foundation, either version 3 of the License, or       #
 # (at your option) any later version.                                        #
 #                                                                            #
 # RASP is distributed in the hope that it will be useful,                    #
 # but WITHOUT ANY WARRANTY; without even the implied warranty of             #
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
 # GNU Lesser General Public License for more details.                        #
 #                                                                            #
 # You should have received a copy of the GNU Lesser General Public License   #
 # along with RASP.  If not, see <http://www.gnu.org/licenses/>.              #
 ##############################################################################

use strict;
use warnings;
binmode(STDOUT, ":utf8");
use XML::Parser;
use Getopt::Std;

my %opt;
getopts('p:x:Ww', \%opt); # colon after each option that takes a value
# the -w option does absolutely nothing
#print $opt{p};

my @stack = ({name => '#ROOT'});
my $in_p = 0;
my $in_x = 0;

my $index;
my $delta;
my $delta_prev;

my %path;
for my $i ('p', 'x') {
    for my $s (split(' ', ($opt{$i} ? $opt{$i} : ""))) {
	#print "Duplicate element name ($s) --- not implemented." if (exists $x_path{$s});
	#print STDERR "$s\n";
	if ($s =~ /^(.*)\[@([^=]+)=(["']?)([^'"]*)\3\]$/) {
	    #print STDERR "A: $1 $2 <<$4>>\n";
	    $path{$i}{$1} = {attr => $2, attrval => $4};
	} elsif ($s =~ /^(.*)\[@([^=]+)\]$/) {
	    #print STDERR "B: $1 $2\n";
	    $path{$i}{$1} = {attr => $2};
	} else {
	    $path{$i}{$s} = undef;
	}
    }
}

#exit;

sub match {
    my ($i, $e, $pa) = @_;
    if (exists $path{$i}{$e}) {
	###return 1 unless defined($path{$i}{$e}); # skip the rest of the if clause
	if (exists($path{$i}{$e}{attr})) {
	    return 0 unless exists($pa->{$path{$i}{$e}{attr}});
	}
	if (exists($path{$i}{$e}{attrval})) {
	    return 0 unless $pa->{$path{$i}{$e}{attr}} eq $path{$i}{$e}{attrval};
	}
	return 1;
    } # else
    return 0;
}

open DELTA, ">", "fifo" if $opt{W};
print "<rasp>\n";
my $parser = new XML::Parser(
    Handlers => {Start => \&start_handler, End => \&end_handler, Char => \&char_handler},
    ParseParamEnt => 1); # expand entities defined in external subset (i.e., DTD files)
$parser->parse(*STDIN);
print "</rasp>\n";

sub start_handler {
    my ($p, $e, %a) = @_;
    #print 'x' . $#stack . 'x' . "\n";
    $stack[-1]->{children}{$e}++; # if $#stack > -1;
    push(@stack, {name => $e});
    $stack[-1]->{id} = $a{id} if exists $a{id};
    $stack[-1]->{id} = $a{sortkey} if exists $a{sortkey}; # overrides the above
                                                          # :-(
                                                          # config'able ids?
    #if (!$in_p && exists $path{p}{$e}) {
    if (!$in_p && match('p', $e, \%a)) {
	#print "\n<para id=\"$a{id}\">\n";
	#print '/' . $stack[0]->{name}; # root
	my $path = '';
	for my $i (1..$#stack) {
	    $path .= '/' . $stack[$i]->{name} . '[';
	    if (exists $stack[$i]->{id}) {
		$path .= "\@id='" . $stack[$i]->{id} . "'";
	    } else {
		$path .= $stack[$i-1]->{children}{$stack[$i]->{name}};
	    }
	    $path .= ']';
	}
	my $elix = $p->element_index();
	print "<text path=\"$path\" i=\"$elix\">";
	#print STDERR "$path\n";
	print DELTA "$path\n" if $opt{W};
	$in_p = 1;
	$stack[-1]->{p} = 1;
	$index = 0;
	$delta = 0;
	$delta_prev = 0;
    #} elsif ($in_p && exists $path{x}{$e}) {
    } elsif ($in_p && !$in_x && match('x', $e, \%a)) {
	$in_x = 1;
	$stack[-1]->{x} = 1;
    }
}

sub end_handler {
    my ($p, $e) = @_;
    #print "</$e>\n" if ($e eq "script" || $e eq "ans")
    if ($stack[-1]->{p}) {
	print "</text>\n";
	$in_p = 0;
	#print STDERR "$delta_prev $index\n";
	print DELTA "$delta_prev $index\n" if $opt{W};
    } elsif ($stack[-1]->{x}) {
	$in_x = 0;
    }
    pop(@stack);
}

sub char_handler {
    #print "[" . (scalar @stack) . ":" . $stack[-1]{name} . "]";
    my ($p, $s) = @_;
    #print "$s" if $p->within_element($opt{p}); # && !$p->within_element($ARGV[0]);
    if ($in_p) {
	unless ($in_x) {
	    if ($delta != $delta_prev) {
		#print STDERR "$delta_prev $index\n";
		print DELTA "$delta_prev $index\n" if $opt{W};
		$delta_prev = $delta;
	    }
	    print $s;
	    $index += length($s);
	} else {
	    $delta += length($s);
	}
    }
}
