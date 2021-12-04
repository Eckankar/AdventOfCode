#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures experimental::smartmatch);
use POSIX qw(ceil);

chomp(my @input = <STDIN>);

my ($time, $linestr) = @input;

my @lines = split(/,/, $linestr);

my @departures = map { +{ id => $_, time => int(ceil($time / $_)*$_) } } @lines;

my ($earliest) = sort { $a->{time} <=> $b->{time} } @departures;

my $dtime = $earliest->{time} - $time;

say $dtime * $earliest->{id};
