#!/usr/bin/env perl
use 5.024;
use warnings;

chomp(my @input = <STDIN>);
my %nums = map { (2020 - $_) => 1} @input;
my ($match) = grep { $nums{$_} } @input;
say $match * (2020 - $match);
