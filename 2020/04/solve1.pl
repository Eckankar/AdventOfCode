#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my $input = do { local $/; <STDIN> });
my @passports = split(/\n\s*?\n/, $input);

my @required = qw(byr iyr eyr hgt hcl ecl pid);

my $count = 0;

for my $passport (@passports) {
    my @present = grep { $passport =~ m/\b$_:/ } @required;
    $count += 1 if scalar @present == scalar @required;
}

say $count;
