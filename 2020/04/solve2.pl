#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures experimental::smartmatch);

use List::MoreUtils qw(uniq);

chomp(my $input = do { local $/; <STDIN> });
my @passports = split(/\n\s*?\n/, $input);

my @required = qw(byr iyr eyr hgt hcl ecl pid);

my $count = 0;

for my $passport (@passports) {
    my @seen;
    my $valid_fields = 1;
    my @entries = split(/\s+/, $passport);

    for my $entry (@entries) {
        my ($field, $value) = $entry =~ m/^([a-z]{3}):(\S*)$/;
        unless ($field) {
            $valid_fields = 0;
            #say "Invalid entry: $entry";
            next;
        }

        my $valid_field = 1;

        push(@seen, $field);
        $valid_field &&= ($field ~~ [@required, 'cid']);

        $valid_field &&= ($value =~ m/^\d{4}$/ && $value >= 1920 && $value <= 2002) if $field eq 'byr';
        $valid_field &&= ($value =~ m/^\d{4}$/ && $value >= 2010 && $value <= 2020) if $field eq 'iyr';
        $valid_field &&= ($value =~ m/^\d{4}$/ && $value >= 2020 && $value <= 2030) if $field eq 'eyr';

        if ($field eq 'hgt') {
            my ($val, $unit) = $value =~ m/^(\d+)(cm|in)$/;

            $valid_field &&= $unit && $unit ~~ [qw(cm in)];
            $valid_field &&= ($val >= 150 && $val <= 193) if $unit && $unit eq 'cm';
            $valid_field &&= ($val >= 59 && $val <= 76) if $unit && $unit eq 'in';
        }

        $valid_field &&= ($value =~ m/^#[0-9a-f]{6}$/) if $field eq 'hcl';
        $valid_field &&= ($value ~~ [qw(amb blu brn gry grn hzl oth)]) if $field eq 'ecl';
        $valid_field &&= ($value =~ m/^\d{9}$/) if $field eq 'pid';

        #say "Invalid field: '$field:$value'" unless $valid_field;
        $valid_fields &&= $valid_field;
    }

    $valid_fields &&= scalar @seen == scalar uniq(@seen);
    $valid_fields &&= scalar @required == scalar grep { $_ ~~ \@seen } @required;

    $count += 1 if $valid_fields;
}

say $count;
