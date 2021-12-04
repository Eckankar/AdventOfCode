#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @input = <STDIN>);

my $pc = 0;
my $acc = 0;
my %seen;

while ($pc <= $#input) {
    if ($seen{$pc}++) {
        say $acc;
        exit(0);
    }

    my ($op, $val) = $input[$pc] =~ m/^(nop|acc|jmp) \+?(-?\d+)$/;

    if    ($op eq 'acc') { $pc += 1;    $acc += $val; }
    elsif ($op eq 'jmp') { $pc += $val;               }
    elsif ($op eq 'nop') { $pc += 1;                  }
}
