#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures experimental::smartmatch);

chomp(my @input = <STDIN>);

my $dir = 'E';
my ($x, $y) = (0, 0);

sub turn($degrees) {
    while ($degrees > 0) {
        $degrees -= 90;
        if    ($dir eq 'N') { $dir = 'E'; }
        elsif ($dir eq 'E') { $dir = 'S'; }
        elsif ($dir eq 'S') { $dir = 'W'; }
        elsif ($dir eq 'W') { $dir = 'N'; }
    }
}

sub move($dir, $dist) {
    if    ($dir eq 'N') { $x += $dist; }
    elsif ($dir eq 'E') { $y += $dist; }
    elsif ($dir eq 'S') { $x -= $dist; }
    elsif ($dir eq 'W') { $y -= $dist; }
}

for my $line (@input) {
    my ($op, $val) = $line =~ m/^([A-Z])(\d+)$/;

    if ($op ~~ [qw(N E S W)]) {
        move($op, $val);
    } elsif ($op eq 'F') {
        move($dir, $val);
    } elsif ($op eq 'L') {
        turn(360 - $val);
    } elsif ($op eq 'R') {
        turn($val);
    }
}

say abs($x) + abs($y);
