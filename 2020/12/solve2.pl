#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures experimental::smartmatch);

chomp(my @input = <STDIN>);

my ($x, $y) = (0, 0);
my ($wx, $wy) = (1, 10);

sub turn($degrees) {
    while ($degrees > 0) {
        $degrees -= 90;
        ($wx, $wy) = (-$wy, $wx);
    }
}

sub move($dir, $dist) {
    if    ($dir eq 'N') { $wx += $dist; }
    elsif ($dir eq 'E') { $wy += $dist; }
    elsif ($dir eq 'S') { $wx -= $dist; }
    elsif ($dir eq 'W') { $wy -= $dist; }
}

for my $line (@input) {
    my ($op, $val) = $line =~ m/^([A-Z])(\d+)$/;

    if ($op ~~ [qw(N E S W)]) {
        move($op, $val);
    } elsif ($op eq 'F') {
        $x += $val * $wx;
        $y += $val * $wy;
    } elsif ($op eq 'L') {
        turn(360 - $val);
    } elsif ($op eq 'R') {
        turn($val);
    }
}

say abs($x) + abs($y);
