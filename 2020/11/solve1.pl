#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

use Storable qw(dclone);

chomp(my @input = <STDIN>);
@input = map { [split(//, $_)] } @input;

my ($height, $width) = (scalar @input, scalar $input[0]->@*);

sub add($p1, $p2) {
    return [$p1->[0] + $p2->[0], $p1->[1] + $p2->[1]];
}

sub valid($p) {
    return ($p->[0] >= 0) && ($p->[0] < $height) && ($p->[1] >= 0) && ($p->[1] < $width);
}

sub neighbors($p) {
    return grep { valid($_) } map { add($p, $_) }
           ([1,1],[1,0],[1,-1],[0,1],[0,-1],[-1,1],[-1,0],[-1,-1]);
}

sub next_gen($data) {
    my $next_gen = dclone($data);

    for my $y (0 .. $height-1) {
        for my $x (0 .. $width-1) {
            my @values = map { $data->[$_->[0]]->[$_->[1]] } neighbors([$y, $x]);
            my $current = $data->[$y]->[$x];

            if ($current eq 'L' && ! grep { $_ eq '#' } @values) {
                $next_gen->[$y]->[$x] = '#';
            } elsif ($current eq '#' && (grep { $_ eq '#' } @values) >= 4) {
                $next_gen->[$y]->[$x] = 'L';
            }
        }
    }

    return $next_gen;
}

sub render($data) {
    return join("\n", map { join("", @$_) } @$data);
}

my $old_data = [];
my $new_data = \@input;

while (render($new_data) ne render($old_data)) {
    $old_data = $new_data;
    $new_data = next_gen($old_data);
}

say scalar grep { $_ eq '#' } map { @$_ } @$new_data;
