#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

chomp(my @input = <STDIN>);

sub find_outlier(@input) {
    my $window_size = 25;

    my @active = @input[0 .. $window_size-1];

    my %active = map { $_ => 1 } @active;

    for my $i ($window_size .. $#input) {
        my $val = $input[$i];
        my $valid = grep { $active{$val-$_} } @active;

        unless ($valid) {
            return $val;
        }

        delete $active{ $input[$i - $window_size] };
        $active{$val} = 1;
        @active = (@active[1 .. $#active], $val);
    }
}

my $outlier = find_outlier(@input);

for my $i (0 .. $#input-1) {
    my $sum = $input[$i];

    my $smallest = $input[$i];
    my $largest  = $input[$i];

    for my $j ($i+1 .. $#input) {
        $sum += $input[$j];

        $smallest = $input[$j] if $input[$j] < $smallest;
        $largest  = $input[$j] if $input[$j] > $largest;

        if ($sum == $outlier) {
            say ($smallest + $largest);
            exit(0);
        }

        last if $sum > $outlier;
    }
}
