#!/usr/bin/env perl
use 5.028;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

use File::Temp qw(tempdir);
use IPC::System::Simple qw(systemx);
use Mojo::File;

chomp(my $input = do { local $/; <STDIN> });

my ($grammar, $strings) = split(/\n\s*\n/, $input);

$grammar =~ s/(\d+)/rule$1/g;
$grammar =~ s/:/ :=/g;
$grammar =~ s#"([ab])"#~/$1/#g;

my $header = <<'EOH';
start: lines

lines := (rule0 "1" /\n/ | ~/[^\n]*\n/)*
EOH

$strings .= "\n" unless $strings =~ m/\n$/;

my $tempdir = tempdir(CLEANUP => 1, TEMPLATE => '/tmp/AoC_XXXXXXXX');
Mojo::File->new("$tempdir/part1.kex")->spurt($header.$grammar);
Mojo::File->new("$tempdir/lines.txt")->spurt($strings);


chdir($tempdir);
systemx(qw(kexc compile part1.kex --out part1));

my @result = qx(cat lines.txt | ./part1);
say scalar @result;
