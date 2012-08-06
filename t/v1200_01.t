#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

use strict;
use Test::More tests => 7;
use Win32;
use Win32API::File qw(:ALL);
use File::Spec;
use Config;
BEGIN {
    $|= 1;

    # when building perl, skip this test if Win32API::File isn't being built
    if ( $ENV{PERL_CORE} ) {
	require Config;
	if ( $Config::Config{extensions} !~ m:(?<!\S)Win32API/File(?!\S): ) {
	    plan skip_all => "Skip Win32API::File extension not built\n";
	    exit();
	}
    }
}

my $temp= File::Spec->tmpdir();
my $dir= "W32ApiF.tmp";

$ENV{WINDIR} = $ENV{SYSTEMROOT} if not exists $ENV{WINDIR};

chdir( $temp )
  or  die "# Can't cd to temp directory, $temp: $!\n";
my $tempdir = File::Spec->catdir($temp,$dir);
if(  -d $dir  ) {
    print "# deleting ",File::Spec->catdir($temp,$dir,'*'),"\n" if glob "$dir/*";

    for (glob "$dir/*") {
	chmod 0777, $_;
	unlink $_;
    }
    rmdir $dir or die "Could not rmdir $dir: $!";
}
mkdir( $dir, 0777 )
  or  die "# Can't create temp dir, $tempdir: $!\n";
print "# chdir $tempdir\n";
chdir( $dir )
  or  die "# Can't cd to my dir, $tempdir: $!\n";

my $h1;  
ok($h1 = createFile( "CanWrite.txt", "rw", FILE_SHARE_WRITE,
	      { Create=>CREATE_ALWAYS } ), "CreateFile worked");

my $flags;
#6 == ERROR_INVALID_HANDLE
ok (!GetHandleInformation(0, $flags) && (fileLastError()+0) == 6, "GetHandleInformation fails on bad handle");
ok (GetHandleInformation($h1, $flags), "GetHandleInformation suceeds on good handle");
SKIP:{
    skip("GetHandleInformation doesn't do SEH, null pointer crashes", 1);
    ok (!GetHandleInformation($h1, []), "GetHandleInformation flags required");
}

ok(WriteFile( $h1, "Original text\n", 0, [], [] ), "WriteFile works");
{
    my ($sizeLow, $sizeHigh);
#this tests makes sure that a DWORD * is extended when copied to a 64b IV correctly on x64
#if the extend logic is broken, on x64 the result will look like 0xFFFFFFFF00000000
#instead of 0.
    $sizeHigh = eval( $Config{ivsize} == 8 ? "0xFFFFFFFFFFFFFFFF" : "0xFFFFFFFF");
    $sizeLow = GetFileSize($h1, $sizeHigh);
    ok($sizeLow == length("Original text\n") && $sizeHigh == 0, "GetFileSize works");
}
ok(CloseHandle($h1), "CloseHandle worked");

chdir( $temp );
if (-e "$dir/ReadOnly.txt") {
    chmod 0777, "$dir/ReadOnly.txt";
    unlink "$dir/ReadOnly.txt";
}
unlink "$dir/CanWrite.txt" if -e "$dir/CanWrite.txt";
rmdir $dir;
