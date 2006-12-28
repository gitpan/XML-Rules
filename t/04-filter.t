#!perl -T

use strict;
use warnings;
use Test::More tests => 5;

use XML::Rules;

my $xml = <<'*END*';
<?xml version="1.0"?>
<doc>
 <person>
  <fname>Jane</fname>
  <lname>Luser</lname>
  <email>JLuser@bogus.com</email>
  <address>
   <street>Washington st.</street>
   <city>Old Creek</city>
   <country>The US</country>
   <bogus>bleargh</bogus>
  </address>
  <phones>
   <phone type="home">123-456-7890</phone>
   <phone type="office">663-486-7890</phone>
   <phone type="fax">663-486-7000</phone>
  </phones>
 </person>
 <person>
  <fname>John</fname>
  <lname>Other</lname>
  <email>JOther@silly.com</email>
  <address>
   <street>Grant's st.</street>
   <city>New Creek</city>
   <country>Canada</country>
   <bogus>sdrysdfgtyh degtrhy <foo>degtrhy werthy</foo>werthy drthyu</bogus>
  </address>
  <phones>
   <phone type="office">663-486-7891</phone>
  </phones>
 </person>
</doc>
*END*

{ #1
	my $parser = new XML::Rules (
		style => 'filter',
		rules => [
#			'^phone' => sub {return exists($_[1]->{type}) and $_[1]->{type} eq 'office'},
			'phone' => sub {
				return unless $_[1]->{type} eq 'office';
				return $_[0] => $_[1];
			}
		]
	);

	my $res = '';
	open my $FH, '>', \$res;
	$parser->filterstring($xml, $FH);
	close $FH;

	(my $correct = $xml) =~ s{<phone type="(home|fax)">[^<]+</phone>}{}g;
	is($res, $correct, "Remove tag according to attribute");
#open my $F, '>', 'test_mine.txt';print $F $res;close $F;
#open $F, '>', 'test_correct.txt';print $F $correct;close $F;
}

{ #2
	my $parser = new XML::Rules (
		style => 'filter',
		rules => [
			'^phone' => sub {return (exists($_[1]->{type}) and $_[1]->{type} eq 'office')},
		]
	);

	my $res = '';
	open my $FH, '>', \$res;
	$parser->filterstring($xml, $FH);
	close $FH;

#print $res;
#exit;

	(my $correct = $xml) =~ s{<phone type="(home|fax)">[^<]+</phone>}{}g;
	is($res, $correct, "Remove tag according to attribute using start rule");
#open my $F, '>', 'test_mine.txt';print $F $res;close $F;
#open $F, '>', 'test_correct.txt';print $F $correct;close $F;
}


{ #1
	my $parser = new XML::Rules (
		style => 'filter',
		rules => [
			'fname' => sub {
				$_[1]->{_content} = ":" . $_[1]->{_content} . ":";
				return $_[0] => $_[1];
			},
		]
	);

	my $res = '';
	open my $FH, '>', \$res;
	$parser->filterstring($xml, $FH);
	close $FH;

	(my $correct = $xml) =~ s{<fname>([^<]+)</fname>}{<fname>:$1:</fname>}g;
	is($res, $correct, "Tweak the content of <fname>");
#open my $F, '>', 'test_mine.txt';print $F $res;close $F;
#open $F, '>', 'test_correct.txt';print $F $correct;close $F;
}

{ #1
	my $parser = new XML::Rules (
		style => 'filter',
		rules => [
			'fname' => sub {return 'firstname' => $_[1]},
			'lname' => sub {return 'lastname' => $_[1]},
		]
	);

	my $res = '';
	open my $FH, '>', \$res;
	$parser->filterstring($xml, $FH);
	close $FH;

	(my $correct = $xml) =~ s{<fname>([^<]+)</fname>}{<firstname>$1</firstname>}g;
	$correct =~ s{<lname>([^<]+)</lname>}{<lastname>$1</lastname>}g;
	is($res, $correct, "Change <fname> to <firstname> and <lname> to <lastname>");
#open my $F, '>', 'test_mine.txt';print $F $res;close $F;
#open $F, '>', 'test_correct.txt';print $F $correct;close $F;
}

{ #1
	my $parser = new XML::Rules (
		style => 'filter',
		rules => [
			'phone' => sub {$_[1]->{type} => $_[1]->{_content}},
			'phones' => sub {
				if (exists $_[1]->{home}) {
					return 'phone' => $_[1]->{home}
				} else {
					return 'phone' => $_[1]->{office}
				}
			},
		]
	);

	my $res = '';
	open my $FH, '>', \$res;
	$parser->filterstring($xml, $FH);
	close $FH;

	(my $correct = $xml) =~ s{<phones>.*?</phones>}{<phone>123-456-7890</phone>}s;
	$correct =~ s{<phones>.*?</phones>}{<phone>663-486-7891</phone>}s;
	is($res, $correct, "Change <phones> to a single <phone> using the home or office phone");
#open my $F, '>', 'test_mine.txt';print $F $res;close $F;
#open $F, '>', 'test_correct.txt';print $F $correct;close $F;
}
