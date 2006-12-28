$|=1;
use XML::Rules;

$xml = <<'*END*';
<root>
<doc boo="789">
 <person active="1">
  <fname>Jane</fname>
  <lname>Luser</lname>
  <email>JLuser@bogus.com</email>
  <address>
   <street>Washington st.</street>
   <city>Old Creek</city>
   <country>The US</country>
   <bogus>bleargh</bogus>
  </address>
 </person>
 <person active="0">
  <fname>John</fname>
  <lname>Other</lname>
  <email>JOther@silly.com</email>
  <address>
   <street>Grant's st.</street>
   <city>New Creek</city>
   <country>Canada</country>
   <bogus>sdrysdfgtyh <foo>degtrhy werthy</foo> drthyu</bogus>
  </address>
 </person>
</doc>
</root>
*END*

use Data::Dumper;
%rules = (
	_default => 'raw',
	email => 'raw extended',
#	lname => sub {$_[1]->{_content} = "Sir. $_[1]->{_content}"; return (':'.$_[0] => $_[1], [$_[0] => $_[1]])},
	lname => sub {$_[1]->{_content} = "Sir. $_[1]->{_content}"; return [$_[0] => $_[1]]},

	lname => sub {$_[1]->{_content} = "Sir. $_[1]->{_content}"; return \'raw'},
	lname => [ sub {$_[1]->{_content} = "Sir. $_[1]->{_content}"}, 'raw'],
	lname => [ 'raw', sub {$_[1]->{_content} = "Sir. $_[1]->{_content}"}],

	'^bogus' => undef, # means "ignore". The subtags ARE NOT processed.
	person => sub {
		print $_[4]->parentsToXML() if not exists($_[3]->[-1]{':printed'});
		$_[3]->[-1]{':printed'} = 1;

		$_[1]->{':email'}{_content} = lc($_[1]->{':email'}{_content});

		print $_[4]->toXML($_[0], $_[1]), "\n" if $_[1]->{active};
	},
	doc => sub {
		print "</doc>\n", $_[4]->closeParentsToXML();
	}, #'pass no content',
	foo => sub {print "FOOOOOOOO\n"},
);

my $parser = new XML::Rules (
	rules => \%rules,
	# other options
);

my $result = $parser->parsestring($xml);
