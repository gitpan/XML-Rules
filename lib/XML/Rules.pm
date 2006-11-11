package XML::Rules;

use warnings;
no warnings qw(uninitialized);
use strict;
use Carp;

use XML::Parser::Expat;

=head1 NAME

XML::Rules - parse XML & process tags by rules starting from leaves

=head1 VERSION

Version 0.08

=cut

our $VERSION = '0.08';

=head1 SYNOPSIS

    use XML::Rules;

	$xml = <<'*END*'
	<doc>
	 <person>
	  <fname>...</fname>
	  <lname>...</lname>
	  <email>...</email>
	  <address>
	   <street>...</street>
	   <city>...</city>
	   <country>...</country>
	   <bogus>...</bogus>
	  </address>
	  <phones>
	   <phone type="home">123-456-7890</phone>
	   <phone type="office">663-486-7890</phone>
	   <phone type="fax">663-486-7000</phone>
	  </phones>
	 </person>
	 <person>
	  <fname>...</fname>
	  <lname>...</lname>
	  <email>...</email>
	  <address>
	   <street>...</street>
	   <city>...</city>
	   <country>...</country>
	   <bogus>...</bogus>
	  </address>
	  <phones>
	   <phone type="office">663-486-7891</phone>
	  </phones>
	 </person>
	</doc>
	*END*

	@rules = (
		_default => sub {$_[0] => $_[1]->{_content}},
			# by default I'm only interested in the content of the tag, not the attributes
		bogus => undef,
			# let's ignore this tag and all inner ones as well
		address => sub {address => "$_[1]->{street}, $_[1]->{city} ($_[1]->{country})"},
			# merge the address into a single string
		phone => sub {$_[1]->{type} => $_[1]->{content}},
			# let's use the "type" attribute as the key and the content as the value
		phones => sub {delete $_[1]->{_content}; %{$_[1]}},
			# remove the text content and pass along the type => content from the child nodes
		person => sub { # lets print the values, all the data is readily available in the attributes
			print "$_[1]->{lname}, $_[1]->{fname} <$_[1]->{email}>\n";
			print "Home phone: $_[1]->{home}\n" if $_[1]->{home};
			print "Office phone: $_[1]->{office}\n" if $_[1]->{office};
			print "Fax: $_[1]->{fax}\n" if $_[1]->{fax};
			print "$_[1]->{address}\n\n";
			return; # the <person> tag is processed, no need to remember what it contained
		},
	);
	$parser = XML::Rules->new(rules => \@rules);
	$parser->Parse( $xml);


=head1 CONSTRUCTOR

	my $parser = XML::Rules->new(
		rules => \@rules,
		[ start_rules => \@start_rules, ]
		[ style => 'parser' / 'filter', ]
		# and optionaly parameters passed to XML::Parser::Expat
	);

Options passed to XML::Parser::Expat: ProtocolEncoding Namespaces NoExpand Stream_Delimiter ErrorContext ParseParamEnt Base

The style specifies whether you want to build a parser used to extract stuff from the XML or filter/modify the XML. If you specify
style => 'filter' then all tags for which you do not specify a subroutine rule or that occure inside such a tag are copied to the output filehandle
passed to the ->filter() or ->filterfile() methods.

=head2 The Rules

The rules option may be either an arrayref or a hashref, the module doesn't care, but if you want to use regexps to specify the groups of tags to be handled
by the same rule you should use the array ref. The rules array/hash is made of pairs in form

	tagspecification => action

where the tagspecification may be either a name of a tag, a string containing comma or pipe ( "|" ) delimited list of tag names
or a string containing a regexp enclosed in // with optional parameters or a qr// compiled regular expressions. The tag names and tag name lists
take precedence to the regexps, the regexps are (in case of arrayref only!!!) tested in the order in which they are specified.

These rules are evaluated/executed whenever a tag if fully parsedin including all the content and child tags and they may access the content and attributes of the
specified tag plus the stuff produced by the rules evaluated for the child tags.

The action may be either

	an undef or empty string = ignore the tag and all its children
	a subroutine reference = the subroutine will be called to handle the tag data&contents
	'content' = only the content of the tag is preserved and added to
		the parent tag's hash as an attribute named after the tag
		sub { $_[0] => $_[1]->{_content}}
	'content trim' = only the content of the tag is preserved, trimmed and added to
		the parent tag's hash as an attribute named after the tag
		sub { s/^\s+//,s/\s+$// for ($_[1]->{_content}); $_[0] => $_[1]->{_content}}
	'as is' = the tag's hash is added to the parent tag's hash
		as an attribute named after the tag
		sub { $_[0] => $_[1]}
	'as is trim' = the tag's hash is added to the parent tag's hash
		as an attribute named after the tag, the content is trimmed
		sub { $_[0] => $_[1]}
	'as array' = the tag's hash is pushed to the attribute named after the tag
		in the parent tag's hash
		sub { '@'.$_[0] => $_[1]}
	'as array trim' = the tag's hash is pushed to the attribute named after the tag
		in the parent tag's hash, the content is trimmed
		sub { '@'.$_[0] => $_[1]}
	'no content' = the _content is removed from the tag's hash and the hash
		is added to the parent's hash into the attribute named after the tag
		sub { delete $_[1]->{_content}; $_[0] => $_[1]}
	'no content array' = similar to 'no content' except the hash is pushed
		into the array referenced by the attribute
	'as array no content' = same as 'no content array'
	'pass' = the tag's hash is dissolved into the parent's hash,
		that is all tag's attributes become the parent's attributes.
		The _content is appended to the parent's _content.
		sub { %{$_[0]}}
	'pass no content' = the _content is removed and the hash is dissolved
		into the parent's hash.
		sub { delete $_[1]->{_content}; %{$_[0]}}
	'pass without content' = same as 'pass no content'
	'raw' = the [tagname => attrs] is pushed to the parent tag's _content.
		You would use this styleif you wanted to be able to print
		the parent tag as XML preserving the whitespace or other textual content
		sub { [$_[0] => $_[1]]}
	'raw extended' = the [tagname => attrs] is pushed to the parent tag's _content
		and the attrs are added to the parent's attribute hash with ":$tagname" as the key
		sub { (':'.$Element => $data, [$Element => $data])};


The subroutines in the rules specification receive five parameters:

	$rule->( $tag_name, \%attrs, \@context, \@parent_data, $parser)

It's OK to destroy the first two parameters, but you should treat the other three as read only!

	$tag_name = string containing the tag name
	\%attrs = hash containing the attributes of the tag plus the _content key
		containing the text content of the tag. If it's not a leaf tag it may
		also contain the data returned by the rules invoked for the child tags.
	\@context = an array containing the names of the tags enclosing the current
		one. The parent tag name is the last element of the array.
	\@parent_data = an array containing the hashes with the attributes
		and content read&produced for the enclosing tags so far.
		You may need to access this for example to find out the version
		of the format specified as an attribute of the root tag. You may
		safely add, change or delete attributes in the hashes, but all bets
		are off if you change the number or type of elements of this array!
	$parser = the parser object.

The subroutine may decide to handle the data and return nothing or
tweak the data as necessary and return just the relevant bits. It may also
load more information from elsewhere based on the ids found in the XML
and provide it to the rules of the ancestor tags as if it was part of the XML.

The possible return values of the subroutines are:

1) nothing or undef or "" - nothing gets added to the parent tag's hash

2) a single string - if the parent's _content is a string then the one produced by this rule is appended to the parent's _content.
If the parent's _content is an array, then the string is push()ed to the array.

3) a single reference - if the parent's _content is a string then it's changed to an array containing the original string and this reference.
If the parent's _content is an array, then the string is push()ed to the array.

4) an even numbered list - it's a list of key & value pairs to be added to the parent's hash.

The handling of the attributes may be changed by adding '@', '+', '*' or '.' before the attribute name.

Without any "sigil" the key & value is added to the hash overwriting any previous values.
The values for the keys starting with '@' are push()ed to the arrays referenced by the key name
without the @. If there already is an attribute of the same name then the value will be preserved and will become
the first element in the array.
The values for the keys starting with '+' are added to the current value, the ones starting with '.' are
appended to the current value and the ones starting with '*' are multiplied by the current value.

5) an odd numbered list - the last element is appended or push()ed to the parent's _content, the rest is handled as in the previous case.

=head2 The Start Rules

Apart from the normal rules that get invoked once the tag is fully parsed, including the contents and child tags, you may want to
attach some code to the start tag to (optionaly) skip whole branches of XML or set up attributes and variables. You may set up
the start rules either in a separate parameter to the constructor or in the rules=> by prepending the tag name(s) by ^.


These rules are in form

	tagspecification => undef / '' / 'skip'	--> skip the element, including child tags
	tagspecification => 1 / 'handle'	--> handle the element, may be needed
		if you specify the _default rule.
	tagspecification => \&subroutine

The subroutines receive the same parameters as for the (end tag) rules, but their return value is treated differently.
If the subroutine returns a false value then the whole branch enclosed by the current tag is skipped, no data are stored and no rules are
executed. You may modify the hash referenced by $attr.

Both types of rules are free to store any data they want in $parser->{pad}. This property is NOT emptied
after the parsing!

=cut

sub new {
	my $class = shift;
	my %params = @_;
	croak "Please specify the rules=> for the parser!" unless $params{rules} and ref($params{rules});

	my $self = {rules => {}, start_rules => {}};
	bless $self, $class;

	my @rules = (ref($params{rules}) eq 'HASH' ? %{$params{rules}} : @{$params{rules}}); # dereference and copy
	delete $params{rules};

	my @start_rules;
	if ($params{start_rules} and ref($params{start_rules})) {
		@start_rules = ref($params{start_rules}) eq 'HASH' ? %{$params{start_rules}} : @{$params{start_rules}}; # dereference and copy
	};
	delete $params{start_rules};

	for (my $i=0; $i <= $#rules; $i+=2) {
		next unless $rules[$i] =~ s/^\^//;
		push @start_rules, splice( @rules, $i, 2);
		$i-=2;
	}

	$self->_split_rules( \@rules, 'rules');
	$self->_split_rules( \@start_rules, 'start_rules');

	$self->{for_parser} = {};
	{ # extract the params for the XML::Parser::Expat constructor
		my @for_parser = grep exists($params{$_}), qw(ProtocolEncoding Namespaces NoExpand Stream_Delimiter ErrorContext ParseParamEnt Base);
		if (@for_parser) {
			@{$self->{for_parser}}{@for_parser} = @params{@for_parser};
			delete @params{@for_parser};
		}
	}

	$self->{opt}{lc $_} = $params{$_} for keys %params;
	$self->{style} = $params{style} || 'parser';

	return $self;
}

sub _split_rules {
	my ($self, $rules, $type) = @_;

	while (@$rules) {
		my ($tag, $code) = (shift(@$rules), shift(@$rules));
		if ($tag =~ m{^/(.*)/(\w*)$}) { # string with a '/regexp/'
			if ($2) {
				push @{$self->{$type.'_re'}}, qr/(?$2)$1/;
			} else {
				push @{$self->{$type.'_re'}}, qr/$1/;
			}
			push @{$self->{$type.'_re_code'}}, $code;
		} elsif (ref($tag) eq 'Regexp') { # a qr// created regexp
			push @{$self->{$type.'_re'}}, $tag;
			push @{$self->{$type.'_re_code'}}, $code;
		} elsif ($tag =~ /[,\|]/) { # a , or | separated list
			if ($tag =~ s/^\^//) {
				my @tags = split(/\s*[,\|]\s*/, $tag);
				$self->{$type}{'^'.$_} = $code for (@tags);
			} else {
				my @tags = split(/\s*[,\|]\s*/, $tag);
				$self->{$type}{$_} = $code for (@tags);
			}
		} else { # a single tag
			$self->{$type}{$tag} = $code;
		}
	}
}


sub _run {
	my $self = shift;
	my $string = shift;
	$self->{parameters} = shift;

	$self->{parser} = XML::Parser::Expat->new( %{$self->{for_parser}});

	$self->{parser}->setHandlers(
		Start => _Start($self),
		End => _End($self),
		Char => _Char($self),
	);

	$self->{data} = [];
	$self->{context} = [];
	$self->{parser}->parse($string);

	delete $self->{normal_handlers};
	delete $self->{ignore_handlers};

	delete $self->{parameters};
	my $data; # return the accumulated data, without keeping a copy inside the object
	($data, $self->{data}) = ($self->{data}[0], undef);
	if (!defined(wantarray()) or ! keys(%$data)) {
		return;

	} elsif (keys(%$data) == 1 and exists(${$data}{_content})) {
		if (ref(${$data}{_content}) eq 'ARRAY' and @{${$data}{_content}} == 1) {
			return ${${$data}{_content}}[0]
		} else {
			return ${$data}{_content}
		}

	} else {
		return $data;
	}
}


sub parsestring;
*parsestring = \&parse;
sub parse {
	my $self = shift;
	croak("This XML::Rules object may only be used in as a parser!") if ($self->{style} eq 'filter');
	$self->_run(@_);
}

sub parsefile {
	my $self = shift;
	croak("This XML::Rules object may only be used in as a parser!") if ($self->{style} eq 'filter');
	my $filename = shift;
	open my $IN, '<', $filename or croak "Cannot open '$filename' for reading: $^E";
	return $self->_run($IN, @_);
}


sub filterstring;
*filterstring = \&filter;
sub filter {
	my $self = shift;
	croak("This XML::Rules object may only be used in as a filter!") unless ($self->{style} eq 'filter');

	my $XML = shift;
	$self->{FH} = shift || select(); # wither passed or the selected filehandle
	$self->_run($XML, @_);
	delete $self->{FH};
}

sub filterfile {
	my $self = shift;
	croak("This XML::Rules object may only be used in as a filter!") unless ($self->{style} eq 'filter');

	my $filename = shift;
	open my $IN, '<', $filename or croak "Cannot open '$filename' for reading: $^E";

	$self->{FH} = shift || select(); # wither passed or the selected filehandle
	$self->_run($IN, @_);
	delete $self->{FH};
}


sub _Start {
	my $self = shift;
	return sub {
		my ( $Parser, $Element , %Attr) = @_;

		$self->_find_rule( 'start_rules', $Element, 'handle') if (! exists $self->{start_rules}{$Element});
		$self->_find_rule( 'rules', $Element, 'as is') if (! exists $self->{rules}{$Element});

		if ($self->{start_rules}{$Element} ne 'handle'
		and (
			!$self->{start_rules}{$Element}
			or $self->{start_rules}{$Element} eq 'skip'
			or !$self->{start_rules}{$Element}->($Element,\%Attr, $self->{context}, $self->{data}, $self)
			)
		) {
			# ignore the tag and the ones below
			if (exists $self->{ignore_handlers}) {
				$self->{parser}->setHandlers(@{$self->{ignore_handlers}})
			} else {
				$self->{ignore_handlers} = [
					Start => _StartIgnore($self),
					Char => undef,
					End => _EndIgnore($self),
				];
				$self->{normal_handlers} = [$self->{parser}->setHandlers(@{$self->{ignore_handlers}})];
			}
			push @{$self->{ignore_context}}, $Element;

		} else {
			# process the tag and the ones below
			push @{$self->{context}}, $Element;
			push @{$self->{data}}, \%Attr;

			if ($self->{style} eq 'filter') {
				if (! $self->{in_interesting}) {
					if (@{$self->{data}}>=2) {
						print {$self->{FH}} $self->{data}->[-2]{_content};
						delete $self->{data}->[-2]{_content};
					}
				}
				$self->{in_interesting}++ if ref($self->{rules}{$Element});
				if (! $self->{in_interesting}) {
					print {$self->{FH}} $self->toXML($Element, \%Attr, "don't close");
				}
			}

		}
	}
}

sub _find_rule {
	my ($self, $type, $Element, $default) = @_;

	if (exists($self->{$type.'_re'})) {
		for(my $i = 0; $i < @{$self->{$type.'_re'}}; $i++) {
			if ($Element =~ $self->{$type.'_re'}[$i]) {
				$self->{$type}{$Element} = $self->{$type.'_re_code'}[$i];
				last;
			}
		}
	}
	if (! exists $self->{$type}{$Element}) {
		$self->{$type}{$Element} = (exists($self->{$type}{_default}) ? $self->{$type}{_default} : $default);
	}
}

sub _Char {
	my $self = shift;
	return sub {
		my ( $Parser, $String) = @_;
		if (!exists $self->{data}[-1]{_content}) {
			$self->{data}[-1]{_content} = $String;
		} elsif (!ref $self->{data}[-1]{_content}) {
			$self->{data}[-1]{_content} .= $String;
		} else {
			if (ref $self->{data}[-1]{_content}[-1]) {
				push @{$self->{data}[-1]{_content}}, $String;
			} else {
				$self->{data}[-1]{_content}[-1] .= $String;
			}
		}
	}
}

sub _End {
	my $self = shift;
	return sub {
		my ( $Parser, $Element) = @_;
		pop @{$self->{context}};

		my $rule = exists ($self->{rules}{$Element}) ? $self->{rules}{$Element} : $self->{rules}{_default};
		my $data = pop @{$self->{data}};

		my @results;
		if (ref $rule) {
			@results = $rule->($Element, $data, $self->{context}, $self->{data}, $self);
			if ($self->{style} eq 'filter') {

				$self->{in_interesting}--;
				if (!$self->{in_interesting}) {
					if (@{$self->{data}}) {
						print {$self->{FH}} $self->{data}[-1]{_content};
						delete $self->{data}[-1]{_content};
					}
					print {$self->{FH}} $self->toXML(@results) if @results;
					@results = ();
				}
			}
		} elsif ($self->{style} eq 'filter' and ! $self->{in_interesting}) {
			print {$self->{FH}} "$data->{_content}</$Element>";

		} elsif ($rule eq '') {
			@results = ();
		} elsif ($rule eq 'content') {
			@results = ($Element => $data->{_content});
		} elsif ($rule eq 'content trim') {
			s/^\s+//,s/\s+$// for ($data->{_content});
			@results = ($Element => $data->{_content});
		} elsif ($rule eq 'as is') {
			@results = ($Element => $data);
		} elsif ($rule eq 'as is trim') {
			s/^\s+//,s/\s+$// for ($data->{_content});
			@results = ($Element => $data);
		} elsif ($rule eq 'as array') {
			@results = ('@'.$Element => $data);
		} elsif ($rule eq 'as array trim') {
			s/^\s+//,s/\s+$// for ($data->{_content});
			@results = ('@'.$Element => $data);
		} elsif ($rule eq 'no content') {
			delete ${$data}{_content}; @results = ($Element => $data);
		} elsif ($rule eq 'no content array' or $rule eq 'as array no content') {
			delete ${$data}{_content}; @results = ('@' . $Element => $data);

		} elsif ($rule eq 'pass') {
			@results = (%$data);
		} elsif ($rule eq 'pass trim') {
			s/^\s+//,s/\s+$// for ($data->{_content});
			@results = (%$data);
		} elsif ($rule eq 'pass no content' or $rule eq 'pass without content') {
			delete ${$data}{_content}; @results = (%$data);

		} elsif ($rule eq 'raw') {
			@results = [$Element => $data];

		} elsif ($rule eq 'raw extended') {
			@results = (':'.$Element => $data, [$Element => $data]);

		} else {
			croak "Unknown predefined rule '$rule'!";
		}

		return unless scalar(@results) or scalar(@results) == 1 and ($results[0] eq '' or !defined($results[0]));

		@{$self->{data}} = ({}) unless @{$self->{data}}; # oops we are already closing the root tag!

		if (scalar(@results) % 2) {
			# odd number of items, last is content
			my $value = pop(@results);
			_add_content( $self->{data}[-1], $value);
		}

		while (@results) {
			my ($key, $value) = ( shift(@results), shift(@results));
			if ($key eq '_content') {
				_add_content( $self->{data}[-1], $value);
			} elsif ($key =~ s/^\@//) {
				if (exists($self->{data}[-1]{$key}) and ref($self->{data}[-1]{$key}) ne 'ARRAY') {
					$self->{data}[-1]{$key} = [$self->{data}[-1]{$key}, $value];
				} else {
					push @{$self->{data}[-1]{$key}}, $value;
				}
			} elsif ($key =~ s/^\+//) {
				if (exists($self->{data}[-1]{$key})) {
					$self->{data}[-1]{$key} += $value;
				} else {
					$self->{data}[-1]{$key} = $value;
				}
			} elsif ($key =~ s/^\*//) {
				if (exists($self->{data}[-1]{$key})) {
					$self->{data}[-1]{$key} *= $value;
				} else {
					$self->{data}[-1]{$key} = $value;
				}
			} elsif ($key =~ s/^\.//) {
				if (exists($self->{data}[-1]{$key})) {
					$self->{data}[-1]{$key} .= $value;
				} else {
					$self->{data}[-1]{$key} = $value;
				}
			} else {
				$self->{data}[-1]{$key} = $value;
			}
		}
	}
}

sub _StartIgnore {
	my ($self) = shift;
	return sub {
		push @{$self->{ignore_context}}, $_[1]; # push the element name
	}
}

sub _EndIgnore {
	my ($self) = shift;
	return sub {
		pop @{$self->{ignore_context}};
		return if @{$self->{ignore_context}}; # there is still something left to pop

		delete $self->{ignore_context};
		$self->{parser}->setHandlers(@{$self->{normal_handlers}})
	}
}

sub _add_content {
	my ($hash, $value) = @_;
	if (ref($value)) {
		if (ref($hash->{_content})) {
			# both are refs, push to @_content
			push @{$hash->{_content}}, $value;
		} elsif (exists($hash->{_content})) {
			# result is ref, _content is not -> convert to an arrayref containing old _content and result
			$hash->{_content} = [ $hash->{_content}, $value]
		} else {
			# result is ref, _content is not present
			$hash->{_content} = [ $value]
		}
	} else {
		if (ref($hash->{_content})) {
			# _content is an arrayref, value is a string
			if (ref $hash->{_content}[-1]) {
				# the last element is a ref -> push
				push @{$hash->{_content}}, $value;
			} else {
				# the last element is a string -> concatenate
				$hash->{_content}[-1] .= $value;
			}
		} else {
			# neither is ref, concatenate
			$hash->{_content} .= $value;
		}
	}
}

=head1 METHODS

=head2 parse

	$parser->parse( $string [, $parameters]);
	$parser->parse( $IOhandle [, $parameters]);

Parses the XML in the string or reads and parses the XML from the opened IO handle,
executes the rules as it encounters the closing tags and returns the resulting structure.

The scalar or reference passed as the second parameter to the parse() method is assigned to
$parser->{parameters} for the parsing of the file or string. Once the XML is parsed the key is
deleted. This means that the $parser does not retain a reference to the $parameters after the parsing.

=head2 parsestring

	$parser->parsestring( $string [, $parameters]);

Just an alias to ->parse().

=head2 parsefile

	$parser->parsefile( $filename [, $parameters]);

Opens the specified file and parses the XML and executes the rules as it encounters
the closing tags and returns the resulting structure.


=head2 filter

	$parser->filter( $string, $OutputIOhandle [, $parameters]);
	$parser->filter( $InputIOhandle, $OutputIOhandle [, $parameters]);

Parses the XML in the string or reads and parses the XML from the opened IO handle,
copies the tags that do not have a subroutine rule specified and do not occure under such a tag,
executes the specified rules and prints the results.

The scalar or reference passed as the third parameter to the filter() method is assigned to
$parser->{parameters} for the parsing of the file or string. Once the XML is parsed the key is
deleted. This means that the $parser does not retain a reference to the $parameters after the parsing.

=head2 filterstring

	$parser->filterstring( $string, $OutputIOhandle [, $parameters]);

Just an alias to ->filter().

=head2 filterfile

	$parser->filterfile( $filename, $OutputIOhandle [, $parameters]);

Opens the specified file and parses the XML and executes the rules as it encounters
the closing tags and returns the resulting structure.

=cut

sub escape_value {
  my($self, $data, $level) = @_;

  return '' unless(defined($data) and $data ne '');

  $data =~ s/&/&amp;/sg;
  $data =~ s/</&lt;/sg;
  $data =~ s/>/&gt;/sg;
  $data =~ s/"/&quot;/sg;

  $level = $self->{opt}->{numericescape} unless defined $level;
  return $data unless $level;

  return $self->_numeric_escape($data, $level);
}

sub _numeric_escape {
  my($self, $data, $level) = @_;

  use utf8; # required for 5.6

  if($self->{opt}->{numericescape} eq '2') {
    $data =~ s/([^\x00-\x7F])/'&#' . ord($1) . ';'/gse;
  }
  else {
    $data =~ s/([^\x00-\xFF])/'&#' . ord($1) . ';'/gse;
  }

  return $data;
}

=head2 escape_value

	$parser->escape_value( $data [, $numericescape])

This method escapes the $data for inclusion in XML, the $numericescape may be 0, 1 or 2
and controls whether to convert 'high' (non ASCII) characters to XML entities.

0 - default: no numeric escaping (OK if you're writing out UTF8)

1 - only characters above 0xFF are escaped (ie: characters in the 0x80-FF range are not escaped), possibly useful with ISO8859-1 output

2 - all characters above 0x7F are escaped (good for plain ASCII output)

You can also specify the default value in the constructor

	my $parser = XML::Rules->new(
		...
		NumericEscape => 2,
	);

=cut

sub toXML {
	my ($self, $tag, $attrs, $no_close) = @_;

	my $content = delete($attrs->{_content});
	my $result = "<$tag";
	my $subtags = '';
	foreach my $key (keys %$attrs) {
		next if $key =~ /^:/;
		if (ref $attrs->{$key}) {
			if (ref $attrs->{$key} eq 'ARRAY') {
				foreach my $subtag (@{$attrs->{$key}}) {
					$subtags .= $self->toXML($key, $subtag);
				}
			} elsif (ref $attrs->{$key} eq 'HASH') {
				$subtags .= $self->toXML($key, $attrs->{$key})
			} else {
				croak(ref($attrs->{$key}) . " attributes not supported in XML::Rules->toXML()!");
			}
		} else {
			$result .= qq{ $key="} . $self->escape_value($attrs->{$key}) . qq{"};
		}
	}
	if (! defined $content and $subtags eq '') {
		if ($no_close) {
			return $result.">";
		} else {
			return $result."/>";
		}

	} elsif (!ref($content)) {
		if ($no_close) {
			return "$result>$subtags$content";
		} else {
			return "$result>$subtags$content</$tag>";
		}

	} elsif (ref($content) eq 'ARRAY') {
		$result .= ">";
		foreach my $snippet (@$content) {
			if (!ref($snippet)) {
				$result .= $self->escape_value($snippet);
			} elsif (ref($snippet) eq 'ARRAY') {
				if (@$snippet == 2) {
					$result .= $self->toXML($snippet->[0], $snippet->[1]);
				} else {
					croak("Arrays in _content must be in format [\$tagname => \%attrs] in XML::Rules->toXML()!");
				}
			} else {
				croak(ref($content) . " not supported in _content in XML::Rules->toXML()!");
			}
		}
		if ($no_close) {
			return $result;
		} else {
			return $result."</$tag>";
		}
	} else {
		croak(ref($content) . " _content not supported in XML::Rules->toXML()!");
	}
}

sub parentsToXML {
	my ($self, $level) = @_;
	my $tag_names = $self->{context};
	my $tag_attrs = $self->{data};

	$level = scalar(@$tag_names) unless $level;

	my $result = '';
	for (my $i = -1; -$i <= $level; $i--) {
		$result = $self->toXML( ${$tag_names}[$i], ${$tag_attrs}[$i], 1) . $result;
	}
	return $result;
}

sub closeParentsToXML {
	my ($self, $level) = @_;
	my $tag_names = $self->{context};

	if ($level) {
		return '</' . join( '></', (reverse(@{$tag_names}))[0..$level-1]) . '>';
	} else {
		return '</' . join( '></', reverse(@$tag_names)) . '>';
	}
}

=head2 toXML

	$xml = $parser->toXML( $tagname, \%attrs[, $do_not_close])

You may use this method to convert the datastructures created by parsing the XML into the XML format.
Not all data structures may be printed! I'll add more docs later, for not please do experiment.

=head2 parentsToXML

	$xml = $parser->parentsToXML( [$level])

Prints all or only the topmost $level ancestor tags, including the attributes and content (parsed so far),
but without the closing tags. You may use this to print the header of the file you are parsing,
followed by calling toXML() on a structure you build and then by closeParentsToXML() to close
the tags left opened by parentsToXML(). You most likely want to use the style => 'filter' option
for the constructor instead.

=head2 closeParentsToXML

	$xml = $parser->closeParentsToXML( [$level])

Prints the closing tags for all or the topmost $level ancestor tags of the one currently processed.

=head1 HOW TO USE

You may view the module either as a XML::Simple on steriods and use it to build a data structure
similar to the one produced by XML::Simple with the added benefit of being able
to specify what tags or attributes to ignore, when to take just the content, what to store as an array etc.

Or you could view it as yet another event based XML parser that differs from all the others only in one thing.
It stores the data for you so that you do not have to use globals or closures and wonder where to attach
the snippet of data you just received onto the structure you are building.

You can use it in a way similar to XML::Twig with simplify(), specify the rules to transform the lower
level tags into a XML::Simple like (simplify()ed) structure and then handle the structure in the rule for
the tag(s) you'd specify in XML::Twig's twig_roots.

=head1 AUTHOR

Jan Krynicky, C<< <Jenda at CPAN.org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-xml-rules at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=XML-Rules>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc XML::Rules

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/XML-Rules>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/XML-Rules>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=XML-Rules>

=item * Search CPAN

L<http://search.cpan.org/dist/XML-Rules>

=item * PerlMonks

Please see L<http://www.perlmonks.org/?node_id=581313> or
L<http://www.perlmonks.org/?node=XML::Rules>for discussion.

=back

=head1 ACKNOWLEDGEMENTS

The escape_value() method is taken with minor changes from XML::Simple.

=head1 COPYRIGHT & LICENSE

Copyright 2006 Jan Krynicky, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1; # End of XML::Rules
