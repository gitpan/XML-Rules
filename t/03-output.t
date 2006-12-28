#!perl -T

use strict;
use warnings;
use Test::More tests => 17;
use Data::Dumper;
use Encode qw(encode);

use XML::Rules;

my $xml = <<'*END*';
<data version="1.0">
	<foo attr="ahoj">cau</foo>
	<bar/>
	<baz>xxx&quot;xxx</baz>
	<baz>yyyy</baz>
</data>
*END*

my $parser = XML::Rules->new(
	rules => [
		_default => 'raw',
#		data => 'as is',
	],
);

my $res = $parser->parse($xml);
#print Dumper($res);
my $new_xml = $parser->ToXML(@$res) . "\n";
#print $new_xml;

is( $xml, $new_xml, "Parse and output");


{
	my $res = $parser->ToXML( 'data', 'some <important> "content"');
	is( $res, '<data>some &lt;important&gt; &quot;content&quot;</data>', "Tag with content only");
}

{
	my $res = $parser->ToXML( 'data', {_content => 'some <important> "content"'});
	is( $res, '<data>some &lt;important&gt; &quot;content&quot;</data>', "Tag with content as attribute");
}

{
	my $res = $parser->ToXML( 'data', {str => 'some <important> "content"'});
	is( $res, '<data str="some &lt;important&gt; &quot;content&quot;"/>', "Tag with one attribute");
}

{
	my $res = $parser->ToXML( 'data', {str => 'string', _content => 'some <important> "content"'});
	is( $res, '<data str="string">some &lt;important&gt; &quot;content&quot;</data>', "Tag with one attribute and content");
}

{
	my $res = $parser->ToXML( 'data', {str => 'some <important> "content"', other => 156});
	ok(
		(
		$res eq '<data str="some &lt;important&gt; &quot;content&quot;" other="156"/>'
		or
		$res eq '<data other="156" str="some &lt;important&gt; &quot;content&quot;"/>'
		),
		"Tag with two attributes"
	);
}


{
	my $res = $parser->ToXML( 'data', [qw(foo bar baz)]);
	is( $res, '<data>foo</data><data>bar</data><data>baz</data>', "Tag with array of contents");
}

{
	my $res = $parser->ToXML( 'data', [{str => 'foo'}, {str => 'bar'}, {str => 'baz'}]);
	is( $res, '<data str="foo"/><data str="bar"/><data str="baz"/>', "Tag with array of attributes");
}

{
	my $res = $parser->ToXML( 'data', {bar => {_content => 'baz'}});
	is( $res, '<data><bar>baz</bar></data>', "Tag with subtag");
}

{
	my $res = $parser->ToXML( 'data', {bar => {}});
	is( $res, '<data><bar/></data>', "Tag with empty subtag");
}

{
	my $res = $parser->ToXML( 'data', {attr => 5, bar => {}});
	is( $res, '<data attr="5"><bar/></data>', "Tag with attribute and empty subtag");
}

{
	my $res = $parser->ToXML( 'data', {attr => 5, bar => {a => 42, _content=> 'string'}});
	is( $res, '<data attr="5"><bar a="42">string</bar></data>', "Tag with attribute and subtag with content and attribute");
}


{
	my $res = $parser->ToXML( 'data', 'some <important> "content"', 1);
	is( $res, '<data>some &lt;important&gt; &quot;content&quot;', "Tag with content only, don't close");
}

{
	my $res = $parser->ToXML( 'data', {_content => 'some <important> "content"'}, "don't close");
	is( $res, '<data>some &lt;important&gt; &quot;content&quot;', "Tag with content as attribute, don't close");
}

{
	my $res = $parser->ToXML( 'data', {str => 'some <important> "content"'}, 1);
	is( $res, '<data str="some &lt;important&gt; &quot;content&quot;">', "Tag with one attribute, don't close");
}

{
	my $res = $parser->ToXML( 'data', {str => 'string', _content => 'some <important> "content"'}, 1);
	is( $res, '<data str="string">some &lt;important&gt; &quot;content&quot;', "Tag with one attribute and content, don't close");
}


{
	my $res = $parser->ToXML( 'data', {_content => ['start', [str => 'foo'], 'middle', [str => 'bar'], [str => 'baz'], 'end']});
	is( $res, '<data>start<str>foo</str>middle<str>bar</str><str>baz</str>end</data>', "Tag with mix of text content and subtags");
}
