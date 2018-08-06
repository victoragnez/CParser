local lpeg = require "lpeglabel"

lpeg.locale(lpeg)

--lpeg functions
local P, S, V, T, Cp = lpeg.P, lpeg.S, lpeg.V, lpeg.T, lpeg.Cp

local alpha, digit, xdigit, alnum = lpeg.alpha, lpeg.digit, lpeg.xdigit, lpeg.alnum

--Label if doesn't match patt
local function expect(patt, label)
	return patt + Cp()*T(label)
end

--Parse token given his pattern and ignore comments and spaces after it
local function token(patt)
	return patt * V"skip"
end

--Parse a token and ignore comments and spaces given the string of the token
local function sym(str)
	return token(P(str))
end

--Parse the word str, guarantee that it's not a preffix of another word
local function kw(str)
	return token( P(str) * - ( V"keywords" * - ( alnum + "_" ) ) )
end

--Parse a list of patt separated by sep
local function sep_by(patt, sep, label)
	return patt * ( sep * expect(patt, label ) )^0
end

--Grammar
local G = { V"translation_unit", 
	translation_unit =
		V"skip" * expect(V"external_decl"^1, "invalid declaration") * expect(-P(1), "invalid declaration");

	external_decl =
		V"function_def" +
		V"decl";

	function_def =
		V"declarator" * V"decl"^0 * V"compound_stat" +
		V"decl_spec" * V"function_def";
	
	decl_spec =
		V"storage_class_spec" +
		V"type_spec" +
		V"type_qualifier";

	decl =
		V"decl_spec" * V"init_declarator_list"^-1 * sym(";") +
		V"decl_spec" * V"decl";

	storage_class_spec =
		kw("auto") +
		kw("register") +
		kw("static") +
		kw("extern") +
		kw("typedef");

	type_spec =
		kw("void") +
		kw("char") +
		kw("short") +
		kw("int") +
		kw("long") +
		kw("float") +
		kw("double") +
		kw("signed") +
		kw("unsigned") +
		V"typedef_name" +
		kw("enum") * V"id"^-1 * sym("{") * expect(V"enumerator", "expected enumerator") * 
			( sym(",") * expect(V"enumerator", "expected enumerator") )^0 * expect(sym("}"), "expected '}'") +
		kw("enum") * V"id" +
		V"struct_or_union" * V"id"^-1 * sym("{") * expect(V"struct_decl"^1, "expected at least one struct/union declaration")
			 * expect(sym("}"), "expected '}'") +
		V"struct_or_union" * V"id";

	type_qualifier =
		kw("const") +
		kw("volatile");

	struct_or_union =
		kw("struct") +
		kw("union");

	init_declarator_list =
		V"init_declarator" * ( sym(",") * expect(V"init_declarator", "expected declarator after ','") )^0;

	init_declarator =
		V"declarator" * sym("=") * V"initializer" +
		V"declarator";

	struct_decl =
		V"spec_qualifier" * V"struct_declarator" * ( sym(",") * expect(V"struct_declarator", "expected declarator after ','") )^0 
			* expect(sym(";"), "expected ';'") + 
		V"spec_qualifier" * V"struct_decl";

	spec_qualifier_list =
		( V"type_spec" + V"type_qualifier" )^1;
	
	spec_qualifier =
		V"type_spec" + V"type_qualifier";

	struct_declarator =
		V"declarator"^-1 * sym(":") * expect(V"const_exp", "invalid expression") + 
		V"declarator";

	enumerator =
		V"id" * sym("=") * expect(V"const_exp", "invalid expression") +
		V"id";

	declarator =
		V"pointer"^-1 * V"direct_declarator";

	direct_declarator =
		( V"id" + sym("(") * V"declarator" * expect(sym(")"), "expected ')'") ) * 
			( sym("[") * V"const_exp"^-1 * expect(sym("]"), "expected ']'") +
			sym("(") * V"param_type_list" * expect(sym(")"), "expected ')'") +
			sym("(") * V"id_list"^-1 * expect(sym(")"), "expected ')'") )^0;

	pointer =
		( sym("*") * V"type_qualifier"^0 ) * V"pointer" +
		( sym("*") * V"type_qualifier"^0 );

	param_type_list =
		V"param_decl" * ( sym(",") * V"param_decl" )^0 * ( sym(",") * sym("...") )^-1;

	param_decl =
		V"decl_spec"^1 * ( V"declarator" + V"abstract_declarator"^-1 );

	id_list =
		V"id" * ( sym(",") * expect(V"id", "expected identifier") )^0;

	initializer =
		sym("{") * V"initializer" * (sym(",") * V"initializer")^0 * sym(",")^-1 * expect(sym("}"), "expected '}'") +
		V"assignment_exp";

	type_name =
		V"spec_qualifier_list" * V"abstract_declarator"^-1;

	abstract_declarator =
		V"pointer" +
		V"pointer"^-1 * V"direct_abstract_declarator";

	direct_abstract_declarator =
		sym("(") * V"abstract_declarator" * expect(sym(")"), "expected ')'") *
			( sym("[") * V"const_exp"^-1 * expect(sym("]"), "expected ']'") + 
			sym("(") * V"param_type_list"^-1 * expect(sym(")"), "expected ')'") )^0;

	typedef_name =
		V"id";

	stat =
		V"id" * sym(":") * V"stat" +
		kw("case") * expect(V"const_exp", "invalid expression") * expect(sym(":"), "expected ':'") * 
			expect(V"stat", "expected statement after \"case\"") +
		kw("default") * expect(sym(":"), "expected ':'") * expect(V"stat", "expected statement after \"default\"") +
		V"exp"^-1 * sym(";") +
		V"compound_stat" +
		kw("if") * expect(sym("("), "expected '(' after \"if\"") * expect(V"exp", "invalid expression") * 
			expect(sym(")"), "expected ')'") * expect(V"stat", "expected statement") * kw("else") * expect(V"stat", "expected statement") +
		kw("if") * expect(sym("("), "expected '(' after \"if\"") * expect(V"exp", "invalid expression") * 
			expect(sym(")"), "expected ')'") * expect(V"stat", "expected statement") +
		kw("switch") * expect(sym("("), "expected '(' after \"switch\"") * expect(V"exp", "invalid expression") * 
			expect(sym(")"), "expected ')'") * expect(V"stat", "expected statement") +
		kw("while") * expect(sym("("), "expected '(' after \"while\"") * expect(V"exp", "invalid expression") * 
			expect(sym(")"), "expected ')'") * expect(V"stat", "expected statement") +
		kw("do") * expect(V"stat", "expected statement") * expect(kw("while"), "expected \"while\"") * 
			expect(sym("("), "expected '(' after \"while\"") * expect(V"exp", "invalid expression") * 
			expect(sym(")"), "expected ')'") * expect(sym(";"), "expected ';'") +
		kw("for") * 
			expect(sym("("), "expected '('") * V"exp"^-1 * 
			expect(sym(";"), "expected ';'") * V"exp"^-1 * 
			expect(sym(";"), "expected ';'") * V"exp"^-1 * 
			expect(sym(")"), "expected ')'") * V"stat" +
		kw("goto") * expect(V"id", "expected identifier") * expect(sym(";"), "expected ';'") +
		kw("continue") * expect(sym(";"), "expected ';'") +
		kw("break") * expect(sym(";"), "expected ';'") +
		kw("return") * V"exp"^-1 * expect(sym(";"), "expected ';'") +
		-sym("}")*expect(sym("}"), "invalid statement");

	compound_stat =
		sym("{") * V"decl"^0 * V"stat"^0 * expect(sym("}"), "expected '}'");

	exp =
		sep_by(V"assignment_exp", sym(","), "expected expression after ',' operator");

	assignment_exp =
		V"unary_exp" * V"assignment_operator" * expect(V"assignment_exp", "invalid expression") +
		V"conditional_exp";

	assignment_operator =
		token(P("=")*-P("=")) +
		sym("*=") +
		sym("/=") +
		sym("%=") +
		sym("+=") +
		sym("-=") +
		sym("<<=") +
		sym(">>=") +
		sym("&=") +
		sym("^=") +
		sym("|=");

	conditional_exp =
		V"logical_or_exp" * sym("?") * expect(V"exp", "invalid expression after '?'") * 
			expect(sym(":"), "expected ':'") * expect(V"conditional_exp", "invalid expression after ':'") +
		V"logical_or_exp";

	const_exp =
		V"conditional_exp";

	logical_or_exp =
		sep_by(V"logical_and_exp", sym("||"), "expected expression after \"||\" operator");
		
	logical_and_exp =
		sep_by(V"inclusive_or_exp", sym("&&"), "expected expression after \"&&\" operator");
		
	inclusive_or_exp =
		sep_by(V"exclusive_or_exp", token(P("|")*-P("|")), "expected expression after '|' operator");
	
	exclusive_or_exp =
		sep_by(V"and_exp", sym("^"), "expected expression after '^' operator");
	
	and_exp = 
		sep_by(V"equality_exp", token(P("&")*-P("&")), "expected expression after '&' operator");
	
	equality_exp = 
		sep_by(V"relational_exp", sym("==") + sym("!="), "expected expression after (in)equality operator");
	
	relational_exp = 
		sep_by(V"shift_exp", sym("<") + sym(">") + sym("<=") + sym(">="), "expected expression after relational operator");
	
	shift_exp =
		sep_by(V"additive_exp", sym("<<") + sym(">>"), "expected expression after shift operator");
	
	additive_exp =
		sep_by(V"multiplicative_exp", sym("+") + sym("-"), "expected expression after additive operator");
	
	multiplicative_exp =
		sep_by(V"cast_exp", sym("*") + sym("/") + sym("%"), "expected expression after multiplicative operator");

	cast_exp =
		( sym("(") * V"type_name" * expect(sym(")"), "expected ')'") ) * V"cast_exp" +
		V"unary_exp";
		
	unary_exp =
		sym("++") * expect(V"unary_exp", "invalid expression after \"++\" operator") +
		sym("--") * expect(V"unary_exp", "invalid expression after \"--\" operator") +
		V"unary_operator" * expect(V"cast_exp", "invalid expression after unary operator") +
		kw("sizeof") * expect(( V"type_name" + V"unary_exp" ), "invalid type_name/expression after \"sizeof\"") +
		V"postfix_exp";
	
	postfix_exp =
		V"primary_exp" * ( 
			sym("[") * expect(V"exp", "invalid expression") * expect(sym("]"), "expected ']'") +
			sym("(") * sep_by(V"assignment_exp", sym(","), "expected parameter expression")^-1 * expect(sym(")"), "expected ')'") +
			sym(".") * expect(V"id", "expected identifier") +
			sym("->") * expect(V"id", "expected identifier") +
			sym("++") +
			sym("--")
		 )^0;
	
	primary_exp = 
		V"id" +
		V"string" +
		V"constant" +
		sym("(") * V"exp" * expect(sym(")"), "expected ')'");
	
	constant = 
		V"int_const" +
		V"char_const" +
		V"float_const" +
		V"enumeration_const";

	unary_operator =
		sym("&") +
		sym("*") +
		sym("+") +
		sym("-") +
		sym("~") +
		sym("!");

	skip =
		(V"space" + V"comment")^0;
	
	space =
		lpeg.space^1;
	
	comment =
		P"/*" * ( P(1) - P"*/" )^0 * expect(P("*/"), "expected \"*/\"");
	
	int_const =
		token( ( ( P"0" * S"xX" * xdigit^1 ) + ( digit - P"0" ) * digit^0 + P"0" * S("01234567")^0 ) * 
			( ( P"U" + P"u" ) * ( P"l" + P"L" ) + ( P"l" + P"L" ) * ( P"U" + P"u" ) + P"l" + P"L" + P"u" + P"U" )^-1 );
	
	float_const = 
		token( P"0x" * (
			( ( P"." + xdigit^1 ) + ( xdigit^1 + P"." ) ) * ( S"eE" * S("+-")^-1 * xdigit^1 )^-1 * ( S"lLfF" )^-1 +
			xdigit^1 * ( S"eE" * S("+-")^-1 * xdigit^1 ) * ( S"lLfF" )^-1 
			) +
		( ( P"." + digit^1 ) + ( digit^1 + P"." ) ) * ( S"eE" * S("+-")^-1 * digit^1 )^-1 * ( S"lLfF" )^-1 +
		digit^1 * ( S"eE" * S("+-")^-1 * digit^1 ) * ( S"lLfF" )^-1 );
	
	char_const =
		token( P"'" * ( V"esc_char" + ( P(1) - S"'\n\\" ) ) * P"'" );
	
	string = 
		token( P'"' * ( V"esc_char" + ( P(1) - S'"\n\\' ) )^0 * P'"' );
	
	esc_char =
		P"\\" * expect((
			P"n" +
			P"t" +
			P"v" +
			P"b" +
			P"r" +
			P"f" +
			P"a" +
			P"\\" +
			P"?" +
			P"'" +
			P"\"" +
			S("01234567") * S("01234567")^-2 +
			P"x" * xdigit^1 ), 
			"invalid esc sequence");
	
	enumeration_const =
		V"id";

	id =
		token( ( alpha + P"_" ) * ( alnum + P"_" )^0 - ( V"keywords" * - ( alnum + "_" ) ) );
	
	keywords = 
		P"auto" + P"double" + P"int" + P"struct" + 
		P"break" + P"else" + P"long" + P"switch" + 
		P"case" + P"enum" + P"register" + P"typedef" + 
		P"char" + P"extern" + P"return" + P"union" + 
		P"const" + P"float" + P"short" + P"unsigned" + 
		P"continue" + P"for" + P"signed" + P"void" + 
		P"default" + P"goto" + P"sizeof" + P"volatile" + 
		P"do" + P"if" + P"static" + P"while";	
}

local parser = {}

function getpos(pos, subject)
	l = 1
	c = 1
	for i = 1, pos-1, 1
	do
		if subject:sub(i,i) == '\n' then
			l = l+1
			c = 1
		else
			c = c+1
		end
	end
	return tostring(l) .. ":" .. tostring(c)
end

function parser.parse (subject, filename)
	lpeg.setmaxstack(2000)
	local ret, label, pos = lpeg.match(G, subject)
	if not ret then
		print(filename .. ":" .. getpos(pos, subject) .. " " .. label)
		print(subject:sub(pos,pos))
		print("^")
	end
	return ret
end

for i = 1, 53, 1
do
	test = tostring(i);
	if test:len() == 1 then
		test = '0' .. test
	end
	filename = "tests/test" .. test .. ".txt"
	file = io.open(filename, "r")
	io.input(file)
	subject = io.read("*all")
	io.close(file)
	if parser.parse(subject, filename) then
		print("Ok")
	end
end

return parser
