local lpeg = require "lpeglabel"

lpeg.locale(lpeg)

--lpeg functions
local P, S, V, T, Cp, Cc = lpeg.P, lpeg.S, lpeg.V, lpeg.T, lpeg.Cp, lpeg.Cc

local alpha, digit, xdigit, alnum = lpeg.alpha, lpeg.digit, lpeg.xdigit, lpeg.alnum

--Label if doesn't match patt
local function expect(patt, label)
	return patt + T(label)
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
	return token( P(str) * - ( alnum + "_" ) )
end

--Parse a list of patt separated by sep
local function sep_by(patt, sep, label)
	return patt * ( sep * expect(patt, label ) )^0
end

local labellist = {
	{"InvalidDecl", "invalid declaration"},
	{"Enumerator", "expected enumerator"},
	{"EnumeratorComma", "expected enumerator after ','"},
	{"Braces", "expected '}'"},
	{"ZeroDecl", "expected at least one struct/union declaration"},
	{"DeclAfterComma", "expected declarator after ','"},
	{"SqBrack", "expected ']'"},
	{"Brack", "expected ')'"},
	{"EndComment", "expected \"*/\""},
	{"Semicolon", "expected ';'"},
	{"Colon", "expected ':'"},
	{"ParamDecl", "expected parameter declaration after ','"},
	{"InvalidStmt", "invalid statement"},
	{"InvalidExpr", "invalid expression"},
	{"InvalidExprInc", "invalid expression after \"++\" operator"},
	{"InvalidExprDec", "invalid expression after \"--\" operator"},
	{"InvalidExprUnary", "invalid expression after unary operator"},
	{"InvalidExprCond1", "invalid expression after '?'"},
	{"InvalidExprCond2", "invalid expression after ':'"},
	{"Identifier", "expected identifier"},
	{"Stat", "expected statement"},
	{"StatCase", "expected statement after \"case\""},
	{"StatDefault", "expected statement after \"default\""},
	{"BrackIf", "expected '(' after \"if\""},
	{"BrackWhile", "expected '(' after \"while\""},
	{"BrackSwitch", "expected '(' after \"switch\""},
	{"BrackFor", "expected '(' after \"for\""},
	{"ExprComma", "expected expression after ',' operator"},
	{"ExprLogOr", "expected expression after \"||\" operator"},
	{"ExprLogAnd", "expected expression after \"&&\" operator"},
	{"ExprIncOr", "expected expression after '|' operator"},
	{"ExprExcOr", "expected expression after '^' operator"},
	{"ExprAnd", "expected expression after '&' operator"},
	{"ExprEq", "expected expression after (in)equality operator"},
	{"ExprRel", "expected expression after relational operator"},
	{"ExprShift", "expected expression after shift operator"},
	{"ExprAdd", "expected expression after additive operator"},
	{"ExprMult", "expected expression after multiplicative operator"},
	{"While", "expected \"while\""},
	{"InvalidSizeof", "invalid type_name/expression after \"sizeof\""},
	{"InvalidEscSeq", "invalid esc sequence"},
}

--Returns error info
local function get(str)
	for _, pair in ipairs(labellist) do
		if pair[1] == str then
			return pair[2]
		end
	end
end

--list of errors
errorlist = { }

--record error given position and label
local function recordOne(pos, label)
	table.insert(errorlist,{pos, label})
end

--captures position and label of an error, then record it
local function record(str)
	return (Cp() * Cc(get(str))) / recordOne
end

--Grammar
local G = { V"translation_unit", 
	translation_unit =
		V"skip" * expect(V"external_decl"^1, "InvalidDecl") * expect(-P(1), "InvalidDecl");
	
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
		kw("enum") * V"id"^-1 * sym("{") * expect(V"enumerator", "Enumerator") * 
			( sym(",") * expect(V"enumerator", "EnumeratorComma") )^0 * expect(sym("}"), "Braces") +
		kw("enum") * V"id" +
		V"struct_or_union" * V"id"^-1 * sym("{") * expect(V"struct_decl"^1, "ZeroDecl")
			 * expect(sym("}"), "Braces") +
		V"struct_or_union" * V"id";
	
	type_qualifier =
		kw("const") +
		kw("volatile");

	struct_or_union =
		kw("struct") +
		kw("union");

	init_declarator_list =
		V"init_declarator" * ( sym(",") * expect(V"init_declarator", "DeclAfterComma") )^0;

	init_declarator =
		V"declarator" * sym("=") * V"initializer" +
		V"declarator";

	struct_decl =
		V"spec_qualifier" * V"struct_declarator" * ( sym(",") * expect(V"struct_declarator", "DeclAfterComma") )^0 
			* expect(sym(";"), "Semicolon") + 
		V"spec_qualifier" * V"struct_decl";

	spec_qualifier_list =
		( V"type_spec" + V"type_qualifier" )^1;
	
	spec_qualifier =
		V"type_spec" + V"type_qualifier";

	struct_declarator =
		V"declarator"^-1 * sym(":") * expect(V"const_exp", "InvalidExpr") + 
		V"declarator";

	enumerator =
		V"id" * sym("=") * expect(V"const_exp", "InvalidExpr") +
		V"id";

	declarator =
		V"pointer"^-1 * V"direct_declarator";

	direct_declarator =
		( V"id" + sym("(") * V"declarator" * expect(sym(")"), "Brack") ) * 
			( sym("[") * V"const_exp"^-1 * expect(sym("]"), "SqBrack") +
			sym("(") * V"param_type_list" * expect(sym(")"), "Brack") +
			sym("(") * V"id_list"^-1 * expect(sym(")"), "Brack") )^0;

	pointer =
		( sym("*") * V"type_qualifier"^0 ) * V"pointer" +
		( sym("*") * V"type_qualifier"^0 );

	param_type_list =
		V"param_decl" * ( sym(",") * V"param_decl" )^0 * ( sym(",") * expect(sym("..."), "ParamDecl" ))^-1;

	param_decl =
		V"decl_spec"^1 * ( V"declarator" + V"abstract_declarator"^-1 );

	id_list =
		V"id" * ( sym(",") * expect(V"id", "Identifier") )^0;

	initializer =
		sym("{") * V"initializer" * (sym(",") * V"initializer")^0 * sym(",")^-1 * expect(sym("}"), "Braces") +
		V"assignment_exp";

	type_name =
		V"spec_qualifier_list" * V"abstract_declarator"^-1;

	abstract_declarator =
		V"pointer" +
		V"pointer"^-1 * V"direct_abstract_declarator";

	direct_abstract_declarator =
		sym("(") * V"abstract_declarator" * expect(sym(")"), "Brack") *
			( sym("[") * V"const_exp"^-1 * expect(sym("]"), "SqBrack") + 
			sym("(") * V"param_type_list"^-1 * expect(sym(")"), "Brack") )^0;

	typedef_name =
		V"id";

	stat =
		V"id" * sym(":") * V"stat" +
		kw("case") * expect(V"const_exp", "InvalidExpr") * expect(sym(":"), "Colon") * 
			expect(V"stat", "StatCase") +
		kw("default") * expect(sym(":"), "Colon") * expect(V"stat", "StatDefault") +
		V"exp"^-1 * sym(";") +
		V"compound_stat" +
		kw("if") * expect(sym("("), "BrackIf") * expect(V"exp", "InvalidExpr") * 
			expect(sym(")"), "Brack") * expect(V"stat", "Stat") * kw("else") * expect(V"stat", "Stat") +
		kw("if") * expect(sym("("), "BrackIf") * expect(V"exp", "InvalidExpr") * 
			expect(sym(")"), "Brack") * expect(V"stat", "Stat") +
		kw("switch") * expect(sym("("), "BrackSwitch") * expect(V"exp", "InvalidExpr") * 
			expect(sym(")"), "Brack") * expect(V"stat", "Stat") +
		kw("while") * expect(sym("("), "BrackWhile") * expect(V"exp", "InvalidExpr") * 
			expect(sym(")"), "Brack") * expect(V"stat", "Stat") +
		kw("do") * expect(V"stat", "Stat") * expect(kw("while"), "While") * 
			expect(sym("("), "BrackWhile") * expect(V"exp", "InvalidExpr") * 
			expect(sym(")"), "Brack") * expect(sym(";"), "Semicolon") +
		kw("for") * 
			expect(sym("("), "BrackFor") * V"exp"^-1 * 
			expect(sym(";"), "Semicolon") * V"exp"^-1 * 
			expect(sym(";"), "Semicolon") * V"exp"^-1 * 
			expect(sym(")"), "Brack") * V"stat" +
		kw("goto") * expect(V"id", "Identifier") * expect(sym(";"), "Semicolon") +
		kw("continue") * expect(sym(";"), "Semicolon") +
		kw("break") * expect(sym(";"), "Semicolon") +
		kw("return") * V"exp"^-1 * expect(sym(";"), "Semicolon");

	compound_stat =
		sym("{") * V"decl"^0 * V"stat"^0 * expect(sym("}"), "Braces");

	exp =
		sep_by(V"assignment_exp", sym(","), "ExprComma");

	assignment_exp =
		V"unary_exp" * V"assignment_operator" * expect(V"assignment_exp", "InvalidExpr") +
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
		V"logical_or_exp" * sym("?") * expect(V"exp", "InvalidExprCond1") * 
			expect(sym(":"), "Colon") * expect(V"conditional_exp", "InvalidExprCond2") +
		V"logical_or_exp";

	const_exp =
		V"conditional_exp";

	logical_or_exp =
		sep_by(V"logical_and_exp", sym("||"), "ExprLogOr");
		
	logical_and_exp =
		sep_by(V"inclusive_or_exp", sym("&&"), "ExprLogAnd");
		
	inclusive_or_exp =
		sep_by(V"exclusive_or_exp", token(P("|")*-P("|")), "ExprIncOr");
	
	exclusive_or_exp =
		sep_by(V"and_exp", sym("^"), "ExprExcOr");
	
	and_exp = 
		sep_by(V"equality_exp", token(P("&")*-P("&")), "ExprAnd");
	
	equality_exp = 
		sep_by(V"relational_exp", sym("==") + sym("!="), "ExprEq");
	
	relational_exp = 
		sep_by(V"shift_exp", sym("<") + sym(">") + sym("<=") + sym(">="), "ExprRel");
	
	shift_exp =
		sep_by(V"additive_exp", sym("<<") + sym(">>"), "ExprShift");
	
	additive_exp =
		sep_by(V"multiplicative_exp", sym("+") + sym("-"), "ExprAdd");
	
	multiplicative_exp =
		sep_by(V"cast_exp", sym("*") + sym("/") + sym("%"), "ExprMult");

	cast_exp =
		( sym("(") * V"type_name" * expect(sym(")"), "Brack") ) * V"cast_exp" +
		V"unary_exp";
		
	unary_exp =
		sym("++") * expect(V"unary_exp", "InvalidExprInc") +
		sym("--") * expect(V"unary_exp", "InvalidExprDec") +
		V"unary_operator" * expect(V"cast_exp", "InvalidExprUnary") +
		kw("sizeof") * expect(( V"type_name" + V"unary_exp" ), "InvalidSizeof") +
		V"postfix_exp";
	
	postfix_exp =
		V"primary_exp" * ( 
			sym("[") * expect(V"exp", "InvalidExpr") * expect(sym("]"), "SqBrack") +
			sym("(") * sep_by(V"assignment_exp", sym(","), "InvalidExpr")^-1 * expect(sym(")"), "Brack") +
			sym(".") * expect(V"id", "Identifier") +
			sym("->") * expect(V"id", "Identifier") +
			sym("++") +
			sym("--")
		 )^0;
	
	primary_exp = 
		V"id" +
		V"string" +
		V"constant" +
		sym("(") * V"exp" * expect(sym(")"), "Brack");
	
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
		P"/*" * ( P(1) - P"*/" )^0 * expect(P("*/"), "EndComment");
	
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
			"InvalidEscSeq");
	
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
	
	Token =
		token(V"keywords") + 
		V"id" + 
		V"string" +
		V"constant" + 
		token(P(1));
	
	InvalidDecl =
		record"InvalidDecl" * V"Token"^0;
	
	Enumerator =
		record"Enumerator" * (V"Token"-sym("}"))^0;
	
	EnumeratorComma =
		record"EnumeratorComma" * (V"Token"-sym("}"))^0;
	
	Braces =
		record"Braces";
	
	ZeroDecl =
		record"ZeroDecl" * (V"Token"-sym("}"))^0;
	
	DeclAfterComma =
		record"DeclAfterComma" * (V"Token"-sym(")"))^0;
	
	SqBrack =
		record"SqBrack";
	
	Brack =
		record"Brack";
	
	EndComment =
		record"EndComment";
	
	Semicolon =
		record"Semicolon";
	
	Colon =
		record"Colon";
	
	ParamDecl =
		record"ParamDecl" * (V"Token"-sym(")"))^0;
	
	InvalidExpr =
		record"InvalidExpr" * (V"Token"-(sym(";") + sym("{") + sym("}") + sym(",") + sym(":")))^0;
	
	InvalidExprInc =
		record"InvalidExprInc" * (V"Token"-(sym(";") + sym("{") + sym("}") + sym(",") + sym(":")))^0;
	
	InvalidExprDec =
		record"InvalidExprDec" * (V"Token"-(sym(";") + sym("{") + sym("}") + sym(",") + sym(":")))^0;
	
	InvalidExprUnary =
		record"InvalidExprUnary" * (V"Token"-(sym(";") + sym("{") + sym("}") + sym(",") + sym(":")))^0;
	
	InvalidExprCond1 =
		record"InvalidExprCond1";
	
	InvalidExprCond2 =
		record"InvalidExprCond2";
	
	Identifier =
		record"Identifier";
	
	Stat =
		record"Stat";
	
	StatCase =
		record"StatCase";
	
	StatDefault =
		record"StatDefault";
	
	BrackIf =
		record"BrackIf";
	
	BrackWhile =
		record"BrackWhile";
	
	BrackSwitch =
		record"BrackSwitch";
	
	BrackFor =
		record"BrackFor";
	
	ExprComma =
		record"ExprComma";
	
	ExprLogOr =
		record"ExprLogOr";
	
	ExprLogAnd =
		record"ExprLogAnd";
	
	ExprIncOr =
		record"ExprIncOr";
	
	ExprExcOr =
		record"ExprExcOr";
	
	ExprAnd =
		record"ExprAnd";
	
	ExprEq =
		record"ExprEq";
	
	ExprRel =
		record"ExprRel";
	
	ExprShift =
		record"ExprShift";
	
	ExprAdd =
		record"ExprAdd";
	
	ExprMult =
		record"ExprMult";
	
	While =
		record"While";
	
	InvalidSizeof =
		record"InvalidSizeof";
	
	InvalidEscSeq =
		record"InvalidEscSeq" * P(1);
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
	return l, c
end

function parser.parse (subject, filename)
	--clear errorlist
	for i in pairs(errorlist) do
		errorlist[i] = nil
	end

	lpeg.setmaxstack(2000)
	local ret, a = lpeg.match(G, subject)
	assert(ret and not a)
	
	lastLine = 0
	for errNo, pair in ipairs(errorlist) do
		pos, label = pair[1], pair[2]
		l, c = getpos(pos,subject)
		if l ~= lastLine then --Avoid error cascade
			print(filename .. ":" .. tostring(l) .. ":" .. tostring(c) .. " " .. label)
			print(subject:sub(pos,pos))
			print("^")
			lastLine = l
		end
	end
	if lastLine == 0 then
		print("OK")
	end
	return ret
end

for i = 1, 53, 1 do
	test = tostring(i);
	if test:len() == 1 then
		test = '0' .. test
	end
	filename = "tests/test" .. test .. ".txt"
	file = io.open(filename, "r")
	io.input(file)
	subject = io.read("*all")
	io.close(file)
	parser.parse(subject, filename)
end

--[[filename = "a"
subject = "enum a{};\nint main(){"
parser.parse(subject, filename)]]

return parser
