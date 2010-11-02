%{
	import java.io.*;
%}

/* tiny C parser */
%token <ival> NUMBER
%token <obj> SYMBOL
%token <sval> STRING
%token VAR
%token IF
%token ELSE
%token RETURN
%token WHILE
%token FOR
%token PRINTLN

%right '='
%left '<' '>'
%left '+' '-'
%left '*'

%type <obj> program external_definitions external_definition 
%type <obj> parameter_list block local_vars symbol_list 
%type <obj> statements statement expr primary_expr arg_list
%type <obj> SYMBOL
%type <ival> NUMBER
%type <sval> STRING
%start program

%%

program: /* empty */ { $$ = program((scala.List)null); }
	| external_definitions { $$ = program((scala.List)reverse((scala.List)$1)); }
	;

external_definitions:
	  external_definition { $$ = list((ExternalDefinition)$1); }
	| external_definitions external_definition { $$ = addList((ExternalDefinition)$2, (scala.List)$1); }
	;

external_definition:
	  SYMBOL parameter_list block  /* fucntion definition */
	{ $$ = new DefineFunction(getSymbol((AST)$1),(AST)$2,(AST)$3); }
	| VAR SYMBOL ';'
	{ $$ = new DeclareVariable(getSymbol((AST)$2),null); }
	| VAR SYMBOL '=' expr ';'
        { $$ = new DeclareVariable(getSymbol((AST)$2),(AST)$4); }
	| VAR SYMBOL '[' expr ']' ';'
	{ $$ = new DeclareArray(getSymbol((AST)$2),(AST)$4); }
	;

parameter_list:
	 '(' ')'
	 { $$ = null; }
	| '(' symbol_list ')' 
	 { $$ = $2; }
	;

block: '{' local_vars statements '}'
	{ $$ = new BLOCK_STATEMENT((AST)$2,(AST)$3); }
	;

local_vars: 
	  /* NULL */ { $$ = null; }
	| VAR symbol_list ';'
	  { $$ = $2; }
	;

symbol_list: 
	  SYMBOL
	 { $$ = makeList1((AST)$1); }
	| symbol_list ',' SYMBOL
	 { $$ = addLast((AST)$1,(AST)$3); }
	;

statements:
	  statement
	 { $$ = makeList1((AST)$1); }
	| statements statement
	 { $$ = addLast((AST)$1,(AST)$2); }
	;

statement:
	 expr ';'
	 { $$ = $1; }
	| block
	 { $$ = $1; }
	| IF '(' expr ')' statement
	 { $$ = new IF_STATEMENT((AST)$3,makeList2((AST)$5,null)); }
        | IF '(' expr ')' statement ELSE statement
	 { $$ = new IF_STATEMENT((AST)$3,makeList2((AST)$5,(AST)$7)); }
	| RETURN expr ';'
	 { $$ = new RETURN_STATEMENT((AST)$2); }
	| RETURN ';'
	 { $$ = new RETURN_STATEMENT(null); }
	| WHILE '(' expr ')' statement
	 { $$ = new WHILE_STATEMENT((AST)$3,(AST)$5); }
	| FOR '(' expr ';' expr ';' expr ')' statement
	 { $$ = new FOR_STATEMENT(makeList3((AST)$3,(AST)$5,(AST)$7),(AST)$9); }
	;

expr: 	 primary_expr
	| SYMBOL '=' expr
	 { $$ = new EQ_OP((AST)$1,(AST)$3); }
	| SYMBOL '[' expr ']' '=' expr
	 { $$ = new SET_ARRAY_OP(makeList2((AST)$1,(AST)$3),(AST)$6); }
	| expr '+' expr
	 { $$ = new PLUS_OP((AST)$1,(AST)$3); }
	| expr '-' expr
	 { $$ = new MINUS_OP((AST)$1,(AST)$3); }
	| expr '*' expr
	 { $$ = new MUL_OP((AST)$1,(AST)$3); }
	| expr '<' expr
	 { $$ = new LT_OP((AST)$1,(AST)$3); }
	| expr '>' expr
	 { $$ = new GT_OP((AST)$1,(AST)$3); }
	;

primary_expr:
	  SYMBOL
	| NUMBER { $$ = new NUM($1); }
	| STRING { $$ = new STR($1); }
	| SYMBOL '[' expr ']'
	  { $$ = new GET_ARRAY_OP((AST)$1,(AST)$3); }
	| SYMBOL '(' arg_list ')'
	 { $$ = new CALL_OP((AST)$1,(AST)$3); }
	| SYMBOL '(' ')'
	 { $$ = new CALL_OP((AST)$1, null); }
        | '(' expr ')'
         { $$ = $2; }  
	| PRINTLN  '(' arg_list ')'
	 { $$ = new PRINTLN_OP((AST)$3); }
	;

arg_list:
	 expr
	 { $$ = makeList1((AST)$1); }
	| arg_list ',' expr
	 { $$ = addLast((AST)$1,(AST)$3); }
	;

%%
  public CLex lexer;

  public int yylex () {
    int yyl_return = -1;
    try {
      yyl_return = lexer.yylex();
    }
    catch (IOException e) {
      System.err.println("IO error :"+e);
    }
    return yyl_return;
  }

  public void yyerror (String error) {
    System.err.println ("Error: " + error);
  }

  public Parser(Reader r) {
    lexer = new CLex(r, this);
  }

  public static void main(String args[]) throws IOException {
    Parser yyparser = new Parser(new FileReader(args[0]));
    System.out.println(yyparser.yyparse());
  }
