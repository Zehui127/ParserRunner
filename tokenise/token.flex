%{
/******************************************************************************
 * Copyright 2002, 2006, 2011, 2012, 2014, 2017, 2018 John Carroll,           *
 *                                                    Oeistein Andersen       *
 *                                                                            *
 * This file is part of RASP.                                                 *
 *                                                                            *
 * RASP is free software: you can redistribute it and/or modify it            *
 * under the terms of the GNU Lesser General Public License as published      *
 * by the Free Software Foundation, either version 3 of the License, or       *
 * (at your option) any later version.                                        *
 *                                                                            *
 * RASP is distributed in the hope that it will be useful,                    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *
 * GNU Lesser General Public License for more details.                        *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public License   *
 * along with RASP.  If not, see <http://www.gnu.org/licenses/>.              *
 ******************************************************************************

   Flex tokeniser for English text in plain ASCII (7-bit) or UTF-8 encodings.
   Sentence boundaries are assumed to have been marked already, by '^ '.
 
   (c) John Carroll, University of Sussex, 2001-3

   Example compilation/test command lines:
     $ flex ../enc/charclasses.flex token.flex
                                  - compiling the flex code
     $ gcc lex.yy.c -o token      - compiling the C code
     $ rm lex.yy.c                - deleting the intermediate file
     $ ./token < token.test       - testing the executable file

   Flushes the output stream after every token so should work fine when
   used interactively via unix pipes.

   The command line option -w controls whether input file character
   start and end positions are output for each token, e.g.

   ^ <w s='2' e='4'>How</w> <w s='6' e='7'>is</w>

*/

  /* On eof explicitly insert a distinguished token into the input stream so that
     a '.' ending the last sentence can be detected
  */
#include <string.h>
#define YY_INPUT(buf, result, max_size)   \
  do {                                    \
    int n, c;                             \
    for (n = 0; n < max_size + 7 &&       \
               (c = getchar()) != '\n'    \
                && c != EOF; ++n)         \
      buf[n] = c;                         \
    if (c == '\n')                        \
      buf[n++] = c;                       \
    if (c == EOF) {                       \
      strcpy(&buf[n], "$$EOF$$");         \
      n += 7;                             \
    }                                     \
    result = n;                           \
  } while (0)

#define maxbuf 1000 
static char buf[maxbuf];
int bufleng; /* length in bytes of current token in buffer */
static char wsbuf[maxbuf];
int wsleng;

int printedspace = 1;

int apostrophe = 0;

int spans = 0;
int nchars = 0;

int start = 0;

/*#define YY_USER_ACTION  printf("<%i>", YY_START);*/

%}


%option noyywrap

contract   (n{apostrophe}t|N{apostrophe}T|{apostrophe}(m|d|s|M|D|S|re|ll|ve|RE|LL|VE))

 /*  grep "' " ../tag/auxiliary_files/seclarge.lex | cut -f1,1 -d" " | sed "s/'\$//"| tr '\n' '|' */
 /*contract2 ({apostrophe}n|An|D|Exterminatin|L|Sant|Sportin|a-readin|an|bushwhackin|checkin|comin|countin|d|dam|dell|dependin|drawin|driftin|dry-gulchin|expressin|fightin|floppin|gettin|goin|han|hankerin|herrin|holdin|killin|kin|knowin|larkin|lettin|livin|lovin|m|nothin|o|pleasin|rubbin|runnin|s{apostrophe}posin|sayin|seein|sho|shootin|singin|smallholders|smilin|somethin|sportin|stealin|swingin|t|takin|talkin|tellin|tootin|travellin|walkin|wantin|whinin|workin|wrappin|y){apostrophe}*/
contract2 ({apostrophe}n|Exterminatin|Sant|Sportin|a-readin|bushwhackin|checkin|comin|countin|dam|dell|dependin|drawin|driftin|dry-gulchin|expressin|fightin|floppin|gettin|goin|han|hankerin|herrin|holdin|killin|kin|knowin|larkin|lettin|livin|lovin|nothin|pleasin|rubbin|runnin|s{apostrophe}posin|sayin|seein|sho|shootin|singin|smallholders|smilin|somethin|sportin|stealin|swingin|takin|talkin|tellin|tootin|travellin|walkin|wantin|whinin|workin|wrappin){apostrophe}

/* {alpha}{1,2}'  removed from contract2 to avoid problems with dangerous trailing context.  Should be fixed properly somehow. */

 /* contract3 for non-curly ASCII apostrophe only, since no ambiguity occurs when curly single quotes or old-style ASCII `...' is used. */
contract3 "'"(cello|Cello|celli|Celli|cos|Cos|hood|Hood|neath|Neath|til|Til|tween|Tween|twixt|Twixt)
 /* CALD also gives 'knock */
 /* 'phone, 'bus, 'plane, 'blog and others could be added, but this would not necessarily give better tokenisation for contemporary English */
 /* Archaic contractions 'tis, 'twas, 'twill &c. cannot simply be added to this list, as they need to be split, and 't handled as a pronoun later on */


END          {sp}|<|{ellipsis}|{dash}|"$$EOF$$"|"^"

xmlw       "<w"([[:space:]][^<>]*)?">"
xmlendw    "</w"[[:space:]]*">"
entity     &[[:alnum:]]+;|&#[[:digit:]]+;|&#x[[:xdigit:]]+;

%s new_token
%x out out_text trail trail_new_token

%%

  /* Go in/out of or terminate tokenisation */

<out,out_text>"^ "                                {etok(""); ECHO;/*nchars=0;*/}
<*>"^ "                                {etok("");  printf("\n"); ECHO; /*nchars=2;*/}
<*>"^^ "                                {etok(""); ECHO; BEGIN(out);}
<out>"$$EOF$$"                           {etok(""); yyterminate();}
<*>"$$EOF$$"                           {etok(""); printf("\n"); yyterminate();}
<out>"\n"	                  {ECHO; fflush(yyout);}
<out>"<text"([[:space:]][^<>]*)?">" {ECHO; BEGIN(out_text); nchars=0; wsleng=0;}
<out_text>{sp} {ws();}
  /* Assume only ws before '^ '; 
     return to <out> upon seeing, e.g., </text> closing an empty <text> elt. */
<out_text>.                       {ECHO; BEGIN(out);}

  /* '.': end of abbreviation at eos / eos / not eos */
  /* ?? Perhaps {END} should be used here ?? */
"."/({trail_end}|{ambiquote})*({sp}|{xmlendw})*("^"|"$$EOF$$")   { etok(" "); tok(); etok("");}
  /*"."                                    {tok();}*/ /* handled by {ns} */

  /* XML w tags delimit tokens - expect only a single level of embedding */

{xmlw}[^<>]*{xmlendw}                  {etok(" "); tok(); etok(" ");}

  /* Leading punctuation: insert blank after */

<new_token>{lead}|{ambiquote}	 {tok(); etok(" ");}

  /* Rule added for cases where the leading punctuation is not separated from the previous token, i.e.: "word1(word2 word3)"
     {lead}                      {etok(" "); tok(); etok(" ");}*/
{alphanum}{leftbracket}{alphanum}*{rightbracket} {tok(); etok(" ");}
{alphanum}/{leftbracket}{alphanum} {tok(); etok(" ");}

  /* Trailing punctuation: insert blank before, unless ; at end of entity */

&raspcirc; {tok(); nchars -= 9; /* kept outside countcharsutf8() for efficiency */}
{entity}                               {tok();}
<INITIAL>({trail}|{apostrophe})({trail}|{apostrophe}|".")*{END} {yyless(0);  BEGIN(trail);}
<new_token>({trail}|{apostrophe}|".")+{END} {yyless(0); BEGIN(trail_new_token);}
<trail,trail_new_token>\' {if(!apostrophe){yytext="&raspsquo;"; yyleng=10; etok(); tok(); nchars -= 9; }else{apostrophe=0; etok(""); tok();} etok(); BEGIN(trail_new_token);}
<trail,trail_new_token>\xE2\x80\x99 {if(!apostrophe){yytext="&rasprsquo;"; yyleng=11; etok(); tok(); nchars -= 10;}else{apostrophe=0; etok(""); tok();} etok(); BEGIN(trail_new_token);}
<trail,trail_new_token>{trail}|"."|\'\' { etok(" "); tok(); etok(""); BEGIN(trail_new_token);}
<trail,trail_new_token>({ellipsis}|{dash})  {etok(" "); tok(); etok(" "); BEGIN(trail_new_token);}
<trail_new_token>(.|\n) { yyless(0); BEGIN(new_token);} /* . does not match \n and EOF */

  /* Infixed punctuation */

{dash}|{ellipsis}     {tok(); etok(" ");}
{ns}/{dash}|{ellipsis}    {tok(); etok(" ");}

  /* Contractions, possessives */

{alpha}/{contract}+({trail}|".")*{END}           {tok(); etok(" ");}
{contract}/{contract}*({trail}|".")*{END}            {tok(); etok(" ");}
<new_token>{contract2}/({trail}|".")*{END}            {tok(); etok(" ");}
[sxSX]/{apostrophe}({trail}|\'|".")*{END}          {tok(); etok("$"); apostrophe=1;}

 /* Rules to handle ASCII ' as opening quotation mark */
<new_token>{contract3}/({trail}|".")*{END}            {tok(); etok(" ");}
<new_token>\'                                  { yytext="&raspsquo;"; yyleng=10; tok(); nchars -= 9; etok(""); }

  /* Inside/outside a token */

{ns}/{sp}                              {tok(); etok("");}
{ns}                                   {tok();}
{sp}                                   {etok(""); /*ECHO;*/ ws();}

%%

int tok()
{
 if ((YYSTATE==new_token || YYSTATE==trail_new_token) && spans) {start=nchars;}
 if (spans)
 {
   if (bufleng+yyleng > maxbuf)
   {
     fprintf(stderr, "Token buffer overflow at '%s'\n", buf); exit(1);
   }
   strncpy(buf+bufleng, yytext, yyleng);
   bufleng += yyleng;
 }
 else
 /*{printf("(");*/ ECHO; /*printf("|%i)", YY_START);}*/
 nchars+=countcharsutf8(yytext,yyleng);

 BEGIN(INITIAL);
 printedspace = 0;
}

int etok(char *str) /* argument not used any more */
{
 if ((YYSTATE==INITIAL || YYSTATE==trail) && spans)
 {
     int i;
     printf("<w i=\"%d-%d\"", start, nchars);
     if (wsleng) {
       printf(" ws=\"");
       for (i = 0; i < wsleng; i++) {
	 if (wsbuf[i] == '\t')
	   printf("&#x9;");
	 else if (wsbuf[i] == '\n')
	   printf("&#xA;");
	 else if (!(wsbuf[i] & 0340)) /* C0 control codes, incl. \f, \r, \v*/
	   printf("\\%03o", wsbuf[i]);
	 else
	   putchar(wsbuf[i]);
       }
       putchar('"');
       wsleng = 0;
     }
     printf(" word=\"");
     for (i = 0; i < bufleng; i++) {
       if (buf[i] == '"')
	 printf("&quot;");
       else if (buf[i] == '&') {
	 if (!strncmp(buf+i, "&raspsquo;", 10)) {
	   putchar('\'');
	   i += 9;
	 } else if (!strncmp(buf+i, "&rasprsquo;", 11)) {
	   printf("’");
	   i += 10;
	 } else
	   printf("&amp;");
       } else if (buf[i] == '<')
	 printf("&lt;");
       else if (buf[i] == '>')
	 printf("&gt;"); /* Not technically required, but the parser gets confused */
       else
	 putchar(buf[i]);
     }
     printf("\">%.*s</w>", bufleng, buf);
   bufleng = 0;
 }
 if (!printedspace) {
   printf(/*str*/ " "); /*printf("[%i]", YY_START);*/
   printedspace = 1;
 }
 fflush(stdout);
 BEGIN(new_token);
}

int main(int argc, char **argv) 
{
 if (argc==2 && strcmp(argv[1], "-w")==0) {spans=1;}

 bufleng = 0;
 BEGIN(out_text);
 yylex();
 /*printf("\n");*/
 /* Test char counting */
 /*int i;
 char str[2]="x";
 for (i=0; i<256; i++){
   str[0] = i;
   printf("%i ", countcharsutf8(str, 1));
   }*/
}


int countcharsutf8(char *text, int len)
#define B7 0x80 /* most significant bit, binary 1000,0000 */
#define B6 0x40                       /* binary 0100,0000 */
{
  int i, c = 0; 
  for (i = 0; i < len; i++) 
    if (!(text[i] & B7) || text[i] & B6)
      c++;
  return c;
}

int ws ()
{
  if (spans) {
    if (wsleng+yyleng > maxbuf) { /* yyleng >= countcharsutf8(yytext,yyleng) */
      fprintf(stderr, "Whitespace buffer overflow\n"); exit(1);
    }
    strncpy(wsbuf+wsleng, yytext, yyleng);
    wsleng += yyleng;
  }
  nchars+=countcharsutf8(yytext,yyleng);
}
