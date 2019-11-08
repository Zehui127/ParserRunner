%{
/******************************************************************************
 * Copyright 2002, 2006, 2011, 2012, 2014, 2015, 2017                         *
 * John Carroll, O. Andersen, Guido Minnen, Erik Hektoen, Greg Grefenstette   *
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

   Flex sentence boundary finder for English text in plain ASCII (7-bit)
   or UTF-8 encodings.
 
   Example compilation/test command lines:
     $ flex ../enc/charclasses.flex sentence.flex
                                       - compiling the flex code
     $ gcc lex.yy.c -o sentence        - compiling the C code
     $ rm lex.yy.c                     - deleting the intermediate file
     $ ./sentence < sentence.txt       - testing the executable file

   All control characters are regarded as whitespace.  

   Interactive behaviour could be improved by removing remaining {sp}* and {sp}+
   patterns and adding flushing to {nl} and {np} rules.

*/

#include <unistd.h>    /* for getopt */

#define YY_INPUT(buf, result, max_size)   \
  do {                                    \
    int n, c;                             \
    for (n = 0; n < max_size &&           \
                (c = getchar()) != '\n'   \
                 && c != EOF; ++n)        \
      buf[n] = c;                         \
    if (c == '\n')                        \
      buf[n++] = c;                       \
    result = n;                           \
  } while (0)

int empty;
int xml_mode = 0;
int hard_wrap = 0;
%}

%option noyywrap

lcons      [bcdfghjklmnpqrstvwxz]

xmltext    "<text"([[:space:]][^<>]*)?">"
xmlendtext "</text"[[:space:]]*">"
xmlw       "<w"([[:space:]][^<>]*)?">"
xmlendw    "</w"[[:space:]]*">"

  /*entity     &[[:alnum:]]+;|&#[[:digit:]]+;|&#x[[:xdigit:]]+;*/

  /* Abbrevations - all ending with '.' */
  /* AV: Added the following abbreviations, common in the biomedical literature:"Mol."|"Cell."|"Chem."|"Biol."|"et al." */

abbrev     [[:alpha:]]"."([[:alpha:]]".")+|[[:upper:]]"."|{lcons}+"."|"Gov."|"MM."|"Mme."|"Mr."|"Ms."|"Mrs."|"Miss."|"Capt."|"Col."|"Dr."|"Drs."|"Rev."|"Prof."|"Sgt."|"Sr."|"St."|"Jr."|"jr."|"Co."|"Corp."|"Inc."|[Cc]"f."|[Ee]"g."|[Ee]"tc."|[Ee]"x."|[Ii]"e."|[Vv]"iz."|[Vv]"s."|[Jj]"an."|[Ff]"eb."|[Mm]"ar."|[Aa]"pr."|[Jj]"un."|[Jj]"ul."|[Aa]"ug."|[Ss]"ep"[t]?"."|[Oo]"ct."|[Nn]"ov."|[Dd]"ec."|[Ee]"d"[s]?"."|"repr."|"Rep."|"Dem."|"trans."|[Vv]"ol"[s]?"."|"p."|"pp."|"rev."|"est."|[Ff]"ig"[s]?"."|[Nn]"o"[s]?"."|[Rr]"ef"[s]?"."|[Ee]"q"[s]?"."|[Cc]"h"[s]?"."|[Ss]"ec"[s]?"."|"mi."|[Dd]"ept"[s]?"."|"Univ."|[Nn]"o"[s]?"."|"Mol."|"Cell."|"Chem."|"Biol."|"et al."

%s new_tok new_sent
%x xml end_sent end_sent_num

%%

  /* Hide any existing start/end sentence anchors */

<xml>"^"                               {printf("&raspcirc;");}
"^"                                    {yyleng=0; tok(); printf("&raspcirc;");}

  /* Unless inside XML tag Text we are out of sentence processing */

<*>{xmltext}          {ECHO; BEGIN(new_sent); empty = 1;}
{xmlendtext}                {if (!empty) printf("^^ "); ECHO; BEGIN(xml);}
<xml>.                                 {ECHO;}
<xml>\n                                {ECHO; fflush(yyout);}
{xmlw}                                 {tok();}
{xmlendw}                              {ECHO; BEGIN(new_tok);}

{lead}                                 {tok();}
<new_tok,new_sent>{abbrev}             {tok();}

  /* Sentence boundary at ending punct (not abbrev) + capital/digit, or new paragraph */

 /* ({endmarker}|"."){trail_end}*({sp}*{xmlendw}{sp}*|{sp}*)/({lead}|\')*{xmlw}?({upper}|[0-9])  {tok(); BEGIN(new_sent);} */

{bl}+"."{trail_end}*{ambiquote}?{trail_end}*  {tok(); BEGIN(new_sent);}
"."{trail_end}*{ambiquote}?{trail_end}*/{bl}  {tok(); BEGIN(new_sent);}

{num}({endmarker}|"."){trail_end}*{ambiquote}?{trail_end}*       {tok(); BEGIN(end_sent_num);}
{num}({endmarker}|"."){trail_end}*{ambiquote}?{trail_end}*{bl}+  {tok(); BEGIN(end_sent);}
({endmarker}|"."){trail_end}*{ambiquote}?{trail_end}*{bl}*  {tok(); BEGIN(end_sent);}
<end_sent_num>({lead}|\'|{ambiquote})*/({upper})  {BEGIN(new_sent); tok();}
<end_sent>({lead}|\'|{ambiquote})*/({upper}|{num})  {BEGIN(new_sent); tok();}
<end_sent,end_sent_num>.|"\n" {unput(yytext[0]); BEGIN(new_tok);}
 
 /*  Remaining issues:  double question mark (??)  */

				   /*{sp}"."{sp}*{trail_end}*{sp}+/({lead}|\')*{sp}*({alpha})  {tok(); BEGIN(new_sent);}*/
({nl}{bl}*{nl}|{np})                          {ECHO; BEGIN(new_sent);}
{nl}                          {ECHO; if (!hard_wrap) BEGIN(new_sent);}

  /* Skip whitespace etc, otherwise can put a start sentence anchor just before here */

<new_sent>{sp} {ECHO; BEGIN(new_sent);} /* added to allow white space at the beginning of file */
{sp}                                   {ECHO; BEGIN(new_tok);}
.                                      {tok(); BEGIN(INITIAL);}


%%

int tok()
{
 if (YYSTATE==new_sent) {printf("^ "); BEGIN(new_tok); empty = 0;}
 ECHO;
}

int main(int argc, char **argv) 
{
  int c;
  while ( (c = getopt(argc, argv, "hx")) != -1 ) {
    switch (c) {
      case 'h': hard_wrap = 1; break;
      case 'x': xml_mode = 1; break;
    }
  }
 if (xml_mode) {
   printf("^^ ");
   BEGIN(xml);
 } else
   BEGIN(new_sent);
 yylex();
}

