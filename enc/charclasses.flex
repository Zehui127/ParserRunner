                /* LETTERS */

alpha   {latin0}|{latin1}|{latinA}|{latinB}|{latinX}|{latinC}|{latinL}

/* Basic Latin (ASCII) letters */
latin0  [A-Za-z]

/* Latin-1 Supplement letters (not a and o superscript) */
latin1  \xC3[\x80-\x96\x98-\xB6\xB8-\xBF]

/* Latin Extended-A  (U+100--U+17F) */
latinA  [\xC4\xC5][\x80-\xBF]

/* Latin Extended-B  (U+180--U+24F) */
latinB  [\xC6-\xC8][\x80-\xBF]|\xC9[\x80-\x8F]

/* Latin Extended Additional  (U+1E00--U+1EFF) */
latinX  \xE1[\xB8-\xBB][\x80-\xBF]

/* Latin Extended-C  (U+2C60--U+2C7F) */
latinC  \xE2\xB1[\xA0-\xBF]

/* Alphabetic Presentation Forms latin ligatures */
latinL  \xEF\xAC[\x80-\x86]

                /* DIGITS */

num     [0-9]
alphanum {alpha}|{num}


                /* UPPERCASE LETTERS */

upper   {upper0}|{upper1}|{upperA}|{upperB}|{upperX}|{upperC}

/* Basic Latin (ASCII)  uppercase letters */
upper0 [A-Z]

/* Latin-1 Supplement  uppercase letters */
upper1 \xC3[\x80-\x96\x98-\x9E]

/* Latin Extended-A  (U+100--U+17F)  uppercase letters */
upperA \xC4[\x80\x82\x84\x86\x88\x8A\x8C\x8E\x90\x92\x94\x96\x98\x9A\x9C\x9E\xA0\xA2\xA4\xA6\xA8\xAA\xAC\xAE\xB0\xB2\xB4\xB6\xB9\xBB\xBD\xBF]|\xC5[\x81\x83\x85\x87\x8A\x8C\x8E\x90\x92\x94\x96\x98\x9A\x9C\x9E\xA0\xA2\xA4\xA6\xA8\xAA\xAC\xAE\xB0\xB2\xB4\xB6\xB8\xB9\xBB\xBD]

/* Latin Extended-B  (U+180--U+24F)  uppercase letters */
upperB \xC6[\x81\x82\x84\x86\x87\x89-\x8B\x8E-\x91\x93\x94\x96-\x98\x9C\x9D\x9F\xA0\xA2\xA4\xA7\xA9\xAC\xAE\xAF\xB1\xB2\xB3\xB5\xB7\xB8\xBC]|\xC7[\x84\x85\x87\x88\x8A\x8B\x8D\x8Fx91\x93\x95\x97\x99\x9B\x9E\xA0\xA2\xA4\xA6\xA8\xAA\xAC\xAE\xB1\xB2\xB4\xB6\xB7\xB8\xBA\xBC\xBE]|\xC8[\x80\x82\x84\x86\x88\x8A\x8C\x8E\x90\x92\x94\x96\x98\x9A\x9C\x9E\xA0\xA2\xA4\xA6\xA8\xAA\xAC\xAE\xB0\xB2\xBA\xBB\xBD\xBE]|\xC9[\x81\x83\x84\x85\x86\x88\x8A\x8C\x8E]

/* Latin Extended Additional  (U+1E00--U+1EFF)  uppercase letters */
upperX  \xE1[\xB8\xB9][\x80\x82\x84\x86\x88\x8A\x8C\x8E\x90\x92\x94\x96\x98\x9A\x9C\x9E\xA0\xA2\xA4\xA6\xA8\xAA\xAC\xAE\xB0\xB2\xB4\xB6\xB8\xBA\xBC\xBE]|\xE1\xBA[\x80\x82\x84\x86\x88\x8A\x8C\x8E\x90\x92\x94\xA0\xA2\xA4\xA6\xA8\xAA\xAC\xAE\xB0\xB2\xB4\xB6\xB8\xBA\xBC\xBE]|\xE1\xBB[\x80\x82\x84\x86\x88\x8A\x8C\x8E\x90\x92\x94\x96\x98\x9A\x9C\x9E\xA0\xA2\xA4\xA6\xA8\xAA\xAC\xAE\xB0\xB2\xB4\xB6\xB8]
 
/* Latin Extended-C  (U+2C60--U+2C7F)  uppercase letters */
upperC  \xE2\xB1[\xA0\xA2\xA3\xA4\xA7\xA9\xAB\xB5]


		/*  WHITESPACE */

/*  24 characters + C0 controls: */

sp  {bl}|{nl}|{np}

/*  New line \n \r \r\n U+85 U+2028 */
nl  [\n\r]|\r\n|\xC2\x85|\xE2\x80\xA8

/*  New paragraph: \f \v U+2029 */
np  [\f\v]|\xE2\x80\xA9

/* Space/blank characters:
   space tab C0 U+A0 U+2000--U+200A U+202F U+205F U+3000 */
bl  [\t \0-\x08\x0E-\x1F]|\xC2\xA0|\xE2\x80[\x80-\x8A\xAF]|\xE2\x81\x9F|\xE3\x80\x80


		/* NON-WHITESPACE */

/* Everything not included in {sp}, including C1 control characters,
   zero-width spaces, soft hyphens and invalid UTF-8 sequences. */

/* Matches ~1,114,088 UTF-8 characters: */
ns  ([^ \t\n\r\f\v\0-\x08\x0E-\x1F\xC2\xE2\xE3]|\xC2[^\x85\xA0]|\xE2[^\x80\x81]|\xE2\x80[^\x80-\x8A\xA8\xA9\xAF]|\xE2\x81[^\x9F]|\xE3[^\x80]|\xE3\x80[^\x80])[\x80-\xBF]*


		/* PUNCTUATION */

/* Apostrophe: ' U+2019 */
apostrophe  \'|&apos;|\xE2\x80\x99

/* Opening quotation marks:
   " ` `` U+AB U+2018 U+201A U+201B U+201C U+201E U+201F U+2039 */
leftquote  `|``|\xC2\xAB|\xE2\x80[\x98\x9A-\x9C\x9E\x9F\xB9]

/* Closing quotation marks: apostrophe " '' U+BB U+201D U+203A */
rightquote {apostrophe}|&raspsquo;|\xC2\xBB|\xE2\x80[\x9D\xBA]|&rasprsquo;
/* Note: '' needs special treatment due to a limitation in flex. */

/* Undifferenciated quotation marks: " U+A8 U+02DD */
ambiquote \"|&quot;|\xC2\xA8|\xCB\x9D

/* Opening brackets */
leftbracket  [(\[{]

/* Closing brackets */
rightbracket  [)\]}]

/* Hyphens: - U+2010 U+2011 */
hyphen  -|\xE2\x80[\x90\x91]

/* Dashes (excluding hyphens): U+2012--U+2015, surrogates */
/* Note: the unambiguous hyphens U+2010--U+2011 never form a dash. */
dash  {realdash}|({realdash}|-){2,}
realdash  \xE2\x80[\x92-\x95]

/* Ellipses: U+2026, surrogates */
ellipsis \.*{realellipsis}+\.*|({realellipsis}|\.){3,}
realellipsis  \xE2\x80\xA6

/* Currency symbols from ASCII, Latin-1 Supplement, U+20A1-U+20CF */
currency $|\xC2[\xA2-\xA5]|\xE2\x82[\xA0-\xBF]|\xE2\x83[\x80-\x8F]

/* Bullet: * U+2022 */
bullet "*"|\xE2\x80\xA2

/* Sentence-final punctuation EXCLUDING full stop to avoid problems with abbreviations */
endmarker  [!?]|{ellipsis}

/* Punctuation at the beginning of a sentence/token, including Spanish exclamation and question marks U+A1 and U+BF */
lead       {leftbracket}|{leftquote}|{currency}|{bullet}|{dash}|\xC2\xA1|\xC2\xBF

/* Punctuation at the end of a sentence */
trail_end  {rightbracket}|{rightquote}

/* Punctuation at the end of a token */
trail  [,;:]|{endmarker}|{trail_end}|%|"/"|"|"|{ambiquote}
