/*
Name: Arun Baskar
UID : u0811338
Project done with Arvind Haran
*/

%{
#include "string.h"
char* LStrip(char* str);
char* RStrip(char* str);
char* Strip(char* str);

int current = 0;
int continuation = 0;
struct list* create_list(int val);
struct list* addElementToList(int val);
void removeElement();
void empty_list();
void handle_indent();
int replaceChar(char *data, char search, char replace);
struct list
{
    int val;
    struct list *next;
};

struct list *head = NULL;
int curly=0, brac=0, squarebrac =0;
%}

DIGIT    [0-9]
ZERO     [0]
HEXPREFIX [xX]
OCTPREFIX [oO]
BINARYPREFIX [bB]
HEXNUMBER [0-9a-fA-F]
OCTNUMBER [0-7]
BINARYNUMBER [0-1]
DECIMALNUMBER [1-9]
NUMBERSUFFIX  [lL]
EXPONENT   [eE][-+]?[0-9]+
IMAGSUFFIX [jJ]
ID       [A-Za-z_][A-Za-z_0-9]*
BACKSLASH [\\]
SPACE    [ ]
NOTSPACE  [^ ]
OPERATORS [<<|>>|<=|>=|==|!=|**|+|-|*|//|/|%|&|\|^|~|<|>]
NEWLINE   [\n]
NOTNEWLINE [^\n\r ]
DELIMITERS  [>>=|<<=|**=|+=|\-=|*=|/=|//=|%=|&=|\|=|^=|\[|\]|{|}|,|:|.|;|@|=|(|)]
KEYWORDS    (False|class|finally|is|return|None|continue|for|lambda|try|True|def|from|nonlocal|while|and|del|global|not|with|as|elif|if|or|yield|assert|else|import|pass|break|except|in|raise)
DOT       [.]
HASHTAG   [#]
COMMENT   [^\r\n]
RETURN    [\r\n]
STAR      [*]
EQUALTO   [=]
LESSTHAN  [<]
GREATERTHAN [>]
NOT [!]
DIVIDE [/]
APPENDEQUAL [+\-*/%&|^=<>]
TILDE [~]
QUOTE [']
DOUBLEQUOTE ["]
NOTDOUBLEQUOTE [^"\n\r]
NOTQUOTE [^'\n\r]
TAB [\t]
TABSPACE [\t ]

INSIDE    .|\n
STRINGPREFIX [urRU]
SHORTSTRINGCHARSINGLE [^\n'\\]
SHORTSTRINGCHARDOUBLE [^\n"\\]
LONGSTRINGCHAR [^\\]
ESCAPSEQ \\.
LINEBREAK \\\n
%%

{STAR}{STAR}{EQUALTO}?   {
                           printf("(PUNCT \"%s\")\n", yytext);
                         }

{GREATERTHAN}{GREATERTHAN}{EQUALTO}?  {
                                          printf("(PUNCT \"%s\")\n", yytext);
                                      }


{LESSTHAN}{LESSTHAN}{EQUALTO}?        {
                                          printf("(PUNCT \"%s\")\n", yytext);
                                      }

{LESSTHAN}{GREATERTHAN}  {
                             printf("(PUNCT \"%s\")\n", yytext);
                         }

{NOT}{EQUALTO}    {
                      printf("(PUNCT \"%s\")\n", yytext);
                  }

{DIVIDE}{DIVIDE}{EQUALTO}?    {
                                  printf("(PUNCT \"%s\")\n", yytext);

                              }

{APPENDEQUAL}{EQUALTO}?   {
                            printf("(PUNCT \"%s\")\n", yytext);
                          }

{TILDE}                 {
                           printf("(PUNCT \"%s\")\n", yytext);
                        }

[urRU]?(({BACKSLASH}{RETURN})?)*'(({BACKSLASH}{RETURN})?({SHORTSTRINGCHARSINGLE}|{ESCAPSEQ})*({BACKSLASH}{RETURN})?({SHORTSTRINGCHARSINGLE}|{ESCAPSEQ})*)*'   {
                   char *new_str = (char*)malloc(strlen(yytext) + 100);
                   char *new_str_ = (char*)malloc(strlen(yytext) + 100);
                   yytext[strlen(yytext) -1] = '\0';
                   yytext++;
                   ReplaceStr(yytext, "\\\n", new_str, "");
                   ReplaceStr(new_str, "\n", new_str_, "\\n");
                   ReplaceStr(new_str_, "'", new_str, "\'");
                   ReplaceStr(new_str, "\"", new_str_, "\"");
                   ReplaceStr(new_str_, "\a", new_str, "\\a");
                   ReplaceStr(new_str, "\b", new_str_, "\\b");
                   ReplaceStr(new_str_, "\f", new_str, "\\f");
                   ReplaceStr(new_str, "\r", new_str_, "\\r");
                   ReplaceStr(new_str_, "\t", new_str, "\\t");
                   ReplaceStr(new_str, "\v", new_str_, "\\v");
                   printf("(LIT \"%s\")\n", new_str_);
                   free(new_str);
                   free(new_str_);
               }

[urRU]?(({BACKSLASH}{RETURN})?)*\"(({BACKSLASH}{RETURN})?({SHORTSTRINGCHARDOUBLE}|{ESCAPSEQ})*({BACKSLASH}{RETURN})?({SHORTSTRINGCHARDOUBLE}|{ESCAPSEQ})*)*\"     {
                       char *new_str = (char*)malloc(strlen(yytext) + 100);
                       char *new_str_ = (char*)malloc(strlen(yytext) + 100);
                       yytext[strlen(yytext) -1] = '\0';
                       yytext++;
                       ReplaceStr(yytext, "\\\n", new_str, "");
                       ReplaceStr(new_str, "\n", new_str_, "\\n");
                       ReplaceStr(new_str_, "'", new_str, "\'");
                       ReplaceStr(new_str, "\\\"", new_str_, "\\\"");
                       ReplaceStr(new_str, "\"", new_str_, "\"");
                       ReplaceStr(new_str_, "\a", new_str, "\\a");
                       ReplaceStr(new_str, "\b", new_str_, "\\b");
                       ReplaceStr(new_str_, "\f", new_str, "\\f");
                       ReplaceStr(new_str, "\r", new_str_, "\\r");
                       ReplaceStr(new_str_, "\t", new_str, "\\t");
                       ReplaceStr(new_str, "\v", new_str_, "\\v");
                       printf("(LIT \"%s\")\n", new_str_);
                       free(new_str);
                       free(new_str_);
                   }

[urRU]?(({BACKSLASH}{RETURN})?)*'''(({BACKSLASH}{RETURN})?({LONGSTRINGCHAR}|{ESCAPSEQ})*({BACKSLASH}{RETURN})?({LONGSTRINGCHAR}|{ESCAPSEQ})*)*'''     {
           char *new_str = (char*)malloc(strlen(yytext) + 100);
           char *new_str_ = (char*)malloc(strlen(yytext) + 100);
           ReplaceStr(yytext, "'''", new_str_, "");
           ReplaceStr(new_str_, "\\\n", new_str, "");
           ReplaceStr(new_str, "\n", new_str_, "\\n");
           ReplaceStr(new_str_, "\'", new_str, "\\\'");
           ReplaceStr(new_str, "\\\"", new_str_, "\\\\\""); 
           ReplaceStr(new_str, "\"", new_str_, "\\\"");
           ReplaceStr(new_str_, "\a", new_str, "\\a");
           ReplaceStr(new_str, "\b", new_str_, "\\b");
           ReplaceStr(new_str_, "\f", new_str, "\\f");
           ReplaceStr(new_str, "\r", new_str_, "\\r");
           ReplaceStr(new_str_, "\t", new_str, "\\t");
           ReplaceStr(new_str, "\v", new_str_, "\\v");
           printf("(LIT \"%s\")\n", new_str_);
           free(new_str);
           free(new_str_);
       }

[urRU]?(({BACKSLASH}{RETURN})?)*\"\"\"(({BACKSLASH}{RETURN})?({LONGSTRINGCHAR}|{ESCAPSEQ})*({BACKSLASH}{RETURN})?({LONGSTRINGCHAR}|{ESCAPSEQ})*)*\"\"\"     {
                 char *new_str = (char*)malloc(strlen(yytext) + 100);
                 char *new_str_ = (char*)malloc(strlen(yytext) + 100);
                 ReplaceStr(yytext, "\"\"\"", new_str_, "");
                 ReplaceStr(new_str_, "\\\n", new_str, "");
                 ReplaceStr(new_str, "\n", new_str_, "\\n"); 
                 ReplaceStr(new_str_, "\'", new_str, "\\\'");
                 ReplaceStr(new_str, "\"", new_str_, "\\\"");
                 ReplaceStr(new_str_, "\a", new_str, "\\a");
                 ReplaceStr(new_str, "\b", new_str_, "\\b");
                 ReplaceStr(new_str_, "\f", new_str, "\\f");
                 ReplaceStr(new_str, "\r", new_str_, "\\r");
                 ReplaceStr(new_str_, "\t", new_str, "\\t");
                 ReplaceStr(new_str, "\v", new_str_, "\\v");
                 printf("(LIT \"%s\")\n", new_str_);
                 free(new_str);
                 free(new_str_);
             }

^{TAB}/{NOTNEWLINE}   {
                         printf("ERROR: Incorrect Indentation1\n");
                     }

^{SPACE}+{TAB}/{NOTNEWLINE}   {
                                printf("ERROR: Incorrect Indentation2\n");
                            }

[{]              {
                      curly++; 
                      printf("(PUNCT \"%s\")\n", yytext);
                 }

[(]              {
                      brac++;
                      printf("(PUNCT \"%s\")\n", yytext);
                 }

[[]              {
                      squarebrac++;
                      printf("(PUNCT \"%s\")\n", yytext);
                 }

[}]              {
                      curly--;
                      printf("(PUNCT \"%s\")\n", yytext);
                 }

[)]              {
                      brac--;
                      printf("(PUNCT \"%s\")\n", yytext);
                 }

[]]              {
                      squarebrac--;
                      printf("(PUNCT \"%s\")\n", yytext);
                 }

[:;.,`@|]        {
                   printf("(PUNCT \"%s\")\n", yytext);
                 }

{ZERO}{HEXPREFIX}{HEXNUMBER}+         {
                                           /*HEX*/
                                           yytext[0] = 35;
                                           printf("(LIT %s)\n", yytext);
                                      }

{ZERO}{OCTPREFIX}{OCTNUMBER}+         {
                                           /*OCT*/
                                           yytext[0] = 35;
                                           printf("(LIT %s)\n", yytext);
                                      }

{ZERO}{BINARYPREFIX}{BINARYNUMBER}+   {
                                          /*BINARY*/
                                          yytext[0] = 35;
                                          printf("(LIT %s)\n", yytext);
                                      }

{DECIMALNUMBER}{DIGIT}*{NUMBERSUFFIX}? {
                                           /*DECIMAL*/
                                           printf("(LIT %s)\n", yytext);
                                       }

{DIGIT}+{DOT}{DIGIT}*{EXPONENT}?     {
                                                       printf("(LIT %s)\n", yytext);
                                                  }

{DOT}{DIGIT}+{EXPONENT}?         {
                                                       printf("(LIT %s)\n", yytext);
                                                }

{DIGIT}+{EXPONENT}?                {
                                                     printf("(LIT %s)\n", yytext);
                                                }

{DIGIT}+          {
                                    printf("(LIT %s)\n", yytext);
                               }

{DIGIT}+{DOT}{DIGIT}*{EXPONENT}?{IMAGSUFFIX}     {
                                                       yytext[strlen(yytext)-1] = 105;
                                                       printf("(LIT +%s)\n", yytext);
                                                  }

{DOT}{DIGIT}+{EXPONENT}?{IMAGSUFFIX}          {
                                                       yytext[strlen(yytext)-1] = 105;
                                                       printf("(LIT +%s)\n", yytext);
                                                }

{DIGIT}+{EXPONENT}?{IMAGSUFFIX}                {
                                                     yytext[strlen(yytext)-1] = 105;
                                                     printf("(LIT +%s)\n", yytext);
                                                }

{DIGIT}+{IMAGSUFFIX}          {
                                    yytext[strlen(yytext)-1] = 105;
                                    printf("(LIT +%s)\n", yytext);
                               }

^{SPACE}{SPACE}*{NEWLINE}    {
                                  continuation = 0;
                                 /*if(!(brac|squarebrac|curly))
                                     printf("(NEWLINE)\n");*/
                             }

^{SPACE}{SPACE}*{NEWLINE}/{NOTNEWLINE}    {
                                               continuation = 0;
                                               empty_list();
                                               /*if(!(brac|squarebrac|curly))
                                                     printf("(NEWLINE)\n");*/
                                          }


{SPACE}     {
            }

^{HASHTAG}{COMMENT}*{RETURN}   {
                                   continuation = 0;
                               }

^{SPACE}+{HASHTAG}{COMMENT}*{RETURN}  {
                                          continuation = 0;
                                      }

{HASHTAG}{COMMENT}*{RETURN} {
                               continuation = 0;
                               if(!(brac|squarebrac|curly))
                                   printf("(NEWLINE)\n");
                            }

{HASHTAG}{COMMENT}*{RETURN}/{NOTNEWLINE} {
                               continuation = 0;
                               if(!(brac|squarebrac|curly))
                                   printf("(NEWLINE)\n");
                               empty_list();
                            }

{BACKSLASH}{RETURN}     {
                          continuation = 1;
                        }

^{SPACE}+/{NOTNEWLINE}    {
                            if(!continuation)
                            {
                            handle_indent(); 
                            }
                          }

^{NEWLINE}  {
                 /*if(!(brac|squarebrac|curly))
                     printf("(NEWLINE)\n");*/
                 continuation = 0;
            }

^{NEWLINE}/{NOTNEWLINE}  {
                            continuation = 0;
                            if(!(brac|squarebrac|curly))
                            {
                            /*printf("(NEWLINE)\n");*/
                            empty_list();
                            }
                         }


{NEWLINE}    {
                   continuation = 0;
                   if(!(brac|squarebrac|curly))
                       printf("(NEWLINE)\n");
             }


{NEWLINE}/{NOTNEWLINE}  {
                         continuation = 0;
                         if(!(brac|squarebrac|curly))
                         { 
                          printf("(NEWLINE)\n");
                          empty_list();
                         }
                        }

^{KEYWORDS}{SPACE} {
                       Strip(yytext);
                       printf("(KEYWORD %s)\n", yytext);
                   }

{KEYWORDS}{SPACE} {
                       Strip(yytext);
                       printf("(KEYWORD %s)\n", yytext);
                  }

{KEYWORDS}{NEWLINE} {
                        continuation = 0;
                        Strip(yytext);
                        printf("(KEYWORD %s)\n", yytext);
                        if(!(brac|squarebrac|curly))
                            printf("(NEWLINE)\n");
                    }

{KEYWORDS}{NEWLINE}/{NOTNEWLINE} {
                                   continuation = 0;
                                   Strip(yytext);
                                   printf("(KEYWORD %s)\n", yytext);
                                   if(!(brac|squarebrac|curly))
                                       printf("(NEWLINE)\n");
                                   empty_list();
                                 }

{KEYWORDS}{SPACE}        {
                            Strip(yytext);
                            printf("(KEYWORD %s)\n", yytext);
                         }

{KEYWORDS}/{DELIMITERS}+ {
                            Strip(yytext);
                            printf("(KEYWORD %s)\n", yytext);
                         }

{ID}        {
                 printf("(ID \"%s\")\n", yytext);
            }

{KEYWORDS}{TABSPACE}     {
                            Strip(yytext);
                            printf("(KEYWORD %s)\n", yytext);
                          }

<<EOF>>    {
               empty_list();
               printf("(ENDMARKER)\n");
               yyterminate(); 
           }

{TAB}     {}


.         {
              printf("(ERROR)\n");
          }
%%

int main( int argc, char **argv )
{
     ++argv, --argc;  /* skip over program name */
     if ( argc > 0 )
             yyin = fopen( argv[0], "r" );
     else
             yyin = stdin;
     create_list(0);
     yylex();
}

char* LStrip(char* str)
{
    int i = 0, j = 0, size;

    if(NULL == str)
        return str;

    size = strlen((char *)str);
    if(size == 0)
        return str;

    while(str[i] == ' ' || str[i] == '\t' || str[i] == '\r' || str[i] == '\n')
        i++;

    if(i == 0)
        return str;

    for(j = 0; i < size; j++, i++)
        str[j] = str[i];
    str[j] = 0;

    return str;
}

char* RStrip(char* str)
{
    int i = 0, size;

    if(NULL == str)
        return str;

    size = strlen((char *)str);
    if(size == 0)
        return str;

    i = size - 1;

    while(str[i] == ' ' || str[i] == '\n' || str[i] == '\r' || str[i] == '\t')
        str[i--] = 0;

    return str;
}

char* Strip(char* str)
{
    LStrip(str);
    RStrip(str);
    return str;
}

struct list* create_list(int val)
{
    struct list* temp = (struct list*)malloc(sizeof(struct list));
    if(NULL == temp)
    {
        return NULL;
    }
    temp->val = val;
    temp->next = NULL;

    head = temp;
    return temp;
}

struct list* addElementToList(int val)
{
    struct list *ptr = (struct list*)malloc(sizeof(struct list));
    if(NULL == ptr)
    {
        return NULL;
    }
    ptr->val = val;
    ptr->next = head;

    head = ptr;
    return ptr;
}

void removeElement()
{
    struct list *temp;
    temp = head;
    head = head->next;
    temp->next = NULL;
    free(temp);
}

void empty_list()
{
    while(head->val != 0)
    {
            printf("(DEDENT)\n");
            removeElement();
    }
}

void handle_indent()
{
    if(brac|squarebrac|curly)
    {
        return;
    }

    if(head->val == 0)
	{
	addElementToList(strlen(yytext));
	printf("(INDENT)\n", head->val);
	}
    else
    {
	if(strlen(yytext) > head->val)
	{
	    addElementToList(strlen(yytext));
	    printf("(INDENT)\n", head->val);
	}
        else if(strlen(yytext) == head->val)
        {
            return;
        }
	else
	{
	    while(head->val !=0)
	    {
		removeElement();
		if(strlen(yytext) > head->val)
		{
		    printf("(DEDENT)\n");
		    printf("(ERROR \"mis-indented program\")\n");
		    break;
		}
		else if(strlen(yytext) == head->val)
		{
		    printf("(DEDENT)\n");
		    break;
		}
                else if(strlen(yytext) < head->val)
                {
                    printf("(DEDENT)\n");
                }
	    }
	}
    }
}

void print_ele()
{
    struct list *temp;
    temp = head;
    while(temp != NULL)
    {
        printf("%d-", temp->val);
        temp = temp->next;
    }
}

int replaceChar(char *data, char search, char replace)
{
    while(*data != '\0')
    {
        if (*data == search)
        {
            *data = replace;
        }
        data ++;
    }
    return 0;
}

void ReplaceStr(char * old_data, char * old_str, char * new_data, char * new_str){
    char * pre = old_data;
    char * next= old_data;
    int old_str_len = strlen(old_str);
    int new_str_len = strlen(new_str);
    int new_pos = 0;
    int size = 0;
    do{
        next = strstr(next, old_str);
        if (NULL != next)
            size = strlen(pre)-strlen(next);
        else
            size = strlen(pre);
        memcpy(new_data + new_pos, pre, size);
        new_pos += size;
        if (NULL != next){
            memcpy(new_data + new_pos, new_str, new_str_len);
            new_pos += new_str_len;
            next     = next + old_str_len;
            pre      = next;
        }
   }while(NULL != next);
    memcpy(new_data + new_pos, "\0", 1);
}

/*
Name: Arun Baskar
UID : u0811338
Project done with Arvind Haran
*/
