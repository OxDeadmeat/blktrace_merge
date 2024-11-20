
#define skip_whitespace skip_space

#ifdef NEVER
#define skip_whitespace(_arg ) strskip(_arg, " \t\n" )
#endif


extern int strlowercase(char *string);
extern int upcase(char *string);
extern int strskip(char **pptr, char *list);
extern int strtrim( char *bgn );
extern int get_token(char **pptr, char *token);
extern int get_tokend(char **pptr, char *token, char *dlimits);
extern int skip_space(char **pptr);
extern int strwcmp( char *pattern, char *string );
extern unsigned long bin_timestamp();


