
#define ANSI_ESCAPE '\033'

#define ANSI_ATTR_RESET  "\033[0m"
#define ANSI_COLOR_BLACK "\033[30m"
#define ANSI_COLOR_RED "\033[31m"
#define ANSI_COLOR_GREEN "\033[32m"
#define ANSI_COLOR_YELLOW "\033[33m"
#define ANSI_COLOR_BLUE "\033[34m"
#define ANSI_COLOR_PURPLE "\033[35m"
#define ANSI_COLOR_CYAN "\033[36m"
#define ANSI_COLOR_WHITE "\033[37m"
#define ANSI_BOLDOFF "\033[0m"
#define ANSI_BOLDON "\033[1m"
#define ANSI_CLEAR "\033[2J"
#define ANSI_HOME "\033[H"
#define ANSI_UP "\033[1A"

int split( char *str, char *connector );
void show_yahoo_packet();
