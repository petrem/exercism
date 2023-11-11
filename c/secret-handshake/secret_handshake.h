#ifndef _SECRET_HANDSHAKE
#define _SECRET_HANDSHAKE

#define CMD_WINK         1
#define CMD_DOUBLE_BLINK 2
#define CMD_CLOSE_EYES   4
#define CMD_JUMP         8
#define CMD_REVERSE     16

const char **commands(unsigned int n);

#endif
