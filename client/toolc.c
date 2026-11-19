/*
 * toolc: client for tool-server.
 *
 *   toolc COMMAND [ARGS...]
 *   COMMAND [ARGS...]          (through a symlink named COMMAND -> toolc)
 *
 * Sends cwd, argv and the environment over the server's unix socket, copies
 * the stdout/stderr frames it gets back to fd 1/2 and exits with the command's
 * status. See src/server.lisp for the protocol.
 */
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <termios.h>
#include <unistd.h>

extern char **environ;

#define CLIENT_NAME "toolc"
#define EXIT_CLIENT_ERROR 255

/* The controlling terminal, which interactive commands draw on directly.
 * Kept open so its settings can be put back if the server leaves it raw. */
static int tty_fd = -1;
static struct termios tty_saved;

static void restore_terminal(void) {
  struct termios now;
  if (tty_fd < 0 || tcgetattr(tty_fd, &now) != 0) return;
  if (memcmp(&now, &tty_saved, sizeof now) != 0) {
    static const char reset[] = "\033[?25h\033[?1049l";
    tcsetattr(tty_fd, TCSANOW, &tty_saved);
    (void)!write(tty_fd, reset, sizeof reset - 1);
  }
}

static void restore_and_reraise(int sig) {
  restore_terminal();
  signal(sig, SIG_DFL);
  raise(sig);
}

/* The real device path: on /dev/tty, which names "my controlling terminal",
 * ttyname() returns "/dev/tty", and that means nothing to the server. */
static const char *open_terminal(void) {
  const char *name = NULL;
  for (int fd = 0; fd <= 2 && !name; fd++)
    if (isatty(fd)) name = ttyname(fd);
  if (!name) return "";
  tty_fd = open(name, O_RDWR | O_NOCTTY | O_CLOEXEC);
  if (tty_fd < 0) return "";
  if (tcgetattr(tty_fd, &tty_saved) != 0) {
    close(tty_fd);
    tty_fd = -1;
    return "";
  }
  atexit(restore_terminal);
  signal(SIGHUP, restore_and_reraise);
  signal(SIGTERM, restore_and_reraise);
  signal(SIGINT, restore_and_reraise);
  return name;
}

static void die(const char *what) {
  fprintf(stderr, CLIENT_NAME ": %s: %s\n", what, strerror(errno));
  exit(EXIT_CLIENT_ERROR);
}

static void protocol_error(const char *what) {
  fprintf(stderr, CLIENT_NAME ": %s\n", what);
  exit(EXIT_CLIENT_ERROR);
}

/* Must match default-socket-path in src/server.lisp. */
static void socket_path(char *buf, size_t size) {
  const char *env = getenv("TOOL_SERVER_SOCKET");
  if (env && *env) {
    snprintf(buf, size, "%s", env);
    return;
  }
  const char *tmp = getenv("TMPDIR");
  if (!tmp || !*tmp) tmp = "/tmp";
  size_t len = strlen(tmp);
  while (len > 0 && tmp[len - 1] == '/') len--;
  snprintf(buf, size, "%.*s/tool-server.sock", (int)len, tmp);
}

static void write_all(int fd, const void *data, size_t n) {
  const char *p = data;
  while (n > 0) {
    ssize_t w = write(fd, p, n);
    if (w < 0) {
      if (errno == EINTR) continue;
      die("write");
    }
    p += w;
    n -= (size_t)w;
  }
}

/* Returns 0 if the connection closed before N bytes arrived. */
static int read_full(int fd, void *data, size_t n) {
  char *p = data;
  while (n > 0) {
    ssize_t r = read(fd, p, n);
    if (r < 0) {
      if (errno == EINTR) continue;
      die("read");
    }
    if (r == 0) return 0;
    p += r;
    n -= (size_t)r;
  }
  return 1;
}

static char *append_field(char *p, const char *s) {
  size_t n = strlen(s) + 1;
  memcpy(p, s, n);
  return p + n;
}

int main(int argc, char **argv) {
  const char *self = strrchr(argv[0], '/');
  self = self ? self + 1 : argv[0];

  const char *command;
  char **args;
  int nargs;
  if (strcmp(self, CLIENT_NAME) == 0) {
    command = argc > 1 ? argv[1] : "help";
    args = argv + (argc > 1 ? 2 : 1);
    nargs = argc > 1 ? argc - 2 : 0;
  } else {
    command = self;
    args = argv + 1;
    nargs = argc - 1;
  }

  char cwd[PATH_MAX];
  if (!getcwd(cwd, sizeof cwd)) die("getcwd");
  char count[16];
  snprintf(count, sizeof count, "%d", nargs + 1);
  int nenv = 0;
  while (environ[nenv]) nenv++;
  char envc[16];
  snprintf(envc, sizeof envc, "%d", nenv);
  char tty[PATH_MAX];
  snprintf(tty, sizeof tty, "%s", open_terminal());

  size_t size = strlen("3") + 1 + strlen(cwd) + 1 + strlen(tty) + 1 + strlen(count) + 1 +
                strlen(command) + 1 + strlen(envc) + 1;
  for (int i = 0; i < nargs; i++) size += strlen(args[i]) + 1;
  for (int i = 0; i < nenv; i++) size += strlen(environ[i]) + 1;
  char *request = malloc(size);
  if (!request) die("malloc");
  char *p = request;
  p = append_field(p, "3");
  p = append_field(p, cwd);
  p = append_field(p, tty);
  p = append_field(p, count);
  p = append_field(p, command);
  for (int i = 0; i < nargs; i++) p = append_field(p, args[i]);
  p = append_field(p, envc);
  for (int i = 0; i < nenv; i++) p = append_field(p, environ[i]);

  struct sockaddr_un addr = {.sun_family = AF_UNIX};
  char path[PATH_MAX];
  socket_path(path, sizeof path);
  if (strlen(path) >= sizeof addr.sun_path) {
    fprintf(stderr, CLIENT_NAME ": socket path too long: %s\n", path);
    return EXIT_CLIENT_ERROR;
  }
  strcpy(addr.sun_path, path);

  int fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (fd < 0) die("socket");
  if (connect(fd, (struct sockaddr *)&addr, sizeof addr) < 0) {
    fprintf(stderr, CLIENT_NAME ": cannot connect to %s: %s (is bin/tool-server running?)\n",
            path, strerror(errno));
    return EXIT_CLIENT_ERROR;
  }
  write_all(fd, request, size);
  free(request);

  static char buf[65536];
  for (;;) {
    unsigned char header[5];
    if (!read_full(fd, header, sizeof header))
      protocol_error("server closed the connection before the command finished");
    uint32_t len = (uint32_t)header[1] << 24 | (uint32_t)header[2] << 16 |
                   (uint32_t)header[3] << 8 | (uint32_t)header[4];
    switch (header[0]) {
    case 'X': {
      char status[16];
      if (len >= sizeof status || !read_full(fd, status, len))
        protocol_error("bad exit frame");
      status[len] = '\0';
      exit(atoi(status));
    }
    case 'O':
    case 'E': {
      int out = header[0] == 'O' ? STDOUT_FILENO : STDERR_FILENO;
      while (len > 0) {
        size_t chunk = len < sizeof buf ? len : sizeof buf;
        if (!read_full(fd, buf, chunk))
          protocol_error("server closed the connection mid-frame");
        write_all(out, buf, chunk);
        len -= (uint32_t)chunk;
      }
      break;
    }
    default:
      protocol_error("unknown frame type");
    }
  }
}
