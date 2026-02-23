#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#ifndef _WINSOCK_DEPRECATED_NO_WARNINGS
#define _WINSOCK_DEPRECATED_NO_WARNINGS
#endif
#include <winsock2.h>
#include <ws2tcpip.h>
typedef SOCKET bern_socket_t;
typedef int bern_socklen_t;
#define BERN_INVALID_SOCKET INVALID_SOCKET
#define bern_close_socket closesocket
#else
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
typedef int bern_socket_t;
typedef socklen_t bern_socklen_t;
#define BERN_INVALID_SOCKET (-1)
#define bern_close_socket close
#endif

#if defined(_WIN32)
#define BERN_HTTP_EXPORT __declspec(dllexport)
#else
#define BERN_HTTP_EXPORT __attribute__((visibility("default")))
#endif

typedef struct BernHttpRoute {
    char method[16];
    char path[512];
    int status;
    char *content_type;
    char *body;
    struct BernHttpRoute *next;
} BernHttpRoute;

typedef struct BernHttpServer {
    bern_socket_t listen_fd;
    int running;
    int default_status;
    char *default_content_type;
    char *default_body;
    BernHttpRoute *routes;
} BernHttpServer;

#ifdef _WIN32
static int bern_wsa_refcount = 0;

static int bern_winsock_startup(void) {
    if (bern_wsa_refcount == 0) {
        WSADATA wsa_data;
        if (WSAStartup(MAKEWORD(2, 2), &wsa_data) != 0) {
            return 0;
        }
    }
    bern_wsa_refcount++;
    return 1;
}

static void bern_winsock_cleanup(void) {
    if (bern_wsa_refcount > 0) {
        bern_wsa_refcount--;
        if (bern_wsa_refcount == 0) {
            WSACleanup();
        }
    }
}
#endif

static char *bern_strdup(const char *src) {
    size_t len;
    char *dst;
    const char *safe_src = (src == NULL) ? "" : src;

    len = strlen(safe_src);
    dst = (char *)malloc(len + 1);
    if (dst == NULL) {
        return NULL;
    }
    memcpy(dst, safe_src, len + 1);
    return dst;
}

static void bern_uppercase(char *text) {
    if (text == NULL) {
        return;
    }

    while (*text != '\0') {
        *text = (char)toupper((unsigned char)*text);
        text++;
    }
}

static const char *bern_reason_phrase(int status) {
    switch (status) {
        case 200:
            return "OK";
        case 201:
            return "Created";
        case 204:
            return "No Content";
        case 400:
            return "Bad Request";
        case 404:
            return "Not Found";
        case 500:
            return "Internal Server Error";
        default:
            return "OK";
    }
}

static int bern_send_all(bern_socket_t sock, const char *data, int len) {
    int sent = 0;

    while (sent < len) {
        int n = (int)send(sock, data + sent, len - sent, 0);
        if (n <= 0) {
            return 0;
        }
        sent += n;
    }
    return 1;
}

static int bern_send_response(bern_socket_t client, int status, const char *content_type, const char *body) {
    char header[1024];
    const char *safe_type = (content_type == NULL) ? "text/plain; charset=utf-8" : content_type;
    const char *safe_body = (body == NULL) ? "" : body;
    int body_len = (int)strlen(safe_body);
    int header_len = snprintf(
        header,
        sizeof(header),
        "HTTP/1.1 %d %s\r\n"
        "Connection: close\r\n"
        "Content-Type: %s\r\n"
        "Content-Length: %d\r\n"
        "Server: bern-http-bridge\r\n"
        "\r\n",
        status,
        bern_reason_phrase(status),
        safe_type,
        body_len
    );

    if (header_len <= 0 || header_len >= (int)sizeof(header)) {
        return 0;
    }

    if (!bern_send_all(client, header, header_len)) {
        return 0;
    }

    if (body_len > 0) {
        if (!bern_send_all(client, safe_body, body_len)) {
            return 0;
        }
    }

    return 1;
}

static void bern_free_routes(BernHttpRoute *route) {
    BernHttpRoute *next;
    while (route != NULL) {
        next = route->next;
        free(route->content_type);
        free(route->body);
        free(route);
        route = next;
    }
}

static BernHttpRoute *bern_find_route(BernHttpServer *server, const char *method, const char *path) {
    BernHttpRoute *current = server->routes;
    while (current != NULL) {
        if ((strcmp(current->method, method) == 0) && (strcmp(current->path, path) == 0)) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

static int bern_handle_client(BernHttpServer *server, bern_socket_t client) {
    char request[8192];
    char method[16];
    char path[1024];
    char *line_end;
    char *query_sep;
    int bytes_read;
    int parsed;
    BernHttpRoute *route;
    int status;
    const char *content_type;
    const char *body;

    bytes_read = (int)recv(client, request, (int)sizeof(request) - 1, 0);
    if (bytes_read <= 0) {
        return 0;
    }

    request[bytes_read] = '\0';
    line_end = strstr(request, "\r\n");
    if (line_end == NULL) {
        line_end = strchr(request, '\n');
    }
    if (line_end != NULL) {
        *line_end = '\0';
    }

    method[0] = '\0';
    path[0] = '\0';
    parsed = sscanf(request, "%15s %1023s", method, path);
    if (parsed != 2) {
        return bern_send_response(client, 400, "text/plain; charset=utf-8", "Bad Request");
    }

    bern_uppercase(method);
    query_sep = strchr(path, '?');
    if (query_sep != NULL) {
        *query_sep = '\0';
    }

    route = bern_find_route(server, method, path);
    if (route != NULL) {
        status = route->status;
        content_type = route->content_type;
        body = route->body;
    } else {
        status = server->default_status;
        content_type = server->default_content_type;
        body = server->default_body;
    }

    return bern_send_response(client, status, content_type, body);
}

BERN_HTTP_EXPORT void *bern_http_start(int port, const char *bind_addr) {
    bern_socket_t sockfd;
    struct sockaddr_in addr;
    int reuse = 1;
    BernHttpServer *server;
    const char *safe_bind = bind_addr;

    if (port <= 0 || port > 65535) {
        return NULL;
    }

#ifdef _WIN32
    if (!bern_winsock_startup()) {
        return NULL;
    }
#endif

    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd == BERN_INVALID_SOCKET) {
#ifdef _WIN32
        bern_winsock_cleanup();
#endif
        return NULL;
    }

    setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, (const char *)&reuse, (int)sizeof(reuse));

    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons((unsigned short)port);

    if (safe_bind == NULL || safe_bind[0] == '\0' || strcmp(safe_bind, "0.0.0.0") == 0) {
        addr.sin_addr.s_addr = htonl(INADDR_ANY);
    } else {
#ifdef _WIN32
        addr.sin_addr.s_addr = inet_addr(safe_bind);
        if (addr.sin_addr.s_addr == INADDR_NONE) {
            bern_close_socket(sockfd);
            bern_winsock_cleanup();
            return NULL;
        }
#else
        if (inet_pton(AF_INET, safe_bind, &addr.sin_addr) != 1) {
            bern_close_socket(sockfd);
            return NULL;
        }
#endif
    }

    if (bind(sockfd, (struct sockaddr *)&addr, (bern_socklen_t)sizeof(addr)) != 0) {
        bern_close_socket(sockfd);
#ifdef _WIN32
        bern_winsock_cleanup();
#endif
        return NULL;
    }

    if (listen(sockfd, 64) != 0) {
        bern_close_socket(sockfd);
#ifdef _WIN32
        bern_winsock_cleanup();
#endif
        return NULL;
    }

    server = (BernHttpServer *)calloc(1, sizeof(BernHttpServer));
    if (server == NULL) {
        bern_close_socket(sockfd);
#ifdef _WIN32
        bern_winsock_cleanup();
#endif
        return NULL;
    }

    server->listen_fd = sockfd;
    server->running = 1;
    server->default_status = 404;
    server->default_content_type = bern_strdup("text/plain; charset=utf-8");
    server->default_body = bern_strdup("Not Found");

    if (server->default_content_type == NULL || server->default_body == NULL) {
        free(server->default_content_type);
        free(server->default_body);
        bern_close_socket(sockfd);
        free(server);
#ifdef _WIN32
        bern_winsock_cleanup();
#endif
        return NULL;
    }

    return (void *)server;
}

BERN_HTTP_EXPORT int bern_http_stop(void *server_handle) {
    BernHttpServer *server = (BernHttpServer *)server_handle;
    if (server == NULL) {
        return 0;
    }

    if (server->listen_fd != BERN_INVALID_SOCKET) {
        bern_close_socket(server->listen_fd);
        server->listen_fd = BERN_INVALID_SOCKET;
    }

    server->running = 0;

    bern_free_routes(server->routes);
    server->routes = NULL;

    free(server->default_content_type);
    free(server->default_body);
    free(server);

#ifdef _WIN32
    bern_winsock_cleanup();
#endif

    return 1;
}

BERN_HTTP_EXPORT int bern_http_is_running(void *server_handle) {
    BernHttpServer *server = (BernHttpServer *)server_handle;
    if (server == NULL) {
        return 0;
    }
    return server->running ? 1 : 0;
}

BERN_HTTP_EXPORT int bern_http_set_default_response(void *server_handle, int status, const char *content_type, const char *body) {
    BernHttpServer *server = (BernHttpServer *)server_handle;
    char *new_type;
    char *new_body;

    if (server == NULL) {
        return 0;
    }

    new_type = bern_strdup(content_type == NULL ? "text/plain; charset=utf-8" : content_type);
    new_body = bern_strdup(body == NULL ? "" : body);

    if (new_type == NULL || new_body == NULL) {
        free(new_type);
        free(new_body);
        return 0;
    }

    free(server->default_content_type);
    free(server->default_body);

    server->default_status = status;
    server->default_content_type = new_type;
    server->default_body = new_body;
    return 1;
}

BERN_HTTP_EXPORT int bern_http_add_route(
    void *server_handle,
    const char *method,
    const char *path,
    int status,
    const char *content_type,
    const char *body
) {
    BernHttpServer *server = (BernHttpServer *)server_handle;
    BernHttpRoute *route;
    size_t method_len;
    size_t path_len;

    if (server == NULL || method == NULL || path == NULL) {
        return 0;
    }

    method_len = strlen(method);
    path_len = strlen(path);
    if (method_len == 0 || method_len >= sizeof(route->method) || path_len == 0 || path_len >= sizeof(route->path)) {
        return 0;
    }

    route = (BernHttpRoute *)calloc(1, sizeof(BernHttpRoute));
    if (route == NULL) {
        return 0;
    }

    memcpy(route->method, method, method_len + 1);
    bern_uppercase(route->method);
    memcpy(route->path, path, path_len + 1);
    route->status = status;
    route->content_type = bern_strdup(content_type == NULL ? "text/plain; charset=utf-8" : content_type);
    route->body = bern_strdup(body == NULL ? "" : body);

    if (route->content_type == NULL || route->body == NULL) {
        free(route->content_type);
        free(route->body);
        free(route);
        return 0;
    }

    route->next = server->routes;
    server->routes = route;
    return 1;
}

BERN_HTTP_EXPORT int bern_http_poll_once(void *server_handle, int timeout_ms) {
    BernHttpServer *server = (BernHttpServer *)server_handle;
    bern_socket_t client;
    struct sockaddr_in client_addr;
#ifdef _WIN32
    bern_socklen_t client_len = (bern_socklen_t)sizeof(client_addr);
    DWORD recv_timeout = 250;
#else
    bern_socklen_t client_len = (bern_socklen_t)sizeof(client_addr);
    struct timeval recv_timeout;
#endif
    fd_set readfds;
    struct timeval timeout;
    struct timeval *timeout_ptr = NULL;
    int ready;
    int handled;

    if (server == NULL || !server->running || server->listen_fd == BERN_INVALID_SOCKET) {
        return -1;
    }

    if (timeout_ms >= 0) {
        timeout.tv_sec = timeout_ms / 1000;
        timeout.tv_usec = (timeout_ms % 1000) * 1000;
        timeout_ptr = &timeout;
    }

    FD_ZERO(&readfds);
    FD_SET(server->listen_fd, &readfds);

    ready = select((int)(server->listen_fd + 1), &readfds, NULL, NULL, timeout_ptr);
    if (ready <= 0) {
        return 0;
    }

    client = accept(server->listen_fd, (struct sockaddr *)&client_addr, &client_len);
    if (client == BERN_INVALID_SOCKET) {
        return 0;
    }

#ifdef _WIN32
    setsockopt(client, SOL_SOCKET, SO_RCVTIMEO, (const char *)&recv_timeout, (int)sizeof(recv_timeout));
#else
    recv_timeout.tv_sec = 0;
    recv_timeout.tv_usec = 250000;
    setsockopt(client, SOL_SOCKET, SO_RCVTIMEO, &recv_timeout, (bern_socklen_t)sizeof(recv_timeout));
#endif

    handled = bern_handle_client(server, client);
    bern_close_socket(client);

    return handled;
}
