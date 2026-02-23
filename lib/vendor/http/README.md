# Bern HTTP C Bindings

This folder adds a lightweight HTTP server bridge for Bern using C and the `foreign` FFI.

## What It Provides

- Start/stop an HTTP server from Bern.
- Register static routes in Bern (`method + path -> response`).
- Poll once per loop iteration so server lifetime is controlled by Bern code.

## Exported C API

- `bern_http_start(int port, const char* bind_addr) -> void*`
- `bern_http_stop(void* server) -> int`
- `bern_http_is_running(void* server) -> int`
- `bern_http_set_default_response(void* server, int status, const char* content_type, const char* body) -> int`
- `bern_http_add_route(void* server, const char* method, const char* path, int status, const char* content_type, const char* body) -> int`
- `bern_http_poll_once(void* server, int timeout_ms) -> int`

## Build

### Linux

```sh
cd lib/vendor/http
sh build_linux.sh
```

Expected output: `lib/vendor/http/linux/libbern_http.so`

### Windows (MinGW)

```powershell
cd lib/vendor/http
powershell -ExecutionPolicy Bypass -File .\build_windows.ps1
```

Expected output: `lib/vendor/http/windows/bern_http.dll`

## Bern Usage

```bern
import vendor/http/http

server = http_server_start(8080)
route_ok = http_server_get(server, "/", "Hello from Bern")
health_ok = http_server_add_route_json(server, "/health", "{\"ok\":true}")

http_server_serve_forever(server, 50)
```

## Notes

- Current model is route-to-static-response (no C callback into Bern yet).
- This is enough to run servers now and can evolve into framework tooling later.
