#include "mongoose.h"

static void handler(struct mg_connection *c, int event, void *ev) {
  if (event == MG_EV_HTTP_MSG) {
    struct mg_http_message *  msg = (struct mg_http_message *) ev;
    struct mg_http_serve_opts opts = { .root_dir = "./web_root" };
    mg_http_serve_dir(c, msg, &opts);
  }
}

int main(void) {
  struct mg_mgr mgr;
  mg_mgr_init(&mgr);
  mg_http_listen(&mgr, "http://0.0.0.0:8000", handler, NULL);
  for (;;) mg_mgr_poll(&mgr, 1000);
  mg_mgr_free(&mgr);
  return 0;
}
