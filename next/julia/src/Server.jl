module Server
export SemagramsServer, serve

using Mux
using Sockets

import Mux: serve

struct SemagramsServer
  assetPath::String
  wsFunction::Function
end

function serve(server::SemagramsServer; host="127.0.0.1", port=8080)
  @app h = (
    Mux.defaults,
    files(server.assetPath),
    Mux.notfound()
  )

  @app w = (
    Mux.wdefaults,
    route("/ws", server.wsFunction),
    Mux.wclose,
    Mux.notfound()
  )

  serve(h, w, port)
end

end
