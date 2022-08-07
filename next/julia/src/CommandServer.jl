module CommandServer
export commandServer

using WebSockets

function commandServer(commands::Channel)
  (ws) -> begin
    while !eof(ws)
      command = take!(commands)
      write(ws, command)
    end
  end
end

end
