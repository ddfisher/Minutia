var net = require('net');

var sockets = [];

var server = net.createServer(function(socket) {
    sockets.push(socket);

    socket.on('data', function(data) {
        for (var ii = 0; ii < sockets.length; ii++) {
            if (sockets[ii] != socket) {
                sockets[ii].write(data);
            }
        }
    });

    socket.on('end', function() {
        var index = sockets.indexOf(socket);
        sockets.splice(index, 1);
    });
});

server.listen(8080);

