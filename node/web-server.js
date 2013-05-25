var http = require('http');

var server = http.createServer( function(req, res) {
    res.writeHead(200, { 'content-type': 'text/plain' });
    res.write("hello world\n");
    res.end();
});

server.listen(8080);
