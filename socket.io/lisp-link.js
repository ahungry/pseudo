/**
 * Pseudo - A pseudo 3d multiplayer roguelike
 * Copyright (C) 2013 Matthew Carter
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

var port                = 7159; // COMMON LISP backend
var listenOn            = 7158; // Our port we are opening
var updateUnitInterval  = 500;

/**
 * Dispatch a tcp request to an arbitrary port
 *
 * In this case we are doing it with lisp running
 * on the back end to handle certain function processing
 * for us.
 *
 * @param string action The backend function to call
 * @param object json An object to be serialized as json
 */
function tcpRequest (socket, action, json)
{
    try
    {
        var net = require('net');
        var bigData = '';
        var tcpSocket = net.connect({port: port},
                                    function () { //'connect' listener
                                        //console.log('connected');
                                        tcpSocket.write(action+'\r\n');
                                        tcpSocket.write(JSON.stringify(json)+'\r\n');
                                        //console.log('Sending in: '+action+'\r\n'+JSON.stringify(json)+'\r\n');
                                    });
        tcpSocket.on('data', function (data) {
            bigData += data;
        });
        tcpSocket.on('end', function () {
            json = JSON.parse(bigData);
            e = json.emit;
            b = json.broadcast;

            //console.log('From lisp: '+action);

            if(typeof(e) !== 'undefined' && e.action != 'nil')
                socket.emit(e.action, e.data );
            if(typeof(b) !== 'undefined' && b.action != 'nil')
                socket.broadcast.emit(b.action, b.data);
            
            //console.log('lisp disconnect');
        });
    }
    catch (e)
    {
        console.log('TCP Request failed');
    }
    finally
    {
        console.log('Hit finally in tcpRequest');
    }
}

/**
 * Need a decent way to keep NPC movements sent out to
 * players without pushing a full resync over and over
 * again
 */
var units = [];
var unitsShadow = [];
function npcUnitSync (socket)
{
    var net      = require('net');
    var bigData = '';
    var action   = 'request-all-units';
    var json     = { unit_id : 0 }
    unitsShadow  = units;
    var tcpSocket = net.connect(
        {port: port},
        function ()
        { //'connect' listener
            //console.log('Grabbing NPC unit data');
            tcpSocket.write(action+'\r\n');
            tcpSocket.write(JSON.stringify(json)+'\r\n');
            //console.log('Sending in: '+action+'\r\n'+JSON.stringify(json)+'\r\n');
            });
        tcpSocket.on('data', function (data) {
        bigData += data;
    });
    tcpSocket.on('end', function () {
        json = JSON.parse(bigData);
        e = json.emit;
        b = json.broadcast;

        units = b.data;
        for(u in units)
        {
            if(typeof unitsShadow[u] == 'undefined' ||
               typeof unitsShadow[u].x == 'undefined' ||
               typeof unitsShadow[u].y == 'undefined' ||
               units[u].x != unitsShadow[u].x ||
               units[u].y != unitsShadow[u].y ||
               units[u].hp != unitsShadow[u].hp ||
               units[u].map_index != unitsShadow[u].map_index)
            {
                socket.emit('update-unit', [ units[u] ]);
                socket.broadcast.emit('update-unit', [ units[u] ]);
            }
        }
    });

    setTimeout(function() { npcUnitSync(socket); }, updateUnitInterval);
}

var items = [];
var itemsShadow = [];
function itemSync (socket)
{
    var net      = require('net');
    var bigData  = '';
    var action   = 'request-all-items';
    var json     = { unit_id : 0 }
    itemsShadow  = items;
    var tcpSocket = net.connect(
        {port: port},
        function ()
        { //'connect' listener
            //console.log('Grabbing item data');
            tcpSocket.write(action+'\r\n');
            tcpSocket.write(JSON.stringify(json)+'\r\n');
            //console.log('Sending in: '+action+'\r\n'+JSON.stringify(json)+'\r\n');
        });
    tcpSocket.on('data', function (data) {
        bigData += data;
    });
    tcpSocket.on('end', function () {
        json = JSON.parse(bigData);
        e = json.emit;
        b = json.broadcast;

        items = b.data;
        for(u in items)
        {
            if(typeof itemsShadow[u] == 'undefined' ||
               typeof itemsShadow[u].x == 'undefined' ||
               typeof itemsShadow[u].y == 'undefined' ||
               items[u].x != itemsShadow[u].x ||
               items[u].y != itemsShadow[u].y ||
               items[u].hp != itemsShadow[u].hp ||
               items[u].map_index != itemsShadow[u].map_index)
            {
                socket.emit('item-update', [ items[u] ]);
                socket.broadcast.emit('item-update', [ items[u] ]);
            }
        }
    });

    setTimeout(function() { itemSync(socket); }, updateUnitInterval);
}

var app = require('http').createServer(handler)
, io = require('socket.io')(app)
, fs = require('fs')

app.listen(listenOn);

// Maybe process not app
app.on('uncaughtException', function (err)
       {
           console.error(err.stack);
           console.log("Node NOT Exiting...");
       });

function handler (req, res)
{
    fs.readFile(__dirname + '/index.html',
                function (err, data) {
                    if(err) {
                        res.writeHead(500);
                        return res.end('Error loading index.html');
                    }

                    res.writeHead(200);
                    res.end(data);
                });
}

//What happens on connection/connected events?
io.sockets.on('connection', function (socket) {
    try
    {
        console.log('connected');
        var webSocket = socket;

        setTimeout(function() { npcUnitSync(socket) }, updateUnitInterval);
        setTimeout(function() { itemSync(socket) }, updateUnitInterval);

        //Someone is joining the game, yay
        socket.on('join-game', function (data) {
            console.log('received join game');
            tcpRequest(socket, 'join-game', data);
        });

        //Someone wants to do an action
        socket.on('set-action', function (data) {
            tcpRequest(socket, 'set-action', data);
        });

        socket.on('request-zone-change', function(data) {
            tcpRequest(socket, 'request-zone-change', data);
        });

        socket.on('draw-card', function (data) {
            tcpRequest(socket, 'draw-card', data);
        });

        // Card related
        socket.on('request-cards', function (data) {
            tcpRequest(socket, 'request-cards', data);
        });

        socket.on('request-hand', function (data) {
            tcpRequest(socket, 'request-hand', data);
        });

        socket.on('request-deck', function (data) {
            tcpRequest(socket, 'request-deck', data);
        });

        socket.on('request-chat', function (data) {
            tcpRequest(socket, 'request-chat', data);
        });

        socket.on('request-signin', function (data) {
            data.client_ip = socket.handshake.address.address;
            tcpRequest(socket, 'request-signin', data);
        });

        socket.on('request-change-job', function (data) {
            tcpRequest(socket, 'request-change-job', data);
        });

        socket.on('request-jobs', function (data) {
            tcpRequest(socket, 'request-jobs', data);
        });

        socket.on('request-give-card', function (data) {
            tcpRequest(socket, 'request-give-card', data);
        });

        socket.on('request-map', function (data) {
            tcpRequest(socket, 'request-map', data);
        });

        socket.on('request-move', function (data) {
            tcpRequest(socket, 'request-move', data);
        });

        socket.on('request-event-log', function (data) {
            tcpRequest(socket, 'request-event-log', data);
        });

        socket.on('request-stairs', function (data) {
            tcpRequest(socket, 'request-stairs', data);
        });

        socket.on('request-reset', function (data) {
            tcpRequest(socket, 'request-reset', data);
        });
        socket.on('request-resync', function (data) {
            tcpRequest(socket, 'request-resync', data);
        });
        socket.on('request-item-resync', function (data) {
            tcpRequest(socket, 'request-item-resync', data);
        });
        socket.on('request-item-pickup', function (data) {
            tcpRequest(socket, 'request-item-pickup', data);
        });
        socket.on('request-item-owned', function (data) {
            tcpRequest(socket, 'request-item-owned', data);
        });
        socket.on('request-item-equip', function (data) {
            tcpRequest(socket, 'request-item-equip', data);
        });
        socket.on('request-item-unequip', function (data) {
            tcpRequest(socket, 'request-item-unequip', data);
        });
    }
    catch (e)
    {
        console.log(e);
    }
    finally
    {
    }
});
