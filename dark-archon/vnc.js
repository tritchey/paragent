/*
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


var screen_width = 0;
var screen_height = 0;

function writeToConsole(string) {
    var newP = document.createElement('li');
    newP.innerHTML = string;
    $('#console').append(newP);
}

function protocolVersion(data) {
    var response = data.binaryResponse;
    var protocol = response.getStringAt(0, response.getLength());
    if(protocol == "RFB 003.008\n") {
	writeToConsole("Protocol: " + protocol);
    } else {
	writeToConsole("Unknown Protocol Version: " + protocol);
    }
    server.send("RFB 003.008" + 0, securityVersion, error);
};

function securityVersion(data) {
    var response = data.binaryResponse;
    var num_sec = response.getByteAt(0);
    
    writeToConsole("Number of Security Types: " + num_sec);

    var i = 1;
    for(i = 1; i <= num_sec; i++) {
	var type = response.getByteAt(i);
	if(type == 0) {
	    writeToConsole("Security: Invalid");
	    writeToConsole("Ending VNC Session");
	} else if(type == 1) {
	    writeToConsole("Security: None");
	    server.send("\1", securityResponse, error);
	}
    }
};

function securityResponse(data) {
    var response = data.binaryResponse;
    var sec_res = response.getLongAt(0);
    if(sec_res == 0) {
	writeToConsole("Security Response: OK");
	// send the client init
	server.send("\1", serverInit, error);
    } else if(sec_res == 1) {
	writeToConsole("Security Response: Failed");
    } else {
	writeToConsole("Security Response: Unknown");
    }
}

function serverInit(data) {
    var response = data.binaryResponse;
    screen_width = response.getWordAt(0, true);
    screen_height = response.getWordAt(2, true);

    var bits_per_pixel = response.getByteAt(4);
    var depth = response.getByteAt(5);
    var big_endian_flag = response.getByteAt(6);
    var true_color_flag = response.getByteAt(7);
    var red_max = response.getWordAt(8, true);
    var green_max = response.getWordAt(10, true);
    var blue_max = response.getWordAt(12, true);
    var red_shift = response.getByteAt(14);
    var green_shift = response.getByteAt(15);
    var blue_shift = response.getByteAt(16);

    var name_length = response.getLongAt(20, true);
    var name = response.getStringAt(24, name_length);
    
    writeToConsole("Server Name: " + name);
    writeToConsole("Screen Dimentions: " + screen_width + " X " + screen_height);
    writeToConsole("Bits Per Pixel: " + bits_per_pixel);
    writeToConsole("Depth: " + depth);
    writeToConsole("Big Endian: " + big_endian_flag);
    writeToConsole("True Color: " + true_color_flag);
    writeToConsole("Red Max: " + red_max);
    writeToConsole("Green Max: " + green_max);
    writeToConsole("Blue Max: " + blue_max);
    writeToConsole("Red Shift: " + red_shift);
    writeToConsole("Green Shift: " + green_shift);
    writeToConsole("Blue Shift: " + blue_shift);

    frameBufferUpdate();
}

function frameBufferUpdate() {
    server.send("\3\1\0\0\0\0\
}

function error() {
    writeToConsole("Error in VNC Protocol");
}

var server = new BinaryAJAX("/vnc");

function connect() {
    server.get(protocolVersion, error);
}

