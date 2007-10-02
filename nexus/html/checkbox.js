/*
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
//global variables that can be used by ALL the functions on this page.
var selectedComputers = new Object();
var selectedComputersLength = 0;

function adjustSelectedComputers() {
    var object1 = document.getElementById(inputId1);
    object1.value = "";
    selectedComputersLength = 0;
    for(computer in selectedComputers) {
	object1.value += computer + " ";
	selectedComputersLength++;
    }
}

function selectComputer(object, id) {
    var classes = object.className;
    var i = classes.indexOf('selected');
    if(i == -1) {
	// the item is currently not selected;
	object.className += " selected";
	selectedComputers[id] = object;
	if(selectedComputersLength == 0) {
	    Effect.SlideDown('batch-action-bar', {duration: 0.25});
	    Effect.Appear('ab-contents');
	}
	adjustSelectedComputers();
    } else {
		object.className = classes.substring(0, i);
		delete selectedComputers[id];
		adjustSelectedComputers();
	if(selectedComputersLength == 0) {
	    Effect.Fade('ab-contents', {duration: 0.25});
	    Effect.SlideUp('batch-action-bar', {duration: 0.25});
	}
    }
}

function selectAllComputers(object) {
    var classes = object.className;
    var i = classes.indexOf('selected');
    if(i == -1) {
	// the item is currently not selected;
	object.className += " selected";
	var a = document.getElementsByTagName('a');
	for(var j = 0; j < a.length; j++) {
	    var classes = a[j].className;
	    	if(classes.indexOf('checkbox') != -1 && classes.indexOf('all') == -1) {
			var k = classes.indexOf('selected');
			if(k == -1) {
			    a[j].onclick();
			}
	    }
	}    
	object.innerHTML = "deselect all";
    } else {
		object.className = classes.substring(0, i);
		var a = document.getElementsByTagName('a');
		for(var j = 0; j < a.length; j++) {
		    var classes = a[j].className;
		    if(classes.indexOf('checkbox') != -1 && classes.indexOf('all') == -1) {
				var k = classes.indexOf('selected');
				if(k != -1) {
				    a[j].onclick();
				}
		    }
		}
	object.innerHTML = "select all";
    }

}
