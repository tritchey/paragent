/*
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/ 

function alreadySelected(comp, selected) {
	var hash = selected.computerHash;
	return (hash[comp.id] == true);
}

function makeFilter (fid, aid, sid) {
    filterAvailable(fid, aid, sid);
    return function () { filterAvailable(fid, aid, sid); };
}


function filterAvailable (fid, aid, sid) {
    var filter = $(fid);
    var available = $(aid);
    var selected = $(sid);
    var tag = filter.options[filter.selectedIndex].text;
	updateComputerHash(selected);

    // clear the available computer list
    available.options.length = 0;

	// first, check to make sure it is not already being used
	var computers = tagsToComputers[tag];
	for (var j=0; j < computers.length; ++j) {
		var curComp = computers[j];

		if(!alreadySelected(curComp, selected)) {
			available.options[available.options.length] = new Option(curComp.name, curComp.id);
		}
	}

   
}

function updateComputerList(clid, fid) {
    var selected = $(fid);
    var computerList = $(clid);
    var length = selected.length;
    var options = selected.options;
    computerList.value = "";
    var newList = "";
    for(var i = 0; i < length; i++) {
		newList += options[i].value + " ";
    }
    computerList.value = newList;
}

function updateComputerHash(selectedId) {
	var selected = $(selectedId);
	var options = selected.options;
	var computerHash = $H(allComputers);

	
	for (var i=0; i <options.length; ++i) {
		var cur = options[i];
		computerHash[cur.value] = true;
	}
	selected.computerHash = computerHash;
}

function moveSelection (fid, tid) {
    var from = $(fid);
    var to = $(tid);
    var length = from.length;
    var options = from.options;

    for(var i = 0; i < length; i++) {
	var current = options[i];
	if(current) {
	    if(current.selected) {
			var txt = current.text;
			var val = current.value;
			to.options[to.length] = new Option(txt,val);
			options[i] = null;
			i--;
			length--;
		    }
		}
    }
}

function moveAllSelection (fid, tid) {
    var from = $(fid);
    var to = $(tid);
    var length = from.length;
    var options = from.options;

    for(var i = 0; i < length; ++i) {
		var current = options[i];
		if(current) {
		    var txt = current.text;
		    var val = current.value;
		    to.options[to.length] = new Option(txt,val);
		    options[i] = null;
		    i--;
		    length--;
		}
    }
}

function addSelection (fid, tid, clid) {
    moveSelection(fid, tid);
    updateComputerList(clid, tid);
	updateComputerHash(tid);
}

function addAllSelection (fid, tid, clid) {
    moveAllSelection(fid, tid);
    updateComputerList(clid, tid);
	updateComputerHash(tid);
}

function delSelection (tid, fid, clid) {
    moveSelection(fid, tid);
    updateComputerList(clid, fid);
	updateComputerHash(fid);
}

function delAllSelection (tid, fid, clid) {
    moveAllSelection(fid, tid);
    updateComputerList(clid, fid);
	updateComputerHash(fid);
}

