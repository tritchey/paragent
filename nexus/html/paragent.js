/*
Copyright (c) 2006 - 2007, Paragent, LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of Paragent, LLC nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
function initCalendar(monthId, dayId, yearId, button, divId, calendar) {
	button = $(button);
	
}

function toggleCalendar(monthId, dayId, yearId, buttonId, divId, calendar) {
	button = $(buttonId)
	calendarDiv = $(divId);
	
	var btnPosition = Position.cumulativeOffset(button);
	var btnBottom =  btnPosition[1] + button.getHeight();

	Position.absolutize(calendarDiv);
	calendarDiv.style.left = btnPosition[0] + "px";
	calendarDiv.style.top = btnBottom + "px";
	
	Element.toggle(calendarDiv);
	
	if (button.calInitialized != true) {
		setTimeout(function() {
			var datePicker = dojo.widget.createWidget('DatePicker', null, dojo.byId(calendar));
			{
				button.calInitialized = true;
				var month = dojo.byId(monthId);
				var day = dojo.byId(dayId);
				var year = dojo.byId(yearId);
				var syncDate = function () {
					var date = new Date;
					var m = parseInt(month.value);
					var d = parseInt(day.value);
					var y = year.value;
					if (y.length <= 2 && y.length > 0) {
						y = '20' + y;
					}
					y = parseInt(y);
					if (!(isNaN(m) || isNaN(d) || isNaN(y))) {
						date.setMonth(m - 1);
						date.setDate(d);
						date.setFullYear(y);
						datePicker.setDate(date);
					}
				};
				syncDate();
				dojo.event.connect(month, 'onchange', syncDate);
				dojo.event.connect(day, 'onchange', syncDate);
				dojo.event.connect(year, 'onchange', syncDate);
			}
			dojo.event.connect(datePicker, 'setDate', function (rfcDate) {
				var date = datePicker.getDate();
				var month = date.getMonth() + 1;
				var day = date.getDate();
				var year = date.getFullYear();
				if (year < 2000) {
					year += 1900;
				};
				++date;
				$(monthId).value = month;
				$(dayId).value = day;
				$(yearId).value = year;
			});
		}, 50);
	}
}


function showMenu(caller, menuId) {
	var menu = $(menuId);
	if (menu != null) {
		if (caller != null) {
			caller = $(caller);
	
			var callerPosition = Position.cumulativeOffset(caller);
			var callerBottom =  callerPosition[1] + caller.getHeight();
	
	
			menu.style.left = callerPosition[0] + "px";
			menu.style.top = callerBottom + "px";
		}
	
		Element.show(menu);
	}
}

function hideMenu(caller, menu) {
	menu = $(menu);
	if (menu != null) {
		Element.hide(menu);
	}
}


function searchTooltip(caller, tipId, search) {
	var tip = $(tipId);
	if (tip != null) {
		if (tip.lastCaller	!= caller || !Element.visible(tip)) {
			searchControl.execute(search);
			tooltip(caller, tip);
		}
	}
}

function tooltip(caller, tipId) {
	var tip = $(tipId);
	if (tip != null) {
		if (tip.lastCaller != caller || !Element.visible(tip)) {
			var pos = Position.cumulativeOffset(caller);


			//	Position.absolutize(tip);
			//Position.absolutize(caller);
			tip.style.left = pos[0] + caller.offsetWidth + "px";
			//	tip.style.top = pos[1] + "px";
		

			var callerTop = Position.positionedOffset(caller)[1];
		
			//alert (callerTop + "," + Position.realOffset(caller)[1] + "," + Position.positionedOffset(caller)[1]); 
			//Position.relativize(caller);
			tip.style.top = callerTop + "px";
			var windowBottom = getWindowSize().height + getWindowOffsets().y;
		
	//		Element.show(tip); // The element doesn't have height until it is shown
			var tipBottom = Position.positionedOffset(tip)[1] + tip.getHeight();
			if (tipBottom > (windowBottom - 20)) {
				tip.style.top = ((getWindowOffsets().y + getWindowSize().height) - (tip.getHeight() + 20)) + "px";
			}
	//		Element.hide(tip);
			Element.show(tip);
			tip.lastCaller = caller;
		}
	}
}

function keepTooltip(tipId) {
	var tip = $(tipId);
	if (tip != null) {
		Element.show(tip);
	}
}

function hideTooltip(tipId) {
	var tip = $(tipId);
	if (tip != null) {
		Element.hide(tip);
	}
}

function collapsingBox(contents, img, scrollSpeed) {
	var c = $(contents);
	if (Element.visible(c)) {
		$(img).src='images/collapsed.gif';
		if (scrollSpeed == 0) {
			Effect.Fade(c, {duration: 0.5});
		} else {
			Effect.SlideUp(c, {duration: scrollSpeed});
		}
	} else {
		$(img).src='images/expanded.gif';
		if (scrollSpeed == 0) {
			//Element.show(c);
			Effect.Appear(c, {duration: 0.5});
		} else {
			Effect.SlideDown(c, {duration: scrollSpeed});
		}
	}
	return false;
}

function showAdvanced(advancedId, buttonId) {
	var advanced = $(advancedId);
	var btn = $(buttonId);
	if (Element.visible(advanced)) {
		Effect.SlideUp(advanced, {duration : 0.3, afterFinish : function() {
			btn.innerHTML = "Advanced";
		}});
	} else {
		Effect.SlideDown(advancedId, {duration : 0.3, afterFinish : function() {
			btn.innerHTML = "Hide Advanced";
		}});
	}
}


function flashMessage(message) {
	var el=document.createElement('div');
	var bt=document.createElement('div');
	var h=document.createElement('h2');
	var b=document.createElement('div');
	var i=document.createElement('img');

	el.id = 'flash-message';
	el.className = 'flash-message';

	bt.className = 'box-title';
	b.className = 'box';
	i.src = '/images/ajax-spinner.gif';

	h.innerHTML = message;
	bt.appendChild(h);
	b.appendChild(i);
	el.appendChild(bt);
	el.appendChild(b);

	var window_width=0, window_height=0;
	if (self.innerHeight) {	
		window_width = self.innerWidth;
		window_height = self.innerHeight;
	} else if (document.documentElement && document.documentElement.clientHeight) { 
		window_width = document.documentElement.clientWidth;
		window_height = document.documentElement.clientHeight;
	} else if (document.body) { 
		window_width = document.body.clientWidth;
		window_height = document.body.clientHeight;
	}
	
	var yOffset=0, xOffset=0;
	if (typeof(window.pageYOffset) == 'number') {
		// Netscape Firefox
		yOffset = window.pageYOffset;
		xOffset = window.pageXOffset;
	} else if (document.body && (document.body.scrollLeft || document.body.scrollTop)) {
		// DOM compliant
		yOffset = document.body.scrollTop;
		xOffset = document.body.scrollLeft;
	} else if (document.documentElement && ( document.documentElement.scrollLeft || document.documentElement.scrollTop)) {
		// IE 6 standards compliant mode
		yOffset = document.documentElement.scrollTop;
		xOffset = document.documentElement.scrollLeft;
	}

	document.body.appendChild(el);
	Position.absolutize(el);
	el.style.top = (yOffset + window_height/2)  + 'px';
	el.style.left = (xOffset + window_width/2)  +  'px';
	return true;
}


function centerY(el) {
	var wh = getWindowSize();
	var window_width=wh.width, window_height=wh.height;
	
	var xy = getWindowOffsets();
	var xOffset = xy.x, yOffset = xy.y;
	
	var elHeight = Element.getHeight(el);
	el.style.top = (yOffset + (window_height/2 - elHeight/2))  + 'px';
}

function getWindowSize() {
	var window_width=0, window_height=0;
	if (self.innerHeight) {	
		window_width = self.innerWidth;
		window_height = self.innerHeight;
	} else if (document.documentElement && document.documentElement.clientHeight) { 
		window_width = document.documentElement.clientWidth;
		window_height = document.documentElement.clientHeight;
	} else if (document.body) { 
		window_width = document.body.clientWidth;
		window_height = document.body.clientHeight;
	}
	
	return {width: window_width, height: window_height};
}

function getWindowOffsets() {
	var yOffset=0, xOffset=0;
	if (typeof(window.pageYOffset) == 'number') {
		// Netscape Firefox
		yOffset = window.pageYOffset;
		xOffset = window.pageXOffset;
	} else if (document.body && (document.body.scrollLeft || document.body.scrollTop)) {
		// DOM compliant
		yOffset = document.body.scrollTop;
		xOffset = document.body.scrollLeft;
	} else if (document.documentElement && ( document.documentElement.scrollLeft || document.documentElement.scrollTop)) {
		// IE 6 standards compliant mode
		yOffset = document.documentElement.scrollTop;
		xOffset = document.documentElement.scrollLeft;
	}
	
	return {x: xOffset, y: yOffset};
}



Object.extend(Array.prototype, {
	removeItem: function (element) {
		var result = false;
		var array = [];
		for (var i = 0; i < this.length; i++) {
			if (this[i] == element) {
				result = true;
			} else {
				array.push(this[i]);
			}
		}
		this.clear();
		for (var i = 0; i < array.length; i++) {
			this.push(array[i]);
		}
		array = null;
		return result;
	}
});

